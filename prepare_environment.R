SETUP_CRAN <- "https://cloud.r-project.org"

setup_message <- function(step, text) {
  message(sprintf("[%s] %s", step, text))
}

setup_abort <- function(text) {
  stop(paste0("\n", text), call. = FALSE)
}

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    required <- file.path(current, c("DESCRIPTION", "renv.lock", "app.R"))
    if (all(file.exists(required))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  setup_abort(
    "Não encontrei a pasta do projeto. Abra um terminal dentro de `projeto_data_sus` e tente novamente."
  )
}

setup_start_directory <- function() {
  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_argument) == 0L) return(getwd())
  script <- sub("^--file=", "", file_argument[[1L]])
  dirname(normalizePath(script, winslash = "/", mustWork = FALSE))
}

read_os_release <- function(path = "/etc/os-release") {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  lines <- lines[grepl("^[A-Za-z_][A-Za-z0-9_]*=", lines)]
  keys <- sub("=.*$", "", lines)
  values <- sub("^[^=]*=", "", lines)
  values <- sub("^[\"']", "", values)
  values <- sub("[\"']$", "", values)
  as.list(stats::setNames(values, keys))
}

validate_operating_system <- function(os_release = read_os_release()) {
  id <- tolower(os_release$ID %||% "")
  related <- tolower(os_release$ID_LIKE %||% "")
  supported <- id %in% c("ubuntu", "debian") || grepl("debian", related, fixed = TRUE)
  if (!supported) {
    setup_abort(
      paste(
        "Este preparador automático foi feito para Ubuntu e Debian.",
        "Sistema detectado:", os_release$PRETTY_NAME %||% "não identificado"
      )
    )
  }
  invisible(os_release)
}

description_imports <- function(path = "DESCRIPTION") {
  description <- read.dcf(path)
  imports <- strsplit(description[[1L, "Imports"]], ",", fixed = TRUE)[[1L]]
  imports <- trimws(sub("\\s*\\(.*\\)$", "", imports))
  unique(imports[nzchar(imports)])
}

description_remote <- function(package, path = "DESCRIPTION") {
  description <- read.dcf(path)
  if (!"Remotes" %in% colnames(description)) return(package)
  remotes <- trimws(strsplit(description[[1L, "Remotes"]], ",", fixed = TRUE)[[1L]])
  match <- remotes[grepl(paste0("/", package, "(?:@|$)"), remotes, perl = TRUE)]
  if (length(match) == 0L) package else match[[1L]]
}

minimum_r_version <- function(path = "DESCRIPTION") {
  depends <- read.dcf(path)[[1L, "Depends"]]
  match <- regexec("R\\s*\\(>=\\s*([0-9.]+)\\)", depends, perl = TRUE)
  pieces <- regmatches(depends, match)[[1L]]
  if (length(pieces) < 2L) return(package_version("0"))
  package_version(pieces[[2L]])
}

lockfile_r_version <- function(path = "renv.lock") {
  lines <- readLines(path, n = 12L, warn = FALSE)
  index <- grep('"Version"\\s*:', lines)
  if (length(index) == 0L) return(NA_character_)
  sub('.*"Version"\\s*:\\s*"([^"]+)".*', "\\1", lines[index[[1L]]])
}

validate_r_version <- function(description = "DESCRIPTION", lockfile = "renv.lock") {
  minimum <- minimum_r_version(description)
  current <- package_version(as.character(getRversion()))
  if (current < minimum) {
    setup_abort(
      paste0(
        "Sua versão do R é ", current, ", mas o projeto precisa do R ", minimum,
        " ou mais recente. Atualize o R antes de continuar."
      )
    )
  }

  locked <- lockfile_r_version(lockfile)
  if (!is.na(locked) && as.character(current) != locked) {
    message(
      "  Observação: o ambiente foi registrado no R ", locked,
      ", e você está usando o R ", current, ". O renv verificará a compatibilidade."
    )
  }
  invisible(current)
}

command_available <- function(command) {
  nzchar(Sys.which(command))
}

running_as_root <- function() {
  if (!command_available("id")) return(FALSE)
  uid <- suppressWarnings(system2("id", "-u", stdout = TRUE, stderr = FALSE))
  length(uid) > 0L && identical(trimws(uid[[1L]]), "0")
}

debian_package_installed <- function(package) {
  status <- suppressWarnings(system2(
    "dpkg-query",
    c("-W", package),
    stdout = FALSE,
    stderr = FALSE
  ))
  identical(status, 0L)
}

run_setup_command <- function(command, arguments, description) {
  status <- system2(command, arguments)
  if (!identical(status, 0L)) {
    setup_abort(paste0("Não foi possível ", description, ". Verifique as mensagens acima."))
  }
  invisible(TRUE)
}

apt_command <- function(arguments) {
  if (running_as_root()) return(list(command = "apt-get", arguments = arguments))
  if (!command_available("sudo")) {
    setup_abort("O comando `sudo` não está disponível. Peça ajuda ao administrador do computador.")
  }
  list(command = "sudo", arguments = c("apt-get", arguments))
}

authorize_sudo <- function() {
  if (running_as_root()) return(invisible(TRUE))
  message("  O Ubuntu pode pedir a senha do computador. Ao digitar, os caracteres não aparecem na tela.")
  status <- system2("sudo", "-v")
  if (!identical(status, 0L)) {
    setup_abort("Não foi possível obter permissão de administrador com `sudo`.")
  }
  invisible(TRUE)
}

install_debian_packages <- function(packages, check_only = FALSE) {
  packages <- sort(unique(packages[nzchar(packages)]))
  if (length(packages) == 0L) return(character())
  if (!command_available("apt-get") || !command_available("dpkg-query")) {
    setup_abort("Os comandos `apt-get` e `dpkg-query` são necessários no Ubuntu/Debian.")
  }

  missing <- packages[!vapply(packages, debian_package_installed, logical(1))]
  if (length(missing) == 0L) {
    message("  Bibliotecas do Ubuntu/Debian: tudo certo.")
    return(character())
  }
  if (check_only) {
    message("  Bibliotecas não registradas pelo APT: ", paste(missing, collapse = ", "))
    return(missing)
  }

  authorize_sudo()
  update <- apt_command(c("update"))
  run_setup_command(update$command, update$arguments, "atualizar a lista de programas do Ubuntu")
  install <- apt_command(c("install", "-y", "--no-install-recommends", missing))
  run_setup_command(install$command, install$arguments, "instalar as bibliotecas do Ubuntu")
  missing[!vapply(missing, debian_package_installed, logical(1))]
}

bootstrap_system_packages <- function() {
  c("build-essential", "ca-certificates", "curl", "git", "pandoc")
}

fallback_system_packages <- function() {
  c(
    "cmake", "libabsl-dev", "libcurl4-openssl-dev", "libgdal-dev",
    "libgeos-dev", "libicu-dev", "liblz4-dev", "libpng-dev",
    "libproj-dev", "libsqlite3-dev", "libssl-dev", "libtbb-dev",
    "libudunits2-dev", "libuv1-dev", "libxml2-dev", "libzstd-dev",
    "xz-utils", "zlib1g-dev"
  )
}

ensure_r_package <- function(package) {
  if (requireNamespace(package, quietly = TRUE)) return(invisible(TRUE))
  message("  Instalando o preparador `", package, "`...")
  utils::install.packages(package, repos = SETUP_CRAN, dependencies = NA)
  if (!requireNamespace(package, quietly = TRUE)) {
    setup_abort(paste0("O pacote `", package, "` não pôde ser instalado."))
  }
  invisible(TRUE)
}

resolve_system_packages <- function(imports) {
  references <- vapply(
    imports,
    function(package) description_remote(package),
    character(1),
    USE.NAMES = FALSE
  )
  requirements <- tryCatch(
    pak::pkg_sysreqs(references, upgrade = FALSE, dependencies = NA),
    error = function(error) error
  )
  if (inherits(requirements, "error")) {
    warning(
      "Não foi possível consultar a lista dinâmica de bibliotecas; usando a lista segura incluída no projeto. ",
      conditionMessage(requirements),
      call. = FALSE
    )
    return(fallback_system_packages())
  }
  resolved <- unlist(requirements$packages$system_packages, use.names = FALSE)
  sort(unique(c(fallback_system_packages(), resolved)))
}

configure_installation <- function() {
  cores <- parallel::detectCores(logical = TRUE)
  if (is.na(cores)) cores <- 2L
  jobs <- max(1L, min(4L, cores - 1L))
  options(
    repos = c(CRAN = SETUP_CRAN),
    timeout = max(1200, getOption("timeout", 60)),
    Ncpus = jobs
  )
  Sys.setenv(
    RENV_CONFIG_CONNECT_TIMEOUT = "120",
    RENV_CONFIG_CONNECT_RETRY = "3",
    RENV_CONFIG_INSTALL_JOBS = as.character(jobs)
  )
  invisible(jobs)
}

restore_r_packages <- function(project) {
  tryCatch(
    renv::restore(
      project = project,
      lockfile = file.path(project, "renv.lock"),
      prompt = FALSE,
      clean = FALSE,
      retry = FALSE,
      transactional = TRUE
    ),
    error = function(error) {
      setup_abort(
        paste(
          "A instalação dos pacotes R não terminou.",
          "O ambiente anterior foi preservado.",
          "Detalhe:", conditionMessage(error)
        )
      )
    }
  )
}

locked_packages_match <- function(project, imports) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(FALSE)
  installed <- vapply(imports, requireNamespace, logical(1), quietly = TRUE)
  if (!all(installed)) return(FALSE)

  lock <- jsonlite::read_json(file.path(project, "renv.lock"), simplifyVector = FALSE)
  all(vapply(imports, function(package) {
    expected <- lock$Packages[[package]]$Version %||% NA_character_
    if (is.na(expected)) return(FALSE)
    actual <- as.character(utils::packageVersion(package))
    package_version(actual) == package_version(expected)
  }, logical(1)))
}

verify_locked_packages <- function(project, imports) {
  missing <- imports[!vapply(imports, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    setup_abort(paste("Pacotes R ausentes após a preparação:", paste(missing, collapse = ", ")))
  }

  lock <- jsonlite::read_json(file.path(project, "renv.lock"), simplifyVector = FALSE)
  mismatches <- vapply(imports, function(package) {
    expected <- lock$Packages[[package]]$Version %||% NA_character_
    actual <- as.character(utils::packageVersion(package))
    !is.na(expected) && package_version(actual) != package_version(expected)
  }, logical(1))
  if (any(mismatches)) {
    packages <- imports[mismatches]
    details <- vapply(packages, function(package) {
      expected <- lock$Packages[[package]]$Version
      actual <- as.character(utils::packageVersion(package))
      paste0(package, " (instalado ", actual, "; esperado ", expected, ")")
    }, character(1))
    setup_abort(paste("Versões incompatíveis:", paste(details, collapse = ", ")))
  }

  status <- renv::status(project = project)
  if (!isTRUE(status$synchronized)) {
    setup_abort("O ambiente R foi instalado, mas não ficou sincronizado com `renv.lock`.")
  }
  invisible(TRUE)
}

prepare_environment <- function(
  project = find_project_root(setup_start_directory()),
  args = commandArgs(trailingOnly = TRUE)
) {
  check_only <- "--check-only" %in% args
  unknown <- setdiff(args, "--check-only")
  if (length(unknown) > 0L) {
    setup_abort(paste("Opção desconhecida:", paste(unknown, collapse = ", ")))
  }

  project <- normalizePath(project, winslash = "/", mustWork = TRUE)
  previous_directory <- getwd()
  on.exit(setwd(previous_directory), add = TRUE)
  setwd(project)

  setup_message("1/6", "Verificando Ubuntu/Debian e a versão do R")
  os_release <- validate_operating_system()
  validate_r_version()
  message("  Sistema: ", os_release$PRETTY_NAME)
  message("  R: ", getRversion())

  setup_message("2/6", "Configurando downloads e instalação")
  jobs <- configure_installation()
  message("  Até ", jobs, " instalação(ões) em paralelo.")

  imports <- unique(c(description_imports(), "renv", "pak"))
  environment_ready <- locked_packages_match(project, imports)
  if (check_only) {
    setup_message("3/6", "Conferindo bibliotecas do Ubuntu/Debian sem alterar o computador")
    check_packages <- fallback_system_packages()
    if (requireNamespace("pak", quietly = TRUE)) {
      check_packages <- c(
        check_packages,
        resolve_system_packages(setdiff(imports, c("renv", "pak")))
      )
    }
    missing_system <- install_debian_packages(
      c(bootstrap_system_packages(), check_packages),
      check_only = TRUE
    )
    if (length(missing_system) > 0L) {
      if (environment_ready) {
        message("  O ambiente R atual já fornece todos os componentes carregáveis necessários.")
      } else {
        message("  A preparação completa tentará instalar essas bibliotecas.")
      }
    }
  } else if (environment_ready) {
    setup_message("3/6", "Reaproveitando o ambiente já instalado")
    message("  Pacotes R e bibliotecas carregáveis já estão nas versões corretas.")
  } else {
    setup_message("3/6", "Instalando bibliotecas necessárias do Ubuntu/Debian")
    unresolved <- install_debian_packages(bootstrap_system_packages())
    if (length(unresolved) > 0L) {
      setup_abort(paste("Bibliotecas básicas não instaladas:", paste(unresolved, collapse = ", ")))
    }
  }

  setup_message("4/6", "Preparando os instaladores de pacotes R")
  if (!check_only && !environment_ready) {
    ensure_r_package("renv")
    ensure_r_package("pak")
    system_packages <- resolve_system_packages(setdiff(imports, c("renv", "pak")))
    unresolved <- install_debian_packages(system_packages)
    if (length(unresolved) > 0L) {
      setup_abort(paste("Bibliotecas do sistema não instaladas:", paste(unresolved, collapse = ", ")))
    }
  }

  setup_message("5/6", if (check_only) "Conferindo pacotes R" else "Restaurando as versões corretas dos pacotes R")
  if (!check_only && !environment_ready) restore_r_packages(project)
  verify_locked_packages(project, imports)

  setup_message("6/6", "Validando o aplicativo")
  source("global.R", local = environment(), chdir = TRUE)
  missing_runtime <- check_runtime_dependencies()
  if (length(missing_runtime) > 0L) {
    setup_abort(paste("Dependências do aplicativo ausentes:", paste(missing_runtime, collapse = ", ")))
  }
  invisible(app_ui())

  message("\nTudo pronto! O ambiente do Projeto Data SUS está funcionando.")
  message("Para abrir o sistema, execute:")
  message("  Rscript -e 'shiny::runApp(\".\", launch.browser = TRUE)'\n")
  invisible(TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

if (sys.nframe() == 0L) {
  prepare_environment()
}
