setup_environment <- new.env(parent = globalenv())
sys.source(
  testthat::test_path("..", "..", "prepare_environment.R"),
  envir = setup_environment
)

test_that("Ubuntu release metadata is parsed", {
  path <- tempfile(fileext = ".txt")
  writeLines(
    c(
      "NAME=Ubuntu",
      "ID=ubuntu",
      "ID_LIKE=debian",
      "PRETTY_NAME=\"Ubuntu Teste\""
    ),
    path
  )

  release <- setup_environment$read_os_release(path)
  expect_equal(release$ID, "ubuntu")
  expect_equal(release$ID_LIKE, "debian")
  expect_equal(release$PRETTY_NAME, "Ubuntu Teste")
  expect_silent(setup_environment$validate_operating_system(release))
})

test_that("unsupported operating systems receive a useful error", {
  expect_error(
    setup_environment$validate_operating_system(list(
      ID = "fedora",
      ID_LIKE = "rhel",
      PRETTY_NAME = "Fedora Teste"
    )),
    "Ubuntu e Debian"
  )
})

test_that("project metadata drives dependency and version checks", {
  project <- normalizePath(
    testthat::test_path("..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
  description <- file.path(project, "DESCRIPTION")
  lockfile <- file.path(project, "renv.lock")
  imports <- setup_environment$description_imports(description)

  expect_equal(setup_environment$find_project_root(testthat::test_path()), project)
  expect_true(all(c("datasus", "pak", "shiny") %in% imports))
  expect_equal(
    setup_environment$description_remote("datasus", description),
    "rpradosiqueira/datasus@v0.16.1"
  )
  expect_equal(as.character(setup_environment$minimum_r_version(description)), "4.1.0")
  expect_equal(setup_environment$lockfile_r_version(lockfile), "4.6.1")
})

test_that("setup arguments are validated before installing anything", {
  project <- testthat::test_path("..", "..")
  expect_error(
    setup_environment$prepare_environment(project = project, args = "--desconhecida"),
    "Opção desconhecida"
  )
})

test_that("fallback system requirements cover native spatial packages", {
  packages <- setup_environment$fallback_system_packages()
  expect_true(all(c(
    "libgdal-dev",
    "libgeos-dev",
    "libproj-dev",
    "libudunits2-dev"
  ) %in% packages))
  expect_length(setup_environment$install_debian_packages(character()), 0L)
})
