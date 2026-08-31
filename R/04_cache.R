cache_file_path <- function(namespace, key) {
  directory <- file.path(cache_directory(), normalize_text(namespace))
  ensure_directory(directory)
  hash <- digest::digest(key, algo = "sha256", serialize = TRUE)
  file.path(directory, paste0(hash, ".rds"))
}

cache_is_fresh <- function(path, max_age) {
  if (!file.exists(path)) return(FALSE)
  age <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "secs"))
  is.finite(age) && age <= max_age
}

cached_call <- function(namespace, key, function_to_run, max_age = 86400, refresh = FALSE) {
  path <- cache_file_path(namespace, key)
  cached <- if (file.exists(path)) {
    tryCatch(readRDS(path), error = function(error) NULL)
  } else {
    NULL
  }
  if (!isTRUE(refresh) && cache_is_fresh(path, max_age) && !is.null(cached)) {
    return(cached)
  }

  source_error <- NULL
  value <- tryCatch(
    function_to_run(),
    error = function(error) {
      source_error <<- error
      NULL
    }
  )
  if (!is.null(source_error)) {
    if (is.null(cached)) stop(source_error)
    cache_time <- file.info(path)$mtime
    warning(
      "A fonte está indisponível; usando o resultado local de ",
      format(cache_time, "%d/%m/%Y %H:%M"), ". Motivo: ",
      conditionMessage(source_error),
      call. = FALSE
    )
    attr(cached, "cache_fallback") <- list(
      cached_at = cache_time,
      reason = conditionMessage(source_error)
    )
    return(cached)
  }

  temporary <- tempfile(pattern = "cache-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(value, temporary, version = 3)
  if (!file.rename(temporary, path)) {
    warning("Não foi possível atualizar o cache local.", call. = FALSE)
  }
  value
}

clear_app_cache <- function(namespace = NULL) {
  root <- cache_directory()
  if (!dir.exists(root)) return(invisible(FALSE))
  target <- if (is.null(namespace)) root else file.path(root, normalize_text(namespace))
  if (!dir.exists(target)) return(invisible(FALSE))
  files <- list.files(target, full.names = TRUE, recursive = TRUE, all.files = TRUE)
  files <- files[basename(files) != ".gitkeep"]
  if (length(files) > 0L) unlink(files, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}
