test_that("persistent cache reuses values and honors refresh", {
  previous <- Sys.getenv("PROJETO_DATASUS_CACHE_DIR", unset = NA_character_)
  cache_root <- tempfile("projeto-datasus-cache-")
  Sys.setenv(PROJETO_DATASUS_CACHE_DIR = cache_root)
  on.exit({
    unlink(cache_root, recursive = TRUE, force = TRUE)
    if (is.na(previous)) {
      Sys.unsetenv("PROJETO_DATASUS_CACHE_DIR")
    } else {
      Sys.setenv(PROJETO_DATASUS_CACHE_DIR = previous)
    }
  }, add = TRUE)

  calls <- 0L
  compute <- function() {
    calls <<- calls + 1L
    list(value = calls)
  }

  first <- cached_call("test", list(id = 1L), compute, max_age = 60)
  second <- cached_call("test", list(id = 1L), compute, max_age = 60)
  refreshed <- cached_call(
    "test",
    list(id = 1L),
    compute,
    max_age = 60,
    refresh = TRUE
  )

  expect_equal(first$value, 1L)
  expect_equal(second$value, 1L)
  expect_equal(refreshed$value, 2L)
  expect_equal(calls, 2L)
})
