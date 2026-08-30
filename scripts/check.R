options(warn = 1)

source("global.R")

files <- c("app.R", "global.R", list.files("R", pattern = "[.]R$", full.names = TRUE))
invisible(lapply(files, parse))
message("Sintaxe: OK")

lints <- c(
  lintr::lint_dir("R"),
  lintr::lint_dir("scripts"),
  lintr::lint_dir("tests")
)
if (length(lints) > 0L) {
  print(lints)
  stop("A verificação de estilo encontrou problemas.", call. = FALSE)
}
message("Lint: OK")

testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
message("Testes: OK")

ui <- app_ui()
html <- htmltools::renderTags(ui)$html
stopifnot(grepl("Atlas de ocorrências e serviços", html, fixed = TRUE))
message("Interface: OK")
