options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

if (file.exists("renv.lock")) {
  renv::restore(prompt = FALSE)
} else {
  renv::init(bare = TRUE, restart = FALSE)
  renv::install("remotes")
  renv::install("rpradosiqueira/datasus@v0.16.1")
  renv::install()
  renv::snapshot(prompt = FALSE)
}

message("Ambiente restaurado. Execute: Rscript -e 'shiny::runApp(\".\")'")
