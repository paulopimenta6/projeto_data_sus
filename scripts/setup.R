file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(file_argument) > 0L) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  file.path(getwd(), "scripts", "setup.R")
}
project_root <- dirname(dirname(normalizePath(script_path, mustWork = TRUE)))

source(file.path(project_root, "prepare_environment.R"), chdir = TRUE)
prepare_environment(project = project_root, args = commandArgs(trailingOnly = TRUE))
