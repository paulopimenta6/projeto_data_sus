support_files <- sort(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE))
for (support_file in support_files) {
  sys.source(support_file, envir = globalenv(), keep.source = TRUE)
}
rm(support_file, support_files)
