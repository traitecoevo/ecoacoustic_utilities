files <- list.files("R", full.names = TRUE)
for (f in files) {
  message("Checking ", f)
  tryCatch(source(f), error = function(e) {
    stop("Failed to source ", f, ": ", e$message)
  })
}
message("All files sourced successfully.")
