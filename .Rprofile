# Activate renv if it has been initialised (renv::init())
if (file.exists("renv/activate.R")) source("renv/activate.R")

# Load environment variables from any .env* files
for (env_file in list.files(all.files = TRUE, pattern = "^\\.env$")) {
  tryCatch({
    readRenviron(env_file)
    message("Loading environment from: ", normalizePath(env_file))
  }, error = function(e) {
    warning("Failed to load ", env_file, ": ", e$message)
  })
}
