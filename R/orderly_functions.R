orderly_dev_start_oli <- function(task, ...) {

  setwd(rprojroot::find_rstudio_root_file())
  orderly_develop_start(task, ...)
  setwd(paste0("src/", task))
  iso3 <<- unlist(...)
}

orderly_clean_all <- function() {

  setwd(rprojroot::find_rstudio_root_file())
  tasks <- grep("README", list.files("src"),  invert = TRUE, value = TRUE)
  lapply(tasks, orderly::orderly_develop_clean)

}
