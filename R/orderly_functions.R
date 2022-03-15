orderly_dev_start_oli <- function(task, iso3 = NULL) {

  setwd(rprojroot::find_rstudio_root_file())
  param <- data.frame(iso3 = iso3)
  orderly_develop_start(task, param)
  setwd(paste0("src/", task))

  if (!is.null(iso3)) 
    iso3 <<- unlist(iso3)
}

orderly_clean_all <- function() {

  setwd(rprojroot::find_rstudio_root_file())
  tasks <- grep("README", list.files("src"),  invert = TRUE, value = TRUE)
  lapply(tasks, orderly::orderly_develop_clean)

}

orderly_pull_oli <- function(task, iso3 = NULL, recursive = FALSE, remote = "main") {

	if (!is.null(iso3)) 
    	orderly_pull_archive(task, id = paste0('latest(parameter:iso3 == "', iso3, '")'), recursive = recursive, remote = remote)
    else
    	orderly_pull_archive(task, recursive = recursive, remote = remote)

}