orderly_dev_start_oli <- function(task, iso3 = NULL, pull_dependencies = FALSE) {

  param <- data.frame(iso3 = iso3)

  if(pull_dependencies)
    orderly_pull_dependencies(task, remote = "main", parameters = param, recursive=FALSE)

  setwd(rprojroot::find_rstudio_root_file())
  orderly_develop_start(task, param)
  setwd(paste0("src/", task))

  if(!is.null(iso3))
    iso3 <<- unlist(iso3)
}

orderly_clean_all <- function() {

  setwd(rprojroot::find_rstudio_root_file())
  tasks <- grep("README", list.files("src"),  invert = TRUE, value = TRUE)
  lapply(tasks, orderly::orderly_develop_clean)
  orderly_cleanup()

}

orderly_pull_oli <- function(task, iso3 = NULL, recursive = FALSE, remote = "main") {

  .onLoad <- function(lib, pkg) {
    possibly_pull <<- purrr::possibly(.f = orderly::orderly_pull_archive, otherwise = "FAIL")
  }

  possibly_pull <- purrr::possibly(.f = orderly::orderly_pull_archive, otherwise = "FAIL")

  if(!is.null(iso3)) {
    res <- map(iso3, ~possibly_pull(task, id = paste0('latest(parameter:iso3 == "', .x, '")'), recursive = recursive, remote = remote))

    fail_iso3 <- res %>%
      setNames(iso3) %>%
      keep(~!is.null(.x))

    if(length(fail_iso3))
      message(paste0(names(fail_iso3), collapse = ", "), " failed")
  } else {
    orderly::orderly_pull_archive(task, recursive = recursive, remote = remote)
  }

}
