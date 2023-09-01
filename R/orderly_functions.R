orderly_dev_start_oli <- function(task, iso3 = NULL, version = 2022, pull_dependencies = FALSE, remote = "inference-web", envir = parent.frame(), ...) {

  varnames <- lapply(substitute(list(...))[-1], deparse)

  if(!is.null(iso3)) {
    param <- data.frame(
      iso3 = iso3,
      version = version
    ) %>% bind_cols(data.frame(varnames))
  } else
    param <- NULL

  if(pull_dependencies)
    orderly::orderly_pull_dependencies(task, remote = remote, parameters = param, recursive=TRUE)

  setwd(rprojroot::find_rstudio_root_file())
  ortderly::orderly_develop_start(task, param, envir = envir)
  setwd(paste0("src/", task))
}

orderly_clean_all <- function() {

  setwd(rprojroot::find_rstudio_root_file())
  tasks <- grep("README", list.files("src"),  invert = TRUE, value = TRUE)
  lapply(tasks, orderly::orderly_develop_clean)
  orderly_cleanup()

}

orderly_pull_oli <- function(task, iso3 = NULL, remote = "inference-web", recursive = TRUE) {

  # .onLoad <- function(lib, pkg) {
  #   possibly_pull <<- purrr::possibly(.f = orderly::orderly_pull_archive, quiet = FALSE)
  # }

  possibly_pull <- purrr::possibly(.f = orderly::orderly_pull_archive, otherwise = NULL, quiet = FALSE)

  if(!is.null(iso3)) {
    res <- purrr::map(iso3, ~possibly_pull(task, id = paste0('latest(parameter:iso3 == "', .x, '" && parameter:version == 2022)'), recursive = recursive, remote = remote))

    # fail_iso3 <- res %>%
    #   setNames(iso3) %>%
    #   purrr::keep(~is.null(.x))

    # if(length(fail_iso3))
    #   message(paste0(names(fail_iso3), collapse = ", "), " failed")
  } else {
    orderly::orderly_pull_archive(task, recursive = recursive, remote = remote)
  }

}
