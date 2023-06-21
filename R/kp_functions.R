kp_to_sex <- function() {
    data.frame(
      kp = c("FSW", "MSM", "PWID", "TGW", "CFSW"),
      sex = c("female", "male", "both", "female", "male")
    )
}

kp_to_eng <- function(df) {

    df %>%
      dplyr::left_join(
        data.frame(
           kp = c("PS", "HSH", "CDI", "TG"),
           new_kp = c("FSW", "MSM", "PWID", "TG")
          )
      ) %>%
      dplyr::select(-kp, kp = new_kp)

}

separate_survey_id <- function(df, kp = T) {

  if(kp) {
    df %>%
      tidyr::separate(survey_id, into = c("iso3", "year", NA), sep = c(3, 7), remove = F, convert = T) %>%
      tidyr::separate(survey_id, into = c(NA, "kp"), sep = "_", remove = F)
  } else {
    df %>%
      tidyr::separate(survey_id, into = c("iso3", "year", NA), sep = c(3, 7), remove = F, convert = T)
  }

}
