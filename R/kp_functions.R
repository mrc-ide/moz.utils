kp_to_sex <- function() {
    data.frame(
      kp = c("FSW", "MSM", "PWID", "TGW", "CFSW"),
      sex = c("female", "male", "both", "female", "male")
    )
}

kp_to_eng <- function(df) {
  
    df %>%
      left_join(
        data.frame(
           kp = c("PS", "HSH", "CDI", "TG"),
           new_kp = c("FSW", "MSM", "PWID", "TG")
          )
      ) %>%
      select(-kp, kp = new_kp)
  
}