aggregate_to_admin <- function(df, group_vars, indicators, target_level, areas_wide) {

  target_id <- paste0("area_id", target_level)
  target_name <- paste0("area_name", target_level)

  group_vars <- c(target_id, target_name, group_vars)

  df %>%
    left_join(areas_wide %>% st_drop_geometry()) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(all_of(indicators), sum)) %>%
    rename_with(~paste("area_id"), starts_with("area_id")) %>%
    rename_with(~paste("area_name"), starts_with("area_name")) %>%
    ungroup()

}


five_year_to_15to49 <- function(df, indicators) {

  age_span <- naomi::get_age_groups() %>%
    dplyr::filter(age_group_span == 5,
           age_group_start %in% 15:45)

  df %>%
    dplyr::filter(age_group %in% age_span$age_group) %>%
    dplyr::group_by(across(-all_of(c(indicators, "age_group")))) %>%
    dplyr::summarise(across(all_of(indicators), sum)) %>%
    dplyr::mutate(age_group = "Y015_049")

}

group_proportion <- function(df, variables) {
  df %>%
    dplyr::group_by(across(all_of(variables))) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(prop = n/sum(n))
}

make_calendar_month <- function(year, month) {
  month <- sapply(month, function(x) {
    if(x < 10)
      x <- paste0("0", x)
    else
      x
  })

  paste0("CY", year, "M", month)
}

cal_month_to_cmc <- function(calendar_month) {
  year <- as.numeric(substr(calendar_month, 3, 6))
  month <- as.numeric(substr(calendar_month, 8, 11))
  12*(year-1900) + month
}

cal_month_to_cal_quar <- function(calendar_month) {
  year <- as.numeric(substr(calendar_month, 3, 6))
  month <- as.numeric(substr(calendar_month, 8, 11))
  quarter <- plyr::round_any(month, 3, f = ceiling)/3
  paste0("CY", year, "Q", quarter)
}

cal_month_to_quarter_labels <- function(calendar_month) {
  year <- as.numeric(substr(calendar_month, 3, 6))
  month <- as.numeric(substr(calendar_month, 8, 11))
  quarter <- plyr::round_any(month, 3, f = ceiling)/3
  paste0(year, "\nQ", quarter)
}

adjust_month <- function(calendar_month, adjustment) {

  cmc <- cal_month_to_cmc(calendar_month)
  adj_cmc <- cmc + adjustment
  month <- adj_cmc %% 12
  year <- 1900 + (adj_cmc-1) %/% 12

  month <- sapply(month, function(x) {
    if(x == 0)
      x <- 12
    else if(x < 10)
      x <- paste0("0", x)
    else
      x
  })

  paste0("CY", year, "M", month)
}

convert_age_groups <- function(df) {

}
