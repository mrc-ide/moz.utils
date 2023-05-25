aggregate_to_admin <- function(df, group_vars, indicators, target_level, areas) {

  target_id <- paste0("area_id", target_level)
  target_name <- paste0("area_name", target_level)

  areas_wide <- spread_areas(areas)

  original_cols <- colnames(df)

  ls <- df %>%
    left_join(areas %>% select(area_id, area_level) %>% st_drop_geometry()) %>%
    group_by(area_level) %>%
    group_split()

  df <- lapply(ls, function(x) {

    lvl <- paste0("area_id", unique(x$area_level))

    x <- x %>%
      left_join(areas_wide %>% st_drop_geometry() %>% select(-area_id), by=c("area_id" = lvl))
  }) %>%
    bind_rows() %>%
    mutate(level_check = area_level >= target_level)

  prep_df <- df %>%
    filter(level_check == TRUE)

  group_vars <- c(target_name, group_vars)

  aggregated_df <- prep_df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(all_of(indicators), sum)) %>%
    left_join(areas_wide %>% st_drop_geometry() %>% select(all_of(c(target_id, target_name))) %>% distinct()) %>%
    rename_with(~paste("area_id"), starts_with("area_id")) %>%
    rename_with(~paste("area_name"), starts_with("area_name")) %>%
    ungroup()

  high_level_df <- df %>%
    filter(level_check == FALSE)

  if(nrow(high_level_df))
    warning("Data at a higher level of aggregation was provided. This has been passed through to the output")

  bind_rows(aggregated_df, high_level_df) %>%
    ungroup() %>%
    select(any_of(original_cols))

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

single_year_to_five_year <- function(df, fifteen_to_49 = TRUE) {
  df <- df %>%
    dplyr::mutate(age_group_label = cut(age, c(0, seq(5, 85, 5)-1), c(paste0(seq(0, 79, 5), "-", seq(5, 80, 5)-1), "80+"), include.lowest=TRUE)) %>%
    dplyr::left_join(naomi.utils::get_age_groups() %>% select(age_group, age_group_label)) %>%
    dplyr::select(-age_group_label)

  if(fifteen_to_49) {
    df %>%
      dplyr::filter(age %in% 15:49) %>%
      dplyr::select(-age)
  } else {
    df %>%
      dplyr::select(-age)
  }
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

sex_aggregation <- function(df, indicators) {

  sex_agg_df <- df %>%
    dplyr::group_by(across(-all_of(c(indicators, "sex")))) %>%
    dplyr::summarise(across(all_of(indicators), sum)) %>%
    dplyr::mutate(sex = "both")

  dplyr::bind_rows(df, sex_agg_df) %>% dplyr::ungroup()
}

calculate_quantile <- function(x, probs = c(0.25, 0.5, 0.75), weights = NULL, wide_format = T, percentage = T) {
  if(all.equal(sort(probs), c(0.25, 0.5, 0.75)) != TRUE)
    quant_labs <- c("lower", "median", "upper")
  else
    quant_labs <- probs

  out <- tibble::tibble(
    value = DescTools::Quantile(x, weights, probs, names = F, na.rm = TRUE),
    quant = quant_labs
  )

  if(percentage)
    out$value <- out$value * 100

  if(wide_format)
    tidyr::pivot_wider(out, names_from = quant, values_from = value)
  else
    out
}