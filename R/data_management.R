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
