extract_mozambique_spectrum <- function(areas,
         path = "input/moz_2021_v6.06_18.03.21") {
  
  spec_paths <- gtools::mixedsort(list.files(path, full.names = TRUE))
  dat <- lapply(spec_paths, naomi::extract_pjnz_naomi)
  
  dat <- dat %>%
    bind_rows() %>%
    separate(spectrum_region_name, sep = "_", into=c(NA, "area_name")) %>%
    mutate(area_name = ifelse(area_name == "Zambezi", "Zamb\u00E9zia", area_name))
  
  df <- dat %>%
    left_join(data.frame(age = c(0:80), age_group_start = c(rep(seq(0, 75, 5), each = 5), 80)) %>%
                dplyr::left_join(naomi::get_age_groups() %>%
                                   dplyr::filter(age_group_span == 5)) %>%
                dplyr::select(age, age_group)
    )
  
  hiv_indicators <- df %>%
    dplyr::group_by(area_name, year, sex, age_group) %>%
    dplyr::summarise(totpop = sum(totpop),
                     hivpop = sum(hivpop),
                     artpop_dec31 = sum(artpop_dec31),
                     infections = sum(infections)) %>%
    dplyr::ungroup() %>%
    mutate(source = "Spectrum 2021") %>%
    left_join(areas %>% select(area_id, area_name) %>% st_drop_geometry())
  
  fertility_indicators <- df %>%
    dplyr::filter(sex == "female", age %in% 15:49) %>%
    group_by(area_name, year, age_group, asfr) %>%
    summarise(population = sum(totpop)) %>%
    mutate(births = population * asfr,
           source = "Spectrum 2021") %>%
    ungroup()  %>%
    left_join(areas %>% select(area_id, area_name) %>% st_drop_geometry())
  
  if(nrow(filter(hiv_indicators, is.na(area_id))))
    stop("HIV indicators has areas with no area IDs")
  
  if(nrow(filter(fertility_indicators, is.na(area_id))))
    stop("Fertility indicators has areas with no area IDs")
  
  out <- list()
  out$hiv_indicators <- hiv_indicators
  out$fertility_indicators <- fertility_indicators
  
  out
  
  
}





