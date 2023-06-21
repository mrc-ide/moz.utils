ssa_names <- function() {
  names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Eritrea", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
  sort(names)
}

ssa_iso3 <- function() {
  names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Eritrea", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
  sort(countrycode::countrycode(names, "country.name", "iso3c"))
}

region <- function() {

  path <- fs::path_package("extdata", "region.csv", package = "moz.utils")
  readr::read_csv(path) %>%
    dplyr::mutate(iso3 = toupper(iso3))

}

national_adj <- function() {
  fs::path_package("extdata", "national_level_adj.adj", package = "moz.utils")
}

admin1_adj <- function() {
  fs::path_package("extdata", "admin1.adj", package = "moz.utils")
}

grey_areas <- function() {
  fs::path_package("extdata", "grey.geojson", package = "moz.utils")
}

national_areas <- function() {
  fs::path_package("extdata", "national_areas.geojson", package = "moz.utils")
}