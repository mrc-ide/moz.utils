standard_theme <- function() {

  theme_minimal() +
    theme(legend.position = "bottom",
          strip.text = element_text(size=13),
          plot.title = element_text(size=16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size=14),
          legend.text = element_text(size=12),
          strip.background = element_rect(fill=NA, colour = "white"),
          panel.background = element_rect(fill=NA, color="black"))
    # labs(y=element_blank(), x=element_blank())

}

every_nth = function(axis_label, n, offset = 0) {
  if(!offset) {
    return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
  } else {
    x <- unique(axis_label)
    i <- rep_len(c(TRUE, rep(FALSE, n - 1)), length(axis_label)-offset)
    return(function(x) {x[c(rep(FALSE, offset), i)]})
  }

}

province_grid <- function() {
  moz_province_grid <- data.frame(
  code = "",
  name = c("Cabo Delgado", "Niassa", "Nampula", "Zambézia", "Tete", "Manica", "Sofala", "Inhambane", "Gaza", "Maputo"),
  row = c(1, 1, 2, 2, 2, 3, 3, 4, 4, 5),
  col = c(4, 3, 4, 3, 1, 2, 3, 3, 2, 2),
  stringsAsFactors = FALSE
)

  facet_geo(~area_name, grid = moz_province_grid)

}
