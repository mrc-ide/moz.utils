standard_theme <- function() {

  theme <- theme_minimal() +
    theme(legend.position = "bottom",
          strip.text = element_text(size=13),
          plot.title = element_text(size=16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size=14, face = "bold"),
          legend.text = element_text(size=12),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill=NA, colour = "white"),
          plot.tag = element_text(size = 16, face = "bold"),
          panel.background = element_rect(fill=NA, color="black"))

}

scale_percent <- function() {
  scale_y_continuous(labels = scales::label_percent())
}

no_labels <- function() {
  labs(x = element_blank(), y = element_blank())
}

scale_manual <- function(type = NULL, n) {

  if(type == "fill") {
    if(n==1) {
      scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[1])
    } else if(n==2) {
      scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)])
    } else if(n==3) {
      scale_fill_manual(values = c(wesanderson::wes_palette("Darjeeling1")[c(2,4,5)]))
    } else if(n==4) {
      scale_fill_manual(values = c(wesanderson::wes_palette("Darjeeling2")[2], wesanderson::wes_palette("Darjeeling1")[c(2,4,5)]))
    } else {
      scale_fill_manual(values = wesanderson::wes_palette("Zissou1"))
    }
  } else {
    if(n==1) {
      scale_color_manual(values = wesanderson::wes_palette("Zissou1")[1])
    } else if(n==2) {
      scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)])
    } else if(n==3) {
      scale_color_manual(values = c(wesanderson::wes_palette("Darjeeling1")[c(2,4,5)]))
    } else if(n==4) {
      scale_color_manual(values = c(wesanderson::wes_palette("Darjeeling2")[2], wesanderson::wes_palette("Darjeeling1")[c(2,4,5)]))
    } else {
      scale_color_manual(values = wesanderson::wes_palette("Zissou1"))
    }
  }

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
  name = c("Cabo Delgado", "Niassa", "Nampula", "Zamb\u00E9zia", "Tete", "Manica", "Sofala", "Inhambane", "Gaza", "Maputo"),
  row = c(1, 1, 2, 2, 2, 3, 3, 4, 4, 5),
  col = c(4, 3, 4, 3, 1, 2, 3, 3, 2, 2),
  stringsAsFactors = FALSE
)

  facet_geo(~area_name, grid = moz_province_grid)

}
