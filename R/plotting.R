standard_theme <- function() {

  theme_minimal() +
    theme(legend.position = "bottom",
          strip.text = element_text(size=13),
          plot.title = element_text(size=16),
          axis.text = element_text(size = 12),
          axis.title = element_text(size=14),
          legend.text = element_text(size=12),
          strip.background = element_rect(fill=NA, colour = "white")
    )

}
