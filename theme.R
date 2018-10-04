mytheme <- function() {
  ggpubr::theme_pubclean() +
  theme(
    axis.line = element_line(color="black"),
    axis.text = element_text(color="black"),
    strip.text = element_text(face="bold")
  )
}

cbPalette <- c(
  "#0072B2", # blue
  "#D55E00", # red
  "#009E73", # green
  "#CC79A7", # purple
  "#E69F00", # orange
  "#999999", # gray
  "#56B4E9", # skyblue
  "#F0E442" # yellow
)

scale_fill_Publication <- function(...){
  discrete_scale("fill","Publication", scales::manual_pal(values = cbPalette), ...)
}

