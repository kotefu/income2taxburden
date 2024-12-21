## ggplotの図を保存する関数
library(ggplot2, ragg)

save_figure <- function(filename, plot, width = 180, height = 90, dpi = 300) {
  ggsave(
    filename = here(filename),
    plot = plot,
    device = ragg::agg_png(),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )
}
