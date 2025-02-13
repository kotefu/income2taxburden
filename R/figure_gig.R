## 税・社会保険料 負担額のグラフ

# コメント:
# > 負担額の中で社会保険料の大きさがわかるようにしたい。
# > 1本の棒にする。左側は社会保険料

make_fig_gig <- function(df) {
  
  my_color = c(
    `健康保険料` = "#ee4d5a",
    `年金保険料` = "#f7945d",
    `消費税` = "#089099",
    `住民税 +\n所得税` = "#46aea0"
    #所得税 = "#7ccba2"
  )
  
  ## フォントサイズ
  fsize <- 5
  
  ## グラフの右側をどの値の位置まで表示するか
  xmax <- sum(df$`負担合計`)
  
  ## x軸のラベル
  x_lab <- c(seq(0, 60, 20), round(df$`負担合計`, 1))
  
  ## データフレームの前処理
  d <- df |> 
    transmute(
      `年収`,
      `健康保険料` = `健康保険`,
      `年金保険料` = `年金`,
      `消費税`,
      `住民税 +\n所得税` = `住民税` + `所得税`
    ) |> 
    pivot_longer(-`年収`, names_transform = list(name = as.factor)) |> 
    mutate(
      cumsum = cumsum(value) - value / 2, # グラフ内の要素名のx座標
      name = factor(
        name,
        levels = names(my_color) # 積み上げバーの並び順を制御
        ),
      ratio = (value / df$`年収`) * 100 # 全体に占める割合
    )

  ## ggplot部分
  fig <- d |>
    ggplot(aes(value, "", fill = name)) +
    geom_col(position = position_stack(reverse = TRUE)) + # 積み上げを逆順に
    scale_fill_manual(
      values = my_color
    ) +
  theme(
      legend.position = "none",
      ## 枠線を消し、x軸のmajor gridのみを引く
      panel.border = element_blank(),
      panel.grid.major.x = element_line(
        linewidth = 0.25,
        colour = "grey80"
      ),
      axis.ticks = element_line(linewidth = 0)
    ) +
    labs(
      x = "負担額 (万円)",
      y = NULL
    ) +
    scale_x_continuous(
      breaks = x_lab,
      labels = x_lab,
      limits = c(0, xmax * 1.1)
    )
  
  ## ラベルが小さすぎる時の処理
  d_mini <- d |> 
    filter(value > 10)
  
  fig <- fig +
    geom_text(
      data = d_mini,
      aes(
        x = cumsum,
        label = paste0(name, "\n", round(value, 1), "万円", "\n(", round(ratio, 1), "%)")
      ),
      colour = "white",
      fontface = "bold",
      family = FONT,
      size = fsize
    )
  
  if (any(d$value < 10)) {
    fig <- fig +
      geom_text(
        data = filter(d, value < 10),
        aes(
          x = sum(d_mini$value) + 0.8,
          label = paste0(name, "\n", round(value, 1), "万円", "\n(", round(ratio, 1), "%)")
        ),
        hjust = 0, 
        colour = "grey10",
        fontface = "bold",
        family = FONT,
        size = fsize
      )
  
  fig <- fig +
    scale_x_continuous(
      breaks = x_lab,
      labels = x_lab,
      #limits = c(0, sum(d$value) * 1.1)
      limits = c(0, xmax * 1.1)
    )
  }
  
  return(fig)
  
  ## personオブジェクトをbind_rowして、複数の積み上げ棒グラフとしたほうがいいのかもしれない。ただ、その場合に、被用者の住民税・所得税のような小さい部分だけ表示しないような処理が簡単にできるのか不明。
  ## 普通に色分けだけのグラフなら楽なのだが・・・
}

save_fig_gig <- function(filename, plot, width = 180, height = 70) {
  ggsave(
    filename = filename,
    path = here("figure"),
    plot = plot,
    device = ragg::agg_png(),
    width = width,
    height = height,
    units = "mm",
    dpi = 1200
  )
}