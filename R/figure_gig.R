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
  
  ## x軸のラベル
  x_lab <- c(seq(0, 60, 20), round(df$`合計`, 1))
  
  fig_result_gig <- df |> 
    select(-`合計`) |>
    transmute(
      `年収`,
      `健康保険料` = `健康保険`,
      `年金保険料` = `年金`,
      `消費税`,
      `住民税 +\n所得税` = `住民税` + `所得税`
    ) |> 
    pivot_longer(-`年収`) |> 
    arrange(desc(value)) |> 
    mutate(cumsum = cumsum(value) - value / 2) |> 
    ggplot(aes(value, "", fill = reorder(name, value))) +
    geom_col() +
    geom_text(
      aes(
        x = cumsum,
        label = paste0(name, "\n", round(value, 1), "万円")
      ),
      colour = "white",
      fontface = "bold",
      family = FONT,
      size = 5.5
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
    scale_fill_manual(
      values = my_color
    ) +
    scale_x_continuous(
      breaks = x_lab,
      labels = x_lab
      ) +
    labs(
      x = "負担額 (万円)",
      y = NULL
    )

  save_figure(
    here("figure/gig_200.png"),
    fig_result_gig,
    width = 180,
    height = 60,
    dpi = 1200
  )
  
  return(fig_result_gig)
}

make_fig_gig(result_gig)
