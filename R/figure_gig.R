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
        label = paste0(name, "\n", round(value, 1))
      ),
      colour = "white",
      fontface = "bold",
      family = FONT,
      size = 6
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
  
  fig_result_gig
}