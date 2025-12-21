# gráfico de barras
library(ggplot2)
library(echarts4r)

# n de indivíduos por familia---------------------------------------------------

# ggplot(dados_familia, aes(x = n, y = reorder(familia, -n) )) +
#   geom_bar(stat = "identity", fill = "darkgreen") +  # cor verde
#   labs(
#     title = "Distribuição por família",
#     x = "Número de indivíduos",
#     y = "Família"
#   ) +
#   theme_minimal(base_size = 10) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(face = "bold", hjust = 0.5)
#   )


dados_familia |>
  dplyr::arrange(desc(n)) |>  # ordena do maior para o menor
  e_charts(familia) |>
  e_bar(n, name = "Número de indivíduos",) |>
  e_flip_coords() |>  # deixa as barras horizontais
  e_color("#838B83") |>  # cor verde escura
  e_title(
    text = "Número de indivíduos por família",
    left = "center"
  ) |>
  e_x_axis(name = "") |>
  e_y_axis(name = "Família", axisLabel = list(interval = 0))|>
  e_theme("roma") |>  # tema limpo, similar ao theme_minimal()
  e_tooltip(trigger = "axis") |>
  e_labels(show = TRUE, position = "right") |>
  e_grid(top = "10%",left = "15%", right = "7%", bottom = "10%") |>
  e_legend(
    bottom = 0,       #  coloca a legenda embaixo
    left = "center"   #  centraliza horizontalmente
  )


#distribuição por classe 

# Gráfico de histograma + densidade para a variável "n"

classe_v |>
  e_charts(classe) |> 
  e_bar(n, 
        name = "Número de indivíduos",
        color = "#838B83",
        y_index = 0) |>
  e_labels(show = TRUE, position = "top") |>
  e_line ( volume_total,
           name = "Volume m3",
           lineStyle = list(color = "darkgreen", width = 2),
           smooth = TRUE,
           symbol = "circle",
           color = "darkgreen",
           symbolSize = 5,
           y_index = 0) |> 
  e_labels(show = TRUE,
           fontSize = 12) |>
  e_theme("roma") |>
  e_tooltip(trigger = "axis") |> 
  e_legend(show = TRUE,
           bottom = 1) |> 
  e_title("Distribuição por classe de diâmetro",
          left = "center")
