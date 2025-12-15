# processamento para inventário/censo 
# estimativas de area basal e volume 

# premissas pre estabelecidas (preencher de acordo com o projeto realizado)
fator <- 0.5 # fator de forma para calcular o volume
area_parcela <- 400 # area da parcela 
n_parcela <- 8
area <- n_parcela * area_parcela
conversao <- 10000/area_parcela #valor de conversao para estimativa por ha (tamanho da parcela ou da area)
t <- 1.96 # valor ta tabela t para 95% , bicaudal

# ler arquivo --------------------------------------------------------------------

dados_base_inv <- openxlsx::read.xlsx( "dados/petrucio.xlsx")

# limpar nomes das colunas 

dados_base_inv <- dados_base_inv|> janitor::clean_names()

# padronizando nomes de colunas para os utilizados no codigo caso necessario -----------------

dados_base_inv <- dados_base_inv |> 
 dplyr::rename(
  cap = 'cap_cm',
  altura_total = 'ht_m',
  altura_comercial = 'hc_m',
  parcela = 'n_parcela',
  plaqueta = 'no_plaqueta'
)

dados_base_inv$altura_comercial <- as.numeric(
  as.character(dados_base_inv$altura_comercial))

# calcular e criar colunas de volume e area transversal ------------------------

dados_volume_inv <- dados_base_inv |>
  dplyr:: mutate(
    dap = cap/pi, # diametro a altura de peito
    g = (dap^2 * pi)/40000, # area transversal
    v_total = g * altura_total * fator,
    v_comercial = g * altura_comercial * fator 
  
    )

dados_volume_inv <- dados_volume_inv|>
  dplyr::select(
    parcela,
    individuo,
    familia,
    especie,
    dap,
    altura_total,
    altura_comercial,
    g,
    v_total,
    v_comercial,
    est
  )

# tirar repeticoes de individuos bifurcados-------------------------------------

dados_volume_inv <- dados_volume_inv|>
  dplyr::group_by (individuo)|>
  dplyr::summarise(
    parcela = dplyr::first(parcela),
    familia = dplyr::first(familia),
    especie = dplyr::first(especie),
    bifurcacoes = dplyr::n(),
    dap = mean(dap),
    g = sum(g),
    v_total = sum(v_total),
    v_comercial = sum(v_comercial),
    altura_comercial = mean(altura_comercial),
    altura_total = mean(altura_total),
    .groups = "drop"
  )

dados_volume_inv <- dados_volume_inv|>
  dplyr::mutate(
    dplyr::across(
      .cols= c (
        dap,
        g,
        altura_comercial,
        altura_total,
        v_total,
        v_comercial
        ),
      .fns = ~round(.x,2)
    )
  )

# tirar individuos mortos-------------------------------------------------------

dados_volume_inv <- dados_volume_inv |>
  dplyr::filter(familia != "Morta")

# resumir dados por parcela ----------------------------------------------------

volume_parcela_ha <- dados_volume_inv|>
  dplyr::group_by(
    parcela)|>
  dplyr::summarise(
    v_total = sum(v_total, na.rm = TRUE) * conversao,
    v_comercial = sum(v_comercial, na.rm = TRUE) * conversao,
    altura_total = mean(altura_total),
    altura_comercial = mean(altura_comercial,na.rm = TRUE),
    g = sum(g) * conversao,
    n = dplyr::n() * conversao,
    .groups = "drop"
  ) 

# calcular dados gerais para inv --------------------------------------------
resumo_total <- volume_parcela_ha|>
  dplyr::summarise(
    G_ha = mean (g),
    volume_total_ha = mean (v_total),
    volume_comercial_ha = mean(v_comercial),
    altura_total_med = mean(altura_total),
    altura_comercial_med = mean(altura_comercial),
    N_ha = mean(n)
  )

resumo_total <- round(resumo_total,digits = 2) #arredondar para duas casas decimais 

# calcular IC , comandos pronto para analise estatistica -----------------------

IC <- function(x,t = 1.96) {
  n_parcela = nrow(volume_parcela_ha)
  media <- mean(x) 
  sd_x  <- sd(x)
  erro  <- t * sd_x / sqrt(n_parcela) 
  lim_inf <- media - erro
  lim_sup <- media + erro
  return(c(media = media, lim_inf = lim_inf, lim_sup = lim_sup))
}

# IC para cada variavel 

resumo_IC <- tidyr::tibble( # criar uma tabela com os limites inferior e superior para o intervalo de confiança
  parametro = c("G_ha", "Volume_total_ha", "N_ha"),
  IC_result = list(
    IC(volume_parcela_ha$g), # ic para a area basal por ha
    IC(volume_parcela_ha$v_total),# ic para o volume por ha
    IC(volume_parcela_ha$n)# ic para o n de arv por ha media,lim_inf, lim_sup 
  )
) |>
  tidyr::unnest_wider(IC_result
) |> 
  dplyr::mutate(
    dplyr::across( c( media,lim_inf,lim_sup), ~ as.numeric(.x))
    ) |> 
  dplyr::mutate(
    dplyr::across( c( media,lim_inf,lim_sup), ~ round(.x,2))
  )

# agrupar e calcular dados por classes------------------------------------------
dados_volume_inv <- dados_volume_inv |>
  dplyr::mutate(
    v_comercial = dplyr::if_else(
      is.na(v_comercial), 0,
      v_comercial
    ))|>
  dplyr::mutate(
    altura_comercial = dplyr::if_else(
      is.na(altura_comercial), 0,
      altura_comercial
    ))

classes_inv <- dados_volume_inv |> 
  dplyr::mutate(
    volume_lenha = dplyr::if_else(
      dap <= 20, 
      g * altura_comercial * fator,0),
    volume_torete = dplyr::if_else(
      dap > 20, 
      g * altura_comercial * fator,0)
  ) |> 
  dplyr::mutate(
    classe = dplyr::case_when(
      dap >= 0 & dap <= 9.99  ~ "00 – 09,9",
      dap > 9.99  & dap <= 19.99 ~ "10 – 19,9",
      dap > 19.99 & dap <= 29.99 ~ "20 – 29,9",
      dap > 29.99 & dap <= 39.99 ~ "30 – 39,9",
      dap > 39.99 & dap <= 49.99 ~ "40 – 49,9",
      dap > 49.99 & dap <= 59.99 ~ "50 – 59,9",
      dap > 59.99                ~ "≥ 60"
    )
  )|>
  dplyr::group_by(
    parcela, classe
  )|>
  dplyr::summarise(
    n = dplyr::n(),
    especies = dplyr::n_distinct(especie),
    volume_lenha = sum(volume_lenha),
    volume_torete = sum(volume_torete),
    .groups = "drop"
  )|>
  dplyr::arrange(
    classe
  )|>
  dplyr::mutate(
    volume_lenha_ha = volume_lenha*conversao,
    volume_torete_ha = volume_torete*conversao,
    n_ha = n*conversao,
    n_rel = n/sum(n)*100
  )|>
  dplyr::relocate(
    classe, 1, 
    n, 2,
    n_ha, 3,
    volume_lenha, 4,
    volume_lenha_ha, 5
    )|> 
  dplyr::group_by(classe)|>
  dplyr::summarise(
    n = mean(n),
    n_ha = mean(n_ha),
    especies = mean (especies),
    volume_lenha = mean(volume_lenha),
    volume_lenha_ha = mean(volume_lenha_ha),
    volume_torete = mean(volume_torete),
    volume_torete_ha = mean(volume_torete_ha),
    .groups = "drop"
  )|>
  dplyr::mutate(
    dplyr::across(
      c(
        volume_lenha_ha,
        volume_torete_ha,
        volume_lenha,
        volume_torete
      ),~round(.x,2)
    )
  )|>
  dplyr::mutate(
    dplyr::across(
      c(n_ha,n,especies),~round(.x,0)
    )
  )

classes_v <- classes_inv |>
  dplyr::mutate(
    volume_total = volume_lenha + volume_torete,
    volume_relativo = volume_total/sum(volume_total)*100
  )

# adicionar linha de total na tabela de classes 

# classes_total <- classes_v |>
#   dplyr::bind_rows(
#     dplyr::summarise(
#       classe,
#       classe = "Total",
#       n = sum(n, na.rm = TRUE),
#       n_ha = sum(n_ha, na.rm = TRUE),
#       volume_lenha = sum(volume_lenha,na.rm = TRUE),
#       volume_lenha_ha = sum(volume_lenha_ha,na.rm = TRUE),
#       volume_torete = sum( volume_torete,na.rm = TRUE),
#       volume_torete_ha = sum( volume_torete_ha,na.rm = TRUE)
      # n_rel = sum(n_rel, na.rm = TRUE),
      # volume_total = sum(volume_total,na.rm = TRUE),
      # volume_relativo = sum(volume_relativo,na.rm = TRUE)
  #   )
  # )


# estrutura vertical------------------------------------------------------------

estrutura_vertical_inv <- dados_volume_inv |>
  dplyr::mutate(
    estrato = dplyr::case_when(
      altura_total <= 13.0 ~ "Inferior",
      altura_total >= 13.1 & altura_total <= 17.0 ~ "Médio",
      altura_total >= 17.1 ~ "Superior")
    ) |>
  dplyr::group_by(parcela, estrato) |>
  dplyr::summarise(
    altura_min = min(altura_total),
    altura_max = max(altura_total),
    individuos = dplyr::n(),
    especies = dplyr::n_distinct(especie),
  .groups = "drop"
  )

estrutura_vertical_inv <- estrutura_vertical_inv|> 
  dplyr::group_by(estrato) |>
  dplyr::summarise(
    altura_min = min(altura_min),
    altura_max = max(altura_max),
    individuos = mean(individuos),
    especies = mean(especies),
    ) |> 
  dplyr::mutate(
    intervalo = dplyr::case_when(
      estrato == "Inferior" ~ "0.0 - 12.9",
      estrato == "Médio" ~ "13 - 16.9",
      estrato == "Superior" ~ ">= 17",
      TRUE ~ NA_character_)
    ) |>
  dplyr::mutate(
    perc_individuos = round(
      (individuos / sum(individuos)) * 100, 1),
    perc_especies = round((especies / sum(especies)) * 100, 1)
  ) |> 
  dplyr::relocate(intervalo, .after = 1) |> 
  dplyr::mutate(
    dplyr::across(
      c(individuos, especies,), ~round(.x,0)
    )
  )

# Exportar os arquivos Excel-------------------------------------
openxlsx::write.xlsx(dados_volume_inv, file = here::here("resultados","dados_volume_inv.xlsx"))
openxlsx::write.xlsx(volume_parcela_ha, file = here::here("resultados","dados_parcela.xlsx"))
openxlsx::write.xlsx(resumo_IC, file = here::here("resultados","resumo_IC.xlsx"))
openxlsx::write.xlsx(classes_inv, file = here::here("resultados","classes_inv.xlsx"))
openxlsx::write.xlsx(estrutura_vertical_inv, file = here::here("resultados","estrutura_vertical_inv.xlsx"))
