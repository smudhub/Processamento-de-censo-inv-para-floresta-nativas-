# processamento para inventário/censo 
# estimativas de area basal e volume 

# premissas pre estabelecidas (preencher de acordo com o projeto realizado)
fator <- 0.5 # fator de forma para calcular o volume
area<- 4400 # area medida no censo
conversao <- 10000/area #valor de conversao para estimativa por ha (tamanho da parcela ou da area)


# ler arquivo --------------------------------------------------------------------
  
dados_base <- openxlsx::read.xlsx( 
    here::here ("dados", 
                  "Ficha_de_Campo_revisada.xlsx"
    )
  )

# padronizando nomes de colunas para os utilizados no codigo caso necessario -----------------
dados_base<-dados_base|>
  dplyr::rename(
    id = 'INDIV',
    cap = 'CAP(cm)',
    altura_total = 'HT(m)',
    altura_comercial = 'HC(cm)',
    especie = 'ESPÉCIE' ,
    #fitossanidade = c,
    familia = 'FAMÍLIA'
  )

# calcular e criar colunas de volume e area transversal ------------------------
dados_volume <- dados_base |>
  dplyr:: mutate(
  dap = cap/pi, #diametro a altura de peito
  g_m2 = (dap^2 * pi)/40000, #area transversal
  v_ind_m3 = g_m2 * altura_total * fator #volume individual (caso seja de plantadas existe a opcao de usar equacoes volumetricas)
  
)

# tirar repeticoes de individuos bifurcados--------------------------------------

dados_volume <- dados_volume|>
  dplyr::group_by(id)|>
  dplyr::summarise(
    familia=dplyr::first(familia),
    especie=dplyr::first(especie),
    bifurcacoes = dplyr::n(),
    dap = mean(dap),
    g_m2 = sum(g_m2),
    v_ind_m3 = sum(v_ind_m3),
    altura_comercial=mean(altura_comercial),
    altura_total= mean(altura_total)
)

dados_volume <- dados_volume|>
  dplyr::mutate(
    dplyr::across(
      .cols= -c (
        familia,
        especie
        ),
      .fns = ~round(.x,2)
    )
  )
  

# visualizar dados base, agora com as novas colunas 
View(dados_volume)

# tirar individuos mortos-------------------------------------------------------
dados_volume <- dados_volume |>
  dplyr::filter(familia != "Morta")

# calcular dados gerais --------------------------------------------------------
resumo_total <- dados_volume|>
  dplyr::summarise(
    G =sum(g_m2),
    volume_total = sum(v_ind_m3),
    altura_med = mean(altura_total),
    n_ha =dplyr::n()
    )


# calcular os valores para ha --------------------------------------------------
# caso seja censo, substituir dados_parcela por dados_volume na linha abaixo
resumo_ha <- dados_volume|>
  dplyr::summarise(
    G_ha = conversao * (sum(g_m2)),
    volume_ha = conversao * (sum(v_ind_m3)),
    altura_med = mean(altura_total),
    N_ha =round(conversao * (dplyr::n()),digits = 0)
  )

resumo_ha <- round(resumo_ha,digits = 2) #arredondar para duas casas decimais 

# agrupar e calcular dados por classes-----------------------------------------------------------

classes <- dados_volume|>
  dplyr::mutate(
    volume_lenha = dplyr::if_else(
      dap <= 20, 
      g_m2 * altura_comercial * fator,0),
    volume_torete = dplyr::if_else(
      dap > 20, 
      g_m2 * altura_comercial * fator,0)
  )|>
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
    classe
  )|>
  dplyr::summarise(
    n = dplyr::n(),
    volume_lenha = sum(volume_lenha),
    volume_torete = sum(volume_torete),
  )|>
  dplyr::arrange(
    classe
  )|>
  dplyr::mutate(
    volume_lenha_ha = volume_lenha*conversao,
    volume_torete_ha = volume_torete*conversao,
    n_ha = n*conversao,
    n_rel = n/sum(n)
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
            c(n_ha,n),~round(.x,0)
            )
          )|>
  dplyr::relocate(
    classe, 1, 
    n, 2,
    n_ha, 3,
    volume_lenha, 4,
    volume_lenha_ha, 5
    
  )

classe_v <- classes |>
  dplyr::mutate(
    volume_total = volume_lenha + volume_torete
  )

# adicionar linha de total na tabela de classes 

classes_total <- classes |>
  dplyr::bind_rows(
    dplyr::summarise(
      classes,
      classe = "Total",
      n = sum(n, na.rm = TRUE),
      n_ha = sum(n_ha, na.rm = TRUE),
      volume_lenha = sum(volume_lenha,na.rm = TRUE),
      volume_lenha_ha = sum(volume_lenha_ha,na.rm = TRUE),
      volume_torete = sum( volume_torete,na.rm = TRUE),
      volume_torete_ha = sum( volume_torete_ha,na.rm = TRUE)
      )
  )


# estrutura vertical------------------------------------------------------------

estrutura_vertical <- dados_volume |>
  dplyr::mutate(
    estrato = dplyr::case_when(
      altura_total <= 13.0 ~ "Inferior",
      altura_total >= 13.1 & altura_total <= 17.0 ~ "Médio",
      altura_total >= 17.1 ~ "Superior"
    )
  ) |>
  dplyr::group_by(estrato) |>
  dplyr::summarise(
    altura_min = min(altura_total),
    altura_max = max(altura_total),
    individuos = dplyr::n(),
    especies = dplyr::n_distinct(especie)
  ) |>
  dplyr::mutate(
    perc_individuos = round(
      (individuos / sum(individuos)) * 100, 1),
    perc_especies = round((especies / sum(especies)) * 100, 1)
  )
estrutura_vertical <- estrutura_vertical|> 
  dplyr::mutate(
    intervalo = dplyr::case_when(
      estrato == "Inferior" ~ "0.0 - 12.9",
      estrato == "Médio" ~ "13 - 16.9",
      estrato == "Superior" ~ ">= 17",
      TRUE ~ NA_character_
    )
  )|> dplyr::relocate(intervalo, .after = 1)

# Exportar os arquivos Excel----------------------------------------------
openxlsx::write.xlsx(dados_volume, file = here::here("resultados","dados_volume.xlsx"))
openxlsx::write.xlsx(resumo_ha, file = here::here("resultados","resumo_ha.xlsx"))
openxlsx::write.xlsx(estrutura_vertical, file = here::here("resultados","estrutura_vertical.xlsx"))
openxlsx::write.xlsx(classes_total, file = here::here("resultados","classes.xlsx"))
