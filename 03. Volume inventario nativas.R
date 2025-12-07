# processamento para inventário/censo 
# estimativas de area basal e volume 

# premissas pre estabelecidas (preencher de acordo com o projeto realizado)
fator <- 0.5 # fator de forma para calcular o volume
area<- 200 # area da parcela 
conversao <- 10000/area #valor de conversao para estimativa por ha (tamanho da parcela ou da area)
t <- 1.96 # valor ta tabela t para 95% , bicaudal
n_parcela <- 1 # numero de parcelas instaladas no inventario para calcular o desvio

# ler arquivo --------------------------------------------------------------------

dados_base <- openxlsx::read.xlsx( 
  here::here ( "dados", # here::here controla  caminho do arquivo (puxar todos os caminhos até o arquivo)
               "Ficha_de_Campo_revisada.xlsx"
  )
)

# padronizando nomes de colunas para os utilizados no codigo caso necessario -----------------

dados_base <- dados_base |> 
 dplyr::rename(
  cap = 'cap_cm',
  altura_total = 'ht_m',
  altura_comercial = 'hc_m',
  parcela = 'n_parcela',
  plaqueta = 'no_plaqueta'
)

names(dados_base)

# calcular e criar colunas de volume e area transversal ------------------


dados_volume <- dados_base |> 
  dplyr::select(
    parcela,
   individuo,
   especie,
   familia,
   cap,
   altura_total,
   altura_comercial
  )|> dplyr:: mutate( 
  dap = cap/pi, # diametro a altura de peito
  g = (dap^2 * pi)/40000, # area transversal
  v_ind = g * altura_total * fator # volume individual 
  
) 

# visualizar dados base, agora com as novas colunas 
View(dados_volume)
sapply(dados_base, class)
# tirar repeticoes de individuos bifurcados--------------------------------------

dados_base$altura_comercial <- as.numeric(as.character(dados_base$altura_comercial))

dados_volume <- dados_volume|>
  dplyr::group_by (individuo)|>
  dplyr::summarise(
    parcela = dplyr::first(parcela),
    familia = dplyr::first(familia),
    especie = dplyr::first(especie),
    bifurcacoes = dplyr::n(),
    dap = mean(dap),
    g = sum(g),
    v_ind = sum(v_ind),
    altura_comercial = mean(altura_comercial),
    altura_total = mean(altura_total),
    .groups = "drop"
  )

dados_volume <- dados_volume|>
  dplyr::mutate(
    dplyr::across(
      .cols= c (
        dap,
        g,
        altura_comercial,
        altura_total,
        v_ind
        ),
      .fns = ~round(.x,2)
    )
  )

# tirar individuos mortos-------------------------------------------------------

dados_volume <- dados_volume |>
  dplyr::filter(familia != "Morta")

View(dados_volume)  

# resumir dados por parcela ----------------------------------------------------

volume_parcela <- dados_volume|>
  dplyr::group_by(
    parcela)|>
  dplyr::summarise(
    v = sum(v_ind),
    h_total = mean(altura_total),
    h_comercial = mean(altura_comercial),
    g = sum(g),
    n = dplyr::n(),
    .groups = "drop"
  ) |> 

# calcular dados gerais para censo --------------------------------------------
resumo_total <- dados_volume|>
  dplyr::summarise(
    G_ha =sum(g_m2),
    volume_ha = sum(v_ind_m3),
    altura_med = mean(altura_total),
    N_ha =dplyr::n()
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

print(resumo_ha) # mostra o resultado das estimativas no console do r
resumo_ha<-round(resumo_ha,digits = 2) #arredondar para duas casas decimais 

# calcular IC , comandos pronto para analise estatistica -----------------------

IC <- function(x, conversao = 1) {
  media <- mean(x) * conversao
  sd_x  <- sd(x)
  erro  <- t * sd_x / sqrt(n_parcela) * conversao
  lim_inf <- media - erro
  lim_sup <- media + erro
  return(c(media = media, lim_inf = lim_inf, lim_sup = lim_sup))
}

# IC para cada variavel 

resumo_IC <- tibble( # criar uma tabela com os limites inferior e superior para o intervalo de confiança
  parametro = c("G_ha", "volume_ha", "N_ha"),
  IC_result = list(
    IC(resumo_ha$G_ha), # ic para a area basal por ha
    IC(resumo_ha$volume_ha),# ic para o volume por ha
    IC(resumo_ha$N_ha)# ic para o n de arv por ha media,lim_inf, lim_sup 
  )
) |>
  unnest_wider(IC_result) # sem precisar repetir

# Exportar os arquivos Excel-------------------------------------
openxlsx::write.xlsx(dados_volume, file = here::here("resultados","dados_volume.xlsx"))
openxlsx::write.xlsx(dados_parcela, file = here::here("analise","dados_parcela.xlsx"))
openxlsx::write.xlsx(resumo_IC, file = here::here("resultados","resumo_IC.xlsx"))
openxlsx::write.xlsx(resumo_ha, file = here::here("resultados","resumo_ha.xlsx"))
openxlsx::write.xlsx(resumo_ha, file = here::here("resultados","resumo_total.xlsx"))
