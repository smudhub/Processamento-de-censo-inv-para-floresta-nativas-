# Realizar estimativas dos indices fitossociologicos para censo/inventario de nativas

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
    dap = cap/pi, #diametro a altura de peito
    g = (dap^2 * pi)/40000, #area transversal
    v_comercial = g * altura_comercial * fator, # volume individual
    v_total = g * altura_total * fator #volume individual
  
)

# tirar individuos bifurcados --------------------------------------------------

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
# agrupar por especie e familia ------------------------------------------------

base_fitossociologico <- dados_volume_inv |>
  dplyr::group_by( 
    parcela,
    familia,
    especie   
) |> 
  dplyr::summarise(
    n = dplyr::n(), # número de indivíduos 
    g = sum(g, na.rm = TRUE), # área basal total
    v_total = sum( v_total, na.rm = TRUE), # volume total
    v_comercial = sum(v_comercial, na.rm = TRUE), # volume comercial
    .groups = "drop"
)|>
  dplyr::group_by(especie) |>
  dplyr::mutate(
    n_parcelas = dplyr::n_distinct(parcela)
  ) |>
  dplyr::ungroup()

base_fito_ha <- base_fitossociologico |> 
  dplyr::filter(familia != "Morta")|>
  dplyr::select(
    familia,
    especie,
    n,
    g,
    v_total,
    v_comercial
  ) |> 
  dplyr::group_by(familia, especie) |> 
  dplyr::summarise(
    familia = dplyr::first(familia),
    especie = dplyr::first(especie),
    n = sum(n)/n_parcela*conversao,
    g =  sum(g)/n_parcela*conversao,
    v_total = sum(v_total)/n_parcela*conversao,
    v_comercial =  sum(v_comercial)/n_parcela*conversao,
    .groups = "drop"
  )

View(base_fitossociologico)

# agrupar por familia ----------------------------------------------------------

base_fito_familia_ha <- base_fitossociologico |> 
  dplyr::filter(familia != "Morta")|>
  dplyr::select(
    familia,
    n,
    g,
    v_total,
    v_comercial
  )
  dplyr::group_by(familia) |> 
  dplyr::summarise(
    familia = dplyr::first(familia),
    n = sum(n)/n_parcela*conversao,
    g =  sum(g)/n_parcela*conversao,
    v_total = sum(v_total)/n_parcela*conversao,
    v_comercial =  sum(v_comercial)/n_parcela*conversao,
    .groups = "drop"
  )
# estimar indices fitossociologicos --------------------------------------------

fitossociologico <- base_fitossociologico |>
    dplyr::filter(familia != "Morta")|> 
    dplyr::group_by(
      familia, 
      especie
    )|> dplyr::summarise(
      familia =  dplyr::first(familia),
      especie = dplyr::first(especie),
      n_parcelas = dplyr::first(n_parcelas),
      n = sum(n)/n_parcela*conversao,
      g = sum(g)/n_parcela*conversao,
      v_total = sum (v_total)/n_parcela*conversao,
      v_comercial = sum(v_comercial)/n_parcela*conversao,
      .groups = "drop"
    ) |> 
    dplyr::mutate(
      dens_abs = n / conversao, # densidade absoluta (ind/ha)
      dens_rel = 100 * (n /sum(n)),  # densidade relativa (%)
      dom_abs = g / conversao, # dominancia absoluta (m²/ha)
      dom_rel = 100 * (g / sum(g)), # dominancia relativa (%)
      freq_abs = (n_parcelas/ n_parcela),  # frequencia absoluta (% de parcelas com a especie)
      freq_rel = 100 * (freq_abs / sum(freq_abs)), # frequencia relativa (%)
      IVI = dens_rel + dom_rel + freq_rel   # IVI = soma dos 3 relativos
) |>
  dplyr::arrange(desc(IVI)) #ordena os dados em forma decrescente

# arredondar
  
fitossociologico <- fitossociologico |> 
  dplyr::mutate(
    dplyr::across(
        dplyr::where(is.numeric),~round(.x,2)
        )) |> 
  dplyr::mutate(
    n = round(n,0)
  )
# exportar arquivo excel -------------------------------------------------------

openxlsx::write.xlsx (
  fitossociologico,file =  here::here ("resultados","fitossociologico.xlsx"))

