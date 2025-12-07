# Realizar estimativas dos indices fitossociologicos para censo/inventario de nativas

# premissas pre estabelecidas (preencher de acordo com o projeto realizado)-----
fator <- 0.5 # fator de forma para calcular o volume
area<- 4400 # area do censo
conversao <- 10000/area #valor de conversao para estimativa por ha (tamanho da area)

# carregar base de dados (planilha excel)---------------------------------------
#pacote here controla o caminho do arquivo
dados_base <- openxlsx::read.xlsx( 
  here::here (
    'dados',
    'Ficha_de_Campo_revisada.xlsx' #nome da planilha que deverá estar na pasta dados
  )
)

n_total <- nrow(dados_base) # numero total de individuos

# padronizando nomes de colunas para os utilizados no codigo caso necessario-----
dados_base <- dados_base |> dplyr::rename(
  id = 'INDIV',
  cap = 'CAP(cm)',
  altura_total = 'HT(m)',
  altura_comercial = 'HC(cm)',
  especie = 'ESPÉCIE' ,
  #fitossanidade = c,
  familia = 'FAMÍLIA'
 #parcela = f # caso seja inventário, se for censo excluir essa linha
)
View(dados_base)

# calcular e criar colunas de volume e area transversal ------------------------
dados_dendrometricos <- dados_base |> dplyr:: mutate(
  dap = cap/pi, #diametro a altura de peito
  g_m2 = (dap^2 * pi)/40000, #area transversal
  v_comercial_m3 = g_m2 * altura_comercial * fator, # volume individual
  v_total_m3 = g_m2 * altura_total * fator #volume individual
  
)

# tirar repeticoes de individuos bifurcados--------------------------------------

dados_dendrometricos <- dados_dendrometricos|>
  dplyr::group_by(id)|>
  dplyr::summarise(
    familia = dplyr::first(familia),
    especie = dplyr::first(especie),
    bifurcacoes = dplyr::n(),
    dap = mean(dap),
    g_m2 = sum(g_m2),
    vi_total = sum(v_total_m3),
    vi_comercial = sum(  v_comercial_m3),
    altura_comercial = mean(altura_comercial),
    altura_total = mean(altura_total)
  )

dados_dendrometricos <- dados_dendrometricos|>
  dplyr::mutate(
    dplyr::across(
      .cols= -c (familia,especie),
      .fns = ~round(.x,2)
    )
  )

# tirar individuos mortos-------------------------------------------------------
dados_dendrometricos <- dados_dendrometricos |>
  dplyr::filter(familia != "Morta")
    
# agrupar por especie e familia ------------------------------------------------

dados_especie <- dados_dendrometricos |> dplyr::group_by(
     familia,especie
) |> dplyr::summarise(
     
      n = dplyr::n(), # número de indivíduos 
      area_basal = sum(g_m2, na.rm = TRUE), # área basal total
      v_total = sum( vi_total, na.rm = TRUE), # volume total
        v_comercial = sum(vi_comercial, na.rm = TRUE), # volume comercial
      n_ha = n * conversao,
      gi_ha = area_basal * conversao,
      VT_ha = v_total * conversao,
      VC_ha = v_comercial * conversao,
      .groups = "drop"
)|> dplyr::mutate(
  dplyr::across(
    dplyr::where(is.numeric), ~ round(.x, 2)))

#agrupar por família------------------------------------------------------------
dados_familia <- dados_dendrometricos |>
   dplyr::group_by(
  familia
) |> dplyr::summarise(
  
  n = dplyr::n(), # número de indivíduos 
  G = sum(g_m2, na.rm = TRUE), # área basal total
  v_total = sum( vi_total, na.rm = TRUE), # volume total
  v_comercial = sum(vi_comercial, na.rm = TRUE), # volume comercial
  .groups = "drop"
)#|>
  # adiciona uma linha com o total geral
  familia_total <- dados_familia|>
    dplyr::bind_rows(
    dplyr::summarise(
      dados_familia, 
      familia = "Total",
      n = sum ( n, na.rm = TRUE ),
      G = sum ( G, na.rm = TRUE ),
      v_total = sum ( v_total, na.rm = TRUE ),
      v_comercial = sum ( v_comercial, na.rm = TRUE )
    )
  )
View(dados_familia)

# estimar indices fitossociologicos---------------------------------------------

fitossociologico <- dados_especie |> dplyr::mutate(
  familia = familia,
  especie = especie,
  dens_abs = n * conversao, # densidade absoluta (ind/ha)
  dens_rel = 100 * (n /sum(n)),  # densidade relativa (%)
  dom_abs = area_basal * conversao, # dominancia absoluta (m²/ha)
  dom_rel = 100 * (area_basal / sum(area_basal)), # dominancia relativa (%)
  IVI = dens_rel + dom_rel 
) |>
  dplyr::arrange(desc(IVI)#ordena os dados em forma decrescente
)
View(fitossociologico)
# arredonda valores

fitossociologico <- fitossociologico |>
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.numeric), ~ round(.x, 2)))

View ( fitossociologico )

names(fitossociologico)

# indices (pielou, shannon e simpson)-------------------------------------------

# Conta quantos indivíduos por espécie

abund <- dados_volume |> 
  dplyr::count(especie) |> 
  dplyr::pull(n)

indices <- data.frame( 
# Shannon
shannon_H = vegan::diversity(abund, "shannon"),

# Simpson
simpson_D = vegan::diversity(abund, "simpson"),

# Pielou
pielou_J = H / log(length(abund))
)

# exportar arquivo excel -------------------------------------------------------

openxlsx::write.xlsx (dados_especie,file =  here::here ("resultados","dados_especie.xlsx"))
openxlsx::write.xlsx(dados_familia, file = here::here("resultados","dados_familia.xlsx"))
openxlsx::write.xlsx(fitossociologico, file = here::here("resultados","fitossociologico.xlsx"))
openxlsx::write.xlsx(indices, file = here::here("resultados","indices.xlsx"))
