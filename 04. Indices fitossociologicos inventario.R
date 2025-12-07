# Realizar estimativas dos indices fitossociologicos para censo/inventario de nativas

# premissas pre estabelecidas (preencher de acordo com o projeto realizado)
fator <- 0.5 # fator de forma para calcular o volume
area<- 4000 # area da parcela
conversao <- 10000/area #valor de conversao para estimativa por ha (tamanho da parcela ou da area)
t <- 1.96 # valor ta tabela t para 95% , bicaudal
n_parcela <- 1 # numero de parcelas instaladas no inventario

area_amostrada_ha <- (area*n_parcela)/10000 #numero total de area amostrada caso seja inventario

# carregar base de dados (planilha excel)---------------------------------------
#pacote here controla o caminho do arquivo
dados_base <- openxlsx::read.xlsx( 
  here::here (
    'dados',
    'Ficha_de_Campo_revisada.xlsx' #nome da planilha que deverá estar na pasta dados
  )
)

n_total <- nrow(dados_base) # numero total de individuos

# padronizando nomes de colunas para os utilizados no codigo caso necessario ---
dados_base<-dados_base |> dplyr::rename(
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
dados_dendrometricos<-dados_base |> dplyr:: mutate(
  dap = cap/pi, #diametro a altura de peito
  g_m2 = (dap^2 * pi)/40000, #area transversal
  v_comercial_m3 = g_m2 * altura_comercial * fator, # volume individual
  v_total_m3 = g_m2 * altura_total * fator #volume individual
  
)

# agrupar por especie e familia ------------------------------------------------

base_fitossociologico <- dados_dendrometricos|>dplyr::group_by(
  #parcela,
  familia,especie  # se for censo retirar o argumento parcela 
) |>dplyr::summarise(
  
  n = dplyr::n(), # número de indivíduos 
  G = sum(g_m2, na.rm = TRUE), # área basal total
  V = sum( v_total_m3, na.rm = TRUE), # volume total
  v_comercial = sum(v_comercial_m3, na.rm = TRUE), # volume comercial
  #parcelas_ocorr = n_distinct(parcela), # numeros de parcelas que a especie ocorre
  .groups = "drop"
)

View(base_fitossociologico)

# estimar indices fitossociologicos---------------------------------------------

fitossociologico <- base_fitossociologico|> dplyr::mutate(
  dens_abs = n / conversao, # densidade absoluta (ind/ha)
  dens_rel = 100 * (n /sum(n)),  # densidade relativa (%)
  dom_abs = G / conversao, # dominancia absoluta (m²/ha)
  dom_rel = 100 * (G / sum(G)), # dominancia relativa (%)
  #freq_abs = 100 * (parcelas_ocorr / n_parcelas),  # frequencia absoluta (% de parcelas com a especie)
  #freq_rel = 100 * (freq_abs / sum(freq_abs)), # frequencia relativa (%)
  IVI = dens_rel + dom_rel #+ freq_rel   # IVI = soma dos 3 relativos
) |>
  dplyr::arrange(desc(IVI)) #ordena os dados em forma decrescente

# visualizar resultado
fitossociologico<-round(fitossociologico|>dplyr::select(-familia,-especie),digits = 2)
View(fitossociologico)

# exportar arquivo excel -------------------------------------------------------

openxlsx::write.xlsx ( fitossociologico,file =  here::here ("resultados","fitossociologico.xlsx")
)
openxlsx::write.xlsx(dados_dendrometricos, file = here::here("resultados","dados_dendrometricos.xlsx"))
