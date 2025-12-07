
# estimar a altura caso seja inventario de plantadas----------------------------

# esse é so um exemplo de eq para altura , rodar regressão linear para estimar alturas
estimar_altura <- function(
  b0,  
  b1,
  dados_base
  ){


b0<-1.59159615446141
b1<-0.474439381066356

dados_base |> dplyr:: mutate(
  altura = exp(
    b0 + (b1* log(DAP))) 
)}

# calcular IC , comandos pronto para analise estatistica -----------------------

IC <- function(
    x,
    area = 4400,
    conversao = 10000/area
    ) {
  media <- mean(x) * conversao
  sd_x  <- sd(x)
  erro  <- t * sd_x / sqrt(n_parcela) * conversao
  lim_inf <- media - erro
  lim_sup <- media + erro
  
  return(c(media = media, lim_inf = lim_inf, lim_sup = lim_sup))
}
