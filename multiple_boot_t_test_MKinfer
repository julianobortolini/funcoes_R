#' Prof. Juliano Bortolini 
#' instagram.com/profjulianobortolini
#' julianobortolini.com
#' 
#' Função para Realizar Testes t com Bootstrap para Combinações de Tratamentos
#'
#' Esta função realiza testes t de duas amostras independentes usando bootstrap 
#' para cada combinação possível de dois tratamentos (níveis de uma variável categórica).
#' O teste é feito usando a função `boot.t.test` da biblioteca `MKinfer`.
#'
#' @param data Dataframe que contém os dados do experimento.
#' @param categorical_var Nome da coluna do dataframe que contém a variável categórica representando os tratamentos.
#' @param resp Nome da coluna do dataframe que contém a variável resposta numérica a ser testada.
#' @param n_bootstrap Número de repetições bootstrap a serem realizadas para cada teste t. Default é 1000.
#'
#' @return Retorna um dataframe que contém os resultados dos testes t bootstrap para cada combinação de tratamentos.
#'   Este dataframe inclui:
#'   - `i`: O primeiro tratamento na combinação.
#'   - `j`: O segundo tratamento na combinação.
#'   - `mean_i`: Média da variável resposta para o tratamento i.
#'   - `sd_i`: Desvio padrão da variável resposta para o tratamento i.
#'   - `n_i`: Número de observações para o tratamento i.
#'   - `mean_j`: Média da variável resposta para o tratamento j.
#'   - `sd_j`: Desvio padrão da variável resposta para o tratamento j.
#'   - `n_j`: Número de observações para o tratamento j.
#'   - `t_stat`: Estatística t do teste bootstrap.
#'   - `p_value`: Valor-p do teste bootstrap.
#'
#' @examples 
#' # Simulando um dataset com três tratamentos e uma variável resposta.
#' set.seed(123)
#' data <- data.frame(
#'   treatment = sample(c("A", "B", "C"), 100, replace = TRUE),
#'   response = rnorm(100)
#' )
#' 
#' # Aplicando a função para realizar os testes t bootstrap.
#' results <- multiple_boot_t_test_MKinfer(data, "treatment", "response")
#' head(results)
#'
#' @importFrom MKinfer boot.t.test
#' @export
multiple_boot_t_test_MKinfer <- function(data, categorical_var, response, n_bootstrap = 1000) {
  
  # Removendo registros com valores NA na variável de resposta
  data <- data[!is.na(data[[response]]), ]
  
  # Obtendo todos os níveis da variável categórica
  levels_var <- unique(data[[categorical_var]])
  
  # Gerando combinações de 2 entre os níveis da variável categórica
  combinations <- combn(levels_var, 2)
  
  # Inicializando o dataframe de resultados
  results <- data.frame(
    i = character(),
    j = character(),
    mean_i = numeric(),
    sd_i = numeric(),
    n_i = integer(),
    mean_j = numeric(),
    sd_j = numeric(),
    n_j = integer(),
    t_stat = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Iterando sobre cada combinação de tratamentos
  for (col in 1:ncol(combinations)) {
    level_i <- combinations[1, col]
    level_j <- combinations[2, col]
    
    # Subconjuntos de dados para cada nível da variável categórica
    subset_i <- subset(data, data[[categorical_var]] == level_i)
    subset_j <- subset(data, data[[categorical_var]] == level_j)
    
    # Se algum subconjunto não tiver dados, continue para a próxima iteração
    if(nrow(subset_i) == 0 || nrow(subset_j) == 0) {
      next
    }
    
    # Realizando o teste t com bootstrap
    boot_result <- MKinfer::boot.t.test(subset_i[[response]], subset_j[[response]], R = n_bootstrap)
    
    # Criando um dataframe para armazenar os resultados desta combinação
    result_row <- data.frame(
      i = level_i,
      j = level_j,
      mean_i = mean(subset_i[[response]]),
      sd_i = sd(subset_i[[response]]),
      n_i = nrow(subset_i),
      mean_j = mean(subset_j[[response]]),
      sd_j = sd(subset_j[[response]]),
      n_j = nrow(subset_j),
      t_stat = boot_result$statistic,
      p_value = boot_result$p.value
    )
    
    # Anexando o resultado ao dataframe de resultados
    results <- rbind(results, result_row)
  }
  
  return(results)
}

