Pedro_Wise_4 <- function(
    modelo,
    treino,
    teste = NULL,
    max_categorias = 10,
    PSI = TRUE,
    metrica = "KS",
    foco = "teste", # "treino" ou "teste"
    ganho = "ambos",# "treino" "teste" ou "ambos"
    test_pvalor = FALSE,
    test_orientacao = TRUE,
    forward_simples = TRUE,
    forward_duplo = TRUE,
    combinacao = TRUE,
    forward_triplo = TRUE,
    check_transformacao = TRUE,
    backward_simples = TRUE,
    backward_complexo = TRUE,
    n_best_duplo = 3,
    n_best_combinacao = 2,
    n_best_triplo_1 = 2,
    n_best_triplo_2 = 2,
    complexidade_maxima = 999
){
  cat("\n Inicializando função \n")
  
  cat("\n * Verificando elegibilidade da base \n")
  
  Base_elegivel <- Base_Elegibilidade(treino, max_categorias)
  if (!Base_elegivel){
    cat("\n Base possui variáveis categóricas com mais categorias que o permitido.\n")
    return(NULL)
  } else {
    cat ("  - Base elegível \n")
  }
  
  if (is.null(teste)){
    cat(" \n * Sem segunda amostra. PSI não será calculado. \n ")
  } else {
    cat("\n * Base de validação disponível. \n")
  }
  
  cat(" \n * Mapeando atributos \n ")
  atributos <- setdiff(colnames(treino), "y")
  cat(sprintf(" - Atributos encontrados: %s \n ", length(atributos)))
  
  # Remover variáveis com PSI >= 0.11, se PSI habilitado e se existe teste
  inelegiveis <- c()
  if(!is.null(teste) & PSI){
    tabela_psi <- calculate_psi_for_all_columns(treino, teste)
    tabela_psi <- tabela_psi[tabela_psi$PSI >= 0.11,]
    inelegiveis <- c(inelegiveis, tabela_psi$Variavel)
    cat(sprintf(" - Atributos removidos (PSI >= 0.11): %s \n ", length(tabela_psi$Variavel)))
  }
  
  # Identificar colunas com NA
  vars_com_na <- identifica_na(treino)
  inelegiveis <- c(inelegiveis, vars_com_na)
  cat(sprintf(" - Atributos removidos (NA): %s \n ", length(vars_com_na)))
  
  # Elegíveis finais
  elegiveis_iniciais <- setdiff(colnames(treino), c("y", inelegiveis))
  
  # Ajustar: 'df' -> 'treino' no count_types
  qtd <- count_types(treino[, elegiveis_iniciais, drop=FALSE])
  
  cat("\n Informação da base \n")
  cat(sprintf("  - Atributos elegíveis: %s \n ", length(elegiveis_iniciais)))
  cat(sprintf("  - Atributos categóricos: %s \n ", qtd[2]))
  cat(sprintf("  - Atributos numéricos: %s \n ", qtd[1]))
  
  # Se for checar orientação, extrair a "tabela_orientacao"
  if(test_orientacao){
    cat("\n * Mapeando orientação das variáveis \n")
    tabela_orientacao <- get_explicacao(treino[, c("y", elegiveis_iniciais)])
  } else {
    tabela_orientacao <- NULL
  }
  cat("  - Concluído \n")
  
  complexidade_atual <- length(get_vars(modelo))
  cat(sprintf("\n * Complexidade atual: %s", complexidade_atual))
  
  cat("\n ############################ \n")
  cat("\n Opções de modelagem \n")
  metrica_show <- ifelse(metrica == "KS", metrica, "custom")
  cat(" - Regressão Logística \n ")
  cat(sprintf("- Métrica: %s \n ", metrica_show))
  cat(sprintf("- Ganho: %s \n ", ganho))
  cat(sprintf("- Uso do p-valor: %s \n ", test_pvalor))
  cat(sprintf("- Orientação: %s \n ", test_orientacao))
  cat("\n ############################ \n")
  
  cat("\n [ Nível 1 ] \n")
  
  modelo_melhorado <- TRUE
  melhor_modelo_global <- modelo
  
  # Filtrar colunas de treino e teste só para as elegíveis + y
  treino <- treino[, c("y", elegiveis_iniciais)]
  if(!is.null(teste)){
    teste <- teste[, c("y", elegiveis_iniciais)]
  }
  
  nivel <- 1
  
  while (modelo_melhorado) {
    # Atualiza complexidade
    complexidade_atual <- length(get_vars(melhor_modelo_global))
    if (complexidade_atual >= complexidade_maxima) {
      cat("\n Modelo atingiu a complexidade máxima \n")
      return(melhor_modelo_global)
    }
    
    if (nivel == 1) {
      modelo_melhorado <- FALSE
      
      # ---------------------------
      # Exemplo: Forward simples
      # ---------------------------
      if (forward_simples) {
        cat("-> Forward simples nível 1 iniciada\n")
        
        # Quais variáveis ainda não foram usadas?
        elegiveis_atual <- filtro_vars(elegiveis_iniciais, melhor_modelo_global)
        
        # Chama função paralelizada (descrita acima)
        fwd_res <- forward_simples_step(
          modelo_atual      = melhor_modelo_global,
          elegiveis_atual   = elegiveis_atual,
          treino            = treino,
          teste             = teste,
          foco              = foco,
          ganho             = ganho,
          test_pvalor       = test_pvalor,
          test_orientacao   = test_orientacao,
          tabela_orientacao = tabela_orientacao
        )
        if(!is.null(fwd_res)) {
          # Achamos ao menos 1 var que melhorou
          var_melhor  <- fwd_res$var[1]
          nova_formula <- update_formula(melhor_modelo_global, var_melhor, "add")
          melhor_modelo_global <- glm(nova_formula, data = treino, family = binomial)
          
          ks_med <- calc_ks_score(melhor_modelo_global, treino, teste)
          if (foco == "treino") {
            ks_atual <- ks_med[1]
          } else {
            ks_atual <- ks_med[2]
          }
          
          cat(sprintf("-> [Global Update] Forward Simples: +%s => KS: %.4f\n", var_melhor, ks_atual))
          
          # Marcamos que houve melhoria
          modelo_melhorado <- TRUE
        }
      }
      
      
      if (check_transformacao) {
        melhor_modelo_global <- check_transformacao_step(melhor_modelo_global, treino, teste,
                                                         elegiveis_iniciais, test_pvalor, test_orientacao,
                                                         tabela_orientacao)
      }
      
        
        
        
      
      
      if (backward_simples) {
        # ... sua lógica ...
      }
      # ------------------------------------
    }
    
    # Exemplos para os demais níveis
    if (nivel == 2) {
      if (forward_duplo) {
        # ... lógica ...
      }
      if (check_transformacao) {
        # ... lógica ...
      }
      if (backward_simples) {
        # ... lógica ...
      }
    }
    
    if (nivel == 3) {
      if (combinacao) {
        # ... lógica ...
      }
      if (check_transformacao) {
        # ... lógica ...
      }
      if (backward_simples) {
        # ... lógica ...
      }
    }
    
    # E assim por diante...
    
  }
  
  
  return(melhor_modelo_global)
}


modelo_teste <- glm( y ~ 1 + Informacao_1_Categorica + Informacao_4_Inversa, data = treino, family = binomial )
teste_fws3 <- Pedro_Wise_4(modelo_teste,treino,teste,forward_simples = FALSE,complexidade_maxima = 5,PSI=FALSE)
end <- Sys.time()
start - end