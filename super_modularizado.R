
### Verificações iniciais

Base_Elegibilidade <- function(df, N) {
  # Seleciona apenas as colunas que são factor ou character
  colunas_categoricas <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  
  # Data frame para armazenar resultado
  resultado <- data.frame(
    coluna = character(0),
    n_categorias = integer(0),
    stringsAsFactors = FALSE
  )
  
  # Para cada coluna categórica, conta quantas categorias existem
  for (col in colunas_categoricas) {
    n_levels <- length(unique(df[[col]]))
    if (n_levels > N) {
      resultado <- rbind(
        resultado,
        data.frame(coluna = col, n_categorias = n_levels, stringsAsFactors = FALSE)
      )
    }
  }
  if (nrow(resultado) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

if (is.null(df_teste)){
  cat(" \n Sem uma segunda amostra \n ")
}


###Funcoes recorrentes básicas

get_vars <- function(modelo) {
  all.vars(formula(modelo))[-1]
}

extrair_base <- function(var) {
  sub("_[^_]+$", "", var)
}

complexidade <- function(modelo, max){
  atual <- length(get_vars(modelo))
  
  if(atual < N ){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

filtro_vars <- function(lista, modelo, transformadas = TRUE, check_transform = NULL){
  lista_original <- lista
  
  usadas <- get_vars(modelo)
  lista <- setdiff(lista,usadas)
  
  if(transformadas){
    brutas_modelo <- extrair_base(usadas)
    brutas_lista <- extrair_base(lista)
    
    diff <- setdiff(brutas_lista, brutas_modelo)
    ######
    
    lista <- lista[
      unlist(sapply(lista, function(v) {
        base_ <- extrair_base(v)
        base_ %in% diff
      }))
    ]
  }
  
  if(!is.null(check_transform)){
    lista <- setdiff(lista_original, lista)
  }
  
  return(lista)
}

get_formula <- function(modelo){
   form <- paste(deparse(formula(modelo)), collapse = " ")
   return(form)
   }

update_formula <- function(modelo, var, acao, extra = NULL ){
  
  formula <- get_formula(modelo)
  
  if (acao == "add"){
    
    nova_formula <- as.formula(paste(formula, "+", var))
    
  }
  
  if (acao == "remove"){
    vars <- get_vars(modelo)
    vars <- setdiff(vars,var)
    
    formula_vars <- formula_str <- paste("y ~ 1 +", paste(vars, collapse = " + "))
    nova_formula <- as.formula(formula_vars)
  }
  
  if (acao == "both"){
    vars <- get_vars(modelo)
    vars <- setdiff(vars,var)
    vars <- c(vars,extra)
    
    formula_vars <- formula_str <- paste("y ~ 1 +", paste(vars, collapse = " + "))
    nova_formula <- as.formula(formula_vars)
  }
  
  return(nova_formula)
  
}

get_explicacao <- function(base) {
  # Nomes das colunas, exceto 'y'
  var_names <- setdiff(names(base), "y")
  
  # Data frame de resultado
  df_result <- data.frame(
    names       = character(),  # nome da variável
    category    = character(),  # nível da variável categórica (NA se for numérica)
    baseline    = logical(),    # se é baseline (TRUE/FALSE) - só faz sentido pra categóricas
    prop_bad    = numeric(),    # proporção de y=1 (apenas em categóricas, NA em numéricas)
    rank_risco  = numeric(),    # rank ordenado por prop_bad (categóricas). Em numéricas, NA
    orientacao  = character(),  # "positivo" ou "negativo" (numéricas). Em categóricas, NA
    stringsAsFactors = FALSE
  )
  
  for (v in var_names) {
    
    K <- base[, c("y", v)]
    
    ## 1) Se a variável for numérica, faz a lógica do 20% e 80% ##
    if (is.numeric(K[[2]])) {
      # Calcula os quantis de 20% e 80%
      qs <- quantile(K[[2]], probs = c(0.2, 0.8), na.rm = TRUE)
      lim_inf <- qs[1]
      lim_sup <- qs[2]
      
      # Subconjunto <= 20%
      subset_inf <- K[K[[2]] <= lim_inf, 1]  # pega y
      # Subconjunto >= 80%
      subset_sup <- K[K[[2]] >= lim_sup, 1]  # pega y
      
      # Proporção de bad em cada faixa
      bad_20 <- mean(subset_inf == 1, na.rm = TRUE)
      bad_80 <- mean(subset_sup == 1, na.rm = TRUE)
      
      # Define 'orientacao' com base em quem tem maior proporção de bad
      if (!is.na(bad_20) && !is.na(bad_80)) {
        if (bad_20 > bad_80) {
          orientacao <- "negativo"
        } else {
          orientacao <- "positivo"
        }
      } else {
        orientacao <- NA
      }
      
      # Monta a linha de resultado (somente 1 linha)
      row_num <- data.frame(
        names      = v,
        category   = NA,       # não faz sentido para numérica
        baseline   = FALSE,    # não faz sentido para numérica
        prop_bad   = NA,       # só usamos em categóricas
        rank_risco = NA,       # só usamos em categóricas
        orientacao = orientacao,
        stringsAsFactors = FALSE
      )
      
      df_result <- rbind(df_result, row_num)
    } 
    else if (is.factor(K[[2]]) || is.character(K[[2]])) {
      fct_var <- as.factor(K[[2]])
      lvls <- levels(fct_var)
      
      
      df_tmp <- data.frame(
        names      = v,
        category   = lvls,
        baseline   = (lvls == lvls[1]),  # Por convenção, a 1ª é a baseline
        prop_bad   = NA,
        rank_risco = NA,
        orientacao = NA,  
        stringsAsFactors = FALSE
      )
      
      for (i_lvl in seq_along(lvls)) {
        subset_rows <- (fct_var == lvls[i_lvl])
        df_tmp$prop_bad[i_lvl] <- mean(K[subset_rows, 1] == 1, na.rm=TRUE)
      }
      
      df_tmp$rank_risco <- rank(df_tmp$prop_bad, ties.method = "min", na.last="keep")
      
      
      df_result <- rbind(df_result, df_tmp)
      
    } else {}
  }
  
  return(df_result)
}

tipo_atributo <- function(Vetor) {
  type <- class(Vetor)
  qtd_unique <- length(unique(Vetor))
  
  if (type %in% c("character", "logical") || qtd_unique <= 10) {
    resposta <- "cat"
  } else {
    resposta <- "num"
  }
  return(resposta)
}

calculate_psi_categorical <- function(base, comparacao) {
  base <- as.character(base)
  comparacao <- as.character(comparacao)
  # Substituir NA por "Sem Info" diretamente nos vetores
  base[is.na(base)] <- "Sem Info"
  comparacao[is.na(comparacao)] <- "Sem Info"
  
  # Calcular as proporções das categorias nas duas distribuições
  base_dist <- prop.table(table(base))
  comparacao_dist <- prop.table(table(comparacao))
  
  # Unir todas as categorias presentes em ambas as distribuições
  categories <- union(names(base_dist), names(comparacao_dist))
  
  # Harmonizar as distribuições nas mesmas categorias
  base_dist <- base_dist[categories]
  comparacao_dist <- comparacao_dist[categories]
  
  # Calcular PSI
  psi_values <- (base_dist - comparacao_dist) * log(base_dist / comparacao_dist)
  
  # Tratar valores NaN ou Inf
  psi_values[is.nan(psi_values) | is.infinite(psi_values)] <- 0
  
  # Valor final de PSI
  psi <- sum(psi_values)
  
  return(psi)
}

calculate_psi_numeric <- function(base, comparacao) {
  # Determina quintis (0.2, 0.4, 0.6, 0.8, 1) a partir da base
  Q <- quantile(base, na.rm = TRUE, probs = c(0.2, 0.4, 0.6, 0.8, 1))
  
  # Binning para a "base"
  base_bins <- ifelse(is.na(base), "CNA",
                      ifelse(base <= Q[1], "C1",
                             ifelse(base <= Q[2], "C2",
                                    ifelse(base <= Q[3], "C3",
                                           ifelse(base <= Q[4], "C4", "C5")))))
  
  # Binning para a "comparacao"
  comp_bins <- ifelse(is.na(comparacao), "CNA",
                      ifelse(comparacao <= Q[1], "C1",
                             ifelse(comparacao <= Q[2], "C2",
                                    ifelse(comparacao <= Q[3], "C3",
                                           ifelse(comparacao <= Q[4], "C4", "C5")))))
  
  # Calcular PSI via função de categóricos
  PSI <- calculate_psi_categorical(base_bins, comp_bins)
  return(PSI)
}

engrenagem <- function(base, comparacao) {
  tipo <- tipo_atributo(base)
  
  if (tipo == "cat") {
    psi_valor <- calculate_psi_categorical(base, comparacao)
  } else {
    psi_valor <- calculate_psi_numeric(base, comparacao)
  }
  
  return(psi_valor)
}

calculate_psi_for_all_columns <- function(df_base, df_comparacao) {
  
  # Interseção de colunas que existam nas duas bases
  cols_comuns <- intersect(names(df_base), names(df_comparacao))
  
  # Data frame para armazenar resultados
  resultado <- data.frame(
    Variavel = character(),
    PSI = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop por cada coluna em comum
  for (coluna in cols_comuns) {
    
    # Vetores
    base_vec <- df_base[[coluna]]
    comp_vec <- df_comparacao[[coluna]]
    
    # Calcular PSI via 'engrenagem' (decide se cat ou num)
    psi_valor <- engrenagem(base_vec, comp_vec)
    
    # Armazenar resultado
    resultado <- rbind(
      resultado,
      data.frame(
        Variavel = coluna,
        PSI = psi_valor,
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(resultado)
}


identifica_na <- function(df) {
  names(df)[sapply(df, anyNA)]
}

count_types <- function(df) {
  # Conta quantas colunas são numéricas
  numeric_count <- sum(sapply(df, is.numeric))
  
  # Conta quantas colunas são fator OU caráter
  categorical_count <- sum(sapply(df, function(x) is.factor(x) || is.character(x)))
  
  # Retorna um vetor nomeado com a contagem
  c(Numeric = numeric_count, Categorical = categorical_count)
}

get_variable_type <- function(x) {
  # Ajuste se quiser tratar 'logical' ou 'ordered' de formas distintas.
  if (is.numeric(x)) {
    return("numérico")
  } else {
    return("categórico")
  }
}

analise_p_valor <- function(x){
  x <- sum(x > 0.05)
  if(x == 0){
    return("aprovado")
  }else{
    return("reprovado")
  }
  
}



forward_simples_step <- function(
    modelo_atual, 
    elegiveis_atual, 
    treino, 
    teste = NULL, 
    foco = "teste",
    ganho = "ambos",
    test_pvalor = FALSE,
    test_orientacao = TRUE,
    tabela_orientacao = NULL
) {
  
  # Calcula KS do modelo atual (base)
  ks_med <- calc_ks_score(modelo_atual, treino, teste)
  ks_atual_treino <- ks_med[1]
  ks_atual_teste <- ks_med[2]
  
  # Paralelização: define quantos núcleos
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # Usamos 'foreach' para iterar sobre cada variável 'v'
  resultados <- foreach(v = elegiveis_atual, .combine = rbind, .packages = c("stats"),
                        .export = c("update_formula", "calc_ks_score", "get_formula",
                                    "analise_p_valor", "get_variable_type"
                        )) %dopar% {
                          # Constrói modelo com a nova variável
                          nova_formula <- update_formula(modelo_atual, v, "add")
                          novo_modelo  <- glm(nova_formula, data = treino, family = binomial)
                          novo_ks      <- calc_ks_score(novo_modelo, treino, teste)
                          
                          # Avaliação p-valor
                          novo_pvalor  <- analise_p_valor(summary(novo_modelo)$coefficients[, 4])
                          
                          # Tipo da variável
                          var_type <- get_variable_type(treino[[v]]) 
                          
                          # Avaliação de orientação (se habilitado e for 'numérico' ou 'categórico')
                          orientacao_status <- "aprovado"  # default
                          if (test_orientacao) {
                            if (var_type == "numérico") {
                              exp_orient <- tabela_orientacao$orientacao[tabela_orientacao$names == v]
                              # Coef da var
                              new_coef   <- coef(novo_modelo)[v]
                              
                              if ((exp_orient == "positivo" && new_coef < 0) ||
                                  (exp_orient == "negativo" && new_coef > 0)) {
                                orientacao_status <- "reprovado"
                              }
                            } else if (var_type == "categórico") {
                              # Lógica resumida do seu código
                              exp_cat <- tabela_orientacao[tabela_orientacao$names == v,]
                              non_base <- exp_cat[!exp_cat$baseline, ]
                              if (nrow(non_base) > 0) {
                                coef_table <- summary(novo_modelo)$coefficients
                                # Verifica cada nível
                                actual_coefs <- sapply(non_base$category, function(cat_level) {
                                  coef_name <- paste0(v, cat_level)
                                  if (coef_name %in% rownames(coef_table)) {
                                    coef_table[coef_name, "Estimate"]
                                  } else {
                                    NA
                                  }
                                })
                                # Rank
                                actual_rank   <- rank(actual_coefs, ties.method = "min", na.last = "keep")
                                expected_rank <- non_base$rank_risco
                                corr_val      <- cor(expected_rank, actual_rank, use = "complete.obs")
                                if (is.na(corr_val) || corr_val < 0.9) {
                                  orientacao_status <- "reprovado"
                                }
                              }
                            }
                          }
                          
                          # Monta linha com os principais indicadores
                          data.frame(
                            var         = v,
                            tipo        = var_type,
                            ks_treino   = novo_ks[1],
                            ks_teste    = novo_ks[2],
                            pvalor      = novo_pvalor,
                            orientacao  = orientacao_status,
                            stringsAsFactors = FALSE
                          )
                        }
  
  # Encerra cluster
  stopCluster(cl)
  
  # Agora 'resultados' é um data.frame com 1 linha por variável testada
  # Filtra por p-valor e orientação (se habilitado)
  if (test_pvalor){
    resultados <- resultados[resultados$pvalor == "aprovado", ]
  }
  if (test_orientacao){
    resultados <- resultados[resultados$orientacao == "aprovado", ]
  }
  
  # Ordenação pelo foco
  if (foco == "treino") {
    resultados <- resultados[order(resultados$ks_treino, decreasing = TRUE), ]
  } else {
    # teste ou "ambos" – mas se for "ambos" na prática, você mencionou que sempre olha a métrica do teste
    resultados <- resultados[order(resultados$ks_teste, decreasing = TRUE), ]
  }
  
  # Filtrar por critério de 'ganho'
  if (ganho == "treino") {
    resultados <- resultados[resultados$ks_treino > ks_atual_treino, ]
  } else if (ganho == "teste") {
    resultados <- resultados[resultados$ks_teste > ks_atual_teste, ]
  } else if (ganho == "ambos") {
    resultados <- resultados[resultados$ks_treino > ks_atual_treino & 
                               resultados$ks_teste  > ks_atual_teste, ]
  }
  
  # Retorna a primeira melhor
  if (nrow(resultados) < 1) {
    return(NULL) # não há melhorias
  } else {
    return(resultados[1, , drop=FALSE]) # retorna só a top 1
  }
}



###########################################
check_transformacao_step <- function(melhor_modelo_global, treino, teste = NULL,
                                     elegiveis_iniciais, test_pvalor = FALSE,
                                     test_orientacao = TRUE) {
  cat("-> Checagem de transformação nível 1 iniciada\n")
  
  
  elegiveis_transformacao <- filtro_vars(elegiveis_iniciais, melhor_modelo_global, check_transform = TRUE)
  vars_modelo <- get_vars(melhor_modelo_global)
  
  if (length(elegiveis_transformacao) == 0) {
    cat("-> Nenhuma variável elegível para transformação encontrada.\n")
    return(melhor_modelo_global)
  }
  
  associacoes <- data.frame(var_out = character(), var_in = character(), stringsAsFactors = FALSE)
  
  for (v_out in vars_modelo) {
    base_v_out <- extrair_base(v_out)
    
    # Filtra apenas os candidatos cuja base seja igual à do var_out
    candidatos <- elegiveis_transformacao[sapply(elegiveis_transformacao, function(x) extrair_base(x) == base_v_out)]
    
    if (length(candidatos) > 0) {
      for (v_in in candidatos) {
        associacoes <- rbind(associacoes,
                             data.frame(var_out = v_out, var_in = v_in, stringsAsFactors = FALSE))
      }
    }
  }
  
  return(associacoes)
  # Data frame para armazenar os resultados de cada tentativa de troca
  resultados_transformacao <- data.frame(var_out = character(), var_in = character(),
                                         ks_treino = numeric(), ks_teste = numeric(),
                                         pvalor = character(), orientacao = character(),
                                         stringsAsFactors = FALSE)
  
  # Itera sobre cada combinação
  for(i in seq_len(nrow(associacoes))) {
    var_out <- associacoes[i, "var_out"]
    var_in  <- associacoes[i, "var_in"]
    
    # Cria nova fórmula substituindo var_out por var_in
    formula_check <- update_formula(melhor_modelo_global, var_out,"both", var_in)
    modelo_check <- glm(formula = formula_check, data = treino, family = binomial)
    
    # Calcula a métrica KS (supondo que calc_ks_score retorne um vetor com ks_treino e ks_teste)
    ks_med <- calc_ks_score(modelo_check, treino, teste)
    ks_treino <- ks_med[1]
    ks_teste  <- ks_med[2]
    
    # Avalia o p-valor geral (por exemplo, usando a função analise_p_valor aplicada aos p-valores dos coeficientes)
    novo_pvalor <- analise_p_valor(summary(modelo_check)$coefficients[, 4])
    
    # Avaliação de orientação pode ser incorporada aqui – para simplificar, vamos assumir "aprovado"
    orientacao_status <- "aprovado"
    
    # Armazena os resultados da tentativa
    resultados_transformacao <- rbind(resultados_transformacao, 
                                      data.frame(var_out = var_out, var_in = var_in,
                                                 ks_treino = ks_treino, ks_teste = ks_teste,
                                                 pvalor = novo_pvalor, orientacao = orientacao_status,
                                                 stringsAsFactors = FALSE))
  }
  
  # Ordena os resultados de acordo com a métrica desejada (por exemplo, KS no treino)
  resultados_transformacao <- resultados_transformacao[order(resultados_transformacao$ks_treino, decreasing = TRUE), ]
  
  # Obtém a métrica atual do modelo antes da troca
  ks_atual <- calc_ks_score(melhor_modelo_global, treino, teste)[1]
  
  # Verifica se houve melhoria
  if(nrow(resultados_transformacao) > 0 && resultados_transformacao$ks_treino[1] > ks_atual) {
    cat(sprintf("-> [Global Update] Transformação: substituindo %s por %s => KS: %.4f\n",
                resultados_transformacao$var_out[1], resultados_transformacao$var_in[1],
                resultados_transformacao$ks_treino[1]))
    # Atualiza o modelo global com a melhor troca encontrada
    melhor_modelo_global <- glm(update_formula(melhor_modelo_global,
                                               resultados_transformacao$var_out[1],
                                               "both",
                                               resultados_transformacao$var_in[1]),
                                data = treino, family = binomial)
  } else {
    cat("-> Nenhuma melhoria encontrada na checagem de transformação.\n")
  }
  
  return(melhor_modelo_global)
}


########################################

check_transformacao_step <- function(melhor_modelo_global, treino, teste = NULL,
                                     elegiveis_iniciais, test_pvalor = FALSE,
                                     test_orientacao = TRUE,
                                     tabela_orientacao = NULL) {
  cat("-> Checagem de transformação nível 1 iniciada\n")
  
  # Filtra as variáveis de transformação que ainda não estão no modelo
  elegiveis_transformacao <- filtro_vars(elegiveis_iniciais, melhor_modelo_global, check_transform = TRUE)
  vars_modelo <- get_vars(melhor_modelo_global)
  
  if (length(elegiveis_transformacao) == 0) {
    cat("-> Nenhuma variável elegível para transformação encontrada.\n")
    return(melhor_modelo_global)
  }
  
  # Constrói pares válidos: somente aquelas com a mesma base (usando extrair_base)
  associacoes_validas <- data.frame(var_out = character(), var_in = character(), stringsAsFactors = FALSE)
  for (v_out in vars_modelo) {
    base_v_out <- extrair_base(v_out)
    # Seleciona apenas as candidatas cuja base seja igual à do var_out
    candidatos <- elegiveis_transformacao[sapply(elegiveis_transformacao, function(x) extrair_base(x) == base_v_out)]
    if (length(candidatos) > 0) {
      for (v_in in candidatos) {
        associacoes_validas <- rbind(associacoes_validas,
                                     data.frame(var_out = v_out, var_in = v_in, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Data frame para armazenar os resultados de cada tentativa de troca
  resultados_transformacao <- data.frame(var_out = character(), var_in = character(),
                                         ks_treino = numeric(), ks_teste = numeric(),
                                         pvalor = character(), orientacao = character(),
                                         stringsAsFactors = FALSE)
  
  # Itera sobre cada par válido
  for(i in seq_len(nrow(associacoes_validas))) {
    var_out <- associacoes_validas[i, "var_out"]
    var_in  <- associacoes_validas[i, "var_in"]
    
    # Cria a nova fórmula, substituindo var_out por var_in
    formula_check <- update_formula(melhor_modelo_global, var_out,"both", var_in)
    modelo_check <- glm(formula = formula_check, data = treino, family = binomial)
    
    # Calcula a métrica KS
    ks_med <- calc_ks_score(modelo_check, treino, teste)
    ks_treino <- ks_med[1]
    ks_teste  <- ks_med[2]
    
    # Avalia o p-valor geral dos coeficientes
    novo_pvalor <- analise_p_valor(summary(modelo_check)$coefficients[, 4])
    
    # Inicializa o status de orientação como "aprovado"
    orientacao_status <- "aprovado"
    
    # Se test_orientacao estiver habilitado e a tabela de orientação for fornecida, avalia a orientação da variável transformada (var_in)
    if(test_orientacao && !is.null(tabela_orientacao)) {
      var_type <- get_variable_type(treino[[var_in]])
      if(var_type == "numérico") {
        exp_orient <- tabela_orientacao$orientacao[tabela_orientacao$names == var_in]
        new_coef <- coef(modelo_check)[var_in]
        if((exp_orient == "positivo" && new_coef < 0) ||
           (exp_orient == "negativo" && new_coef > 0)) {
          orientacao_status <- "reprovado"
        }
      } else if(var_type == "categórico") {
        exp_cat <- tabela_orientacao[tabela_orientacao$names == var_in, ]
        non_base <- exp_cat[!exp_cat$baseline, ]
        if(nrow(non_base) > 0) {
          coef_table <- summary(modelo_check)$coefficients
          actual_coefs <- sapply(non_base$category, function(cat_level) {
            coef_name <- paste0(var_in, cat_level)
            if(coef_name %in% rownames(coef_table)) {
              coef_table[coef_name, "Estimate"]
            } else {
              NA
            }
          })
          actual_rank <- rank(actual_coefs, ties.method = "min", na.last = "keep")
          expected_rank <- non_base$rank_risco
          corr_val <- cor(expected_rank, actual_rank, use = "complete.obs")
          if(is.na(corr_val) || corr_val < 0.9) {
            orientacao_status <- "reprovado"
          }
        }
      }
    }
    
    resultados_transformacao <- rbind(resultados_transformacao, 
                                      data.frame(var_out = var_out, var_in = var_in,
                                                 ks_treino = ks_treino, ks_teste = ks_teste,
                                                 pvalor = novo_pvalor,
                                                 orientacao = orientacao_status,
                                                 stringsAsFactors = FALSE))
  }
  
  # Ordena os resultados com base no KS do treino (ou outro critério que você preferir)
  resultados_transformacao <- resultados_transformacao[order(resultados_transformacao$ks_treino, decreasing = TRUE), ]
  
  # Obtém a métrica atual do modelo antes da troca
  ks_atual <- calc_ks_score(melhor_modelo_global, treino, teste)[1]
  
  # Se o melhor swap melhorar o KS e atender aos critérios (p-valor e orientação aprovados), atualiza o modelo
  if(nrow(resultados_transformacao) > 0 && 
     resultados_transformacao$ks_treino[1] > ks_atual &&
     resultados_transformacao$pvalor[1] == "aprovado" &&
     resultados_transformacao$orientacao[1] == "aprovado") {
    
    cat(sprintf("-> [Global Update] Transformação: substituindo %s por %s => KS: %.4f\n",
                resultados_transformacao$var_out[1], resultados_transformacao$var_in[1],
                resultados_transformacao$ks_treino[1]))
    
    melhor_modelo_global <- glm(update_formula(melhor_modelo_global,
                                               resultados_transformacao$var_out[1],
                                               "both",
                                               resultados_transformacao$var_in[1]),
                                data = treino, family = binomial)
  } else {
    cat("-> Nenhuma melhoria encontrada na checagem de transformação.\n")
  }
  
  return(melhor_modelo_global)
}
