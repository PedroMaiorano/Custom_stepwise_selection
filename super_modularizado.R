
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

update_formula <- function(modelo, var, acao ){
  
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
          orientacao <- "positivo"
        } else {
          orientacao <- "negativo"
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

