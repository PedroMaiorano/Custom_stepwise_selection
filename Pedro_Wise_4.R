Pedro_Wise_4 <- function(
    modelo,
    treino,
    teste = NULL,
    max_categorias = 10,
    PSI = TRUE,
    metrica = "KS",
    foco = c("treino","teste"),
    ganho = c("treino","teste","ambos"),
    pvalor = FALSE,
    orientacao = TRUE,
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
  
  Base_elegivel <- Base_Elegibilidade(treino,max_categorias)
  if (Base_elegivel == FALSE){
    cat("\n Base possui variaveis categoricas com mais categorias permitidas \n")
    return()
  } else {
    cat ("  - Base elegível \n")
  }
  
  
  if (is.null(teste)){
    cat(" \n * Sem uma segunda amostra \n ")
  }else{
    cat("\n * Base de validação será levada em conta")
  }
  
  cat(" \n * Mapeando atributos \n ")
  atributos <- setdiff(colnames(treino),"y")
  cat(sprintf(" - Atributos encontrados: %s \n ",length(atributos)))
  inelegiveis <- c()
  if(!is.null(teste) & PSI){
    tabela <- calculate_psi_for_all_columns(treino,teste)
    tabela <- tabela[tabela$PSI >= 0.11,]
    inelegiveis <- c(inelegiveis,tabela$Variavel)
    len_PSI <- length(inelegiveis)
    cat(sprintf(" - Atributos removidos (PSI): %s \n ",len_PSI))
  }
  
  vars_com_na <- identifica_na(treino)
  inelegiveis <- c(inelegiveis,vars_com_na)
  
  cat(sprintf(" - Atributos removidos (NA): %s \n ",length(vars_com_na)))
  
  elegiveis_iniciais <- setdiff(colnames(treino),c("y",inelegiveis))
  
  qtd <- count_types(df[,elegiveis_iniciais])
  cat("\n Informação da base \n")
  cat(sprintf("  - Atributos elegíveis: %s \n ",length(elegiveis_iniciais)))
  cat(sprintf(" - Atributos categóricos: %s \n ",qtd[2]))
  cat(sprintf(" - Atributos numércos: %s \n ",qtd[1]))
  
  if(orientacao){
  cat( "\n * Mapeando orientacao das variáveis \n")
  tabela_orientacao <- get_explicacao(treino)
  }
  cat( "  - Concluido \n")
  
  
  complexidade_atual <- length(get_vars(modelo))
  cat(sprintf("\n * Complexidade atual: %s", complexidade_atual))
  
  metrica_show = ifelse(metrica == "KS",metrica,"custom")
  cat("\n ############################ \n")
  cat("\n Opções de modelagem \n")
  cat(" - Regressão Logística \n ")
  cat(sprintf("- Métrica: %s \n ",metrica_show))
  cat(sprintf("- Ganho: %s \n ",ganho))
  cat(sprintf("- Uso do pvalor: %s \n ",pvalor))
  cat(sprintf("- Orientação: %s \n ",orientacao))
  
  cat("\n ############################ \n")
  cat("\n [ Nível 1 ] \n")
  
  #####################################
  modelo_melhorado <- TRUE
  elegiveis_iniciais <- elegiveis_iniciais
  nivel <- 1
  melhor_modelo_global <- modelo
  treino <- treino[,c("y",elegiveis_iniciais)]
  
  if(!is.null(teste)){
    teste <- teste[,c("y",elegiveis_iniciais)]
  }
    
  while (modelo_melhorado){
    
    complexidade_atual <- length(get_vars(melhor_modelo_global))
    if (complexidade_atual >= complexidade_maxima){
      
      cat("\n Modelo atingiu a complexidade máxima \n")
      
      return(melhor_modelo_global)
    }
    
    if (nivel == 1){
      modelo_melhorado < -FALSE
      
      if (forward_simples){
        
      }
      
      if (check_transformacao){
        
      }
      
      if (backward_simples){
        
      }
      
    }
    
    if (nivel == 2){
      
      if (forward_duplo){
        
      }
      
      if (check_transformacao){
        
      }
      
      if (backward_simples){
        
      }
      
    }
    
    if (nivel == 3){
      
      if (combinacao){
        
      }
      
      if (check_transformacao){
        
      }
      
      if (backward_simples){
        
      }
      
    }
    
    if (nivel == 4){
      
      if (forward_triplo){
        
      }
      
      if (check_transformacao){
        
      }
      
      if (backward_simples){
        
      }
      
    }
    
    if (nível == 5){
      if(backward_complexo){
        
        
      }
    
      
      }
    
    
    
    
    
    
    return(melhor_modelo_global)
    
  }
  
  }


Pedro_Wise_4(modelo_nulo,treino,teste)
