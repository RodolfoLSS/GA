populacaoInicial <- function(){
  
  #Indivíduos
  linhas <- 16
  
  #Matriz de população
  populacao <- matrix(NA, linhas, 8)
  
  #Valores de 1 a 8 são dados aleatoriamente
  for(i in 1:linhas){
    for(j in 1:8){
    
      #Um número aleatório entre 1 e 8
      populacao[i,j] <- sample(1:8,replace = FALSE, 1) 
    }
  }
  
  return(populacao)
}

repeticoes <- function(vetor){
  
  soma <- 0
  
  #Verifica numero de itens iguais
  for(i in 1:16){
    
    j <- i + 1
    while(j <= 8){
      
      if(vetor[i] == vetor[j]){
        
        soma <- soma + 1
      }
      j <- j + 1
    }
  }
  
  return(soma)
}

#Função fitness -- verifica repetições na linha, coluna ou diagonal
fitness <- function(vetor){
  
  aux <- rep(NA, 8)
  
  #Verifica coluna
  soma <- repeticoes(vetor)
  
  #Verifica diagonal positiva
  for(i in 1:8){
    
    aux[i] <- (i - vetor[i])
  }
  soma <- soma + repeticoes(aux)
  
  #Verifica diagonal negativa
  for(i in 1:8){
    
    aux[i] <- (i + vetor[i])
  }
  soma <- soma + repeticoes(aux)
  
  return(soma)
}

#Seleção
selecao <- function(populacao){
  
  #Matriz com os pais selecionads
  pais <- matrix(NA, 8, 8)
  
  #Percorre população e analisa valores fitness
  for(i in 1:8){
    
    #Seleciona 2 indivíduos da populacao aleatoriamente
    linha1 <- sample(1:16, 1)
    linha2 <- sample(1:16, 1)
    f1 <- fitness(populacao[linha1,])
    f2 <- fitness(populacao[linha2,])
    
    if(f1 <= f2){
      
      pais[i,] <- populacao[linha1,] 
    }else{
      
      pais[i,] <- populacao[linha2,]
    }
  }
  
  return(pais)
}

crossover <- function(pais){
  
  j <- 1
  filhos <- matrix(NA, 8, 8)
  
  #Percorrendo os pais para gerar os filhos
  while(j < 8){
    
    p1 <- 1
    p2 <- 5
    while(p1 < 5){
      
      filhos[j,p1] <- pais[j,p1]
      filhos[j,p2] <- pais[j+1,p2]
      p1 <- p1 + 1
      p2 <- p2 + 1
    }
    p1 <- 1
    p2 <- 5
    while(p1 < 5){
      
      filhos[j+1, p1] <- pais[j+1, p1]
      filhos[j+1, p2] <- pais[j, p2]
      p1 <- p1 + 1
      p2 <- p2 + 1
    }
    
    j <- j + 2
  }
  
  return(filhos)
}

#Função que atualiza população inserindo filhos
atualizaPopulacao <- function(populacao, filhos){
  
  fit <- fitness(populacao[1,])
  linha <- 1
  
  #Seleciona o melhor indivíduo
  for(i in 2:16){
    
    aux <- fitness(populacao[i,])
    if(aux <= fit){
      
      fit <- aux
      linha <- i
    }
  }
  
  indices <- sample(1:16, replace = FALSE, 8)
  
  i <- 1
  while(i < 9){
    
    if(indices[i] != linha){
      
      populacao[indices[i],] <- filhos[i,]
    }
    i <- i + 1
  }
  
  #Chama a mutação
  for(i in 1:16){
    
    if(i != linha){
      
      populacao[i,] <- mutacao(populacao[i,])
    }
  }
  
  return(populacao)
}

#Função que vai fazer a mutação do algoritmo
mutacao <- function(vetor){
  
  #Gerando um número aleatório que representa a porcentagem
  p <- runif(1,0.0,1.0)
  
  if(p <= 0.03){
    
    #Pegando duas posoções aleatórias
    posic <- sample(1:8, replace = FALSE, 2)
    
    #Salvando o valor da segunda posição
    segundaP <- vetor[posic[2]]
    
    #Alterando os valores
    vetor[posic[2]] <- vetor[posic[1]]
    vetor[posic[1]] <- segundaP  
  }
  
  return(vetor)
}

#---------------------------------- MAIN --------------------------------------
geracao <- 1

populacao <- populacaoInicial() 

while(geracao < 5000){

  pais <- selecao(populacao) #Selecionamelhores pais
  filhos <- crossover(pais) #Faz o crossover dos pais
  populacao <- atualizaPopulacao(populacao, filhos)

  fit <- fitness(populacao[1,])
  melhor <- populacao[1,]
  
  #Iterando pelas linhas da população
  for(i in 2:16){
    
    if(fitness(populacao[i,]) < fit){
      
      fit <- fitness(populacao[i,])
      melhor <- populacao[i,]
    }
  }
  
  cat("Geração: ", geracao, "Repetições: ", fit, "\n")
  if(fit == 0){
    
    cat("Melhor individuo: ", melhor)
    break
  }
  
  geracao <- geracao + 1
}