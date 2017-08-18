#Algoritmo Genético 
#Declaração de algumas variáveis globais
tamanhoPop <- 20
quantidadePais <- 20
quantidadeFilhos <- quantidadePais/2

#Função que cria a população inicial
populacaoInicial = function(){
  
  linhas <- tamanhoPop
  colunas <- 300
  matriz <- matrix(data <- NA, nrow <- tamanhoPop, ncol <- colunas)#declarando a matriz
  for(i in 1:tamanhoPop){
    
    for(j in 1:colunas){
      
      #Gerando números aleatórios entre 0 e 1
      matriz[i,j] <- sample(0:1, 1)
    }
  }

  return(matriz)
}
  
#Função fitness
fitness = function(vetorPop, vetorValor){
  
  #variaveis que vão receber a soma de cada população
  soma0 = 0
  soma1 = 0
  
  #Percorrendo um indivíduo da matriz de população inicial
  for(i in 1:300){
    
    if(vetorPop[i] == 0){
      
      soma0 <- soma0 + vetorValor[i]
    }else{
      
      soma1 = soma1 + vetorValor[i]
    }
  }
  
  #Calculado a diferença em módulo
  dif <- soma0 - soma1
  if(dif < 0){
    
    dif <- dif * (-1)
  }
  
  return(dif)
}

#Função que faz o crossover
crossover = function(pais){
  
  #Declaração de variáveis
  i <- 1 #Itera pelos pais
  k <- 1 #Itera pelos filhos
  
  #Declaração do vetor de filhos
  filhos <- matrix(NA, quantidadeFilhos, 300)
  
  #Gerando os filhos
  while(i < quantidadePais){
    
    pai1 <- pais[i,]
    pai2 <- pais[i+1,]
    
    #Percorrendo as linhas dos pais
    for(j in 1:300){
      
      if(sample(0:1, 1) == 0){
        
        filhos[k, j] <- pai1[j]
      }else{
        
        filhos[k,j] <- pai2[j]
      }
    }
    
    i <- i + 2
    k <- k + 1
  }
  
  return(filhos)
}

#Função que vai atualizar a população
atualizaPopulacao = function(populacao, filhos, valores){

  #DECLARAÇÃO DE VARIÁVEIS
  #Vetor que conterá o fitness de cada linha da matriz de população
  lista_fit <- rep(NA, tamanhoPop)
  #Matriz da nova população
  nova_populacao <- matrix(NA, tamanhoPop, 300)
  
  #Percorrendo a matriz de população para pegar o valor fitness de cada linha
  for(i in 1:tamanhoPop){
    
    fit <- fitness(populacao[i,], valores)
    lista_fit[i] <- fit
  }
  #Ordenando os fitness
  lista_fit <- sort.int(lista_fit)
  
  #Criando uma nova matriz ordenada de acordo com o fitness
  #Percorrendo o vetor de fitness
  for(i in 1:tamanhoPop){
    
    #Percorrendo as linhas da matriz que representa a antiga população
    for(j in 1:tamanhoPop){
      
      if(fitness(populacao[j,], valores) == lista_fit[i]){
        
        #Adicionando a linha da matriz antiga na posição correta da nova matriz
        nova_populacao[i,] <- populacao[j,]
      }
    }
  }
  
  #Substituindo os piores indivíduos da população pelos novos filhos
  i <- 1
  j <- quantidadeFilhos+1
  while(i < (quantidadeFilhos+1)){
    
    nova_populacao[j,] <- filhos[i,]
    i <- i + 1
  }

  return(nova_populacao)
}

#Função que faz a mutação, se necessária
mutacao = function(individuo){
  
  #Vetor de limiar
  limiar <- rep(NA, 300)
  
  #Populando o vetor de limiar
  for(i in 1:300){
    
    limiar[i] <- runif(1, 0.0, 1.0)
  }
  
  for(i in 1:300){
    
    if(limiar[i] <= 0.03){
      
      if(individuo[i] == 0){
        individuo[i] <- 1
      }else{
        
        individuo[i] <- 0
      }
    }
  }
  
  return(individuo)
}

#Função que faz a mutação de um indivíduo

#---------------------------MAIN------------------------
#Fazendo a leitura do arquivo
#Declaração de variáveis
pais <- matrix(data = NA, nrow = quantidadePais, ncol = 300)#Matriz de pais
valores <- scan("numeros_particao.txt", numeric())
populacao <- populacaoInicial()#inicializando a população
geracao <- 0 
erro <- 1000000#Um milhão

while(geracao < 1000){

  j <- 1 #Variável que vai iterar a linha do vetor de pais
  
  #Seleção
  while(j < (quantidadePais+1)){

    #Selecionando 3 indivíduos da populacao
    linha1 <- sample(1:tamanhoPop, 1)
    linha2 <- sample(1:tamanhoPop, 1)
    linha3 <- sample(1:tamanhoPop, 1)
    f1 <- fitness(populacao[linha1,], valores)#Pega um indivíduo
    f2 <- fitness(populacao[linha2,], valores)#Pega o indivíduo subsequente
    f3 <- fitness(populacao[linha3,], valores)#Pega o indivíduo subquente do subequente
    
    #Comparando o valor do do fitness de cada indivíduo e selecionando o melhor
    #Verifica se o fitness f1 é o menor
    if(f1 <= f2 && f1 <= f3){
      
      pais[j,] <- populacao[linha1,]
    }else if(f2 < f1 && f2 <= f3){#Verifica se o fitness f2 é o menor
      
      pais[j,] <- populacao[linha2,]
    }else if(f3 < f1 && f3 < f2){#Verifica se o fitness f3 é o menor
      
      pais[j,] <- populacao[linha3,]
    }
    
    j <- j + 1
  }
  
  #recebendo os filhos gerados pelo crossover dos pais
  filhos <- crossover(pais)

  #Atualizando a população com os filhos
  populacao <- atualizaPopulacao(populacao, filhos, valores)
  
  #Pegando o fitness da primeira linha
  fit <- fitness(populacao[1,],valores)
  
  #Iterando pelas linhas da população
  for(i in 2:tamanhoPop){
   
    if(fitness(populacao[i,], valores) < fit){
      
      fit <- fitness(populacao[i,], valores)
    }
  }
  for(k in 1:tamanhoPop){
    
    if(fitness(populacao[k,], valores) != fit){
      
      populacao[k,] <- mutacao(populacao[k,])
    }
  }
  
  #Imprimindo o erro do melhor indivíduo a cada geração
  cat("Geração: ", geracao, "Erro: ", fit, "\n")
  
  geracao <- geracao + 1#Itera as gerações
} 
#warnings()

