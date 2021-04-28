#Lista 2
#Alunos
#Vinnicius Pereira da Silva - 11821BCC046
#Pedro Henrique da Silva Oliveira - 11811BCC040
#Renata Cristina Gomes da Silva - 11721BCC012


#Exercicio 1
contador <- 0
for (i in 1:100000){
  dado <- sample(1:6, size = 2, replace = TRUE)
  if(sum(dado) == 7 || sum(dado) == 11){
    contador <- contador + 1
  }
}
contador/100000 #probabilidade de dar errado

#Exercicio 2
contador2 <- 0
urna1 <- c('P','P','P','P','P','P','B','B','B','V','V','V','V')
urna2 <- c('P','P','P','B','B','B','B','B','V','V')
urna3 <- c('P','P','P','P','B','B','V','V')

for (i in 1:100000){
  dado1 <- sample(1:6, size = 1, replace = FALSE)
  if(dado1 == 5){
    tiraBola <- sample(urna1, size = 1, replace = FALSE)
  }else if (dado1 == 2 || dado1 == 3){
    tiraBola <- sample(urna3, size = 1 , replace = FALSE)
  }else{
    tiraBola <- sample(urna2, size = 1, replace = FALSE)
  }
  
  if (tiraBola == 'V'){
    contador2 <- contador2 + 1
  }
}
contador2 / 100000

#Exercicio 3
#Exercicio 3
ganhou <- 0
for (i in 1:100000) {
  dado3 <- sample(1:6, size = 2, replace = TRUE)
  somaDado <- sum(dado3)
  
  if (somaDado == 7 || somaDado == 11) {
    ganhou <- ganhou+1
    
  }else if (somaDado == 2 || somaDado == 3 || somaDado == 12) {
    
  }else{
    
    while(TRUE) {
      dado3b <- sample(1:6, size = 2, replace = TRUE)
      somaDado2 <- sum(dado3b)
      if (somaDado2 == 7){
        break
      }else if (somaDado2 == somaDado){
        ganhou <- ganhou + 1
        break
      }
    }
  }
}

ganhou / 100000



#Exercicio 4
ganhaBran <- c(0, 1, 0)
ganhaArya <- c(0, 0, 1)
probAryaGanha <- 0
for (i in 1:100000) {
  
  jogaMoeda <- sample(0:1, size = 3, replace = TRUE)
  
  
  
  if(all(ganhaArya == jogaMoeda)) {
    probAryaGanha <- probAryaGanha + 1
  }else if (all(ganhaBran == jogaMoeda)){
  }else{
    while(TRUE) {
      
      nMoeda <- sample(0:1, size = 1, replace = FALSE)
      jogaMoeda <- c(nMoeda, head(jogaMoeda, -1))
      
      if(all(ganhaArya == jogaMoeda)) {
        probAryaGanha <- probAryaGanha + 1
        break
      }else if (all(ganhaBran == jogaMoeda)){
        break
      }
    }
  }
}

probAryaGanha / 100000

#Exercicio 5

#A Sim, porque para que ele consiga sair e volta a origem, ele so conseguiria com o numero de jogadas pares


#B 
#I
jogada <- 0 
lukeSky <- function(n){
for(i in 1:100){
  total <- 0
  jogadas <- 0
  while(jogadas < n){
    moedaHonesta <- sample(0:1, size = 1, replace = TRUE)
    if( moedaHonesta == 0){
      total <- total + 1
    
    }else if(moedaHonesta == 1){
      total <- total - 1
    
    jogadas <- jogadas +1
    }
  
  }
  if(total == 0)
    jogada <- jogada + 1
}
jogada / 100
}

#Exercicio 6
#6.1 
m <- 10000
a <- -1
b <- 2

a6 <- runif(m, min = a, max = b)

exe6 <- ((1/sqrt(2 * pi))* exp(-a6^2/2) )

sumExe6 <- ((sum(exe6)/m) * (b - a))

#6.2
m2 <- 100
a2 <- 0 
b2 <- 3.14159265


b6 <- runif(m2, min = a2, max = b2)

exe6b <- ( (cos(b6)^2))

sumExe6b <- ((sum(exe6b) / m2) * (b2 - a2) )

#Exercicio 7
geraValores <- function(n){
  ntms2 <- 0
  
  for(i in 1:n){
    nrs <- sample(c(1,2), size = 1, replace = TRUE, prob = c(1/3, 2/3))
    if (nrs == 2) {
      ntms2 <- ntms2 + 1
    }
  }
  
  ntms2/n
}


#Exercicio 8
funcEx8 <- function(p, k){
  deucerto8 <- 0
  for(i in 1:10000){
    moeda <- sample(c(0,1), size = k, replace = TRUE, prob = c(1-p, p))
    repeat{
      if(sum(moeda) == k){
      y <- length(moeda)
      break
    }else {
      moeda <- c(moeda, sample(c(0,1), size = 1, replace = TRUE, prob = c(1-p, p)))
    }
  }
  if(y > 8){
    deucerto8 <- deucerto8 + 1
  }
  }
  deucerto8 / 10000
}

#Exercicio 9
funcEx9 <- function(n){
  probX07 <- 0
  for(i in 1:n){
    randNum <- runif(1)
    varX <- (-1+sqrt(1+8 * randNum))
    if(varX < 0.7) {
      probX07 <- probX07 + 1
      espera <- sum(i * varX) 
    }
  
  }
  probX07 / n
}
