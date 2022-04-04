#PROVA2

#Renata Cristina Gomes da Silva 11721BCC012

#1
#A
sorteio <- function(n){
  u <- runif(n)
  vetor <- c()
  for(i in 1:n){
    if(u[i] <= 3/21){
      vetor <- c(vetor, "vermelha")
    }else if(u[i] <= 8/21){
      vetor <- c(vetor, "amarela")
    }else if(u[i] <= 13/21){
      vetor <- c(vetor, "verde")
    }else if(u[i] <= 20/21){
      vetor <- c(vetor, "azul")
    }else{
      vetor <- c(vetor, "preta")
    }
  }
  return (vetor)
}

#B
vetor2 <- c()
for(i in 1:10000){
  vetor <- c()
  a <- 0
  v <- 0
  repeat{
    aux <- sorteio(1)
    if(aux[1] == "amarela"){
      a <- a + 1
    }
    else if(aux[1] == "vermelha"){
      v <- v + 1
    }
    if(a >= 1 & v >= 1){
      break
    }
  }
  vetor2 <- c(vetor2, length(vetor))
}
mean(vetor2)

#2 
#A
pinguim = read.csv("./pinguim.csv", header = TRUE, sep = ',')
pinguim$species = as.factor(pinguim$species)
pinguim$sex = as.factor(pinguim$sex)
str(pinguim)

male = pinguim[pinguim$sex == 'MALE', ]

female = pinguim[pinguim$sex == 'FEMALE', ]


#B
pinguim = read.table("pinguim.csv",header = TRUE,sep = ",")
pinguim
str(pinguim)
pinguim.normalizado = scale(pinguim$sex == 'MALE')
pinguim.normalizado

pinguim.matriz = dist(pinguim.normalizado)
pinguim.matriz

pinguim.modelo = hclust(d=pinguim.matriz,method = "ward.D2")
plot(pinguim.species,labels = pinguim$sex)

#C


#D
test_locais <- c()
for(i in 1:nrow(treino)){
 test_locais[i] <- arvore_de_decisao(treino[i, ]$comprimento_asa, treino[i,]$peso)
  }
 mean(treino$local == test_locais)

#com teste
test_locais <- c()
for(i in 1:nrow(teste)){
  test_locais[i] <- arvore_de_decisao(teste[i,]$comprimento_asa, teste[i,]$peso)
}
mean(teste$local == test_locais)
table(teste$local, test_locais)

#E


#F
resultado = c()
for (i in 1:nrow(teste)) {
  resultado[i] = arvore(teste$comprimento_asa[i], teste$peso[i])
}

mean(teste$local == resultado)