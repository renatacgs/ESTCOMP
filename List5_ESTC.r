# Gabriel Teodoro Ribeiro  - 11911BCC013
# Renata Cristina Gomes da Silva - 11721BCC012
# Robson Roberto Vieira Junior - 11911BCC056


dados = read.table("papagaio.txt",header = TRUE,sep = ",")
dados
#1
#a
head(dados)
tail(dados)
str(dados)
summary(dados)
#b
#tamanho

desvioCorniculataTamanho = sd(dados$tamanho[dados$especie=="corniculata"])
desvioCorniculataTamanho
desvioCirrhataTamanho = sd(dados$tamanho[dados$especie=="cirrhata"])
desvioCirrhataTamanho
desvioArcticaTamanho = sd(dados$tamanho[dados$especie=="arctica"])
desvioArcticaTamanho

mediaCorniculataTamanho = mean(dados$tamanho[dados$especie=="corniculata"])
mediaCorniculataTamanho
mediaCirrhataTamanho = mean(dados$tamanho[dados$especie=="cirrhata"])
mediaCirrhataTamanho
mediaArcticaTamanho = mean(dados$tamanho[dados$especie=="arctica"])
mediaArcticaTamanho

#peso
desvioCorniculataPeso = sd(dados$peso[dados$especie=="corniculata"])
desvioCorniculataPeso
desvioCirrhataPeso = sd(dados$peso[dados$especie=="cirrhata"])
desvioCirrhataPeso
desvioArcticaPeso = sd(dados$peso[dados$especie=="arctica"])
desvioArcticaPeso

mediaCorniculataPeso = mean(dados$peso[dados$especie=="corniculata"])
mediaCorniculataPeso
mediaCirrhataPeso = mean(dados$peso[dados$especie=="cirrhata"])
mediaCirrhataPeso
mediaArcticaPeso = mean(dados$peso[dados$especie=="arctica"])
mediaArcticaPeso

#envergadura
desvioCorniculataEnvergadura = sd(dados$envergadura[dados$especie=="corniculata"])
desvioCorniculataEnvergadura
desvioCirrhataEnvergadura = sd(dados$envergadura[dados$especie=="cirrhata"])
desvioCirrhataEnvergadura
desvioArcticaEnvergadura = sd(dados$envergadura[dados$especie=="arctica"])
desvioArcticaEnvergadura

mediaCorniculataEnvergadura = mean(dados$envergadura[dados$especie=="corniculata"])
mediaCorniculataEnvergadura
mediaCirrhataEnvergadura = mean(dados$envergadura[dados$especie=="cirrhata"])
mediaCirrhataEnvergadura
mediaArcticaEnvergadura = mean(dados$envergadura[dados$especie=="arctica"])
mediaArcticaEnvergadura


boxplot(dados$peso[dados$especie=="arctica"],dados$peso[dados$especie=="corniculata"],dados$peso[dados$especie=="cirrhata"],names = c("Arctica","Corniculata","Cirrhata"))

#c)
class(dados$especie)   
dados$especie<- as.factor(dados$especie)
class(dados$especie)


#d)
dados = dados[sample(nrow(dados)),]
n = round(nrow(dados)*0.8)

treino = dados[1:n,]
treino

teste= dados[(n+1):nrow(dados),]
teste


#E)

peso = treino$peso
peso
t = sort(peso)

medias = c()
for(j in 1:(length(peso)-1)){
  medias[j]= mean(peso[j:(j+1)])
}
medias

gini = c()
for(k in 1:(length(peso)-1)){
  separacao= treino$peso<=medias[k]
  sim = treino[separacao,]$especie
  table(sim)
  simArctica = sum(sim == "arctica")
  simCinrrhata =sum(sim == "cirrhata")
  simCorniculata =sum(sim == "corniculata")
  
  giniSim= 1- (simArctica/length(sim))**2 - (simCinrrhata/length(sim))**2 - (simCorniculata/length(sim))**2
  
  nao = treino[!separacao,]$especie
  table(nao)
  naoArctica = sum(nao == "arctica")
  naoCinrrhata =sum(nao == "cirrhata")
  naoCorniculata =sum(nao == "corniculata")
  giniNao= 1- (naoArctica/length(nao))**2 - (naoCinrrhata/length(nao))**2 - (naoCorniculata/length(nao))**2

  gini[k] = ((length(sim))/(length(sim)+length(nao)))*giniSim + ((length(nao))/(length(sim)+length(nao)))*giniNao
}
#menor é o gini
min(gini)




#F)
library(rpart)
library(rpart.plot)
arvore.dados = rpart(formula = especie ~.,data = treino)
rpart.plot(arvore.dados,extra=101)


previsao =predict(arvore.dados,newdata = teste,type="class")
taxa_acerto =mean(previsao == teste$especie)
taxa_acerto

table(previsao,teste$especie)
#Apos alguns teste foi encontrado aproximadamente 
#73% de acerto deste modelo




#2
#a)
dadosArctica  = dados[dados$especie=="arctica",]
dadosCirrhata = dados[dados$especie=="cirrhata",]
dadosCorniculata = dados[dados$especie=="corniculata",]

#b)
coef = function(x,y){
  
  n=length(x)
  n
  r1= sum(x*y) - n*mean(x)*mean(y)
  r1
  
  raizX=sqrt(sum(x**2)-n*mean(x)**2)
  raizX
  raizY=sqrt(sum(y**2)-n*mean(y)**2)
  raizY
  
  r=r1/(raizX*raizY)
  return (r)
}

x = c(1.7,1.2,2.5,2.8,3.6,2.2,0.8,1.5,2.4,5.9)
y = c(552.6,462.3,475.4,374.3,748.5,400.9,253.0,318.6,496.8,1180.6)
cor(x,y)

coef(x,y)

correArctica = coef(dados$tamanho[dados$especie=="arctica"],dados$peso[dados$especie=="arctica"])
correArctica
correCirrahata = coef(dados$tamanho[dados$especie=="cirrhata"],dados$peso[dados$especie=="cirrhata"])
correCirrahata
correCorniculata =coef(dados$tamanho[dados$especie=="corniculata"],dados$peso[dados$especie=="corniculata"])
correCorniculata
#A correlação mais forte é a dos corniculatas


#c
x = dados$tamanho[dados$especie=="corniculata"]
y = dados$peso[dados$especie=="corniculata"]
plot(x,y)

  
m = (sum(x+y)-sum(x)*sum(y))/(n*sum(x**2)-(sum(x))**2)
m
b= mean(y)-m*mean(x)
b

#d 
#A variação de 0,5cm no tamanho de uma ave provocaria uma variação
#de aproximadamente 8.779g isso ocorre pela pelo multiplicação
#de 0,5 na inclinação adicionando metade do valor de m + o 
#valor de b (intercepto de Y)

y_hat1 = m * 10 + b
y_hat2 = m * 10.5 + b
dif = y_hat2 - y_hat1


#E
detrm_peso = function (tam){
  if(class(tam)!="numeric")
  {
    print("Valor inapropriado")
    break
  }
  x = dados$tamanho[dados$especie=="corniculata"]
  y = dados$peso[dados$especie=="corniculata"]
  m = (sum(x+y)-sum(x)*sum(y))/(n*sum(x**2)-(sum(x))**2)
  m
  b= mean(y)-m*mean(x)
  b
  y_hat = m * tam + b
  return (y_hat)
}
# 3
#A
olive = read.table("olive.txt",header = TRUE,sep = ",")
olive
str(olive)
olive.normalizado = scale(olive[,2:9])
#olive.normalizado = scale(olive[1:10 ,2:9])
olive.normalizado

olive.matriz = dist(olive.normalizado)
olive.matriz

olive.modelo = hclust(d=olive.matriz,method = "ward.D2")
plot(olive.modelo,labels = olive$region)
#plot(olive.modelo,labels = olive$region[1:10])

#B
abline(h=20,col ="red")

aglomerados = cutree(olive.modelo,k=5)
aglomerados
#"Southern Italy" "Northern Italy" "Sardinia"  
#for (i in 1:5){
propAglome = function(i){
agl = c()
  agl = olive[aglomerados==i,]
  contSouthern=0
  contNorthern=0
  contSardinia=0
  nrow(agl)
  for(j in 1:nrow(agl)){
    if(agl[i,]$region =="Sardinia"){
      contSardinia=contSardinia+1
    } else if(agl[i,]$region =="Northern Italy"){
      contNorthern=contNorthern+1
    }else{
      contSouthern=contSouthern+1
    }
  }
  r1 = paste("Aglomerado[",i,"] Sardinia:",contSardinia," Northern Italy:",contNorthern," Southern Italy:",contSouthern)
  return (r1)
    
}
propAglome(1)
propAglome(2)
propAglome(3)
propAglome(4)
propAglome(5)


#c

olive.kmeans =kmeans(x=olive.normalizado,centers = 5, nstart = 50)
olive.kmeans

colors =rainbow(5)[olive.kmeans$cluster]

#install.packages("scatterplot3d") # Install
#library("scatterplot3d") # load
#shapes = c(15:19)
#scatterplot3d(olive[,2:9], pch=shapes[olive.kmeans$cluster], color=rainbow(5)[olive.kmeans$cluster])

#plot3d(olive[,2:4],col=rainbow(5)[olive.kmeans$cluster],type='s',radius = .1)
plot(olive[,2], olive[,3], type = "n")
for(i in 1:5){
  points(olive[olive.kmeans$cluster == i,2],olive[olive.kmeans$cluster == i,3],  pch =16, col = colors)
}

centroides <- olive.kmeans$centers
plot(centroides[,2], centroides[,3], type = "n")
for(i in 1:5){
  points(x = centroides[i, 2], y = centroides[i, 3], col=colors, pch = 16, cex = 2) 
}



library(rgl)
plot3d(olive[,2:4],col=colors,type='s',radius = .1,xlab="Palmitic ", ylab="Palmitoleic", zlab="Stearic")

plot3d(centroides[, 1:4],col=colors,type='s',radius = .2,xlab="Palmitic ", ylab="Palmitoleic", zlab="Stearic")

