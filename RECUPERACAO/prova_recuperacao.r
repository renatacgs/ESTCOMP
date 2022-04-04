#Aluna: Renata Cristina Gomes da Silva 11721BCC012

#QUESTÃO 1
# 1
# A

letterA <- function(){
  v <- c()
  counter <- 0
  
  repeat{
    
    flag <- 1
    
    y <- sum(sample(x = 1:6, size = 2, replace = TRUE))
    counter <- counter + 1
    
    if(length(v) == 0){
      v <- c(v,y)
    }else{
      for(i in 1:length(v)){
        if(v[i] == y){
          flag <- 0
          break
        }
      }
      if(flag){
        v <- c(v,y)
      }
    }
    if(length(v) == 11) break
  }
  
  return (counter)
  
}
letterA()

# B
letterB <- function(n){
  
  v <- c()
  for(i in 1:n){
    v <- c(v, letterA())
  }
  
  return (v)
  
}
letterB(10)

# C
v <- letterB(100000)
e <- mean(v) # E[X]
e
p <- length(v[v >= 22 & v <= 25])/100000 # P(22 <= X <= 25)
p

#-----------------------------------------------------------------------------------------------------

#QUESTÃO 2
pinguim = read.csv("pinguim.csv",header=TRUE,sep=",")
summary(pinguim)

#A
pinguim.Torgersen = pinguim[pinguim$island=="Torgersen",]
pinguim.Torgersen
pinguim.Biscoe = pinguim[pinguim$island=="Biscoe",]
pinguim.Biscoe
pinguim.Dream = pinguim[pinguim$island=="Dream",]
pinguim.Dream

#B
pinguim.Biscoe.Femeas = pinguim.Biscoe[pinguim.Biscoe$sex=="FEMALE",]
pinguim.Biscoe.Femeas
biscoe.normalizado = scale(pinguim.Biscoe.Femeas[,3:6])
biscoe.normalizado 
matriz = dist(biscoe.normalizado)
matriz
modelo = hclust(d=matriz,method = "ward.D2")
plot(modelo)

#C
abline(h=2,col ="blue") #12 aglomerados
#Justificativa: mesmo que houvesse muitos aglomerados, haveria uma quantidade grande de aglomerados proximos um do outro, porem essa quantidade de aglomerados que restaram tem maior semelhanca do que se houvesse mais aglomerados

aglomerados = cutree(modelo, k=12)
aglomerados

aglomerado = c()

aglomerado1= pinguim.Biscoe.Femeas[aglomerados == 1,]
aglomerado2= pinguim.Biscoe.Femeas[aglomerados == 2,]
aglomerado3= pinguim.Biscoe.Femeas[aglomerados == 3,]
aglomerado4= pinguim.Biscoe.Femeas[aglomerados == 4,]
aglomerado5= pinguim.Biscoe.Femeas[aglomerados == 5,]
aglomerado6= pinguim.Biscoe.Femeas[aglomerados == 6,]
aglomerado7= pinguim.Biscoe.Femeas[aglomerados == 7,]
aglomerado8= pinguim.Biscoe.Femeas[aglomerados == 8,]
aglomerado9= pinguim.Biscoe.Femeas[aglomerados == 9,]
aglomerado10= pinguim.Biscoe.Femeas[aglomerados == 10,]
aglomerado11= pinguim.Biscoe.Femeas[aglomerados == 11,]
aglomerado12= pinguim.Biscoe.Femeas[aglomerados == 12,]



especie_por_algomerado = function(aglomeradoN){
  quantidade = c()
  especies = c("Adelie","Cinstrap","Gentoo")
  for(i in 1:3){
    quantidade[i]=sum(aglomeradoN$species == especies[i])
    quantidade[i]=quantidade[i]/length(quantidade)
  }
  
  return (quantidade) 
}

prop1 = especie_por_algomerado(aglomerado1)
prop1
prop2 =especie_por_algomerado(aglomerado2)
prop2
prop3 =especie_por_algomerado(aglomerado3)
prop3
prop4 =especie_por_algomerado(aglomerado4)
prop4
prop5 =especie_por_algomerado(aglomerado5)
prop5
prop6 =especie_por_algomerado(aglomerado6)
prop6
prop7 =especie_por_algomerado(aglomerado7)
prop7
prop8 =especie_por_algomerado(aglomerado8)
prop8
prop9 =especie_por_algomerado(aglomerado9)
prop9
prop10 =especie_por_algomerado(aglomerado10)
prop10
prop11 =especie_por_algomerado(aglomerado11)
prop11
prop12 =especie_por_algomerado(aglomerado12)
prop12
#Comentarios:


#D
treino = read.table("treino.txt", header = TRUE, sep = " ")
summary(treino)
index = c(1,3:6)
treino = treino[,index]
summary(treino)

library(rpart)
library(rpart.plot)

arvore = rpart(formula =species ~.,data = treino)
rpart.plot(arvore,extra=101)

#E
funcaoE = function(vetor){
  # 1 = body_mass_g
  # 2 = flipper_length_mm
  # 3 = culmen_length_mm
  # 4 = culmen_depth_mm
  if(vetor[3] < 44){
    return ("Especie: Adelie")
  } else if(vetor[4] >=18){
    return ("Especie: Chinstrap")
  } else{
    return ("Especie: Gentoo")
  }
}

#F
teste = read.table("teste.txt", header = TRUE, sep = " ")
teste

previsao = predict(arvore,newdata = teste,type = "class")
acerto = mean (previsao ==teste$species)
acerto
table(previsao,teste$species)
#Comentario: esse modelo tem uma acertividade muito boa, superior a 90%, visando isso é algo prático pois tem uma taxa de erro não muito grande.