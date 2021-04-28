#Lista 2
#Alunos
#Vinnicius Pereira da Silva - 11821BCC046
#Pedro Henrique da Silva Oliveira - 11811BCC040
#Renata Cristina Gomes da Silva - 11721BCC012


#Exercicio 1
#a
x1 <- c(1:20)

#b
x2 <- c(20:1)

#c
x3 <- c(x1, seq(19, 1, -1))

#d
d1 <- seq(3, 36, 3)
d2 <- seq(1, 34, 3)
d3 <- 0.1 ^ (d1) * 0.2 ^ (d2)

#e
e1 <- c(4, 6, 3)
e2 <- c(rep(e1, 9), 4)

#f
f1 <- c(4, 6, 3)
f2 <- c(rep(f1, 10), 4)


#Exercicio 2

ex21 <- seq(3, 6, 0.1)
ex22 <- exp(ex21) * cos(ex21)


#Exercicio 3
#a
ex31 <- c(10:100)
ex32 <- sum((ex31**3) + (4*(ex31**2)))

#b
ex31b <- c(10:25)
ex32b <- sum(((2**ex31b)/ex31b) + ((3**ex31b)/ex31b**2))

#Exercicio 4

xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)

#a
funcImpar <- function(x) x [x %% 2 == 1]
xVecImpar <- funcImpar(xVec)

#b 
vetorB <- yVec[-1] - xVec[-length(xVec)]

#c
vetorC <- sin(yVec[-1]) / cos(xVec[-length(xVec)])

#d
vetorD <- xVec + 2*xVec[+1] - xVec[+2]

#e
valor4E <- sum(exp(-xVec[-1]) / (xVec[-length(xVec)] + 10))

#Exercicios 5

#a
pos5A <- which(yVec > 600)

#b
valores5B <- yVec[pos5A]

#c
xVec[yVec > 600]

#d
mediaVectX <- mean(xVec)
vetor5D <- sqrt(abs(xVec - mediaVectX))

#e
valMaxVecY <- max(yVec)
pos5E <- which(valMaxVecY - yVec <= 200)
vetor5E <- yVec[pos5E]

#f
sum(xVec %% 2 == 0)

#g
xVec[order(yVec)]

yVec[c(T, F, F)]

#Exercicio 6

1 + sum(cumprod(seq(2, 38, b = 2) / seq(2, 39, b = 2)))

#Exercicio 7

install.packages(dslabs) 
library(dslabs) 
data(murders)

#a
mediaPop <- mean(murders$population)
posMax <- which.max(murders$population)
posMin <- which.min(murders$population)
stateMax <- murders$state[posMax]
stateMin <- murders$state[posMin]

#b
murders$taxa <- (murders$total * 10000) / murders$population

#c
murders$state[order(murders$total)]

#d
txMort <- murders$population / murders$total
murders$state[which.max(txMort)]
murders$state[which.min(txMort)]

#e

south <- murders[murders$region == "South", ]
west <- murders[murders$region == "West", ]
northeast <- murders[murders$region == "Northeast", ]
northcentral <- murders[murders$region == "North Central", ]

mediaSouth <- mean(south$taxa)
mediaWest <- mean(west$taxa)
mediaNortheast <- mean(northeast$taxa)
mediaNorthCentral <- mean(northcentral$taxa)

dpSouth <- sd(south$taxa)
dpWest <- sd(west$taxa)
dpNortheast <- sd(northeast$taxa)
dpNorthCentral <- sd(northcentral$taxa)

max(mediaSouth, mediaWest, mediaNortheast, mediaNorthCentral)
min(mediaSouth, mediaWest, mediaNortheast, mediaNorthCentral)

#f
boxplot(south$taxa, west$taxa, northeast$taxa, northcentral$taxa, names = c("South", "West", "Northeast", "North Central"))


