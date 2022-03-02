#1
read.csv("chicago.csv")
mortes <- read.csv("chicago.csv")

mortes

#a
mortes$cvd = sum(death == "cvd")
mortes$cvd



#1b
a <-mortes$year[sort(mortes$resp)]
a <-a[length(a)]
a

#c

read.csv("chicago.csv")


winter <- mortes[mortes$season == "Winter", ]
spring <- mortes[mortes$season == "Spring", ]
summer <- mortes[mortes$season == "Summer", ]
autumn <- mortes[mortes$season == "Autumn", ]

mediawinter <- mean(winter$taxa)
mediaspring <- mean(spring$taxa)
mediasummer <- mean(summer$taxa)
mediaautumn <- mean(autumn$taxa)



#2
baralho <- function() {
  return(sample(c(1:13, 1:13, 1:13, 1:13), size = 3, replace = FALSE))
}

#2a
v<-1
for(i in 1:52)
  bolaverde <- sample (c(1:52), size = 3)
if(bolaverde<-1){
  baralho <- sample(c(1:52), size = 3)
  if(baralhos <-1){
    v<-v+1
  }
}
v/52

#b

v<-1
for(i in 1:10000){
  terceiracarta <- sample(c(1:52), size = 3)
  if(terceiracarta==8){ 
    v<-v+1
  }
}

v/10000

#c
V<-1
for(i in 1:10000){
  todascartas <- sample(c(1:52), size = 3)
  if(todascartas==todascartas)
    v<-v+1
}
v/10000