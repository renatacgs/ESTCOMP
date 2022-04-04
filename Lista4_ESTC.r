# Gabriel Teodoro Ribeiro  - 11911BCC013
# Renata Cristina Gomes da Silva - 11721BCC012
# Robson Roberto Vieira Junior - 11911BCC056

#1 
#gerando um valor da variável discreta X que possui a seguinte lei de
#probabilidade: P(X = 1) = 1/3 e P(X=2) =2/3


prop = function(n){
  cont2 = 0
  
  for (i in 1:n) {
    u <- runif(1)
    if(u <= 1/3){
      x = 1
    }else{
      x = 2
      cont2=cont2+1
    }
    
  }
  results=cont2/n
  return (results)
}

prop(100)

prop(1000)

prop(10000)


 #2
q2= function(vet){
  lambda = 1 #taxa media de ocorrencia
  p_atual = exp(-lambda)
  u = runif(1)
  j = 0
  repeat{
    if(u<=p_atual){
      x = j
      break;
    }
    else{
      j = j + 1
      p_atual = p_atual +exp(-lambda)*(lambda^j)/factorial((j))
    }
    
  }
  return (vet[x])
}

v = c(1/3,2/3)

q2(v)


#3???


inversa = function(y){
  return ((-1+sqrt(1+8*y))/2)
}

geradora = function(n){
  u = runif(n)
  return(inversa(u))
}

n = geradora(10000)
cont = 0
for (i in 1:10000){
  if(n[i] < 0.7){
    cont=cont+1
    }
}
cont

experanca_X = cont/10000
experanca_X










