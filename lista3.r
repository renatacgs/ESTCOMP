#Lista 3 - Estatística Computacional

# 1
l1 = sample(1:6, size = 100000, replace = TRUE)
l2 = sample(1:6, size = 100000, replace = TRUE)
somas = l1 + l2
mean(somas == 7 | somas == 11)

urna1 = c(rep("preta", 8), rep("branca", 3), rep("vermelha", 4))
urna2 = c(rep("preta", 3), rep("branca", 5), rep("vermelha", 7))
urna3 = c(rep("preta", 4), rep("branca", 3), rep("vermelha", 2))
urna4 = c(rep("preta", 2), rep("branca", 1), rep("vermelha", 8))

# 2
bolas = c()
for(i in 1:10000) {
  d1 = sample(1:6, size = 1)
  d2 = sample(1:6, size = 1)
  sum = d1 + d2
  if(sum < 4) {
    bolas[i] = sample(urna1, size = 1)
  } else if (sum >= 4 && sum < 7) {
    bolas[i] = sample(urna2, size = 1)
  } else if(sum == 7) {
    bolas[i] = sample(urna3, size = 1)
  } else {
    bolas[i] = sample(urna4, size = 1)
  }
}
mean(bolas == "vermelha")


# FALTA EXERCICIO 3 





# 4
wins = c()

for(i in 1:1000000) {
  d1 = sample(1:6, size = 1)
  d2 = sample(1:6, size = 1)
  soma = d1 + d2
  if(soma == 7 || soma == 11) {
    wins[i] = TRUE
  } else if(soma == 2 || soma == 3 || soma == 12) {
    wins[i] = FALSE
  } else {
    somaInicial = soma
    repeat {
      d1 = sample(1:6, size = 1)
      d2 = sample(1:6, size = 1)
      soma = d1 + d2
      if(soma == 7) {
        wins[i] = FALSE
        break
      } else if(soma == somaInicial) {
        wins[i] = TRUE
        break
      }
    }
  } 
}
mean(wins)

# 5
dwith = c(0, 1, 0)
jim = c(0, 0, 1)

wins = c()
for(i in 1:100000) {
  sequencia = sample(0:1, size = 3, replace=TRUE)
  repeat {
    if(all(sequencia == dwith)) {
      wins[i] = FALSE
      break
    } else if(all(sequencia == jim)) {
      wins[i] = TRUE
      break
    }
    sequencia = c(sequencia[2:3], sample(0:1, size = 1)) 
  }
}
mean(wins)

# 6
lukeNpassos = function(n) {
  positions = c()
  for(cases in 1:100000) {
    pos = 0;
    for(i in 1:n) {
      pos = pos + sample(c(-1, 1), size = 1)
    }
    positions[cases] = pos
  }
  return(mean(positions == 0))
}
# i
lukeNpassos(4)
# ii
lukeNpassos(6)
# iii
lukeNpassos(10)
# iv
lukeNpassos(20)

# 7

f1 = function(x) {
  (1/sqrt(2*pi))*exp(-x^2/2)
}
3*mean(f1(runif(1000000, min = -1, max = 2)))


f2 = function(x) {
  cos(x)^2
}

pi*mean(f2(runif(1000000, min = 0, max = pi)))

