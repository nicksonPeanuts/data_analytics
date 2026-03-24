

getwd()
setwd("C:/Users/pinat/Desktop/DA/HMW1")

datiLife <- read.csv("Life.csv")

# ESERCIZIO 1

# sequenza 
sequenza <- seq(20,50)
sequenza


sequenza <- seq(20,50, by = 5)
sequenza

# calcolo dei valori
S <- sum(sequenza)
M <- mean(sequenza)
# media
M


# ESERCIZIO 2

set.seed(0)

vector <- runif(10, min=1, max = 10)
matrice <- matrix(nrow = 3, ncol = 3)
cantanti <- list("A", "B", "C")

lista <- list(vector, matrice, cantanti)
names(lista) <- c("numeri", "matrice", "cantanti")


# ESERCIZIO 3
# matrice 2x2

numeri <- seq(1,4)
multipli2 <- 2*numeri
matrice <- matrix(multipli2, nrow = 2, ncol = 2)


is.matrix(matrice)
is.array(matrice)

b <- matrice[,1]

class(b)

# convertito a matrice
matrix(b)

# ESERCIZIO 4


fibonacci <- function(n, m, s){
  if(n == 1){
    return(1)
  }else{
    if(n == 2){
      return(1)
    }
  }
  
  return(fibonacci(n-1) + fibonacci(n-2))
}

fibonacci(5)

fibonacci <- function(n){
  # facciamo iterativo
  if(n == 0){
    return(list())
  }
  if(n == 1){
    return(list(0))
  }
  
  numeri <- numeric(n)
  numeri[1] <- 0
  numeri[2] <- 1
  
  if(n > 2)
  {
    for(i in 3:n){
      numeri[i] <- numeri[i-1] + numeri[i-2]
    }
  }
  
  media <- mean(numeri)
  somma <- sum(numeri)
  
  output <- list(numeri, media, somma)
  
  return(output)
}

fibonacci(10)

# ESERCIZIO 5



summary(datiLife)
str(datiLife)




















