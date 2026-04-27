---
  editor_options: 
  markdown: 
  wrap: 72
---
  
# 
setwd("C:/Users/pinat/Desktop/DA/HMW1")

# 
datiLife <- read.csv("Life.csv")

# ES1

# Sequenza
sequenza <- seq(20, 50)
sequenza

sequenza <- seq(20, 50, by = 5)
sequenza

# Calcolo dei valori
S <- sum(sequenza)
M <- mean(sequenza) # media
M

# ES2

set.seed(0)

vector <- runif(10, min = 1, max = 10)
matrice <- matrix(nrow = 3, ncol = 3)
cantanti <- list("A", "B", "C")

lista <- list(vector, matrice, cantanti)
names(lista) <- c("numeri", "matrice", "cantanti")

# ES3

# Matrice 2x2
numeri <- seq(1, 4)
multipli2 <- 2 * numeri
matrice <- matrix(multipli2, nrow = 2, ncol = 2)

is.matrix(matrice)
is.array(matrice)

b <- matrice[, 1]
class(b)

# Convertito a matrice
matrix(b)

# ES4

# Versione Ricorsiva
fibonacci_rec <- function(n) {
  if (n == 1) {
    return(1)
  } else if (n == 2) {
    return(1)
  }
  return(fibonacci_rec(n - 1) + fibonacci_rec(n - 2))
}

fibonacci_rec(5)

# Versione Iterativa
fibonacci <- function(n) {
  if (n == 0) return(list())
  if (n == 1) return(list(0))
  
  numeri <- numeric(n)
  numeri[1] <- 0
  numeri[2] <- 1
  
  if (n > 2) {
    for (i in 3:n) {
      numeri[i] <- numeri[i - 1] + numeri[i - 2]
    }
  }
  
  media <- mean(numeri)
  somma <- sum(numeri)
  
  cat("media: ", media, "\n")
  cat("somma: ", somma, "\n")
  
  output <- list(numeri)
  return(output)
}

fibonacci(10)

# ES5

summary(datiLife)
str(datiLife)

# a) Variabili: char, char, int, num. Alcune sono fattori (country_name, country_code)

# b) Gestione mancanti (9999 -> NA)
datiLife$value[datiLife$value == 9999] <- NA
nas <- is.na(datiLife$value)
sum(nas)

# c) Pulizia e analisi per anno
datiLife <- datiLife[-1, ] 
datiLife$year <- factor(datiLife$year)
table(datiLife$year)

# d) Media Australia
media_austria <- mean(datiLife[datiLife$country_code == "AUT", ]$value, na.rm = TRUE)
media_austria

# Confronto medie stati
medie <- tapply(datiLife$value, datiLife$country_name, mean, na.rm = TRUE)
hist(medie)

# ES6

# a)
nazioni <- read.csv("nazioni.csv")
str(nazioni)

# b) 
apply(is.na(nazioni), 2, sum)

# c) 
righe_na <- apply(nazioni, 1, function(x) any(is.na(x)))
paesi_na <- nazioni$nome[righe_na]
unique(paesi_na)

# d) Rimozione NA
nazioni_nuovo <- na.omit(nazioni)

# e) Frequenze areaGeo
numero_righe <- nrow(nazioni)
frequenze <- sort(table(nazioni$areaGeo) / numero_righe, decreasing = TRUE)
paesi <- names(frequenze)

# f) 
areaGeofac <- factor(nazioni$areaGeo, labels = paesi, ordered = TRUE)
nazioni$areaGeo <- NULL
nazioni$areaGeofac <- areaGeofac

# g) Gestione variabile oil
oilnuovo <- factor(nazioni$oil, levels = c(1, 2), labels = c("yes", "no"))
nazioni$oil <- oilnuovo

# h) Paesi esportatori petrolio
export <- nazioni[nazioni$oil == "yes", c("nome", "areaGeofac")]
export <- na.omit(export) # Pulizia per sicurezza
export

# i) 
tapply(nazioni$infmort, nazioni$areaGeofac, mean, na.rm = TRUE)

# j)
sum(nazioni$infmort >= 300, na.rm = TRUE)

# k) 
nazioni_interessate <- nazioni[which(nazioni$infmort >= 300), ]
nrow(nazioni_interessate[nazioni_interessate$oil == "yes", ])

# l) 
redditoCat <- cut(nazioni$reddito, breaks = quantile(nazioni$reddito, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 
                  include.lowest = TRUE, 
                  labels = c("1", "2", "3", "4"))
table(redditoCat)

# m) 
cat_table <- table(redditoCat)
cat_table[4] 

dis <- table(redditoCat, nazioni$areaGeofac)
dis[4, ] 
  
