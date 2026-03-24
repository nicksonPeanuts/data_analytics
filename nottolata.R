

#esercitazione in R per il primo esame di data analytics


#parte di R

# (10) Scrivere una funzione che prenda in input due scalari numerici interi diciamo n > 1 e p >0. 
# La funzione, dopo avere controllato che i due argomenti rispettino le condizioni date, 
# creer`a una matrice che abbia n righe e p colonne popolata da numeri generati casualmente da una variabile 
# gaussiana di media 0 e varianza 1. La funzione restituir` a una lista contenente la matrice suddetta, 
# il vettore delle medie delle colonne, il vettore delle varianze delle colonne e la matrice di correlazione delle colonne. Chiamare la funzione funzione. (5) Fornire un’istruzione R che su un data frame X che contiene un 
# fattore X$y con k > 4 modalit` a selezioni solo le unit` a corrispondenti al secondo e quarto livello del fattore.

myfun <- function(n, p)
{
  if(n > 1 & p >0){
    #esegui codice ed esercizio se parametri sono ok
    miaMatrice <- matrix(rnorm(n*p, mean = 0, sd = 1), nrow=n, ncol=p)
    
    means = colMeans(miaMatrice)
    var = apply(miaMatrice, 2, var)  
    correlazione <- cor(miaMatrice)
    
    return(list(matrice=miaMatrice, media =means, varianza = var, correlazionevista = correlazione) )
        
  }else{
    print("No, i numeri devono essere diversi!")
  }
}


# (5) Fornire un’istruzione R che su un data frame X che contiene un fattore X$y con k > 4 
# modalit` a selezioni solo le unit` a corrispondenti al secondo e quarto livello del fattore.

X[X$y %in% levels(X$y)[c(2, 4)], ]


# Scrivere una funzione che prenda in input due variabili: una variabile numerica, 
# diciamo Y e un fattore diciamo X. La funzione, dopo avere verificato che le due variabili siano del tipo 
# desiderato e avere eliminato i casi per cui una delle due variabili non ` e osservata, restituir`a 
# una matrice che contiene in ogni riga media, mediana e scarto quadratico 
# medio della variabile Y per ciascuna delle modalit` a della variabile X. Chiamare la funzione funzione.


funzione <- function(Y, X){
  #check del tipo di input nella funzione
  if(!is.numeric(Y)){stop("Y deve essere numeric")}
  if(!is.factor(X)){stop("X deve essere factor")}
  
  

  dati <- data.frame(X,Y)
  dati_na <- na.omit(dati)
  
  str <- t(sapply(split(dati_na$X, dati_na$Y), function(gr){
    c(media=mean(gr), mediana = median(gr), varianza = var(gr))
  }))
  
  return(str)
}

#fornire un comando che permette di dire quanti valori mancanti ci sono in una funzione

sum(is.na(x))
#easy as hell bruh


getwd()
setwd("C:\\Users\\pinat\\Desktop\\DA")
dati <- read.csv("fev.csv")

#richiesta 1

altezza = dati$HEIGHT
devianza = var(altezza) * length(altezza) - 1 

#richiesta 2

age_std = (dati$AGE - mean(dati$AGE)) / sd(dati$AGE)
height_std = (dati$HEIGHT - mean(dati$HEIGHT)) / sd(dati$HEIGHT)
fev_std = (dati$FEV - mean(dati$FEV)) / sd(dati$FEV)

library(knitr)


datiSTD = data.frame(age_std, height_std, fev_std)
covarianza <- cov(datiSTD)

#richiesta 3

#indice CHI QUADRO fra smoke e sex

dati2 <- table(dati$SEX, dati$SMOKE)

chisq.test(dati2)


#richiesta 4, indice personalizzato

q <- quantile(dati$FEV, probs=c(0.1, 0.5, 0.9))
K <- (q[3] + q[1] - 2*q[2]) / q[3] - q[1]
print(K)


#richiesta 5 prodotto logico

dati$GRUPPO <- interaction(dati$SMOKE, dati$SEX)

tapply(dati$FEV, dati$GRUPPO, mean)


#richiesta di regressione lineare



regressione <- lm(FEV ~ AGE + SMOKE, data = dati )

summary(regressione)
anova(regressione)


#APPELLO A

# devianza di FEV

devianza <- var(dati$FEV) * (length(dati$FEV) -1)


# richiesta 2 relazione lineare per quali coppie è piu forte


mod1 <- lm(dati$AGE ~ dati$FEV)
mod2 <- lm(dati$AGE ~ dati$HEIGHT)
mod3 <- lm(dati$FEV ~ dati$HEIGHT)

summary(mod1)
summary(mod2)
summary(mod3)

#controllo R^2 tanto piu alto quanto meglio per la correlazione lineare



#richiesta 3
#differenza fra 80esimo e 20esimo percentile



q <- quantile(dati$FEV, probs = c(0.8, 0.2))
devs <- q[1] - q[2]
devs

#richiesta 4 odds ratio fra smoke e sex

tab <- table(dati$SMOKE, dati$SEX)
odds_ratio <- tab[1,1] * tab[2,2] / (tab[1,2] * tab[2,1])


#richiesta 5 prodotto logico

logical <- interaction(dati$SMOKE, dati$SEX)

tapply(dati$FEV, logical, mean)

#richiesta di regressione multipla


modello <- lm(FEV ~ SMOKE + SEX, data = dati)

summary(modello)
anova(modello)



# ESAME DI PROVA



# Scrivere una funzione che prenda in input un dataframe ed il nome di una variabile numerica e 
# restituisca in output il dataframe con l'aggiunta di una
# nuova variabile. La nuova variabile sarà la variabile speci cata in input
# divisa in categorie secondo i quantili della variabile numerica.

miaFun <- function(dataframe, name){
  valoriVariabile <- dataframe[[name]]
  
  quantili <- quantile(valoriVariabile, probs = c(0,0.25,0.50,0.75,1), na.rm=TRUE)
  
  categorie <- cut(valoriVariabile, breaks = quantili, labels = c("q1", "q2", "q3","q4","q5")) 
  
  new_name <- paste0(name, "_quantile")
  
  dataframe[[name]] <- categorie
  
}

#2. Riportare un esempio di comando che restituisca le medie per colonna di matrici organizzate in una lista.

lapply(matrici, colMeans)















