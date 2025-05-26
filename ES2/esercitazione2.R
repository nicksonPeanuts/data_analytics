#ESERCITAZIONE DI R numero 2


#SETTING AREA DI LAVORO
getwd()
<<<<<<< HEAD
setwd("C:\\Users\\pinat\\Desktop\\Data Analytics\\ES2") #path windows
#setwd("") path linux
=======
setwd("/home/nic/Scrivania/data_analytics/ESERCITAZIONI/ES2")
>>>>>>> ed3a92d (prova)

#caricamento del dataset
dati <- read.csv("fev.csv")

#controllo dei valori mancanti
is.na.data.frame(dati)
#non ci sono dati mancanti nel dataframe


#1 SVOLGERE UNA ANALISI UNIVARIATA DI CIASCUNA VARIABILE

#plotting dei grafici nella stessa schermata
<<<<<<< HEAD
old_par <- par(no.readonly = TRUE)

=======
>>>>>>> ed3a92d (prova)
par(mfrow = c(2,3), mar = c())

#age
#evidenziare la frequenza dell'età delle persone analizzate
tabage<-table(dati$AGE)
barplot(tabage, xlab="frequenza", ylab = "età")

#fev = capacità polmonare in litri

FEV <- dati$FEV
#in questo caso sarebbe utile fare un'analisi di FEV con boxplot

fivenum(FEV)
boxplot(FEV, horizontal=TRUE, xlab= "FEV")

#non applichiamo una trasformata logaritmica, abbiamo abbastanza dati in questo modo


#HEIGHT
#variabile di altezza, non intera, avrebbe senso anche in questo caso fare un boxplot

height <- dati$HEIGHT

fivenum(height)
boxplot(height, horizontal=TRUE, xlab="height")

#SEX
#essendo una variabile binaria, la possiamo visualizzare con barplot
#in modo da contare le frequenze

barplot(table(dati$SEX))

#SMOKE
#anche questa è binaria, possiamo fare come con sex, visualizziamo le frequenze
#con un barplot


barplot(table(dati$SMOKE))



<<<<<<< HEAD
# Si usi uno strumento grafico per 
# evidenziare le eventuali differenze nella capacità polmonare per i maschi e le femmine.

par(old_par)


FEVfemale <- dati$FEV[dati$SEX=="Female"]
FEVmale <- dati$FEV[dati$SEX=="Male"]

boxplot(FEVfemale, FEVmale, names = c("Females", "Males"))

# Si conduca una analisi di regressione per prevedere la capacità polmonare 
# utlizzando come predittore la statura e si fornisca la rappresentazione grafica e gli indici per valutare 
# la qualità di tale rappresentazione. Cosa succede se la statura viene trasformata in centimetri?

par(old_par)


# plot grafico della fev "in funzione" della statura

plot(dati$HEIGHT, dati$FEV, xlab = "height", ylab = "fev")

# sembra esserci correlazione lineare
# usiamo indici per confermare o meno

print(paste("indice di covarianza fra height e fev", cov(dati$HEIGHT, dati$FEV)))

dev = sum((FEV-mean(FEV))^2)

# Y = FEV, X = height
b1 <- cov(height, FEV) / var(height)  # var(X)
b0 <- mean(FEV) - b1 * mean(height)   # Ybar - b1 * Xbar


print(paste("coefficiente di regressione: ", b1))
print(paste("intercetta: ", b0))

plot(height, FEV, pch=10)
abline(b0,b1, col=2)
=======












>>>>>>> ed3a92d (prova)




















































