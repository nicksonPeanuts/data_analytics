# -----------------
# ESERCITAZIONE R
# -----------------


# -----------------
# ESERCIZIO 1
# -----------------
print(seq(from=20, to=50))
print(seq(from=20, to=50, length=3000))

sum <- sum(seq(from=20, to=50, length=3000))
mean <- mean(seq(from=20, to=50, length=3000))


# -----------------
# ESERCIZIO 2
# -----------------

fib <- function(n){
    sum <- 0
    a <- 0
    b <- 1
    print(sum)
    print(a)
    valoriFib <- c(a,b)
    
    for(i in 1:n){
      sum <- a + b
      a <- b
      b <- sum
      valoriFib <- c(valoriFib, sum)
      print(sum)
    }

    cat("media: ", mean(valoriFib), "\n")
    cat("somma: ", sum(valoriFib), "\n")
}

fibonacci <- fib(10)


# -----------
# ESERCIZIO 3
# -----------

x1 <- runif(10, min = 0, max = 10)
x2 <- matrix(1:10, ncol = 3, nrow = 3)
x3 <- c("Nicola", "Silvestro", "Rodolfo")

x <- vector("list", length = 3)

lista = list(x1,x2,x3)
names(lista) <- c("Numeri", "Matrice", "Cantanti")

str(lista)


# ------------
# ESERCIZIO 4
# ------------

#imposto la directory di lavoro per importare il file csv
getwd()
setwd("/home/nic/Scrivania/data_analytics/ESERCITAZIONI/ES1")
getwd()

dati <- read.csv("Life.csv")

#rispondo alle domande leggendo la struttura di "dati"

str(dati)

#a)
# Le variabili sono 4 e rispettivamente sono: char, char, intero e numerico, 
# si, alcune di esse possono considerare dei fattori, in particolare la prima, la seconda varibile
# anche la terza

#b)
# Si, sono presenti delle osservazioni mancanti come suggerito ad inizio file
# Come procedere? I mancanti sono indicati con 9999
#   

#sostituisco i 9999 nel dataframe e faccio qualche conteggio

dati$value[dati$value == 9999] <- NA
is.na(dati$value)
sum(is.na(dati$value))

#c) quante osservazioni per stato e anno
#elimino la prima riga inutile per le osservazioni

dati <- dati[-1,]

#devo contare le osservazioni per stato, lo faccio usando i factor con livelli

dati$country_name <- factor(dati$country_name)
table(dati$country_name)


dati$year <- factor(dati$year)
table(dati$year)

#d) media australia & company
media_australia <- mean(dati[dati$country_code == "AUT", ]$value)

#come posso confrontarla con gli altri paesi?
#potrei fare un plot delle varie medie, ordinate

medie_generali <- tapply(dati$value, dati$country_name, mean, na.rm=TRUE)
medie_generali

barplot(sort(medie_generali))

 
#--------------
# ESERCIZIO 5
#--------------

#a)
#costruisci una matrice 2*3 composta da multipli di 2


vettore <- seq(from = 2, to = 12, by = 2)
 
matrice <- matrix(vettore, ncol = 3, nrow=2)
matrice

is.matrix(matrice)
is.array(matrice)

#TRUE in entrambi i casi, una matrice è un array, e in questo caso,
#matrice è una matrice e quindi un array

#b)

b <- matrice[,3]
str(b)
<<<<<<< HEAD
<<<<<<< HEAD
=======

>>>>>>> 5b99cf6 (first commit)
=======
>>>>>>> ed3a92d (prova)
# b è ora un vettore numerico di 2 elementi

matrix(b)


<<<<<<< HEAD
<<<<<<< HEAD


=======
>>>>>>> 5b99cf6 (first commit)
=======


>>>>>>> ed3a92d (prova)
#-------------
# ESERCIZIO 6
#-------------

#1
nazioni <- read.csv("nazioni.csv")
str(nazioni)

#2 vettore con numero di valori mancanti per ogni variabile

#uso colSums oppure apply

apply(is.na(nazioni), 2, sum)
colSums(is.na(nazioni))

#nome dei paesi in cui sono presenti valori mancanti

righe_na <- apply(nazioni, 1, function(x) any(is.na(x)))

paesi_na <- nazioni$nome[righe_na]
print(unique(paesi_na))



#eliminare valori mancanti nel dataset
nazioni <- na.omit(nazioni)
numeroRighe <- nrow(nazioni)

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> ed3a92d (prova)


frequenze_paesi <- sort(table(nazioni$areaGeo) / numeroRighe, decreasing = TRUE)
paesi_nomi <- names(frequenze_paesi)

#factor

areaGeofact <- factor(nazioni$areaGeo, levels = paesi_nomi, ordered = TRUE)

areaGeofact

<<<<<<< HEAD
=======
# Calcolo delle frequenze percentuali
freq_area <- prop.table(table(nazioni$areaGeo)) * 100

# Ordinamento decrescente
freq_area_sorted <- sort(freq_area, decreasing = TRUE)

# Visualizzazione
print(freq_area_sorted)

#factor

# Creazione dei livelli ordinati
ordered_levels <- names(freq_area_sorted)

# Creazione del factor ordinato
nazioni$areaGeofact <- factor(nazioni$areaGeo, levels = ordered_levels, ordered = TRUE)
nazioni$areaGeofact

# Rimozione della vecchia variabile
>>>>>>> 5b99cf6 (first commit)
=======
>>>>>>> ed3a92d (prova)
nazioni$areaGeo <- NULL

#7

nazioni$oil <- factor(nazioni$oil, levels= c(1, 2), labels=c("no", "yes"))
nazioni$oil

# 8 quali paesi esportano petrolio? 

paesi_esportatori <- nazioni[nazioni$oil == "yes", c("nome", "areaGeo")]
print(paesi_esportatori)

#9

tapply(nazioni$infmort, nazioni$areaGeo, mean, na.rm = TRUE )

# 10. Quante nazioni hanno un tasso di mortalità infantile superiore o uguale a 300?

sum(nazioni$infmort >= 300)

# 11. Quante delle nazioni identificate al punto 10 esportano petrolio?

nazioni_interessate <- nazioni[nazioni$infmort >= 300, ]
nrow(nazioni_interessate[nazioni_interessate$oil == "yes", ])

# 12. Dividere la finestra grafica in 2 righe e 2 colonne. In ogni spazio, rappresentare con un boxplot
# la distribuzione della mortalità infantile condizionata alla regione geografica. Impostare lo stesso
# range sull’asse y ed il titolo del grafico.

y_range <- range(nazioni$infmort, na.rm=TRUE)

par(mfrow = c(2,2))

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> ed3a92d (prova)
america <- nazioni[nazioni$areaGeo == "Americas", 4] 
africa <- nazioni[nazioni$areaGeo == "Africa", 4] 
europa <- nazioni[nazioni$areaGeo == "Europe", 4] 
asia <- nazioni[nazioni$areaGeo == "Asia", 4]
<<<<<<< HEAD
=======
america <- nazioni$infmort[nazioni$areaGeofact == "Americas"]
aftica <- nazioni$infmort[nazioni$areaGeofact == "Africas"]
europa <- nazioni$infmort[nazioni$areaGeofact == "Europe"]
asia <- nazioni$infmort[nazioni$areaGeofact == "Asia"]
>>>>>>> 5b99cf6 (first commit)
=======
>>>>>>> ed3a92d (prova)

fivenum(america)
fivenum(africa)
fivenum(europa)
fivenum(asia)

<<<<<<< HEAD
<<<<<<< HEAD
#plotting boxes
=======
#plotting boxes, non è esplicitato l'orientamento del grafico
>>>>>>> 5b99cf6 (first commit)
=======
#plotting boxes
>>>>>>> ed3a92d (prova)
boxplot(america, horizontal=TRUE, main=paste("Area: ", "america"), ylim = y_range)
boxplot(africa, horizontal=TRUE, main=paste("Area: ", "africa"), ylim = y_range)
boxplot(europa, horizontal=TRUE, main=paste("Area: ", "europa"), ylim = y_range)
boxplot(asia, horizontal=TRUE, main=paste("Area: ", "asia"), ylim = y_range)


# 13. Rappresentare con un istogramma la distribuzione del reddito. Modificare l’etichetta dell’asse x
# con il nome della variabile ed eliminare il titolo.

par(mfrow = c(1,1))

hist(nazioni$reddito, main= "", xlab="Reddito pro capite")


#14. Aggiungere al grafico precedente le mediane del reddito per area geografica utilizzando dei punti
# di colore diverso.

mediane <- tapply(nazioni$reddito, nazioni$areaGeo, median, na.rm=TRUE)

points(x = mediane, y = rep(5, length(mediane)), col = rainbow(length(mediane)),      
       pch = 19,
       cex = 1.5)


# 15. Dividere la variabile reddito in classi utilizzando le seguenti categorie: “fino a 500”, “(500, 1500]”,
# “(1500, 4000]”, “4000 e più”. Salvare la nuova variabile in un oggetto chiamato redditoCat.

redditoCat = cut(nazioni$reddito, breaks= c(-Inf,500,1500, 4000, Inf), labels=c("(0-500]", "(500-1500]", "(1500,4000]", "più di 4000"))
redditoCat

sum(redditoCat == "più di 4000", na.rm = TRUE)


tabella <- prop.table(table(redditoCat, nazioni$oil), margin = 2)
round(tabella * 100, 2)








