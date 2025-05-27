#ESERCITAZIONE DI R numero 2


#SETTING AREA DI LAVORO
getwd()
#setwd("C:\\Users\\pinat\\Desktop\\Data Analytics\\ES2") #path windows
#setwd("") path linux
setwd("/home/nic/Scrivania/data_analytics/ESERCITAZIONI/ES2")
#caricamento del dataset
dati <- read.csv("fev.csv")

#controllo dei valori mancanti
is.na.data.frame(dati)
#non ci sono dati mancanti nel dataframe


#1 SVOLGERE UNA ANALISI UNIVARIATA DI CIASCUNA VARIABILE

#plotting dei grafici nella stessa schermata
old_par <- par(no.readonly = TRUE)

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


#calcoliamo R^2
# 1 - SSE/SST

FEVprev <- predict(lm(FEV ~ HEIGHT, data = dati))
SSE <- sum((FEV - FEVprev)^2)

#salvo il primo indiceR1 per fare confronti in futuro
firstR2 <- 1 - SSE/dev


#------ proviamo a fare l'analisi cambiando height in cm -----

cm_height = height * 2.34
plot(cm_height, dati$FEV, xlab = "height in cm", ylab = "fev")

# sembra esserci correlazione lineare
# usiamo indici per confermare o meno

print(paste("indice di covarianza fra height e fev", cov(cm_height, dati$FEV)))

dev = sum((FEV-mean(FEV))^2)

# Y = FEV, X = height
b1 <- cov(cm_height, FEV) / var(cm_height)  # var(X)
b0 <- mean(FEV) - b1 * mean(cm_height)   # Ybar - b1 * Xbar

FEVprev <- predict(lm(FEV ~ HEIGHT*2.34, data = dati))
SSE <- sum((FEV - FEVprev)^2)

#salvo il primo indiceR1 per fare confronti in futuro
R2cm <- 1 - SSE/dev



print(paste("coefficiente di regressione con cm: ", b1))
print(paste("intercetta con cm: ", b0))
print(paste("R2: ", R2cm))

plot(cm_height, FEV, pch=10)
abline(b0,b1, col=2)

#che succede? sostanzialmente viene riscalato il coefficiente di regressione,
#ma notiamo che l'andamento della retta di regressione rimane uguale
#cambia la covarianza per una questione ri riscalatura dei dati e il coefficiente 
#R2 non cambia nei due casi



#Si calcolino i residui della regressione di cui al punto 3 e si producùa 
#un grafico con sull’asse orizzontale il valore della variabile dipendente 
#e su quello verticale i residui. Si aggiunga al grafico due linee orizzontali 
#in corrispondenza di più o meno la radice quadrata della varianza dei residui.

#calcolo dei residui

temp <- -b0 + b1*FEV
res <- cm_height - temp

model <- lm(FEV ~ cm_height)
res<-resid(model)

plot(FEV, res, xlab = "FEV osservata", ylab = "residui")

var_res <- sqrt(var(res))
abline(h = var_res, col = "red")
abline(h = -var_res, col = "red")



#Si ipotizzi che la relazione fra le due variabili non sia lineare, 
#ma lineare a tratti. 
#Cioè si immagini che vi sia una relazione lineare per le stature fino a 63 pollici 
# e un’altra per le stature oltre 63. 
#Si svolga l’analisi e si rappresenti questa su un grafico.


#dividerò il dataframe in due gruppi
subsetMaggiore <- subset(dati, dati$HEIGHT>63)

subsetMinore <- subset(dati, dati$HEIGHT<63)


#analisi dei due dataframe
model1 <- lm(FEV ~ HEIGHT, data = subsetMaggiore)
model2 <- lm(FEV  ~ HEIGHT, data = subsetMinore)


model1_b0 <- model1$coefficients[1]
model2_b0 <- model2$coefficients[1]


model1_b1 <- model1$coefficients[2]
model2_b1 <- model2$coefficients[2]

par(mfrow = c(1,2))

plot(altezza1, FEV1)
abline(model1_b0, model1_b1, col =2)

plot(altezza2, FEV2)
abline(model2_b0, model2_b1, col =2)

par(old_par)


#IN CASO PLOTTING COMPLETO
# 1. Plotta tutti i punti
plot(dati$HEIGHT, dati$FEV, 
     xlab = "HEIGHT (in pollici)", ylab = "FEV (litri)", 
     main = "Regressione lineare a tratti", pch = 16, col = "grey")

# 2. Aggiungi i punti in base alla fascia
points(subsetMinore$HEIGHT, subsetMinore$FEV, col = "blue", pch = 16)
points(subsetMaggiore$HEIGHT, subsetMaggiore$FEV, col = "darkgreen", pch = 16)

# 3. Aggiungi la retta del primo modello
abline(mod1, col = "blue", lwd = 2)

# 4. Aggiungi la retta del secondo modello
abline(mod2, col = "darkgreen", lwd = 2)

# (opzionale) Linea verticale di separazione a 63
abline(v = 63, lty = 2, col = "black")



# Si valuti la qualità della regressione a tratti con un opportuno misura in cui 
# la qualità dipende dalla distanza del valore osservato da quello previsto al quadrato. 
# Si confronti con quanto ottenuto al punto 3 e si commenti.

# distanza del valore osservato dal volore previsto al quadrato
# eseguiamo una analisi di entrambi i modelli lineari
# coefficiente di determinazione lineare

#SQR

#valori di FEV previsti per i due modelli
#resid = osservato - previsto

FEV1prev <- predict(model1)
FEV2prev <- predict(model2)

res1 <- resid(model1)
res2 <- resid(model2)

SQT1 <- sum(res1^2)
SQT2 <- sum(res2^2)

SQTtotale <- SQT1 + SQT2

#calcolo di R^2 per i modelli

summary(model1)$r.squared
summary(model2)$r.squared
firstR2


model_unico <- lm(FEV ~ HEIGHT, data = dati)
res_unico <- resid(model_unico)
SQT_unico <- sum(res_unico^2)
R2_unico <- summary(model_unico)$r.squared


cat("Errore quadratico totale (modello unico): ", SQT_unico, "\n")
cat("Errore quadratico totale (modello a tratti): ", SQTtotale, "\n\n")

cat("R² modello unico: ", R2_unico, "\n")
cat("R² modello 1 (HEIGHT < 63): ", summary(model1)$r.squared, "\n")
cat("R² modello 2 (HEIGHT > 63): ", summary(model2)$r.squared, "\n")




# PUNTO 7

# 1. Crea la variabile per il cambio di pendenza a 63
dati$D <- pmax(0, dati$HEIGHT - 63)

# 2. Modello a tratti continuo
mod_continua <- lm(FEV ~ HEIGHT + D, data = dati)

# 3. Plot dei dati
plot(dati$HEIGHT, dati$FEV, pch = 16, col = "grey",
     xlab = "HEIGHT", ylab = "FEV", main = "Regressione a tratti continua")

# 4. Rette della regressione spezzata continua

# Primo segmento (HEIGHT <= 63)
x1 <- seq(min(dati$HEIGHT), 63, length.out = 100)
y1 <- coef(mod_continua)[1] + coef(mod_continua)[2] * x1

# Secondo segmento (HEIGHT > 63)
x2 <- seq(63, max(dati$HEIGHT), length.out = 100)
y2 <- coef(mod_continua)[1] + coef(mod_continua)[2] * x2 + coef(mod_continua)[3] * (x2 - 63)

# Aggiunta delle due rette al grafico
lines(x1, y1, col = "blue", lwd = 2)
lines(x2, y2, col = "blue", lwd = 2)

# Linea verticale a 63 (facoltativa)
abline(v = 63, lty = 2)

# 5. Valutazione della qualità del modello
res_cont <- resid(mod_continua)
SQE_cont <- sum(res_cont^2)
cat("SQE modello continuo:", SQE_cont, "\n")
cat("R² modello continuo:", summary(mod_continua)$r.squared, "\n")















































