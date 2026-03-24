# caricamento dataset

datiUN  <- UNLifeExpectancy

# Si tratta di dati sull’aspettativa di vita in vari paesi:  
# si vuole cercare di individuare paesi con profili simili.
# Si svolga un’analisi di raggruppamento usando una delle tecniche presentate nel corso.


# iniziamo con una pulizia dei dati del dataset ATTENZIONE! IN CASO DI INCOMPRENSIONI RIFARE

sum(is.na(datiUN))

# 340 osservazioni hanno na all'interno
# valutiamo una pulizia dei dati

#controllo LE PERCENTUALI DI NA SULLE COLONNE
colMeans(is.na(datiUN))*100

# faccio una imputazione con le colonne rilevate
# al posto di NA ci metto la media
# nelle colonne SMOKING, FEMALEBOSS e RESEARCHER, c'è una alta percentuale di NA, imputiamo queste

datiUN$SMOKING[is.na(datiUN$SMOKING)] <- mean(datiUN$SMOKING, na.rm = TRUE)
mean(datiUN$SMOKING)

datiUN$FEMALEBOSS[is.na(datiUN$FEMALEBOSS)] <- mean(datiUN$FEMALEBOSS, na.rm=TRUE)

datiUN$RESEARCHERS[is.na(datiUN$RESEARCHERS)] <- mean(datiUN$RESEARCHERS, na.rm=TRUE)

datiUN$PUBLICEDUCATION[is.na(datiUN$PUBLICEDUCATION)] <- mean(datiUN$PUBLICEDUCATION, na.rm=TRUE)

datiUN$PRIVATEHEALTH[is.na(datiUN$PRIVATEHEALTH)] <- mean(datiUN$PRIVATEHEALTH, na.rm=TRUE)


#uso di opportune trasformazioni, col procedere dell'analisi vedremo cosa fare

#eseguo il clustering non gerarchico con k-means e pam

dati_numerici <- datiUN[sapply(datiUN, is.numeric)]
dati_numerici

dati_std <- scale(dati_numerici)

dati_std[is.na(dati_std)] <- 0

#km.out <- kmeans(dati_std, centers=3)
#str(km.out$cluster)
#table(km.out$cluster)
#plot(datiUN, col = (km.out$cluster+1), pch=19)

pam.out <- pam(datiUN, 2, metric = "euclidean", stand=TRUE)
pam.out

plot(datiUN, col=(pam.out$cluster+1), main="PAM K(2)", pch=19)

#plotting della silhouette
plot(pam.out, which=2, main="")



