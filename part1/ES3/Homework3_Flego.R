#-----------------------------------------------------------------------
# ESERCIZIO 1: ANALISI DI RAGGRUPPAMENTO (CLUSTERING) - VERSIONE CORRETTA
#-----------------------------------------------------------------------

# 0. Installazione e caricamento delle librerie necessarie
# Se non hai mai installato queste librerie, decommenta ed esegui le righe seguenti
# install.packages("factoextra")
# install.packages("ggplot2")

library(factoextra)
library(ggplot2)

cat("--- Inizio Esercizio 1: Analisi di Raggruppamento ---\n\n")

# 1. Caricamento e Pulizia dei Dati
cat("1. Caricamento e pulizia dei dati sull'aspettativa di vita...\n")
url_life <- "https://instruction.bus.wisc.edu/jfrees/jfreesbooks/regression%20modeling/bookwebdec2010/CSVData/UNLifeExpectancy.csv"
life_data <- read.csv(url_life)

# Rimuoviamo le righe con valori mancanti (NA)
life_data_clean <- na.omit(life_data)

# Salviamo i nomi dei paesi per l'interpretazione finale
countries <- life_data_clean$Country

# Ispezioniamo la struttura dei dati per identificare variabili numeriche
cat("Struttura del dataset:\n")
str(life_data_clean)
cat("\n")

# Selezioniamo solo le variabili numeriche per il clustering
numeric_cols <- sapply(life_data_clean, is.numeric)
life_data_for_clustering <- life_data_clean[, numeric_cols]

# Verifichiamo che abbiamo variabili numeriche
if(ncol(life_data_for_clustering) == 0) {
  stop("Nessuna variabile numerica trovata nel dataset!")
}

cat("Variabili numeriche selezionate:", names(life_data_for_clustering), "\n")
cat("Dati caricati e puliti. Dimensioni del dataset per clustering:", dim(life_data_for_clustering), "\n\n")

# 2. Trasformazione delle Variabili
cat("2. Standardizzazione delle variabili...\n")
life_data_scaled <- scale(life_data_for_clustering)
cat("Variabili standardizzate con successo.\n\n")

# 3. Scelta del Numero Ottimale di Cluster (k)
cat("3. Valutazione del numero ottimale di cluster (k).\n")
cat("    Verranno generati due grafici: Metodo del Gomito e Metodo della Silhouette.\n")
cat("    Chiudi le finestre dei grafici per continuare l'esecuzione dello script.\n\n")

# Metodo del Gomito (WSS)
elbow_plot <- fviz_nbclust(life_data_scaled, kmeans, method = "wss", k.max = 10) +
  labs(subtitle = "Metodo del Gomito",
       title = "Numero Ottimale di Cluster") +
  theme_minimal()
print(elbow_plot)

# Metodo della Silhouette
silhouette_plot <- fviz_nbclust(life_data_scaled, kmeans, method = "silhouette", k.max = 10) +
  labs(subtitle = "Metodo della Silhouette",
       title = "Numero Ottimale di Cluster") +
  theme_minimal()
print(silhouette_plot)

# Sulla base dei grafici, scegliamo k=3
k_optimale <- 3
cat("Scelta di k =", k_optimale, "cluster per l'analisi.\n\n")

# 4. Esecuzione dell'Algoritmo K-means
cat("4. Esecuzione dell'algoritmo K-means con k =", k_optimale, "...\n")
set.seed(123) # Per garantire la riproducibilità dei risultati
kmeans_result <- kmeans(life_data_scaled, centers = k_optimale, nstart = 25)

# Aggiungiamo l'appartenenza al cluster al nostro dataset pulito
life_data_clean$cluster <- kmeans_result$cluster
cat("Algoritmo K-means completato.\n\n")

# 5. Valutazione e Interpretazione dei Cluster
cat("5. Valutazione e interpretazione dei cluster ottenuti.\n\n")

# Stampa delle medie delle variabili per ciascun cluster per l'interpretazione
cat("Medie delle variabili per ogni cluster (sui dati originali):\n")
cluster_means <- aggregate(life_data_for_clustering, by=list(cluster=kmeans_result$cluster), mean)
print(cluster_means)
cat("\n")

# Tabella di frequenza dei cluster
cat("Numero di paesi per cluster:\n")
print(table(kmeans_result$cluster))
cat("\n")

# Visualizzazione dei cluster utilizzando le prime due componenti principali (PCA)
cat("Generazione del grafico dei cluster...\n")
cluster_plot <- fviz_cluster(kmeans_result, data = life_data_scaled,
                             geom = "point",
                             ellipse.type = "convex",
                             ggtheme = theme_minimal(),
                             main = "Clustering dei Paesi per Aspettativa di Vita") +
  labs(title = "Analisi dei Cluster - Aspettativa di Vita",
       subtitle = paste("K-means con k =", k_optimale),
       caption = "Visualizzazione basata su PCA")
print(cluster_plot)

# Mostriamo alcuni paesi rappresentativi per ogni cluster
cat("Primi paesi per ogni cluster:\n")
for(i in 1:k_optimale) {
  cat("Cluster", i, ":\n")
  cluster_countries <- countries[kmeans_result$cluster == i]
  print(head(cluster_countries, 10))
  cat("\n")
}

cat("--- Fine Esercizio 1 ---\n\n\n")
cat("--- Inizio Esercizio 2: Modello di Regressione Multipla ---\n\n")

# 1. Caricamento Dati
cat("1. Caricamento del dataset 'earnings' da file locale...\n")

# Carica il file CSV scaricato manualmente
#  Modifica il percorso con il path corretto sul tuo computer
local_path <- "earnings.csv"
earnings_data <- read.csv(local_path)

cat("Dati caricati con successo.\n")
cat("Dimensioni del dataset:", dim(earnings_data), "\n")
cat("Struttura del dataset:\n")
str(earnings_data)
cat("\n")

# 2. Pulizia e Trasformazione dei Dati
cat("2. Pulizia e trasformazione dei dati...\n")

# Esplora i nomi reali delle colonne
cat("Nomi delle colonne disponibili:\n")
print(names(earnings_data))
cat("\n")

# Rinominiamo alcune colonne per uniformità (solo se necessario)
# Adatta in base al tuo dataset
names(earnings_data) <- tolower(names(earnings_data))

# Filtra solo le righe con guadagni positivi e nessun NA nella variabile 'earn'
earnings_pos <- earnings_data[earnings_data$earn > 0 & !is.na(earnings_data$earn), ]

# Colonne richieste (alcune potrebbero non esserci)
required_cols <- c("height", "sex", "ed", "age", "race")

# Verifica quali colonne esistono effettivamente nel dataset filtrato
existing_cols <- intersect(required_cols, names(earnings_pos))

# Se alcune colonne sono mancanti, avvisa ma non interrompere
missing_cols <- setdiff(required_cols, existing_cols)
if(length(missing_cols) > 0) {
  warning(paste("Attenzione: mancano le seguenti colonne e verranno ignorate:", 
                paste(missing_cols, collapse=", ")))
}

# Rimuove righe con NA nelle colonne esistenti
earnings_pos <- earnings_pos[complete.cases(earnings_pos[, existing_cols, drop = FALSE]), ]

# Trasformazione logaritmica della variabile risposta
earnings_pos$log_earn <- log(earnings_pos$earn)

# Aggiunta del termine quadratico per l’età (se presente)
if("age" %in% existing_cols) {
  earnings_pos$age_sq <- earnings_pos$age^2
} else {
  earnings_pos$age_sq <- NULL
}

cat("Numero di osservazioni con guadagni positivi:", nrow(earnings_pos), "\n")
cat("Creato il logaritmo del guadagno (log_earn) e il termine quadratico per l'età (age_sq) se disponibile.\n\n")

# Statistiche descrittive sulle colonne esistenti
cat("Statistiche descrittive delle variabili disponibili:\n")
print(summary(earnings_pos[, c("earn", "log_earn", existing_cols), drop = FALSE]))
cat("\n")

# 3. Modelli di Regressione Progressivi
cat("3. Costruzione di modelli di regressione progressivi...\n\n")

# Prepara formula dinamica con le colonne disponibili
# Escludiamo age_sq se age non presente
predictors <- existing_cols
if("age" %in% predictors && !is.null(earnings_pos$age_sq)) {
  predictors <- c(predictors, "age_sq")
}

# Formula finale
formula_str <- paste("log_earn ~", paste(predictors, collapse = " + "))
final_formula <- as.formula(formula_str)

# Costruiamo i modelli progressivi se le variabili sono disponibili

# Modello 1: solo altezza (se presente)
if("height" %in% existing_cols) {
  model1 <- lm(log_earn ~ height, data = earnings_pos)
  cat("Modello 1 - Solo altezza:\n")
  cat("R-quadrato:", summary(model1)$r.squared, "\n\n")
} else {
  cat("Modello 1 saltato: colonna 'height' mancante.\n\n")
}

# Modello 2: altezza + sesso (se entrambi presenti)
if(all(c("height", "sex") %in% existing_cols)) {
  model2 <- lm(log_earn ~ height + sex, data = earnings_pos)
  cat("Modello 2 - Altezza + Genere:\n")
  cat("R-quadrato:", summary(model2)$r.squared, "\n\n")
} else {
  cat("Modello 2 saltato: colonne 'height' o 'sex' mancanti.\n\n")
}

# Modello 3: altezza + sesso + istruzione (se presenti)
if(all(c("height", "sex", "ed") %in% existing_cols)) {
  model3 <- lm(log_earn ~ height + sex + ed, data = earnings_pos)
  cat("Modello 3 - Altezza + Genere + Istruzione:\n")
  cat("R-quadrato:", summary(model3)$r.squared, "\n\n")
} else {
  cat("Modello 3 saltato: colonne 'height', 'sex' o 'ed' mancanti.\n\n")
}

# Modello finale completo (con tutte le variabili disponibili)
final_model <- lm(final_formula, data = earnings_pos)
cat("Modello Finale - tutte le variabili disponibili:\n")
print(summary(final_model))
cat("\n")

# 4. Modello con Interazioni
cat("4. Aggiunta di interazioni tra sesso e altre variabili (se 'sex' presente)...\n")
if("sex" %in% existing_cols) {
  # Costruiamo formula con interazioni tra sex e altre variabili tranne sex stessa
  other_predictors <- setdiff(predictors, "sex")
  interactions_terms <- paste("sex * (", paste(other_predictors, collapse = " + "), ")", sep = "")
  formula_int <- as.formula(paste("log_earn ~", interactions_terms))
  
  model_interactions <- lm(formula_int, data = earnings_pos)
  cat("Modello con interazioni:\n")
  print(summary(model_interactions))
  cat("\n")
} else {
  cat("Interazioni non aggiunte: colonna 'sex' mancante.\n\n")
}

cat("--- Fine Esercizio 2 ---\n")
