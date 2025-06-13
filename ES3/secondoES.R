# Caricamento
earnings <- read.csv("earnings.csv")

# Pulizia: rimuove outlier su earn
earnings <- subset(earnings, earn <= 150000)


# Elimina codici 99 non validi da education dei genitori
earnings$mother_education[earnings$mother_education > 18] <- NA
earnings$father_education[earnings$father_education > 18] <- NA

# Variabili per il modello completo
model_vars <- c("earn", "height", "weight", "male", "education", "mother_education", "father_education", "walk", 
                "exercise", "smokenow", "tense", "angry", "age")

# Rimozione dei NA
dati_model <- na.omit(earnings[ , model_vars])

# Modello completo
model_completo <- lm(earn ~ height + weight + male + education +
                       mother_education + father_education + walk + 
                       exercise + smokenow + tense + angry + age,
                     data = dati_model)

summary(model_completo)

# ====== MODELLO ======
# Manteniamo solo variabili significative:
# (education, male, age, height)

model_parsimonioso <- lm(earn ~ education + male + age + height,
                         data = dati_model)

summary(model_parsimonioso)
