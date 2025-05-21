# Script R per implementare l'analisi di mercati del lavoro endogeni
# basato sull'approccio metodologico di Nimczik (2017)

# Installiamo i pacchetti necessari
if (!require("igraph")) install.packages("igraph")
if (!require("blockmodels")) install.packages("blockmodels")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

library(igraph)
library(blockmodels)
library(ggplot2)
library(dplyr)
library(tidyr)

# Impostazione del seed per la riproducibilità
set.seed(123)

#-------------------------------------------------
# 1. Creazione di dati simulati di mobilità lavorativa
#-------------------------------------------------

# Simuliamo una rete di mobilità lavorativa tra aziende
# In un caso reale, si userebbero dati amministrativi come nel paper

# Parametri della simulazione
n_aziende <- 200  # Numero di aziende
n_regioni <- 5    # Numero di regioni
n_industrie <- 8  # Numero di settori industriali
n_mercati_veri <- 4  # Numero di mercati del lavoro "veri" (non osservati)

# Assegniamo le aziende a regioni e industrie
regioni_aziende <- sample(1:n_regioni, n_aziende, replace = TRUE)
industrie_aziende <- sample(1:n_industrie, n_aziende, replace = TRUE)

# Assegniamo le aziende a mercati del lavoro "veri" (non osservati)
# Questa è l'informazione che cercheremo di recuperare con l'SBM
mercati_veri <- sample(1:n_mercati_veri, n_aziende, replace = TRUE)

# Creiamo una matrice di adiacenza che rifletta i flussi di lavoratori
# Le aziende nello stesso mercato hanno maggiore probabilità di scambiare lavoratori
matrice_adiacenza <- matrix(0, n_aziende, n_aziende)

# Parametri per controllare la densità della rete
p_stesso_mercato <- 0.3  # Probabilità di transizione nello stesso mercato
p_mercati_diversi <- 0.02  # Probabilità di transizione tra mercati diversi

# Generiamo i collegamenti
for (i in 1:n_aziende) {
  for (j in 1:n_aziende) {
    if (i != j) {  # Evitiamo self-loops
      if (mercati_veri[i] == mercati_veri[j]) {
        # Aziende nello stesso mercato
        matrice_adiacenza[i, j] <- rbinom(1, 1, p_stesso_mercato)
      } else {
        # Aziende in mercati diversi
        matrice_adiacenza[i, j] <- rbinom(1, 1, p_mercati_diversi)
      }
    }
  }
}

# Creiamo un grafo diretto pesato dall'adiacenza
grafo_mobilita <- graph_from_adjacency_matrix(matrice_adiacenza, mode = "directed", weighted = TRUE)

# Aggiungiamo attributi alle aziende (nodi)
V(grafo_mobilita)$regione <- regioni_aziende
V(grafo_mobilita)$industria <- industrie_aziende
V(grafo_mobilita)$mercato_vero <- mercati_veri

#-------------------------------------------------
# 2. Implementazione del modello stocastico a blocchi (SBM)
#-------------------------------------------------

# Convertiamo il grafo in una matrice di adiacenza per il modello SBM
adj_matrix <- as_adjacency_matrix(grafo_mobilita, sparse = FALSE)

# Il pacchetto blockmodels implementa SBM
# Se vogliamo un SBM corretto per grado, come nel paper di Nimczik
cat("Stimando il modello SBM...\n")
cat("Questo può richiedere del tempo per reti più grandi.\n")

# Applichiamo il modello SBM - CORREZIONE: Usiamo "SBM" invece di "SBM_sym" per reti dirette
sbm_model <- blockmodels::BM_bernoulli(
  membership_type = "SBM",  # Correzione: usiamo SBM per reti dirette, non SBM_sym
  adj = adj_matrix,
  plotting = "none"
)

# Eseguiamo la stima per diversi numeri di gruppi
# Nel paper, Nimczik utilizza il punteggio di modularità per scegliere il numero ottimale
k_max <- 10  # Numero massimo di gruppi da considerare
cat("Stima del modello per k da 1 a", k_max, "\n")

sbm_model$estimate(k_max)

# Analizziamo i risultati per scegliere il numero ottimale di gruppi
# Nel paper, viene usata la modularità, qui usiamo l'ICL (Integrated Completed Likelihood)
icl_scores <- sapply(1:k_max, function(k) sbm_model$ICL[k])
k_ottimale <- which.max(icl_scores)


cat("Numero ottimale di gruppi (mercati) secondo ICL:", k_ottimale, "\n")

# Otteniamo le assegnazioni ai mercati stimati
mercati_stimati <- sbm_model$memberships[[k_ottimale]]$Z

# Se abbiamo più di una possibile assegnazione, prendiamo quella con ICL più alto
if (is.list(mercati_stimati)) {
  mercati_stimati <- mercati_stimati[[1]]
}

# Convertiamo in un vettore di assegnazioni
mercati_stimati <- apply(mercati_stimati, 1, which.max)

# Aggiungiamo i mercati stimati come attributo al grafo
V(grafo_mobilita)$mercato_stimato <- mercati_stimati

#-------------------------------------------------
# 3. Valutazione della performance del modello
#-------------------------------------------------

# Creiamo un data frame con le informazioni delle aziende
df_aziende <- data.frame(
  azienda = 1:n_aziende,
  regione = regioni_aziende,
  industria = industrie_aziende,
  mercato_vero = mercati_veri,
  mercato_stimato = mercati_stimati
)

# Calcoliamo alcune statistiche per valutare come si comporta il modello
cat("Valutazione della performance del modello SBM:\n")

# Tabella di contingenza tra mercati veri e stimati
contingenza <- table(df_aziende$mercato_vero, df_aziende$mercato_stimato)
print(contingenza)

# Calcoliamo l'indice di Rand aggiustato per valutare la corrispondenza
# Questo richiederebbe il pacchetto 'fossil' o funzioni simili
# In alternativa, possiamo esaminare la concentrazione di mercati veri nei mercati stimati

# Esaminiamo la relazione tra mercati stimati e regioni/industrie
mercati_per_regione <- table(df_aziende$mercato_stimato, df_aziende$regione)
mercati_per_industria <- table(df_aziende$mercato_stimato, df_aziende$industria)

cat("\nConcentrazione dei mercati stimati per regione:\n")
print(mercati_per_regione)

cat("\nConcentrazione dei mercati stimati per industria:\n")
print(mercati_per_industria)

#-------------------------------------------------
# 4. Visualizzazione dei risultati
#-------------------------------------------------

# Visualizziamo il grafo con colori diversi per i mercati stimati
plot(grafo_mobilita, 
     vertex.color = mercati_stimati,
     vertex.size = 5, 
     vertex.label = NA,
     edge.arrow.size = 0.3,
     main = "Rete di mobilità lavorativa con mercati endogeni stimati")

# Creiamo un grafo che rappresenta il modello a blocchi
# Calcoliamo le probabilità di transizione tra mercati, come Nimczik nel paper
sbm_graph <- matrix(0, k_ottimale, k_ottimale)
for (i in 1:k_ottimale) {
  for (j in 1:k_ottimale) {
    # Aziende nel mercato i
    aziende_i <- which(mercati_stimati == i)
    # Aziende nel mercato j
    aziende_j <- which(mercati_stimati == j)
    
    # Collegamenti tra i e j
    collegamenti <- sum(adj_matrix[aziende_i, aziende_j])
    
    # Numero potenziale di collegamenti
    potenziali <- length(aziende_i) * length(aziende_j)
    
    # Probabilità di transizione
    if (potenziali > 0) {
      sbm_graph[i, j] <- collegamenti / potenziali
    }
  }
}

# Visualizziamo il modello a blocchi come rete
grafo_sbm <- graph_from_adjacency_matrix(sbm_graph, mode = "directed", weighted = TRUE)

# Plot del grafo SBM
plot(grafo_sbm, 
     vertex.size = 20, 
     vertex.label = 1:k_ottimale,
     edge.width = E(grafo_sbm)$weight * 20,
     edge.arrow.size = 0.5,
     layout = layout_with_fr(grafo_sbm),
     main = "Modello a blocchi: transizioni tra mercati del lavoro endogeni")

#-------------------------------------------------
# 5. Analisi di concentrazione geografica e industriale
#-------------------------------------------------

# Come nel paper di Nimczik, esaminiamo la concentrazione geografica e industriale
# dei mercati stimati usando un indice di concentrazione simile a Ellison-Glaeser

# Funzione per calcolare l'indice Ellison-Glaeser semplificato
calcola_eg <- function(quote_mercato, quote_totali) {
  if (length(quote_mercato) != length(quote_totali)) {
    stop("Le quote devono avere la stessa lunghezza")
  }
  
  # Calcolo dell'indice EG semplificato
  sum((quote_mercato - quote_totali)^2)
}

# Calcoliamo gli indici EG per concentrazione geografica
eg_geografici <- numeric(k_ottimale)
quote_totali_regioni <- table(regioni_aziende) / n_aziende

for (k in 1:k_ottimale) {
  aziende_in_k <- which(mercati_stimati == k)
  if (length(aziende_in_k) > 0) {
    quote_k_regioni <- table(factor(regioni_aziende[aziende_in_k], levels = 1:n_regioni)) / length(aziende_in_k)
    eg_geografici[k] <- calcola_eg(quote_k_regioni, quote_totali_regioni)
  }
}

# Calcoliamo gli indici EG per concentrazione industriale
eg_industriali <- numeric(k_ottimale)
quote_totali_industrie <- table(industrie_aziende) / n_aziende

for (k in 1:k_ottimale) {
  aziende_in_k <- which(mercati_stimati == k)
  if (length(aziende_in_k) > 0) {
    quote_k_industrie <- table(factor(industrie_aziende[aziende_in_k], levels = 1:n_industrie)) / length(aziende_in_k)
    eg_industriali[k] <- calcola_eg(quote_k_industrie, quote_totali_industrie)
  }
}

# Creiamo un dataframe con i risultati
df_concentrazione <- data.frame(
  mercato = 1:k_ottimale,
  concentrazione_geografica = eg_geografici,
  concentrazione_industriale = eg_industriali
)

cat("\nConcentrazione geografica e industriale dei mercati endogeni:\n")
print(df_concentrazione)

# Visualizziamo il confronto tra concentrazione geografica e industriale
# come nel paper di Nimczik (Figura 10)
ggplot(df_concentrazione, aes(x = concentrazione_geografica, y = concentrazione_industriale)) +
  geom_point(size = 3) +
  geom_text(aes(label = mercato), vjust = -0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    x = "Indice di concentrazione geografica",
    y = "Indice di concentrazione industriale",
    title = "Relazione tra concentrazione geografica e industriale"
  ) +
  theme_minimal()

#-------------------------------------------------
# 6. Simulazione di uno shock economico
#-------------------------------------------------

# Come nel paper di Nimczik, simuliamo uno shock economico
# e osserviamo come il modello predice le risposte

# Simuliamo uno shock che colpisce un particolare mercato del lavoro (es. mercato 1)
mercato_colpito <- 1
cat("\nSimulazione di uno shock economico nel mercato", mercato_colpito, "\n")

# Identifichiamo le aziende nel mercato colpito
aziende_colpite <- which(mercati_stimati == mercato_colpito)

# Effetto sui collegamenti:
# - Diminuisce i collegamenti in entrata (inflows)
# - Aumenta i collegamenti in uscita (outflows)

# Creiamo una nuova matrice di adiacenza post-shock
matrice_adiacenza_post <- matrice_adiacenza

# Riduciamo i collegamenti in entrata (-30%)
for (i in 1:n_aziende) {
  if (!(i %in% aziende_colpite)) {
    matrice_adiacenza_post[i, aziende_colpite] <- matrice_adiacenza_post[i, aziende_colpite] * 0.7
  }
}

# Aumentiamo i collegamenti in uscita (+20%)
for (i in aziende_colpite) {
  matrice_adiacenza_post[i, -aziende_colpite] <- pmin(1, matrice_adiacenza_post[i, -aziende_colpite] * 1.2)
}

# Calcoliamo i cambiamenti nei flussi prima e dopo lo shock
flussi_prima <- colSums(matrice_adiacenza)
flussi_dopo <- colSums(matrice_adiacenza_post)

# Calcoliamo il cambiamento percentuale nei flussi
cambiamento_percentuale <- (flussi_dopo - flussi_prima) / pmax(1, flussi_prima) * 100

# Aggreghiamo i risultati per mercato
df_effetti <- data.frame(
  azienda = 1:n_aziende,
  mercato = mercati_stimati,
  regione = regioni_aziende,
  cambiamento = cambiamento_percentuale
)

# Media del cambiamento per mercato
effetti_per_mercato <- df_effetti %>%
  group_by(mercato) %>%
  summarize(
    cambiamento_medio = mean(cambiamento),
    n_aziende = n()
  )

cat("\nEffetti dello shock sui flussi di lavoratori per mercato:\n")
print(effetti_per_mercato)

# Grafico degli effetti per mercato
ggplot(effetti_per_mercato, aes(x = factor(mercato), y = cambiamento_medio, fill = factor(mercato))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(cambiamento_medio, 1)), vjust = -0.5) +
  labs(
    x = "Mercato del lavoro endogeno",
    y = "Cambiamento percentuale nei flussi",
    title = "Effetti dello shock economico sui flussi di lavoratori per mercato",
    subtitle = paste("Mercato colpito:", mercato_colpito)
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Effetti per mercato e regione
effetti_per_mercato_regione <- df_effetti %>%
  group_by(mercato, regione) %>%
  summarize(
    cambiamento_medio = mean(cambiamento),
    n_aziende = n()
  ) %>%
  ungroup()

# Visualizziamo gli effetti per regione e mercato
ggplot(effetti_per_mercato_regione, aes(x = factor(regione), y = cambiamento_medio, fill = factor(mercato))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    x = "Regione",
    y = "Cambiamento percentuale nei flussi",
    title = "Effetti dello shock economico per regione e mercato",
    fill = "Mercato"
  ) +
  theme_minimal()

cat("\nScript completato. I risultati mostrano come si può applicare\n")
cat("l'approccio di Nimczik (2017) per identificare mercati del lavoro endogeni\n")
cat("e analizzare gli effetti degli shock economici.\n")

