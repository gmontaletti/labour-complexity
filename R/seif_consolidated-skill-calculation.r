# Script R per il calcolo del Consolidated Skill Requirement
# secondo la metodologia di Seif, Toh e Lee (2024)
# ---------------------------------------------------------------------------
# Questo script dimostra:
# 1. Come integrare i ranking della conoscenza esperta (SFw) con i dati del mercato del lavoro
# 2. Come implementare la formula di ponderazione temporale per il calcolo di R_LM
# 3. Come calcolare il punteggio W(s, O) e derivare il ranking consolidato

# Impostare il seed per la riproducibilità
set.seed(123)

# ----- PARTE 1: Creazione di dati sintetici -----

# Definire alcune competenze per l'Instrumentation Engineer
skills <- c(
  "electrical, electronic and control engineering",
  "communication",
  "failure analysis",
  "operation management",
  "decision making",
  "instrumentation and control design engineering management",
  "engineering support management",
  "instrumentation and control system design",
  "data and statistical analysis",
  "programming and coding",
  "mathematical concepts application"
)

# Simulare i dati della conoscenza esperta (SFw)
# Le competenze sono classificate da esperti del settore
# 1 = più importante, NA = non presente nel framework
sfw_ranks <- c(1, 2, 3, 4, 5, NA, NA, NA, 6, NA, NA)

# Creare il dataframe del SFw
sfw_data <- data.frame(
  skill = skills,
  sfw_rank = sfw_ranks
)

# Definire gli anni per i quali abbiamo dati del mercato del lavoro
years <- 2018:2023

# Generare i rank dal mercato del lavoro per ogni anno
# In un caso reale, questi dati verrebbero estratti dagli annunci di lavoro
# Qui simuliamo variazioni nei ranking nel tempo

# Funzione per generare rank con variazioni casuali ma con una tendenza
generate_yearly_ranks <- function(base_rank, years, trend_factor = 0.1) {
  n_years <- length(years)
  ranks <- numeric(n_years)
  
  for (i in 1:n_years) {
    # Aggiungi un po' di casualità ma mantieni una tendenza
    if (!is.na(base_rank)) {
      noise <- sample(-2:2, 1)
      trend <- round(trend_factor * (i - 1))
      ranks[i] <- max(1, min(11, base_rank + noise + trend))
    } else {
      # Per le competenze non presenti nel SFw, generiamo ranking più variabili
      ranks[i] <- sample(1:11, 1)
    }
  }
  
  return(ranks)
}

# Creare una matrice per i ranking annuali (righe = skills, colonne = anni)
yearly_ranks <- matrix(NA, nrow = length(skills), ncol = length(years))
colnames(yearly_ranks) <- paste0("rank_", years)
rownames(yearly_ranks) <- skills

# Generiamo ranking annuali con tendenze diverse per diverse competenze
for (i in 1:length(skills)) {
  # Competenze tradizionali hanno una tendenza a peggiorare (trend positivo = rank aumenta)
  if (i <= 5) {
    trend <- 0.2
  } 
  # Competenze emergenti migliorano (trend negativo = rank diminuisce)
  else if (i <= 8) {
    trend <- -0.3
  }
  # Altre competenze hanno trend neutro
  else {
    trend <- 0
  }
  
  yearly_ranks[i, ] <- generate_yearly_ranks(sfw_ranks[i], years, trend)
}

# Creare un dataframe con tutti i dati annuali
yearly_data <- as.data.frame(yearly_ranks)
yearly_data$skill <- skills

# Riorganizzare i dati in formato long per facilitare l'analisi
library(tidyr)
yearly_data_long <- pivot_longer(
  yearly_data, 
  cols = starts_with("rank_"), 
  names_to = "year", 
  values_to = "rank"
)
yearly_data_long$year <- as.numeric(gsub("rank_", "", yearly_data_long$year))

# ----- PARTE 2: Implementazione della formula per R_LM -----

# Parametri per il calcolo di R_LM
y_max <- max(years)
delta_y <- 3  # Lunghezza della finestra temporale (ultimi 3 anni)
epsilon_SFw <- 0.6  # Parametro per il peso della conoscenza esperta

# Funzione per calcolare il peso temporale per ogni anno nella finestra
temporal_weight <- function(y, y_max) {
  (1 + y_max - y) / 2
}

# Funzione per calcolare R_LM secondo l'Equazione 1 dell'articolo
calculate_R_LM <- function(skill, y_max, delta_y, yearly_data_long) {
  # Filtrare i dati per la competenza e la finestra temporale
  window_data <- yearly_data_long[
    yearly_data_long$skill == skill & 
    yearly_data_long$year >= (y_max - delta_y) & 
    yearly_data_long$year <= y_max,
  ]
  
  if (nrow(window_data) == 0) {
    return(NA)  # Non ci sono dati per questa competenza nella finestra
  }
  
  # Calcolare il peso temporale per ogni anno e l'inverso del rank
  window_data$temporal_weight <- sapply(window_data$year, temporal_weight, y_max = y_max)
  window_data$inverse_rank <- 1 / window_data$rank
  
  # Calcolo della somma ponderata degli inversi dei rank
  weighted_sum <- sum(window_data$temporal_weight * window_data$inverse_rank)
  
  # Calcoliamo un "pseudo-rank" basato sull'inverso della somma ponderata
  # Più grande è la somma ponderata, più importante è la competenza
  # Quindi ordiniamo in modo decrescente per ottenere i rank
  return(weighted_sum)
}

# Calcolare R_LM per tutte le competenze
r_lm_values <- sapply(skills, calculate_R_LM, y_max = y_max, delta_y = delta_y, 
                     yearly_data_long = yearly_data_long)

# Creare un dataframe con i valori di R_LM
r_lm_data <- data.frame(
  skill = skills,
  r_lm_value = r_lm_values
)

# Ordinare per valore R_LM decrescente e assegnare il rank
r_lm_data <- r_lm_data[order(r_lm_data$r_lm_value, decreasing = TRUE), ]
r_lm_data$r_lm_rank <- 1:nrow(r_lm_data)

# ----- PARTE 3: Calcolo del punteggio consolidato W(s, O) -----

# Unire i dati SFw e R_LM
consolidated_data <- merge(sfw_data, r_lm_data, by = "skill", all = TRUE)

# Calcolare W(s, O) secondo l'Equazione 2 dell'articolo
consolidated_data$inverse_sfw_rank <- ifelse(is.na(consolidated_data$sfw_rank), 0, 1/consolidated_data$sfw_rank)
consolidated_data$inverse_r_lm_rank <- 1/consolidated_data$r_lm_rank

# Formula: W(s, O) = epsilon_SFw * (1/R_SFw) + (1/R_LM)
consolidated_data$W_score <- epsilon_SFw * consolidated_data$inverse_sfw_rank + 
                             consolidated_data$inverse_r_lm_rank

# Ordinare per punteggio W decrescente e assegnare il rank consolidato
consolidated_data <- consolidated_data[order(consolidated_data$W_score, decreasing = TRUE), ]
consolidated_data$consolidated_rank <- 1:nrow(consolidated_data)

# ----- PARTE 4: Visualizzazione dei risultati -----

# Selezioniamo le colonne rilevanti per la visualizzazione
result_data <- consolidated_data[, c("skill", "sfw_rank", "r_lm_rank", "consolidated_rank", "W_score")]

# Rinominiamo le colonne per maggiore chiarezza
names(result_data) <- c("Competenza", "Rank SFw", "Rank Mercato Lavoro", "Rank Consolidato", "Punteggio W")

# Mostriamo i risultati ordinati per rank consolidato
print(result_data)

# ----- PARTE 5: Visualizzazione grafica -----

# Visualizzare i top 5 ranking consolidati
top5 <- head(result_data, 5)
print("Top 5 competenze secondo il Rank Consolidato:")
print(top5)

# Confrontiamo i ranking SFw, R_LM e consolidato per le prime 5 competenze
if(require(ggplot2)) {
  # Prepariamo i dati per il grafico
  top5_long <- pivot_longer(
    top5, 
    cols = c("Rank SFw", "Rank Mercato Lavoro", "Rank Consolidato"), 
    names_to = "Tipo di Rank", 
    values_to = "Rank"
  )
  
  # Sostituiamo NA con un valore alto (es. 15) per il grafico
  top5_long$Rank[is.na(top5_long$Rank)] <- 15
  
  # Convertiamo le competenze in un fattore ordinato per il grafico
  top5_long$Competenza <- factor(top5_long$Competenza, levels = top5$Competenza)
  
  # Creiamo il grafico
  p <- ggplot(top5_long, aes(x = Competenza, y = Rank, fill = `Tipo di Rank`)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_reverse(limits = c(15, 0), breaks = seq(0, 15, by = 3)) +  # Invertiamo l'asse y (rank più basso = più importante)
    labs(
      title = "Confronto tra i diversi tipi di ranking per le top 5 competenze",
      x = "Competenza",
      y = "Rank (più basso = più importante)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  
  # Creiamo un secondo grafico che mostra solo il punteggio W
  p2 <- ggplot(top5, aes(x = reorder(Competenza, `Punteggio W`), y = `Punteggio W`)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Punteggio W per le top 5 competenze",
      x = "Competenza",
      y = "Punteggio W (più alto = più importante)"
    ) +
    theme_minimal()
  
  print(p2)
}

# ----- PARTE 6: Esempio della Tabella 4 dell'articolo -----

# Ricreiamo un esempio simile alla Tabella 4 dell'articolo
example_skills <- c(
  "Data and Statistical Analysis",
  "Programming and Coding",
  "Mathematical Concepts Application"
)

example_table <- data.frame(
  Skill = example_skills,
  R_SFw = c(1, 2, NA),
  R_2018 = c(2, 5, 38),
  Inverse_R_SFw = c(1.0, 0.5, 0),
  Inverse_R_2018 = c(0.5, 0.2, 0.026)
)

print("Esempio simile alla Tabella 4 dell'articolo:")
print(example_table)

