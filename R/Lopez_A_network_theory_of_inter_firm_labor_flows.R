# Script R per implementare gli indicatori sviluppati da López et al. (2020)
# title <- "A network theory of inter-firm labor flows"
# gsub(" |\\-", "_", title)

# Caricamento dei pacchetti necessari
library(igraph)      # Per l'analisi di rete
library(dplyr)       # Per la manipolazione dei dati
library(ggplot2)     # Per la visualizzazione
library(tidyr)       # Per la manipolazione dei dati

#=============================================================================
# PARTE 1: COSTRUZIONE DELLA RETE DI FLUSSO DI LAVORO (LFN)
#=============================================================================

# Funzione per testare la persistenza dei flussi e costruire la rete LFN
costruisci_lfn <- function(dati_transizioni, finestra_tempo_1, finestra_tempo_2, soglia_W = 2) {
  # dati_transizioni: dataframe con colonne [anno, azienda_origine, azienda_destinazione, n_transizioni]
  # finestra_tempo_1: periodo T< (es. 1988-1997)
  # finestra_tempo_2: periodo T> (es. 1998-2007)
  # soglia_W: il numero minimo di transizioni per considerare un collegamento persistente
  
  # Filtrare i dati per le due finestre temporali
  transizioni_T1 <- dati_transizioni %>% 
    filter(anno %in% finestra_tempo_1) %>%
    group_by(azienda_origine, azienda_destinazione) %>%
    summarize(flusso_totale = sum(n_transizioni), .groups = "drop")
  
  transizioni_T2 <- dati_transizioni %>% 
    filter(anno %in% finestra_tempo_2) %>%
    group_by(azienda_origine, azienda_destinazione) %>%
    summarize(flusso_totale = sum(n_transizioni), .groups = "drop")
  
  # Identificare le aziende presenti in entrambi i periodi
  aziende_T1 <- unique(c(transizioni_T1$azienda_origine, transizioni_T1$azienda_destinazione))
  aziende_T2 <- unique(c(transizioni_T2$azienda_origine, transizioni_T2$azienda_destinazione))
  aziende_comuni <- intersect(aziende_T1, aziende_T2)
  
  # Filtrare le transizioni per includere solo le aziende comuni
  transizioni_T1_filtrate <- transizioni_T1 %>%
    filter(azienda_origine %in% aziende_comuni & azienda_destinazione %in% aziende_comuni)
  
  transizioni_T2_filtrate <- transizioni_T2 %>%
    filter(azienda_origine %in% aziende_comuni & azienda_destinazione %in% aziende_comuni)
  
  # Identificare i collegamenti che soddisfano la soglia W nel periodo T1
  collegamenti_W <- transizioni_T1_filtrate %>%
    filter(flusso_totale >= soglia_W)
  
  # Calcolare le metriche di persistenza
  # Totale dei possibili collegamenti tra aziende comuni
  n_possibili_collegamenti <- length(aziende_comuni) * (length(aziende_comuni) - 1) / 2
  
  # Numero di collegamenti con flusso > 0 nel periodo T2
  collegamenti_T2 <- transizioni_T2_filtrate %>%
    filter(flusso_totale > 0)
  
  # Numero di collegamenti che soddisfano la soglia W in T1 e hanno flusso > 0 in T2
  collegamenti_persistenti <- inner_join(
    collegamenti_W,
    collegamenti_T2 %>% select(azienda_origine, azienda_destinazione),
    by = c("azienda_origine", "azienda_destinazione")
  )
  
  # Calcolo delle probabilità ℘ e ℘W
  p_rho <- nrow(collegamenti_T2) / n_possibili_collegamenti
  p_rho_W <- nrow(collegamenti_persistenti) / nrow(collegamenti_W)
  eccesso_probabilita <- p_rho_W / p_rho
  
  # Costruire il grafo LFN non diretto
  collegamenti_lfn <- collegamenti_persistenti %>%
    select(azienda_origine, azienda_destinazione)
  
  lfn <- graph_from_data_frame(collegamenti_lfn, directed = FALSE, vertices = aziende_comuni)
  
  return(list(
    lfn = lfn,
    p_rho = p_rho,
    p_rho_W = p_rho_W,
    eccesso_probabilita = eccesso_probabilita,
    aziende = aziende_comuni,
    collegamenti = collegamenti_lfn
  ))
}

#=============================================================================
# PARTE 2: CALCOLO DEGLI INDICATORI BASATI SULLA RETE
#=============================================================================

# Funzione per calcolare i principali indicatori del modello
calcola_indicatori <- function(lfn, dati_aziende) {
  # lfn: oggetto grafo igraph della rete di flusso di lavoro
  # dati_aziende: dataframe con colonne [id_azienda, dimensione, tasso_separazione, tasso_attesa]
  
  # Calcolare il grado di ogni nodo nella rete
  gradi <- degree(lfn)
  
  # Creare un dataframe con gli indicatori
  indicatori <- data.frame(
    id_azienda = names(gradi),
    grado_k = as.numeric(gradi)
  )
  
  # Unire con i dati delle aziende
  indicatori <- left_join(indicatori, dati_aziende, by = "id_azienda")
  
  # Calcolare il rapporto k/λ per ogni azienda
  indicatori <- indicatori %>%
    mutate(
      rapporto_k_lambda = grado_k / tasso_separazione,
      rapporto_k_xi = grado_k / tasso_attesa
    )
  
  return(indicatori)
}

#=============================================================================
# PARTE 3: ANALISI E VISUALIZZAZIONE
#=============================================================================

# Funzione per verificare la relazione tra dimensione dell'impresa e rapporto k/λ
analizza_relazione_dimensione <- function(indicatori) {
  # Modello di regressione (eq. 18 del paper)
  modello <- lm(dimensione ~ rapporto_k_lambda, data = indicatori)
  coeff_CL <- coefficients(modello)[2]  # Coefficiente di proporzionalità CL
  
  # Visualizzazione
  p <- ggplot(indicatori, aes(x = rapporto_k_lambda, y = dimensione)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", col = "red") +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = "Relazione tra dimensione dell'impresa e k/λ",
      subtitle = paste("Li ≈", round(coeff_CL, 2), "* (ki/λi)"),
      x = "ki/λi (grado/tasso di separazione)",
      y = "Li (dimensione dell'impresa)"
    ) +
    theme_minimal()
  
  # Analisi della distribuzione
  hist_dim <- ggplot(indicatori, aes(x = dimensione)) +
    geom_histogram(bins = 30) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = "Distribuzione delle dimensioni delle imprese",
      x = "Dimensione (Li)",
      y = "Frequenza"
    ) +
    theme_minimal()
  
  hist_k_lambda <- ggplot(indicatori, aes(x = rapporto_k_lambda)) +
    geom_histogram(bins = 30) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = "Distribuzione del rapporto ki/λi",
      x = "ki/λi",
      y = "Frequenza"
    ) +
    theme_minimal()
  
  # Calcolo dell'esponente di decadimento della distribuzione power-law
  # Questo è un'approssimazione semplificata del calcolo dell'esponente z nella eq. 20
  dati_log <- indicatori %>%
    filter(dimensione > 1) %>%  # Escludere valori troppo piccoli
    mutate(
      log_dim = log10(dimensione),
      log_k_lambda = log10(rapporto_k_lambda)
    )
  
  modello_power_law <- lm(log_dim ~ log_k_lambda, data = dati_log)
  esponente_z <- coefficients(modello_power_law)[2]
  
  return(list(
    modello = modello,
    coeff_CL = coeff_CL,
    esponente_z = esponente_z,
    plot_relazione = p,
    plot_dist_dim = hist_dim,
    plot_dist_k_lambda = hist_k_lambda
  ))
}

# Funzione per analizzare la disoccupazione specifica dell'impresa
analizza_disoccupazione_specifica <- function(indicatori) {
  # Calcolo della disoccupazione specifica secondo l'eq. 21
  indicatori <- indicatori %>%
    mutate(disoccupazione_specifica = rapporto_k_xi)
  
  # Visualizzazione
  p <- ggplot(indicatori, aes(x = rapporto_k_xi, y = disoccupazione_specifica)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", col = "blue") +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = "Relazione tra disoccupazione specifica dell'impresa e k/ξ",
      subtitle = "Ui ~ ki/ξi",
      x = "ki/ξi (grado/tasso di attesa)",
      y = "Ui (disoccupazione specifica)"
    ) +
    theme_minimal()
  
  # Distribuzione della disoccupazione specifica
  hist_u <- ggplot(indicatori, aes(x = disoccupazione_specifica)) +
    geom_histogram(bins = 30) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = "Distribuzione della disoccupazione specifica dell'impresa",
      x = "Disoccupazione specifica (Ui)",
      y = "Frequenza"
    ) +
    theme_minimal()
  
  return(list(
    plot_relazione = p,
    plot_distribuzione = hist_u,
    dati = indicatori
  ))
}

#=============================================================================
# PARTE 4: ESEMPIO DI APPLICAZIONE
#=============================================================================

# Questo è un esempio di come utilizzare le funzioni sopra con dati simulati
# In un caso reale, si caricherebbero dati veri da file

# Simulazione di dati di transizione
set.seed(123)
n_aziende <- 100
n_anni <- 20
anni <- 1988:(1988+n_anni-1)

# Creare ID univoci per le aziende
id_aziende <- paste0("A", 1:n_aziende)

# Simulare transizioni tra aziende
simulazione_transizioni <- function() {
  n_transizioni <- 5000
  dati <- data.frame(
    anno = sample(anni, n_transizioni, replace = TRUE),
    azienda_origine = sample(id_aziende, n_transizioni, replace = TRUE),
    azienda_destinazione = sample(id_aziende, n_transizioni, replace = TRUE),
    n_transizioni = rpois(n_transizioni, lambda = 2)
  )
  
  # Rimuovere le auto-transizioni
  dati <- dati %>% filter(azienda_origine != azienda_destinazione)
  
  return(dati)
}

# Simulare dati aziendali
simulazione_dati_aziende <- function(id_aziende) {
  # Simulare dimensioni con distribuzione power-law
  # e tassi di separazione/attesa con distribuzione esponenziale
  dati <- data.frame(
    id_azienda = id_aziende,
    dimensione = floor(rlnorm(length(id_aziende), meanlog = 3, sdlog = 1.5)),
    tasso_separazione = rexp(length(id_aziende), rate = 5),
    tasso_attesa = rexp(length(id_aziende), rate = 3)
  )
  
  # Normalizzare i tassi per essere nell'intervallo (0, 1]
  dati <- dati %>%
    mutate(
      tasso_separazione = pmin(tasso_separazione, 0.99),
      tasso_attesa = pmin(tasso_attesa, 0.99)
    )
  
  return(dati)
}

# Esecuzione delle funzioni con dati simulati
esempio_applicazione <- function() {
  # Generare dati simulati
  dati_transizioni <- simulazione_transizioni()
  
  # Dividere i dati in due finestre temporali
  finestra_T1 <- 1988:1997
  finestra_T2 <- 1998:2007
  
  # Costruire la rete LFN
  risultato_lfn <- costruisci_lfn(
    dati_transizioni = dati_transizioni,
    finestra_tempo_1 = finestra_T1,
    finestra_tempo_2 = finestra_T2,
    soglia_W = 2
  )
  
  # Simulare dati aziendali per le aziende nella rete
  dati_aziende <- simulazione_dati_aziende(risultato_lfn$aziende)
  
  # Calcolare gli indicatori
  indicatori <- calcola_indicatori(risultato_lfn$lfn, dati_aziende)
  
  # Analizzare la relazione tra dimensione e k/λ
  analisi_dimensione <- analizza_relazione_dimensione(indicatori)
  
  # Analizzare la disoccupazione specifica
  analisi_disoccupazione <- analizza_disoccupazione_specifica(indicatori)
  
  # Output dei risultati
  cat("\nRisultati dell'analisi di persistenza dei flussi:\n")
  cat("p_rho =", risultato_lfn$p_rho, "\n")
  cat("p_rho_W =", risultato_lfn$p_rho_W, "\n")
  cat("Eccesso di probabilità =", risultato_lfn$eccesso_probabilita, "\n\n")
  
  cat("Coefficiente di proporzionalità CL =", analisi_dimensione$coeff_CL, "\n")
  cat("Esponente z della distribuzione power-law =", analisi_dimensione$esponente_z, "\n\n")
  
  # Visualizzare i plot
  print(analisi_dimensione$plot_relazione)
  print(analisi_dimensione$plot_dist_dim)
  print(analisi_dimensione$plot_dist_k_lambda)
  print(analisi_disoccupazione$plot_relazione)
  print(analisi_disoccupazione$plot_distribuzione)
  
  return(list(
    lfn = risultato_lfn,
    indicatori = indicatori,
    analisi_dimensione = analisi_dimensione,
    analisi_disoccupazione = analisi_disoccupazione
  ))
}

# Eseguire l'esempio
risultati <- esempio_applicazione()

# Nota: decommentare la riga sopra per eseguire l'esempio completo

indicatori <- risultati$indicatori
