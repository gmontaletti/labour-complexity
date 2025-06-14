# Script R per riprodurre l'analisi di mobilità omofilia di Heine et al. (2021)
# "Analysis of mobility homophily in Stockholm based on social network data"

# Caricamento librerie necessarie
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(MASS)      # Per negative binomial regression
  library(sf)        # Per dati spaziali
  library(corrplot)  # Per matrice di correlazione
  library(broom)     # Per output tidy dei modelli
  library(cluster)   # Per clustering
  library(sandwich)  # Per clustered standard errors
  library(tibble)    # Per column_to_rownames
})

# Risolvi conflitti di namespace
select <- dplyr::select

# ==============================================================================
# 1. FUNZIONI PER IL CALCOLO DELLA LINKAGE STRENGTH
# ==============================================================================

#' Calcola la linkage strength tra due aree basata su utenti Twitter condivisi
#' 
#' @param user_area_matrix matrice utenti x aree con conteggi tweet
#' @param area_a indice dell'area A
#' @param area_b indice dell'area B
#' @return valore di linkage strength
calculate_linkage_strength <- function(user_area_matrix, area_a, area_b) {
  # Implementazione della formula: f_{A,B} = Σ_{i∈U_A} min{x_{i,A}, x_{i,B}}
  
  # Ottieni utenti che hanno tweetato in area A
  users_in_a <- which(user_area_matrix[, area_a] > 0)
  
  linkage_strength <- 0
  for (user in users_in_a) {
    tweets_in_a <- user_area_matrix[user, area_a]
    tweets_in_b <- user_area_matrix[user, area_b]
    linkage_strength <- linkage_strength + min(tweets_in_a, tweets_in_b)
  }
  
  return(linkage_strength)
}

#' Calcola matrice completa di linkage strength per tutte le coppie di aree
#' 
#' @param user_area_matrix matrice utenti x aree
#' @return matrice simmetrica di linkage strength
calculate_linkage_matrix <- function(user_area_matrix) {
  n_areas <- ncol(user_area_matrix)
  linkage_matrix <- matrix(0, nrow = n_areas, ncol = n_areas)
  
  # Calcola linkage strength per tutte le coppie
  for (i in 1:(n_areas-1)) {
    for (j in (i+1):n_areas) {
      linkage_ij <- calculate_linkage_strength(user_area_matrix, i, j)
      linkage_matrix[i, j] <- linkage_ij
      linkage_matrix[j, i] <- linkage_ij  # Simmetrica
    }
  }
  
  return(linkage_matrix)
}

# ==============================================================================
# 2. FUNZIONI PER PREPROCESSING DEI DATI
# ==============================================================================

#' Filtra bot e business dai dati Twitter
#' 
#' @param twitter_data dataframe con dati Twitter
#' @return dataframe filtrato
filter_twitter_data <- function(twitter_data) {
  # Filtri per rimuovere bot e business (semplificati)
  filtered_data <- twitter_data %>%
    filter(
      # Rimuovi utenti con troppi tweet (possibili bot)
      user_tweet_count <= quantile(user_tweet_count, 0.99, na.rm = TRUE),
      # Mantieni solo utenti con almeno 2 tweet
      user_tweet_count >= 2
    )
  
  # Filtro aggiuntivo per account business solo se la colonna esiste
  if ("user_description" %in% names(twitter_data)) {
    filtered_data <- filtered_data %>%
      filter(
        # Rimuovi account business (se identificabili)
        !grepl("business|company|official", tolower(ifelse(is.na(user_description), "", user_description)))
      )
  }
  
  return(filtered_data)
}

#' Aggrega tweet a livello di stadsdel
#' 
#' @param twitter_data dataframe con coordinate lat/lon
#' @param stadsdel_boundaries sf object con confini stadsdel
#' @return dataframe aggregato per stadsdel
aggregate_to_stadsdel <- function(twitter_data, stadsdel_boundaries) {
  # Converti coordinate in oggetto sf
  tweets_sf <- st_as_sf(twitter_data, coords = c("lon", "lat"), crs = 4326)
  
  # Associa ogni tweet al suo stadsdel
  tweets_with_stadsdel <- st_join(tweets_sf, stadsdel_boundaries)
  
  # Aggrega per utente e stadsdel
  user_stadsdel_counts <- tweets_with_stadsdel %>%
    st_drop_geometry() %>%
    filter(!is.na(stadsdel_id)) %>%
    group_by(user_id, stadsdel_id) %>%
    summarise(tweet_count = n(), .groups = 'drop')
  
  return(user_stadsdel_counts)
}

# ==============================================================================
# 3. FUNZIONI PER VARIABILI ESPLICATIVE
# ==============================================================================

#' Calcola differenze socioeconomiche tra coppie di stadsdel
#' 
#' @param stadsdel_data dataframe con dati socioeconomici per stadsdel
#' @return dataframe con differenze per tutte le coppie
calculate_socioeconomic_differences <- function(stadsdel_data) {
  n_areas <- nrow(stadsdel_data)
  pairs_data <- data.frame()
  
  for (i in 1:(n_areas-1)) {
    for (j in (i+1):n_areas) {
      pair_row <- data.frame(
        area_a = stadsdel_data$stadsdel_id[i],
        area_b = stadsdel_data$stadsdel_id[j],
        income_diff = abs(stadsdel_data$mean_income[i] - stadsdel_data$mean_income[j]),
        education_diff = abs(stadsdel_data$pct_higher_ed[i] - stadsdel_data$pct_higher_ed[j]),
        foreign_diff = abs(stadsdel_data$pct_foreign_born[i] - stadsdel_data$pct_foreign_born[j]),
        # Variabili di controllo (somme per modello simmetrico)
        population_sum = stadsdel_data$population[i] + stadsdel_data$population[j],
        accessibility_sum = stadsdel_data$accessibility[i] + stadsdel_data$accessibility[j],
        poi_sum = stadsdel_data$poi_count[i] + stadsdel_data$poi_count[j],
        income_sum = stadsdel_data$mean_income[i] + stadsdel_data$mean_income[j],
        education_sum = stadsdel_data$pct_higher_ed[i] + stadsdel_data$pct_higher_ed[j],
        foreign_sum = stadsdel_data$pct_foreign_born[i] + stadsdel_data$pct_foreign_born[j]
      )
      pairs_data <- rbind(pairs_data, pair_row)
    }
  }
  
  return(pairs_data)
}

#' Calcola rank-distance model per mobilità attesa
#' 
#' @param stadsdel_data dataframe con dati stadsdel
#' @return vettore con valori rank-distance model
calculate_rank_distance_model <- function(stadsdel_data) {
  # Semplificazione del rank-distance model
  # rank_A(B) = numero di POI più vicini ad A di quanto B sia ad A
  
  n_areas <- nrow(stadsdel_data)
  pairs_data <- data.frame()
  
  for (i in 1:(n_areas-1)) {
    for (j in (i+1):n_areas) {
      # Calcola distanza euclidea semplificata
      dist_ij <- sqrt((stadsdel_data$x_coord[i] - stadsdel_data$x_coord[j])^2 + 
                      (stadsdel_data$y_coord[i] - stadsdel_data$y_coord[j])^2)
      
      # Rank basato su POI e distanza
      rank_value <- 1 / (1 + dist_ij / mean(c(stadsdel_data$poi_count[i], stadsdel_data$poi_count[j])))
      
      pair_row <- data.frame(
        area_a = i,
        area_b = j,
        rank_distance = rank_value
      )
      pairs_data <- rbind(pairs_data, pair_row)
    }
  }
  
  return(pairs_data$rank_distance)
}

# ==============================================================================
# 4. MODELLI STATISTICI
# ==============================================================================

#' Stima modello binomiale negativo per tutti gli stadsdel
#' 
#' @param regression_data dataframe con variabili dipendenti e indipendenti
#' @param socioeco_var variabile socioeconomica da utilizzare ("income", "education", "foreign")
#' @return oggetto del modello stimato
estimate_negative_binomial_model <- function(regression_data, socioeco_var = "income") {
  # Prepara formula dinamicamente
  if (socioeco_var == "income") {
    difference_var <- "income_diff"
    sum_var <- "income_sum"
  } else if (socioeco_var == "education") {
    difference_var <- "education_diff"
    sum_var <- "education_sum"
  } else if (socioeco_var == "foreign") {
    difference_var <- "foreign_diff"
    sum_var <- "foreign_sum"
  }
  
  # Formula del modello
  formula_str <- paste(
    "linkage_strength ~", difference_var, "+ transit_time + driving_time +",
    "log(accessibility_sum) + log(poi_sum) + log(population_sum) +",
    "rank_distance +", sum_var, "+ log(twitter_activity_sum)"
  )
  
  # Stima modello
  model <- glm.nb(as.formula(formula_str), data = regression_data)
  
  return(model)
}

#' Calcola clustered standard errors
#' 
#' @param model oggetto del modello
#' @param cluster_vars vettore con variabili di clustering
#' @return matrice di covarianza corretta
calculate_clustered_se <- function(model, cluster_vars) {
  # Implementazione semplificata di clustered standard errors
  vcov_cluster <- vcovCL(model, cluster = cluster_vars, type = "HC1")
  return(vcov_cluster)
}

#' Stima modelli individuali per ogni stadsdel
#' 
#' @param regression_data dataframe completo
#' @param socioeco_var variabile socioeconomica
#' @return lista di modelli per ogni stadsdel
estimate_individual_models <- function(regression_data, socioeco_var = "income") {
  areas <- unique(c(regression_data$area_a, regression_data$area_b))
  individual_models <- list()
  
  for (area in areas) {
    # Filtra dati per area specifica
    area_data <- regression_data %>%
      filter(area_a == area | area_b == area)
    
    if (nrow(area_data) > 10) {  # Solo se abbiamo abbastanza osservazioni
      # Formula semplificata per modelli individuali
      if (socioeco_var == "income") {
        formula_str <- "linkage_strength ~ income_diff + driving_time + log(accessibility_sum) + log(poi_sum) + rank_distance"
      } else if (socioeco_var == "education") {
        formula_str <- "linkage_strength ~ education_diff + driving_time + log(accessibility_sum) + log(poi_sum) + rank_distance"
      } else if (socioeco_var == "foreign") {
        formula_str <- "linkage_strength ~ foreign_diff + driving_time + log(accessibility_sum) + log(poi_sum) + rank_distance"
      }
      
      tryCatch({
        model <- glm.nb(as.formula(formula_str), data = area_data)
        individual_models[[as.character(area)]] <- model
      }, error = function(e) {
        message(paste("Errore nel modello per area", area, ":", e$message))
      })
    }
  }
  
  return(individual_models)
}

# ==============================================================================
# 5. FUNZIONI DI VISUALIZZAZIONE
# ==============================================================================

#' Visualizza matrice di linkage strength
#' 
#' @param linkage_matrix matrice di linkage strength
#' @param stadsdel_names nomi degli stadsdel
#' @return oggetto ggplot
plot_linkage_matrix <- function(linkage_matrix, stadsdel_names = NULL) {
  # Converti matrice in formato long per ggplot
  linkage_df <- expand.grid(Area_A = 1:nrow(linkage_matrix), 
                           Area_B = 1:ncol(linkage_matrix))
  linkage_df$LinkageStrength <- as.vector(linkage_matrix)
  
  p <- ggplot(linkage_df, aes(x = Area_A, y = Area_B, fill = LinkageStrength)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue", name = "Linkage\nStrength") +
    labs(title = "Matrice di Linkage Strength tra Stadsdel",
         x = "Area A", y = "Area B") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

#' Visualizza correlazione con metrica OD tradizionale
#' 
#' @param linkage_strength vettore linkage strength
#' @param od_flow vettore flussi OD tradizionali
#' @return oggetto ggplot
plot_linkage_od_correlation <- function(linkage_strength, od_flow) {
  correlation_data <- data.frame(
    LinkageStrength = linkage_strength,
    ODFlow = od_flow
  )
  
  correlation_coef <- cor(linkage_strength, od_flow, use = "complete.obs")
  
  p <- ggplot(correlation_data, aes(x = LinkageStrength, y = ODFlow)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = paste("Correlazione Linkage Strength vs Flussi OD tradizionali"),
         subtitle = paste("Coefficiente di correlazione:", round(correlation_coef, 3)),
         x = "Linkage Strength", y = "Flussi OD (4 ore)") +
    theme_minimal()
  
  return(p)
}

#' Visualizza coefficienti dei modelli individuali
#' 
#' @param individual_models lista di modelli
#' @param coefficient_name nome del coefficiente da visualizzare
#' @return oggetto ggplot
plot_individual_coefficients <- function(individual_models, coefficient_name) {
  coefficients_data <- data.frame()
  
  for (area_name in names(individual_models)) {
    model <- individual_models[[area_name]]
    coef_value <- coef(model)[coefficient_name]
    p_value <- summary(model)$coefficients[coefficient_name, 4]
    
    coefficients_data <- rbind(coefficients_data, data.frame(
      Area = area_name,
      Coefficient = coef_value,
      PValue = p_value,
      Significant = p_value < 0.05
    ))
  }
  
  p <- ggplot(coefficients_data, aes(x = Coefficient, fill = Significant)) +
    geom_histogram(bins = 20, alpha = 0.7) +
    scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "red")) +
    labs(title = paste("Distribuzione coefficienti", coefficient_name, "nei modelli individuali"),
         x = "Valore del coefficiente", y = "Frequenza") +
    theme_minimal()
  
  return(p)
}

# ==============================================================================
# 6. FUNZIONE PRINCIPALE PER ANALISI COMPLETA
# ==============================================================================

#' Esegue analisi completa di mobilità homophily
#' 
#' @param twitter_data dataframe con dati Twitter
#' @param stadsdel_data dataframe con dati socioeconomici stadsdel
#' @param travel_time_data dataframe con tempi di viaggio
#' @return lista con tutti i risultati
run_mobility_homophily_analysis <- function(twitter_data, stadsdel_data, travel_time_data) {
  
  cat("Inizio analisi mobilità homophily...\n")
  
  # 1. Preprocessing dati Twitter
  cat("1. Preprocessing dati Twitter...\n")
  filtered_twitter <- filter_twitter_data(twitter_data)
  
  # 2. Calcolo linkage strength
  cat("2. Calcolo linkage strength...\n")
  
  # Verifica le colonne disponibili
  cat("Colonne disponibili nei dati filtrati:", paste(names(filtered_twitter), collapse = ", "), "\n")
  
  # Assumendo che i dati siano già aggregati per utente-stadsdel
  # Se i dati non sono nel formato giusto, li aggreghiamo
  if (all(c("user_id", "stadsdel_id", "tweet_count") %in% names(filtered_twitter))) {
    user_area_data <- filtered_twitter %>%
      select(user_id, stadsdel_id, tweet_count)
  } else {
    # Se le colonne non esistono, usiamo quello che abbiamo
    cat("Adattamento nomi colonne...\n")
    # Assumiamo che le prime 3 colonne siano user_id, stadsdel_id, tweet_count
    colnames(filtered_twitter)[1:3] <- c("user_id", "stadsdel_id", "tweet_count")
    user_area_data <- filtered_twitter %>%
      select(user_id, stadsdel_id, tweet_count)
  }
  
  user_area_matrix <- user_area_data %>%
    pivot_wider(names_from = stadsdel_id, values_from = tweet_count, values_fill = 0) %>%
    column_to_rownames("user_id") %>%
    as.matrix()
  
  linkage_matrix <- calculate_linkage_matrix(user_area_matrix)
  
  # 3. Preparazione dati per regressione
  cat("3. Preparazione dati per regressione...\n")
  socioeco_diff <- calculate_socioeconomic_differences(stadsdel_data)
  
  # Converti linkage matrix in formato long
  linkage_long <- expand.grid(area_a = 1:nrow(linkage_matrix), 
                             area_b = 1:ncol(linkage_matrix))
  linkage_long$linkage_strength <- as.vector(linkage_matrix)
  linkage_long <- linkage_long[linkage_long$area_a < linkage_long$area_b, ]  # Solo triangolo superiore
  
  # Debug: verifica struttura dati
  cat("Dimensioni linkage_long:", nrow(linkage_long), "x", ncol(linkage_long), "\n")
  cat("Colonne linkage_long:", paste(names(linkage_long), collapse = ", "), "\n")
  cat("Dimensioni socioeco_diff:", nrow(socioeco_diff), "x", ncol(socioeco_diff), "\n")
  cat("Colonne socioeco_diff:", paste(names(socioeco_diff), collapse = ", "), "\n")
  cat("Dimensioni travel_time_data:", nrow(travel_time_data), "x", ncol(travel_time_data), "\n")
  cat("Colonne travel_time_data:", paste(names(travel_time_data), collapse = ", "), "\n")
  
  # Combina con altre variabili usando merge invece di cbind per evitare duplicazioni
  regression_data <- merge(linkage_long, socioeco_diff, by = c("area_a", "area_b"), all.x = TRUE)
  
  # Verifica se le colonne di merge esistono in travel_time_data
  if (all(c("area_a", "area_b") %in% names(travel_time_data))) {
    regression_data <- merge(regression_data, travel_time_data, 
                            by = c("area_a", "area_b"), all.x = TRUE)
  } else {
    cat("Attenzione: colonne area_a e area_b non trovate in travel_time_data\n")
    cat("Aggiungendo dati travel_time_data manualmente...\n")
    
    # Se le colonne non corrispondono, aggiungi manualmente
    if (nrow(regression_data) == nrow(travel_time_data)) {
      regression_data <- cbind(regression_data, 
                              travel_time_data[, !names(travel_time_data) %in% c("area_a", "area_b")])
    } else {
      cat("Errore: dimensioni non compatibili per combinare i dati\n")
    }
  }
  
  # 4. Stima modelli
  cat("4. Stima modelli statistici...\n")
  
  # Modelli per diverse variabili socioeconomiche
  model_income <- estimate_negative_binomial_model(regression_data, "income")
  model_education <- estimate_negative_binomial_model(regression_data, "education")  
  model_foreign <- estimate_negative_binomial_model(regression_data, "foreign")
  
  # Modelli individuali
  individual_models_income <- estimate_individual_models(regression_data, "income")
  
  # 5. Visualizzazioni
  cat("5. Creazione visualizzazioni...\n")
  
  linkage_plot <- plot_linkage_matrix(linkage_matrix)
  
  # Se disponibili dati OD tradizionali
  if ("od_flow" %in% names(regression_data)) {
    correlation_plot <- plot_linkage_od_correlation(regression_data$linkage_strength, 
                                                   regression_data$od_flow)
  } else {
    correlation_plot <- NULL
  }
  
  individual_coef_plot <- plot_individual_coefficients(individual_models_income, "income_diff")
  
  # 6. Risultati
  results <- list(
    linkage_matrix = linkage_matrix,
    regression_data = regression_data,
    models = list(
      income = model_income,
      education = model_education,
      foreign = model_foreign
    ),
    individual_models = individual_models_income,
    plots = list(
      linkage_matrix = linkage_plot,
      correlation = correlation_plot,
      individual_coefficients = individual_coef_plot
    )
  )
  
  cat("Analisi completata!\n")
  return(results)
}

# ==============================================================================
# 7. ESEMPIO DI UTILIZZO
# ==============================================================================

# Esempio con dati simulati (sostituire con dati reali)
generate_sample_data <- function() {
  # Dati Twitter simulati
  n_users <- 1000
  n_areas <- 20
  
  twitter_data <- data.frame(
    user_id = rep(1:n_users, each = 5),
    stadsdel_id = sample(1:n_areas, n_users * 5, replace = TRUE),
    tweet_count = rpois(n_users * 5, 2),
    user_tweet_count = rep(sample(10:100, n_users, replace = TRUE), each = 5)
  ) %>%
    group_by(user_id, stadsdel_id) %>%
    summarise(tweet_count = sum(tweet_count), 
              user_tweet_count = first(user_tweet_count), .groups = 'drop')
  
  # Dati stadsdel simulati
  stadsdel_data <- data.frame(
    stadsdel_id = 1:n_areas,
    mean_income = rnorm(n_areas, 50000, 15000),
    pct_higher_ed = runif(n_areas, 0.2, 0.8),
    pct_foreign_born = runif(n_areas, 0.1, 0.4),
    population = sample(3000:8000, n_areas, replace = TRUE),
    accessibility = runif(n_areas, 0.3, 0.9),
    poi_count = rpois(n_areas, 50),
    x_coord = runif(n_areas, 0, 100),
    y_coord = runif(n_areas, 0, 100)
  )
  
  # Dati tempi di viaggio simulati
  n_pairs <- (n_areas * (n_areas - 1)) / 2
  travel_time_data <- data.frame(
    area_a = rep(1:(n_areas-1), times = (n_areas-1):1),
    area_b = unlist(lapply(2:n_areas, function(x) x:n_areas))
  )[1:n_pairs, ]
  
  travel_time_data$transit_time <- runif(nrow(travel_time_data), 10, 60)
  travel_time_data$driving_time <- runif(nrow(travel_time_data), 5, 45)
  travel_time_data$rank_distance <- runif(nrow(travel_time_data), 0.1, 1)
  travel_time_data$twitter_activity_sum <- rpois(nrow(travel_time_data), 100)
  
  return(list(
    twitter_data = twitter_data,
    stadsdel_data = stadsdel_data,
    travel_time_data = travel_time_data
  ))
}

# Esecuzione esempio
if (FALSE) {  # Impostare TRUE per eseguire
  # Genera dati di esempio
  sample_data <- generate_sample_data()
  
  # Esegui analisi
  results <- run_mobility_homophily_analysis(
    twitter_data = sample_data$twitter_data,
    stadsdel_data = sample_data$stadsdel_data,
    travel_time_data = sample_data$travel_time_data
  )
  
  # Visualizza risultati principali
  print("Modello Income:")
  summary(results$models$income)
  
  print("Modello Education:")
  summary(results$models$education)
  
  print("Modello Foreign Background:")
  summary(results$models$foreign)
  
  # Mostra grafici
  print(results$plots$linkage_matrix)
  print(results$plots$individual_coefficients)
}
