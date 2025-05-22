# Script R per riprodurre la metodologia di Liu, Pant e Sheng (2023)

# Installazione e caricamento dei pacchetti necessari
required_packages <- c("dplyr", "tidyr", "igraph", "randomForest", "pROC", "Matrix")

# Installa i pacchetti se non sono già installati
install_if_missing <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

# Installa e carica tutti i pacchetti necessari
sapply(required_packages, install_if_missing)

set.seed(123) # Per riproducibilità

#----------------------------------------------
# 1. CREAZIONE DATI SINTETICI
#----------------------------------------------

# Funzione per generare dati sintetici simulando il dataset del paper
generate_synthetic_data <- function(n_firms = 100, n_employees = 1000, n_years = 15, n_skills = 500) {
  # Creazione di aziende
  firms <- data.frame(
    firm_id = 1:n_firms,
    sic_code = sample(1000:9999, n_firms, replace = TRUE),
    revenue = rlnorm(n_firms, meanlog = 10, sdlog = 2),
    n_employees = rpois(n_firms, lambda = 200),
    stringsAsFactors = FALSE
  )
  
  # Generazione di competenze
  skills <- data.frame(
    skill_id = 1:n_skills,
    skill_name = paste0("skill_", 1:n_skills),
    stringsAsFactors = FALSE
  )
  
  # Creazione di dipendenti e assegnazione alle aziende
  employees <- data.frame(
    employee_id = 1:n_employees,
    initial_firm = sample(1:n_firms, n_employees, replace = TRUE),
    years_working = rpois(n_employees, 10),
    has_graduate_degree = rbinom(n_employees, 1, 0.3),
    university_rank = sample(1:200, n_employees, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Assegnazione di competenze ai dipendenti (10-20 competenze per dipendente)
  employee_skills <- lapply(1:n_employees, function(emp_id) {
    n_emp_skills <- sample(10:20, 1)
    data.frame(
      employee_id = emp_id,
      skill_id = sample(1:n_skills, n_emp_skills, replace = FALSE)
    )
  }) %>% bind_rows()
  
  # Generazione di movimenti di dipendenti tra aziende (Human Capital Flow - HCF)
  years <- 1:n_years
  
  # Inizializza dataframe per tutti i movimenti HCF
  all_hcf <- data.frame()
  
  # Simulazione di movimenti per tutti gli anni
  for (year in years) {
    # Numero di movimenti per anno
    n_moves <- rpois(1, n_firms * 0.3)
    
    if (n_moves > 0) {
      # Selezione casuale di dipendenti che cambiano lavoro
      moving_employees <- sample(1:n_employees, min(n_moves, n_employees), replace = FALSE)
      
      # Per ogni dipendente che si muove, trova l'azienda di origine e destinazione
      for (emp_id in moving_employees) {
        # Trova l'azienda attuale
        if (year == 1) {
          source_firm <- employees$initial_firm[emp_id]
        } else {
          # Cerca l'ultimo movimento nei dati precedenti
          prev_moves <- subset(all_hcf, all_hcf$employee_id == emp_id & all_hcf$year < year)
          if (nrow(prev_moves) > 0) {
            # Ordina per anno e prendi l'ultimo movimento
            prev_moves <- prev_moves[order(prev_moves$year, decreasing = TRUE), ]
            source_firm <- prev_moves$target_firm[1]
          } else {
            source_firm <- employees$initial_firm[emp_id]
          }
        }
        
        # Scegli un'azienda di destinazione (probabilità più alta per aziende dello stesso settore)
        source_sic <- firms$sic_code[source_firm]
        
        # Divisione del SIC in 2 cifre per il settore
        source_sic2 <- floor(source_sic / 100)
        
        # Calcola probabilità: più alta per aziende dello stesso settore
        firm_sic2 <- floor(firms$sic_code / 100)
        same_sector <- (firm_sic2 == source_sic2)
        
        # Maggiore probabilità per le aziende dello stesso settore, ma non la stessa azienda
        probs <- numeric(n_firms)
        probs[same_sector] <- 3
        probs[!same_sector] <- 1
        probs[source_firm] <- 0  # Non può andare nella stessa azienda
        
        # Assicurati che ci sia almeno una probabilità > 0
        if (sum(probs) == 0) {
          probs[1:n_firms] <- 1
          probs[source_firm] <- 0
        }
        
        target_firm <- sample(1:n_firms, 1, prob = probs)
        
        # Aggiungi il movimento
        new_move <- data.frame(
          employee_id = emp_id,
          source_firm = source_firm,
          target_firm = target_firm,
          year = year
        )
        
        all_hcf <- rbind(all_hcf, new_move)
      }
    }
  }
  
  # Creazione di descrizioni aziendali sintetiche
  business_descriptions <- lapply(1:n_firms, function(firm_id) {
    sic <- firms$sic_code[firm_id]
    industry_terms <- switch(
      as.character(floor(sic / 1000)),
      "1" = c("agriculture", "farming", "crops", "livestock"),
      "2" = c("manufacturing", "production", "goods", "factory"),
      "3" = c("electronics", "technology", "devices", "components"),
      "4" = c("transportation", "utilities", "infrastructure", "services"),
      "5" = c("retail", "wholesale", "distribution", "sales"),
      "6" = c("finance", "banking", "insurance", "investing"),
      "7" = c("services", "consulting", "business", "professional"),
      "8" = c("healthcare", "education", "social", "institutions"),
      "9" = c("government", "administration", "public", "services"),
      c("general", "business", "services", "company")
    )
    
    # Genera descrizione casuale basata sul settore
    n_terms <- sample(5:15, 1)
    terms <- sample(industry_terms, n_terms, replace = TRUE)
    
    data.frame(
      firm_id = firm_id,
      business_description = paste(terms, collapse = " ")
    )
  }) %>% bind_rows()
  
  return(list(
    firms = firms,
    skills = skills,
    employees = employees,
    employee_skills = employee_skills,
    hcf = all_hcf,
    business_descriptions = business_descriptions
  ))
}

# Genera i dati sintetici
data <- generate_synthetic_data()
firms <- data$firms
skills <- data$skills
employees <- data$employees
employee_skills <- data$employee_skills
hcf <- data$hcf
business_descriptions <- data$business_descriptions

#----------------------------------------------
# 2. COSTRUZIONE DELLE METRICHE
#----------------------------------------------

# Funzione per ottenere l'azienda corrente di un dipendente per un dato anno
get_current_firm <- function(employee_id, year, hcf_data, employees_data) {
  # Ottieni i movimenti del dipendente fino all'anno specificato
  emp_moves <- subset(hcf_data, hcf_data$employee_id == employee_id & hcf_data$year <= year)
  
  if (nrow(emp_moves) > 0) {
    # Ordina per anno e prendi l'ultima azienda di destinazione
    emp_moves <- emp_moves[order(emp_moves$year, decreasing = TRUE), ]
    return(emp_moves$target_firm[1])
  } else {
    # Se non ci sono movimenti, usa l'azienda iniziale
    return(employees_data$initial_firm[employee_id])
  }
}

# Funzione per creare una matrice firm-skill
create_firm_skill_matrix <- function(employees, employee_skills, firms, year, hcf_data) {
  # Crea la matrice firm-skill
  firm_skill <- matrix(0, nrow = nrow(firms), ncol = nrow(skills))
  
  # Per ogni dipendente, aggiunge le sue competenze all'azienda corrente
  for (emp_id in unique(employee_skills$employee_id)) {
    current_firm <- get_current_firm(emp_id, year, hcf_data, employees)
    emp_skills <- employee_skills$skill_id[employee_skills$employee_id == emp_id]
    
    if (!is.na(current_firm) && current_firm <= nrow(firms)) {
      for (skill_id in emp_skills) {
        if (skill_id <= ncol(firm_skill)) {
          firm_skill[current_firm, skill_id] <- firm_skill[current_firm, skill_id] + 1
        }
      }
    }
  }
  
  # Applica la ponderazione SF-IFF come nel paper
  # SF: Skill Frequency
  # IFF: Inverse Firm Frequency
  skill_counts <- colSums(firm_skill > 0)
  skill_counts[skill_counts == 0] <- 1  # Evita divisione per zero
  iff <- log(nrow(firms) / skill_counts)
  
  # Moltiplica ogni colonna per il suo IFF
  for (j in 1:ncol(firm_skill)) {
    firm_skill[, j] <- firm_skill[, j] * iff[j]
  }
  
  return(firm_skill)
}

# Calcolo della similarità del coseno tra due vettori
cosine_similarity <- function(a, b) {
  norm_a <- sqrt(sum(a^2))
  norm_b <- sqrt(sum(b^2))
  
  if (norm_a == 0 || norm_b == 0) {
    return(0)
  }
  
  return(sum(a * b) / (norm_a * norm_b))
}

# Funzione per generare la matrice di similarità del skill term tra tutte le aziende
create_skill_term_similarity_matrix <- function(firm_skill_matrix) {
  n_firms <- nrow(firm_skill_matrix)
  sim_matrix <- matrix(0, n_firms, n_firms)
  
  for (i in 1:n_firms) {
    for (j in i:n_firms) {
      sim <- cosine_similarity(firm_skill_matrix[i, ], firm_skill_matrix[j, ])
      sim_matrix[i, j] <- sim
      sim_matrix[j, i] <- sim
    }
  }
  
  return(sim_matrix)
}

# Funzione per creare skill topics usando clustering semplificato
create_skill_topics <- function(employee_skills, n_topics = 6) {
  # Approccio semplificato: raggruppa le competenze per similarità utilizzando clustering
  
  # Crea una matrice di co-occorrenza delle competenze
  skills_per_employee <- split(employee_skills$skill_id, employee_skills$employee_id)
  
  # Crea la matrice di co-occorrenza
  all_skills <- unique(employee_skills$skill_id)
  n_skills <- length(all_skills)
  
  cooccurrence_matrix <- matrix(0, nrow = n_skills, ncol = n_skills)
  rownames(cooccurrence_matrix) <- all_skills
  colnames(cooccurrence_matrix) <- all_skills
  
  # Popola la matrice di co-occorrenza
  for (emp_skills in skills_per_employee) {
    if (length(emp_skills) > 1) {
      for (i in 1:(length(emp_skills) - 1)) {
        for (j in (i + 1):length(emp_skills)) {
          skill1 <- emp_skills[i]
          skill2 <- emp_skills[j]
          idx1 <- which(all_skills == skill1)
          idx2 <- which(all_skills == skill2)
          if (length(idx1) > 0 && length(idx2) > 0) {
            cooccurrence_matrix[idx1, idx2] <- cooccurrence_matrix[idx1, idx2] + 1
            cooccurrence_matrix[idx2, idx1] <- cooccurrence_matrix[idx2, idx1] + 1
          }
        }
      }
    }
  }
  
  # Usa k-means per raggruppare le competenze
  if (n_skills >= n_topics) {
    skill_clusters <- kmeans(cooccurrence_matrix, centers = n_topics, nstart = 10)
    skill_to_topic <- skill_clusters$cluster
    names(skill_to_topic) <- all_skills
  } else {
    # Se ci sono meno competenze dei topic richiesti, assegna casualmente
    skill_to_topic <- sample(1:n_topics, n_skills, replace = TRUE)
    names(skill_to_topic) <- all_skills
  }
  
  # Crea la distribuzione di topic per ogni dipendente
  n_employees <- max(employee_skills$employee_id)
  employee_topics <- matrix(0, nrow = n_employees, ncol = n_topics)
  
  for (emp_id in 1:n_employees) {
    emp_skills <- employee_skills$skill_id[employee_skills$employee_id == emp_id]
    if (length(emp_skills) > 0) {
      for (skill in emp_skills) {
        topic <- skill_to_topic[as.character(skill)]
        if (!is.na(topic)) {
          employee_topics[emp_id, topic] <- employee_topics[emp_id, topic] + 1
        }
      }
      # Normalizza per ottenere probabilità
      if (sum(employee_topics[emp_id, ]) > 0) {
        employee_topics[emp_id, ] <- employee_topics[emp_id, ] / sum(employee_topics[emp_id, ])
      } else {
        # Se il dipendente non ha competenze, assegna probabilità uniformi
        employee_topics[emp_id, ] <- rep(1/n_topics, n_topics)
      }
    } else {
      # Se il dipendente non ha competenze, assegna probabilità uniformi
      employee_topics[emp_id, ] <- rep(1/n_topics, n_topics)
    }
  }
  
  return(list(model = NULL, employee_topics = employee_topics))
}

# Funzione per generare la matrice di similarità del skill topic tra tutte le aziende
create_skill_topic_similarity_matrix <- function(employee_topics, year, hcf_data, employees_data, n_firms) {
  # Calcola la distribuzione di topic per ogni azienda
  firm_topics <- matrix(0, n_firms, ncol(employee_topics))
  
  for (emp_id in 1:nrow(employee_topics)) {
    current_firm <- get_current_firm(emp_id, year, hcf_data, employees_data)
    
    if (!is.na(current_firm) && current_firm <= n_firms) {
      firm_topics[current_firm, ] <- firm_topics[current_firm, ] + employee_topics[emp_id, ]
    }
  }
  
  # Normalizza le distribuzioni di topic
  firm_topics_normalized <- t(apply(firm_topics, 1, function(x) {
    if (sum(x) > 0) {
      return(x / sum(x))
    } else {
      return(rep(1/ncol(firm_topics), ncol(firm_topics)))
    }
  }))
  
  # Calcola la similarità del coseno tra tutte le coppie di aziende
  sim_matrix <- matrix(0, n_firms, n_firms)
  
  for (i in 1:n_firms) {
    for (j in i:n_firms) {
      sim <- cosine_similarity(firm_topics_normalized[i, ], firm_topics_normalized[j, ])
      sim_matrix[i, j] <- sim
      sim_matrix[j, i] <- sim
    }
  }
  
  return(sim_matrix)
}

# Costruzione della rete di flusso del capitale umano (HCF network)
create_hcf_network <- function(hcf_data, year, n_firms) {
  # Filtra i dati HCF fino all'anno specificato
  hcf_filtered <- subset(hcf_data, hcf_data$year <= year)
  
  if (nrow(hcf_filtered) == 0) {
    # Se non ci sono dati, crea un grafo vuoto
    g <- make_empty_graph(n = n_firms, directed = TRUE)
    return(g)
  }
  
  # Calcola i pesi degli archi come numero di dipendenti che si sono spostati
  edge_weights <- hcf_filtered %>%
    group_by(source_firm, target_firm) %>%
    summarise(weight = n(), .groups = 'drop')
  
  # Assicurati che tutti i nodi siano presenti
  vertices <- data.frame(id = 1:n_firms)
  
  # Crea il grafo diretto e ponderato
  g <- graph_from_data_frame(
    d = edge_weights,
    directed = TRUE,
    vertices = vertices
  )
  
  return(g)
}

# Funzione per calcolare la similarità upstream
calculate_upstream_similarity <- function(g, n_firms) {
  # Matrice di adiacenza del grafo
  adj <- as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
  
  # Assicurati che la matrice abbia le dimensioni corrette
  if (nrow(adj) != n_firms || ncol(adj) != n_firms) {
    # Crea una matrice vuota se le dimensioni non corrispondono
    adj <- matrix(0, nrow = n_firms, ncol = n_firms)
  }
  
  # Matrice di adiacenza transposed (dalla target alla source)
  adj_t <- t(adj)
  
  # Calcola la similarità del coseno per ogni coppia di aziende
  sim_matrix <- matrix(0, n_firms, n_firms)
  
  for (i in 1:n_firms) {
    for (j in i:n_firms) {
      # Se entrambe le aziende hanno collegamenti upstream
      if (sum(adj_t[i, ]) > 0 && sum(adj_t[j, ]) > 0) {
        sim <- cosine_similarity(adj_t[i, ], adj_t[j, ])
        sim_matrix[i, j] <- sim
        sim_matrix[j, i] <- sim
      }
    }
  }
  
  return(sim_matrix)
}

# Funzione per calcolare la similarità downstream
calculate_downstream_similarity <- function(g, n_firms) {
  # Matrice di adiacenza del grafo
  adj <- as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
  
  # Assicurati che la matrice abbia le dimensioni corrette
  if (nrow(adj) != n_firms || ncol(adj) != n_firms) {
    # Crea una matrice vuota se le dimensioni non corrispondono
    adj <- matrix(0, nrow = n_firms, ncol = n_firms)
  }
  
  # Calcola la similarità del coseno per ogni coppia di aziende
  sim_matrix <- matrix(0, n_firms, n_firms)
  
  for (i in 1:n_firms) {
    for (j in i:n_firms) {
      # Se entrambe le aziende hanno collegamenti downstream
      if (sum(adj[i, ]) > 0 && sum(adj[j, ]) > 0) {
        sim <- cosine_similarity(adj[i, ], adj[j, ])
        sim_matrix[i, j] <- sim
        sim_matrix[j, i] <- sim
      }
    }
  }
  
  return(sim_matrix)
}

# Funzione per rilevare le comunità nella rete HCF
detect_communities <- function(g) {
  # Se il grafo è vuoto, restituisci comunità casuali
  if (vcount(g) == 0 || ecount(g) == 0) {
    n_nodes <- vcount(g)
    if (n_nodes == 0) n_nodes <- nrow(firms)
    community_membership <- sample(1:min(5, n_nodes), n_nodes, replace = TRUE)
    return(community_membership)
  }
  
  # Usa l'algoritmo di Louvain per il rilevamento delle comunità
  communities <- cluster_louvain(as_undirected(g))
  
  # Estrai le appartenenze alle comunità
  community_membership <- membership(communities)
  
  return(community_membership)
}

# Funzione per creare la matrice di similarità della comunità HCF
create_community_similarity_matrix <- function(community_membership, n_firms) {
  sim_matrix <- matrix(0, n_firms, n_firms)
  
  # Assicurati che community_membership abbia la lunghezza corretta
  if (length(community_membership) < n_firms) {
    # Estendi con valori casuali se necessario
    community_membership <- c(community_membership, 
                              sample(1:max(community_membership), 
                                     n_firms - length(community_membership), 
                                     replace = TRUE))
  }
  
  for (i in 1:n_firms) {
    for (j in i:n_firms) {
      # Se due aziende appartengono alla stessa comunità
      if (community_membership[i] == community_membership[j]) {
        sim_matrix[i, j] <- 1
        sim_matrix[j, i] <- 1
      }
    }
  }
  
  return(sim_matrix)
}

# Funzione per calcolare la similarità SIC
calculate_sic_similarity <- function(firms) {
  n_firms <- nrow(firms)
  sim_matrix <- matrix(0, n_firms, n_firms)
  
  for (i in 1:n_firms) {
    for (j in i:n_firms) {
      sic_i <- firms$sic_code[i]
      sic_j <- firms$sic_code[j]
      
      # Calcola la similarità secondo la tabella 3 del paper
      if (sic_i == sic_j) {
        sim <- 4  # Stessi 4 digit
      } else if (floor(sic_i / 10) == floor(sic_j / 10)) {
        sim <- 3  # Stessi 3 digit
      } else if (floor(sic_i / 100) == floor(sic_j / 100)) {
        sim <- 2  # Stessi 2 digit
      } else if (floor(sic_i / 1000) == floor(sic_j / 1000)) {
        sim <- 1  # Stesso 1 digit
      } else {
        sim <- 0  # Completamente diversi
      }
      
      sim_matrix[i, j] <- sim
      sim_matrix[j, i] <- sim
    }
  }
  
  return(sim_matrix)
}

# Calcolo delle metriche per ogni anno
calculate_metrics_for_year <- function(year) {
  # Crea matrice firm-skill
  firm_skill_matrix <- create_firm_skill_matrix(employees, employee_skills, firms, year, hcf)
  
  # Calcola similarità skill term
  skill_term_sim <- create_skill_term_similarity_matrix(firm_skill_matrix)
  
  # Crea e calcola skill topics
  lda_results <- create_skill_topics(employee_skills)
  employee_topics <- lda_results$employee_topics
  
  # Calcola similarità skill topic
  skill_topic_sim <- create_skill_topic_similarity_matrix(employee_topics, year, hcf, employees, nrow(firms))
  
  # Crea rete HCF
  hcf_network <- create_hcf_network(hcf, year, nrow(firms))
  
  # Calcola similarità upstream e downstream
  upstream_sim <- calculate_upstream_similarity(hcf_network, nrow(firms))
  downstream_sim <- calculate_downstream_similarity(hcf_network, nrow(firms))
  
  # Rileva comunità
  communities <- detect_communities(hcf_network)
  
  # Calcola similarità della comunità
  community_sim <- create_community_similarity_matrix(communities, nrow(firms))
  
  # Calcola similarità SIC
  sic_sim <- calculate_sic_similarity(firms)
  
  return(list(
    skill_term_sim = skill_term_sim,
    skill_topic_sim = skill_topic_sim,
    upstream_sim = upstream_sim,
    downstream_sim = downstream_sim,
    community_sim = community_sim,
    sic_sim = sic_sim
  ))
}

#----------------------------------------------
# 3. PREPARAZIONE DEI DATI PER LA PREDIZIONE
#----------------------------------------------

# Funzione per preparare il dataset di coppie azienda-azienda per la predizione
prepare_prediction_data <- function(years, train_years, test_years) {
  all_pairs <- data.frame()
  
  for (year in years) {
    # Calcola le metriche per l'anno precedente
    prev_year <- year - 1
    
    if (prev_year >= min(years)) {
      cat(sprintf("Processando anno %d...\n", year))
      
      metrics <- calculate_metrics_for_year(prev_year)
      
      # Ottieni i flussi HCF per l'anno corrente
      year_hcf <- subset(hcf, hcf$year == year)
      
      # Crea tutte le possibili coppie di aziende
      firm_pairs <- expand.grid(source_firm = 1:nrow(firms), target_firm = 1:nrow(firms))
      firm_pairs <- subset(firm_pairs, firm_pairs$source_firm != firm_pairs$target_firm)
      
      # Calcola HCF per ogni coppia di aziende
      firm_pairs$hcf <- 0
      if (nrow(year_hcf) > 0) {
        hcf_counts <- year_hcf %>%
          group_by(source_firm, target_firm) %>%
          summarise(count = n(), .groups = 'drop')
        
        for (i in 1:nrow(hcf_counts)) {
          source <- hcf_counts$source_firm[i]
          target <- hcf_counts$target_firm[i]
          count <- hcf_counts$count[i]
          
          idx <- which(firm_pairs$source_firm == source & firm_pairs$target_firm == target)
          if (length(idx) > 0) {
            firm_pairs$hcf[idx] <- count
          }
        }
      }
      
      # Aggiungi le metriche calcolate
      firm_pairs$year <- year
      firm_pairs$skill_term_sim <- NA
      firm_pairs$skill_topic_sim <- NA
      firm_pairs$upstream_sim <- NA
      firm_pairs$downstream_sim <- NA
      firm_pairs$community_sim <- NA
      firm_pairs$sic_sim <- NA
      
      for (i in 1:nrow(firm_pairs)) {
        source <- firm_pairs$source_firm[i]
        target <- firm_pairs$target_firm[i]
        
        firm_pairs$skill_term_sim[i] <- metrics$skill_term_sim[source, target]
        firm_pairs$skill_topic_sim[i] <- metrics$skill_topic_sim[source, target]
        firm_pairs$upstream_sim[i] <- metrics$upstream_sim[source, target]
        firm_pairs$downstream_sim[i] <- metrics$downstream_sim[source, target]
        firm_pairs$community_sim[i] <- metrics$community_sim[source, target]
        firm_pairs$sic_sim[i] <- metrics$sic_sim[source, target]
      }
      
      # Calcola la variabile lag HCF (HCF dell'anno precedente)
      firm_pairs$hcf_lag <- 0
      if (prev_year > min(years)) {
        prev_year_hcf <- subset(hcf, hcf$year == prev_year)
        
        if (nrow(prev_year_hcf) > 0) {
          prev_hcf_counts <- prev_year_hcf %>%
            group_by(source_firm, target_firm) %>%
            summarise(count = n(), .groups = 'drop')
          
          for (i in 1:nrow(prev_hcf_counts)) {
            source <- prev_hcf_counts$source_firm[i]
            target <- prev_hcf_counts$target_firm[i]
            count <- prev_hcf_counts$count[i]
            
            idx <- which(firm_pairs$source_firm == source & firm_pairs$target_firm == target)
            if (length(idx) > 0) {
              firm_pairs$hcf_lag[idx] <- count
            }
          }
        }
      }
      
      # Aggiungi le caratteristiche economiche delle aziende
      firm_pairs$source_revenue <- firms$revenue[firm_pairs$source_firm]
      firm_pairs$target_revenue <- firms$revenue[firm_pairs$target_firm]
      firm_pairs$source_n_employees <- firms$n_employees[firm_pairs$source_firm]
      firm_pairs$target_n_employees <- firms$n_employees[firm_pairs$target_firm]
      
      # Inverti HCF (da target a source)
      firm_pairs$inv_hcf <- 0
      if (nrow(year_hcf) > 0) {
        inv_hcf_counts <- year_hcf %>%
          group_by(target_firm, source_firm) %>%
          summarise(count = n(), .groups = 'drop') %>%
          rename(source_firm = target_firm, target_firm = source_firm)
        
        for (i in 1:nrow(inv_hcf_counts)) {
          source <- inv_hcf_counts$source_firm[i]
          target <- inv_hcf_counts$target_firm[i]
          count <- inv_hcf_counts$count[i]
          
          idx <- which(firm_pairs$source_firm == source & firm_pairs$target_firm == target)
          if (length(idx) > 0) {
            firm_pairs$inv_hcf[idx] <- count
          }
        }
      }
      
      # Aggiungi le caratteristiche delle risorse umane
      # (Simuliamo dati sintetici per queste caratteristiche)
      firm_pairs$source_avg_years_working <- rnorm(nrow(firm_pairs), 10, 2)
      firm_pairs$target_avg_years_working <- rnorm(nrow(firm_pairs), 10, 2)
      firm_pairs$source_pct_graduate <- runif(nrow(firm_pairs), 0, 0.5)
      firm_pairs$target_pct_graduate <- runif(nrow(firm_pairs), 0, 0.5)
      firm_pairs$source_avg_university_rank <- runif(nrow(firm_pairs), 50, 150)
      firm_pairs$target_avg_university_rank <- runif(nrow(firm_pairs), 50, 150)
      
      # Crea variabili binarie per i target di predizione
      firm_pairs$competitor_d1 <- ifelse(firm_pairs$hcf >= 1, 1, 0)
      firm_pairs$competitor_d2 <- ifelse(firm_pairs$hcf >= 2, 1, 0)
      
      # Aggiungi al dataset completo
      all_pairs <- rbind(all_pairs, firm_pairs)
    }
  }
  
  # Dividi in training e test
  train_data <- subset(all_pairs, all_pairs$year %in% train_years)
  test_data <- subset(all_pairs, all_pairs$year %in% test_years)
  
  return(list(
    all_data = all_pairs,
    train_data = train_data,
    test_data = test_data
  ))
}

#----------------------------------------------
# 4. MODELLAZIONE PREDITTIVA
#----------------------------------------------

# Definisci gli anni di simulazione
years <- 1:15
train_years <- 1:12  # 2000-2012
test_years <- 13:15  # 2013-2015

# Prepara i dati per la predizione
cat("Preparazione dei dati per la predizione...\n")
prediction_data <- prepare_prediction_data(years, train_years, test_years)
train_data <- prediction_data$train_data
test_data <- prediction_data$test_data

cat(sprintf("Dati di training: %d osservazioni\n", nrow(train_data)))
cat(sprintf("Dati di test: %d osservazioni\n", nrow(test_data)))

# Definisci i set di feature come nel paper
feature_sets <- list(
  Economic = c("hcf_lag", "inv_hcf", "source_revenue", "target_revenue", 
               "source_n_employees", "target_n_employees",
               "source_avg_years_working", "target_avg_years_working",
               "source_pct_graduate", "target_pct_graduate",
               "source_avg_university_rank", "target_avg_university_rank"),
  
  Product = c("sic_sim"),
  
  Labor = c("skill_term_sim", "skill_topic_sim"),
  
  Network = c("upstream_sim", "downstream_sim", "community_sim")
)

# Funzione per addestrare e valutare il Random Forest
train_and_evaluate_rf <- function(train_data, test_data, features, target = "competitor_d1") {
  # Rimuovi righe con NA
  complete_train <- complete.cases(train_data[, c(features, target)])
  complete_test <- complete.cases(test_data[, c(features, target)])
  
  train_clean <- train_data[complete_train, ]
  test_clean <- test_data[complete_test, ]
  
  if (nrow(train_clean) == 0 || nrow(test_clean) == 0) {
    warning("Dati insufficienti dopo la rimozione dei NA")
    return(list(auc = 0, new_competitors_identified = 0))
  }
  
  # Prepara i dati di addestramento
  X_train <- train_clean[, features, drop = FALSE]
  y_train <- train_clean[[target]]
  
  # Addestra il Random Forest
  rf_model <- randomForest(
    x = X_train,
    y = as.factor(y_train),
    ntree = 100,  # Ridotto per velocità
    mtry = max(1, floor(sqrt(length(features)))),
    importance = TRUE
  )
  
  # Prepara i dati di test
  X_test <- test_clean[, features, drop = FALSE]
  y_test <- test_clean[[target]]
  
  # Ottieni le probabilità predette
  pred_probs <- predict(rf_model, X_test, type = "prob")
  
  # Controlla se ci sono abbastanza classi
  if (ncol(pred_probs) < 2) {
    warning("Il modello non ha predetto entrambe le classi")
    return(list(auc = 0.5, new_competitors_identified = 0))
  }
  
  pred_probs_1 <- pred_probs[, "1"]
  
  # Calcola AUC
  if (length(unique(y_test)) > 1) {
    roc_obj <- roc(y_test, pred_probs_1, quiet = TRUE)
    auc_value <- as.numeric(auc(roc_obj))
  } else {
    auc_value <- 0.5
  }
  
  # Identifica nuovi competitor (non presenti nel training)
  new_competitors <- test_clean[test_clean[[target]] == 1, ]
  
  # Calcola la proporzione di nuovi competitor correttamente identificati
  if (nrow(new_competitors) > 0) {
    # Usa la probabilità a priori come soglia per la classificazione
    prior_prob <- mean(train_clean[[target]])
    new_comp_probs <- predict(rf_model, new_competitors[, features, drop = FALSE], type = "prob")
    if (ncol(new_comp_probs) >= 2) {
      correctly_identified <- mean(new_comp_probs[, "1"] > prior_prob)
    } else {
      correctly_identified <- 0
    }
  } else {
    correctly_identified <- 0
  }
  
  return(list(
    model = rf_model,
    auc = auc_value,
    new_competitors_identified = correctly_identified,
    feature_importance = if (exists("rf_model")) importance(rf_model) else NULL
  ))
}

# Valuta il modello con diversi set di feature
evaluate_feature_sets <- function(delta = 1) {
  target <- if (delta == 1) "competitor_d1" else "competitor_d2"
  
  # Valuta i diversi set di feature
  results <- list()
  
  # Solo Economic
  cat("Valutando set Economic...\n")
  features_economic <- feature_sets$Economic
  results$Economic <- train_and_evaluate_rf(train_data, test_data, features_economic, target)
  
  # Economic + Product
  cat("Valutando set Economic + Product...\n")
  features_economic_product <- c(feature_sets$Economic, feature_sets$Product)
  results$Economic_Product <- train_and_evaluate_rf(train_data, test_data, features_economic_product, target)
  
  # Economic + Product + Labor
  cat("Valutando set Economic + Product + Labor...\n")
  features_economic_product_labor <- c(feature_sets$Economic, feature_sets$Product, feature_sets$Labor)
  results$Economic_Product_Labor <- train_and_evaluate_rf(train_data, test_data, features_economic_product_labor, target)
  
  # Economic + Product + Labor + Network
  cat("Valutando set completo...\n")
  features_all <- c(feature_sets$Economic, feature_sets$Product, feature_sets$Labor, feature_sets$Network)
  results$All <- train_and_evaluate_rf(train_data, test_data, features_all, target)
  
  return(results)
}

# Valuta i modelli per entrambi i valori di delta
cat("Valutazione modelli per delta = 1...\n")
results_delta1 <- evaluate_feature_sets(delta = 1)

cat("Valutazione modelli per delta = 2...\n")
results_delta2 <- evaluate_feature_sets(delta = 2)

# Mostra i risultati
print_results <- function(results, delta) {
  cat(sprintf("Risultati per delta = %d:\n", delta))
  cat("--------------------------\n")
  
  for (name in names(results)) {
    cat(sprintf("%s: AUC = %.3f, Nuovi competitor identificati = %.1f%%\n", 
                name, 
                results[[name]]$auc, 
                100 * results[[name]]$new_competitors_identified))
  }
  cat("\n")
}

print_results(results_delta1, 1)
print_results(results_delta2, 2)

#----------------------------------------------
# 5. VISUALIZZAZIONE DEI RISULTATI
#----------------------------------------------

# Crea un grafico di confronto delle performance
plot_comparison <- function(results_delta1, results_delta2) {
  # Estrai valori AUC
  feature_sets_names <- names(results_delta1)
  auc_delta1 <- sapply(feature_sets_names, function(name) results_delta1[[name]]$auc)
  auc_delta2 <- sapply(feature_sets_names, function(name) results_delta2[[name]]$auc)
  
  # Crea il dataframe per il plot
  auc_values <- c(auc_delta1, auc_delta2)
  names_vector <- c(paste0(feature_sets_names, "_d1"), paste0(feature_sets_names, "_d2"))
  
  # Crea il plot
  par(mar = c(12, 4, 4, 2) + 0.1)
  bp <- barplot(
    height = auc_values,
    names.arg = names_vector,
    col = rep(c("lightblue", "darkblue"), each = length(feature_sets_names)),
    ylim = c(0, 1),
    main = "Confronto delle Performance AUC",
    ylab = "AUC",
    cex.names = 0.6,
    las = 2
  )
  
  legend("topleft", 
         legend = c("δ = 1 (tutti i competitor)", "δ = 2 (competitor forti)"), 
         fill = c("lightblue", "darkblue"))
  
  # Aggiungi importanza delle caratteristiche per il modello con tutte le feature
  if (!is.null(results_delta1$All$feature_importance)) {
    cat("Importanza delle caratteristiche (δ = 1, tutte le feature):\n")
    print(results_delta1$All$feature_importance)
  }
  
  if (!is.null(results_delta2$All$feature_importance)) {
    cat("\nImportanza delle caratteristiche (δ = 2, tutte le feature):\n")
    print(results_delta2$All$feature_importance)
  }
}

# Visualizza i risultati
plot_comparison(results_delta1, results_delta2)

# Crea una visualizzazione 2-D della competizione come in Figura 7 del paper
plot_2d_competition <- function(test_data) {
  # Verifica che i dati esistano
  if (nrow(test_data) == 0) {
    cat("Nessun dato di test disponibile per la visualizzazione 2-D\n")
    return()
  }
  
  # Calcola l'overlap di prodotto e lavoro
  test_data$product_overlap <- pmax(0, pmin(1, test_data$sic_sim / 4))  # Normalizza tra 0 e 1
  test_data$labor_overlap <- pmax(0, pmin(1, (test_data$skill_term_sim + test_data$skill_topic_sim) / 2))
  
  # Rimuovi NA
  complete_cases <- complete.cases(test_data[, c("labor_overlap", "product_overlap", "competitor_d2")])
  test_clean <- test_data[complete_cases, ]
  
  if (nrow(test_clean) == 0) {
    cat("Nessun dato completo disponibile per la visualizzazione 2-D\n")
    return()
  }
  
  # Filtra per competitor forti (δ = 2)
  strong_competitors <- test_clean[test_clean$competitor_d2 == 1, ]
  
  # Calcola le mediane
  median_product <- median(test_clean$product_overlap, na.rm = TRUE)
  median_labor <- median(test_clean$labor_overlap, na.rm = TRUE)
  
  # Crea il plot
  plot(
    test_clean$labor_overlap, test_clean$product_overlap,
    pch = ".", col = "lightgray",
    xlim = c(0, 1), ylim = c(0, 1),
    xlab = "Labor Overlap", ylab = "Product Overlap",
    main = "2-D Competitor Analysis"
  )
  
  # Aggiungi i competitor forti se esistono
  if (nrow(strong_competitors) > 0) {
    points(
      strong_competitors$labor_overlap, strong_competitors$product_overlap,
      pch = 19, col = "red",
      cex = pmax(0.5, pmin(2, strong_competitors$hcf / 5))  # Dimensiona i punti in base all'HCF
    )
  }
  
  # Aggiungi le mediane
  abline(v = median_labor, lty = 2)
  abline(h = median_product, lty = 2)
  
  # Aggiungi etichette per i quadranti
  text(0.25, 0.75, "II: Indirect\nCompetitors", cex = 0.8)
  text(0.75, 0.75, "I: Direct\nCompetitors", cex = 0.8)
  text(0.25, 0.25, "III: Weak\nCompetitors", cex = 0.8)
  text(0.75, 0.25, "IV: Potential\nCompetitors", cex = 0.8)
}

# Visualizza il grafico 2-D
plot_2d_competition(test_data)

cat("Script completato con successo!\n")
