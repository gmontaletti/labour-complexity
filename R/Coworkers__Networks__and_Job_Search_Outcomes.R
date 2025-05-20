# Implementazione in R dei metodi utilizzati da Saygin, Weber e Weynandt (2014)
# "Coworkers, Networks, and Job Search Outcomes" -> title
# gsub(" |\\-|,", "_", title)

# Librerie necessarie
library(survival)    # Per i modelli di Cox
library(lmtest)      # Per i test sui modelli lineari
library(sandwich)    # Per errori standard robusti
library(plm)         # Per modelli a effetti fissi
library(dplyr)       # Per manipolazione dati

#---------------------------------------------------------------
# 1. ANALISI DALLA PROSPETTIVA DEL CERCATORE DI LAVORO
#---------------------------------------------------------------

# Supponiamo di avere un dataset 'displaced_workers' con le seguenti variabili:
# - id_worker: identificatore del lavoratore
# - id_closing_firm: identificatore dell'azienda in chiusura
# - time_to_job: giorni per trovare un nuovo lavoro (censurato a 365)
# - censored: 1 se il lavoratore non ha trovato lavoro entro 365 giorni, 0 altrimenti
# - network_size: dimensione della rete (numero di ex-colleghi)
# - share_employed: proporzione di ex-colleghi occupati al momento del licenziamento
# - share_same_industry: proporzione di ex-colleghi occupati nello stesso settore
# - share_hiring_firms: proporzione di ex-colleghi in aziende in espansione
# - share_high_wage_firms: proporzione di ex-colleghi in aziende ad alto salario
# - [varie variabili di controllo]

# Simuliamo alcuni dati per l'esempio
set.seed(123)
n <- 5000

displaced_workers <- data.frame(
  id_worker = 1:n,
  id_closing_firm = sample(1:500, n, replace = TRUE),
  time_to_job = sample(1:365, n, replace = TRUE),
  censored = rbinom(n, 1, 0.15),
  network_size = rlnorm(n, meanlog = 3.5, sdlog = 1),
  share_employed = rbeta(n, 5, 4),
  share_same_industry = rbeta(n, 2, 8),
  share_hiring_firms = rbeta(n, 2, 6),
  share_high_wage_firms = rbeta(n, 3, 7),
  female = rbinom(n, 1, 0.4),
  age = runif(n, 20, 55),
  blue_collar = rbinom(n, 1, 0.53),
  austrian = rbinom(n, 1, 0.91),
  tenure = runif(n, 1, 20),
  past_unemployed = rpois(n, 0.5),
  past_firms = rpois(n, 2) + 1
)

# Per i worker che non hanno trovato lavoro (censored=1), impostiamo time_to_job = 365
displaced_workers$time_to_job[displaced_workers$censored == 1] <- 365

# Trasformiamo network_size in scala logaritmica
displaced_workers$log_network_size <- log(displaced_workers$network_size)

# 1.1 Modello di Cox per il tasso di trovare un nuovo lavoro
# Modello 1: Solo dimensione della rete e quota di impiegati
cox_model1 <- coxph(
  Surv(time_to_job, 1 - censored) ~ 
    log_network_size + 
    share_employed + 
    female + age + blue_collar + austrian + tenure + 
    past_unemployed + past_firms + 
    strata(id_closing_firm),
  data = displaced_workers
)

summary(cox_model1)

# Modello 2: Aggiunta della quota di impiegati nello stesso settore
cox_model2 <- coxph(
  Surv(time_to_job, 1 - censored) ~ 
    log_network_size + 
    share_employed + 
    share_same_industry +
    female + age + blue_collar + austrian + tenure + 
    past_unemployed + past_firms + 
    strata(id_closing_firm),
  data = displaced_workers
)

summary(cox_model2)

# Modello 3: Aggiunta della quota di impiegati in aziende in espansione
cox_model3 <- coxph(
  Surv(time_to_job, 1 - censored) ~ 
    log_network_size + 
    share_employed + 
    share_same_industry +
    share_hiring_firms +
    female + age + blue_collar + austrian + tenure + 
    past_unemployed + past_firms + 
    strata(id_closing_firm),
  data = displaced_workers
)

summary(cox_model3)

# Modello 4: Aggiunta della quota di impiegati in aziende ad alto salario
cox_model5 <- coxph(
  Surv(time_to_job, 1 - censored) ~ 
    log_network_size + 
    share_employed + 
    share_same_industry +
    share_high_wage_firms +
    female + age + blue_collar + austrian + tenure + 
    past_unemployed + past_firms + 
    strata(id_closing_firm),
  data = displaced_workers
)

summary(cox_model5)

# 1.2 Alternativa: Modelli di probabilità lineare per trovare lavoro entro 3 mesi
displaced_workers$found_job_3months <- ifelse(displaced_workers$time_to_job <= 90, 1, 0)

lpm_model1 <- lm(
  found_job_3months ~ 
    log_network_size + 
    share_employed + 
    female + age + blue_collar + austrian + tenure + 
    past_unemployed + past_firms + 
    factor(id_closing_firm),
  data = displaced_workers
)

# Errori standard robusti
coeftest(lpm_model1, vcov = vcovHC(lpm_model1, type = "HC1"))

#---------------------------------------------------------------
# 2. ANALISI DALLA PROSPETTIVA DELL'AZIENDA CHE ASSUME
#---------------------------------------------------------------

# Supponiamo di avere un dataset 'firm_connections' con le seguenti variabili:
# - id_closing_firm: identificatore dell'azienda in chiusura
# - id_connected_firm: identificatore dell'azienda connessa
# - n_total_displaced: numero totale di lavoratori licenziati dall'azienda in chiusura
# - n_linked_displaced: numero di lavoratori licenziati con un link diretto ad un ex-collega nell'azienda connessa
# - n_hired_with_link: numero di lavoratori licenziati con link che vengono assunti dall'azienda connessa
# - n_hired_without_link: numero di lavoratori licenziati senza link che vengono assunti dall'azienda connessa
# - same_industry: 1 se le aziende sono nello stesso settore, 0 altrimenti
# - n_links: numero di collegamenti tra l'azienda in chiusura e quella connessa

# Simuliamo alcuni dati per l'esempio
set.seed(456)
m <- 10000

firm_connections <- data.frame(
  id_closing_firm = sample(1:500, m, replace = TRUE),
  id_connected_firm = sample(1:5000, m, replace = TRUE),
  n_total_displaced = sample(5:20, m, replace = TRUE)
)

# Aggiungiamo altre variabili
firm_connections <- firm_connections %>%
  mutate(
    n_linked_displaced = rbinom(m, n_total_displaced, 0.3),
    n_non_linked_displaced = n_total_displaced - n_linked_displaced,
    n_hired_with_link = rbinom(m, n_linked_displaced, 0.01),
    n_hired_without_link = rbinom(m, n_non_linked_displaced, 0.005),
    same_industry = rbinom(m, 1, 0.2),
    n_links = n_linked_displaced * runif(m, 1, 3),
    r_link = ifelse(n_linked_displaced > 0, n_hired_with_link / n_linked_displaced, 0),
    r_no_link = ifelse(n_non_linked_displaced > 0, n_hired_without_link / n_non_linked_displaced, 0),
    diff = r_link - r_no_link
  )

# 2.1 Implementazione dell'approccio di Kramarz e Thesmar
# Modello di base: confronto tra lavoratori della stessa azienda in chiusura ma con/senza link
link_effect_model <- lm(
  diff ~ 1,
  data = firm_connections,
  weights = n_links  # Ponderazione per il numero di collegamenti
)

summary(link_effect_model)

# 2.2 Analisi di eterogeneità per sottogruppi
# Supponiamo di avere variabili che indicano le caratteristiche dei gruppi
# Aggiungiamo variabili dummy per il confronto

firm_connections$vienna <- rbinom(m, 1, 0.3)
firm_connections$year_post_1995 <- rbinom(m, 1, 0.6)
firm_connections$large_layoff <- ifelse(firm_connections$n_total_displaced > 10, 1, 0)

# Solo aziende nello stesso settore
link_same_industry <- lm(
  diff ~ 1,
  data = firm_connections[firm_connections$same_industry == 1, ],
  weights = n_links
)

summary(link_same_industry)

# Solo aziende a Vienna
link_vienna <- lm(
  diff ~ 1,
  data = firm_connections[firm_connections$vienna == 1, ],
  weights = n_links
)

summary(link_vienna)

# Solo eventi post-1995
link_post_1995 <- lm(
  diff ~ 1,
  data = firm_connections[firm_connections$year_post_1995 == 1, ],
  weights = n_links
)

summary(link_post_1995)

#---------------------------------------------------------------
# 3. ETEROGENEITÀ DEGLI EFFETTI DI RETE
#---------------------------------------------------------------

# Supponiamo di avere un dataset con caratteristiche dei lavoratori licenziati,
# e per ogni azienda connessa indichiamo se hanno un link diretto o no
# qui simuliamo una versione semplificata

# Preparazione dati per analisi di eterogeneità
firm_connections$link_women_share <- runif(m, 0, 1)
firm_connections$link_blue_collar_share <- runif(m, 0, 1)
firm_connections$link_austrian_share <- runif(m, 0.7, 1)
firm_connections$link_older_share <- runif(m, 0, 1)

# Modello per l'eterogeneità di genere
het_gender_model <- lm(
  diff ~ link_women_share,
  data = firm_connections,
  weights = n_links
)

summary(het_gender_model)

# Modello per l'eterogeneità per tipo di occupazione
het_occupation_model <- lm(
  diff ~ link_blue_collar_share,
  data = firm_connections,
  weights = n_links
)

summary(het_occupation_model)

# Modello per l'eterogeneità per nazionalità
het_nationality_model <- lm(
  diff ~ link_austrian_share,
  data = firm_connections,
  weights = n_links
)

summary(het_nationality_model)

# Modello per l'eterogeneità per età
het_age_model <- lm(
  diff ~ link_older_share,
  data = firm_connections,
  weights = n_links
)

summary(het_age_model)

# Modello completo con tutte le caratteristiche
het_full_model <- lm(
  diff ~ link_women_share + link_blue_collar_share + 
    link_austrian_share + link_older_share,
  data = firm_connections,
  weights = n_links
)

summary(het_full_model)

#---------------------------------------------------------------
# NOTA: Nel paper reale, gli autori utilizzano una struttura di
# dati molto più dettagliata e complessa, con match esatti tra
# lavoratori licenziati e aziende connesse. Questa implementazione
# è una versione semplificata che illustra l'approccio metodologico
# utilizzato nel paper.
#---------------------------------------------------------------