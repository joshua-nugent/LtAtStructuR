# helper-generate-data.R
# Generates a synthetic 1000-subject stress-test dataset with edge cases
# for verifying assignAC() correctness and benchmarking performance.

generate_synthetic_data <- function(n_subjects = 1000, seed = 42) {
  set.seed(seed)
  library(data.table)

  ids <- seq_len(n_subjects)

  # --- Cohort data ---
  index_dates <- as.Date("2010-01-01") + sample(0:365, n_subjects, replace = TRUE)
  follow_up_days <- sample(30:1500, n_subjects, replace = TRUE)
  eof_dates <- index_dates + follow_up_days

  # ~20% have outcome (AMI), rest are censored
  eof_types <- sample(c("AMI", "Censor"), n_subjects, replace = TRUE,
                      prob = c(0.2, 0.8))

  # baseline covariates
  age_entry <- round(runif(n_subjects, 40, 80), 1)
  sex <- sample(c("M", "F"), n_subjects, replace = TRUE)
  race <- sample(c("White", "Black", "Asian", "Other"), n_subjects,
                 replace = TRUE)
  a1c_base <- round(rnorm(n_subjects, 7, 1.5), 1)
  egfr_base <- round(rnorm(n_subjects, 60, 20), 0)

  cohort <- data.table(
    ID = ids,
    IndexDate = index_dates,
    EOFDate = eof_dates,
    EOFtype = eof_types,
    ageEntry = age_entry,
    sex = sex,
    race = race,
    A1c = a1c_base,
    eGFR = egfr_base
  )

  # --- Exposure data (binary) ---
  # Each subject has 0-4 exposure episodes
  exp_list <- lapply(ids, function(id) {
    n_episodes <- sample(0:4, 1, prob = c(0.15, 0.35, 0.25, 0.15, 0.10))
    if (n_episodes == 0) return(NULL)

    idx <- cohort[ID == id, IndexDate]
    eof <- cohort[ID == id, EOFDate]
    total_days <- as.numeric(eof - idx)
    if (total_days < 2) return(NULL)

    # generate non-overlapping episodes
    starts <- sort(sample(1:(total_days - 1), min(n_episodes * 2, total_days - 1)))
    episodes <- list()
    last_end <- 0
    for (s in starts) {
      if (s <= last_end) next
      duration <- sample(5:min(90, total_days - s), 1)
      end_day <- min(s + duration, total_days)
      episodes[[length(episodes) + 1]] <- data.table(
        ID = id,
        startA = idx + s,
        endA = idx + end_day
      )
      last_end <- end_day + 1
      if (length(episodes) >= n_episodes) break
    }
    if (length(episodes) == 0) return(NULL)
    rbindlist(episodes)
  })
  exp_dt <- rbindlist(exp_list[!sapply(exp_list, is.null)])

  # --- A1c covariate data ---
  a1c_list <- lapply(ids, function(id) {
    n_measures <- sample(0:8, 1)
    if (n_measures == 0) return(NULL)
    idx <- cohort[ID == id, IndexDate]
    eof <- cohort[ID == id, EOFDate]
    total_days <- as.numeric(eof - idx)
    if (total_days < 2) return(NULL)
    days <- sort(sample(1:(total_days - 1), min(n_measures, total_days - 1)))
    data.table(
      ID = id,
      A1cDate = idx + days,
      A1c = round(rnorm(length(days), 7, 1.5), 1)
    )
  })
  a1c_dt <- rbindlist(a1c_list[!sapply(a1c_list, is.null)])

  # --- eGFR covariate data ---
  egfr_list <- lapply(ids, function(id) {
    n_measures <- sample(0:6, 1)
    if (n_measures == 0) return(NULL)
    idx <- cohort[ID == id, IndexDate]
    eof <- cohort[ID == id, EOFDate]
    total_days <- as.numeric(eof - idx)
    if (total_days < 2) return(NULL)
    days <- sort(sample(1:(total_days - 1), min(n_measures, total_days - 1)))
    data.table(
      ID = id,
      eGFRDate = idx + days,
      eGFR = round(rnorm(length(days), 60, 20), 0)
    )
  })
  egfr_dt <- rbindlist(egfr_list[!sapply(egfr_list, is.null)])

  list(
    cohort = cohort,
    exposure = exp_dt,
    a1c = a1c_dt,
    egfr = egfr_dt
  )
}
