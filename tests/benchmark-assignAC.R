# Targeted benchmark: measures just assignAC() time
# by constructing the LtAtData object, running createIntervals() manually,
# then timing assignAC() separately.

library(data.table)
library(LtAtStructuR)
library(future)
plan(sequential)

source(file.path(dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))),
  "helper-generate-data.R"))

fixtures_dir <- file.path(dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]))),
  "fixtures")

syn <- readRDS(file.path(fixtures_dir, "synthetic_input.rds"))

message("=== Targeted assignAC() benchmark (1000 subjects) ===\n")

for (cfg in list(
  list(name = "t15_f1_p75", time_unit = 15, first_exp_rule = 1, exp_threshold = 0.75),
  list(name = "t30_f0_p50", time_unit = 30, first_exp_rule = 0, exp_threshold = 0.50)
)) {
  cohort <- setCohort(
    data.table::copy(syn$cohort), "ID", "IndexDate", "EOFDate", "EOFtype",
    "AMI", c("ageEntry", "sex", "race", "A1c", "eGFR"),
    list(
      "ageEntry" = list("categorical" = FALSE, "impute" = NA,
                        "impute_default_level" = NA),
      "sex" = list("categorical" = TRUE, "impute" = NA,
                   "impute_default_level" = NA),
      "race" = list("categorical" = TRUE, "impute" = NA,
                    "impute_default_level" = NA)
    )
  )
  exposure <- setExposure(data.table::copy(syn$exposure), "ID", "startA", "endA")
  covariate1 <- setCovariate(data.table::copy(syn$a1c), "sporadic", "ID",
                              "A1cDate", "A1c", categorical = FALSE)
  covariate2 <- setCovariate(data.table::copy(syn$egfr), "sporadic", "ID",
                              "eGFRDate", "eGFR", categorical = TRUE)

  spec <- cohort + exposure + covariate1 + covariate2

  # Run setAlgoOptions and createIntervals (not timed)
  suppressWarnings(suppressMessages({
    spec$setAlgoOptions(cfg$time_unit, first_exp_rule = cfg$first_exp_rule,
                        exp_threshold = cfg$exp_threshold)
    spec$createIntervals()
  }))

  # Time JUST assignAC
  timing <- system.time({
    spec$assignAC()
  })
  message(sprintf("  %s assignAC: %.1f sec elapsed", cfg$name, timing["elapsed"]))

  # Time assignL too for comparison
  timing_l <- system.time({
    spec$assignL()
  })
  message(sprintf("  %s assignL:  %.1f sec elapsed", cfg$name, timing_l["elapsed"]))
}
