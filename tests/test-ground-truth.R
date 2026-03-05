# test-ground-truth.R
# Generates ground-truth snapshots from the CURRENT assignAC() implementation,
# and (when snapshots exist) verifies new output matches them exactly.
#
# Usage:
#   Rscript tests/test-ground-truth.R generate   # save snapshots
#   Rscript tests/test-ground-truth.R verify     # compare against snapshots
#   Rscript tests/test-ground-truth.R benchmark  # time the synthetic data run

library(data.table)
library(LtAtStructuR)
library(future)
plan(sequential)

# Determine script directory robustly
this_dir <- tryCatch(
  dirname(sys.frame(1)$ofile),
  error = function(e) {
    # Rscript: use commandArgs to find script path
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
      dirname(normalizePath(sub("^--file=", "", file_arg[1])))
    } else {
      "tests"
    }
  }
)

source(file.path(this_dir, "helper-generate-data.R"))

fixtures_dir <- file.path(this_dir, "fixtures")

# ============================================================
# Helper: run construct() and return the output data.table
# ============================================================
run_pipeline <- function(cohort_dt, exp_dt, a1c_dt, egfr_dt,
                         time_unit, first_exp_rule, exp_threshold) {
  cohort <- setCohort(
    data.table::copy(cohort_dt), "ID", "IndexDate", "EOFDate", "EOFtype",
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
  exposure <- setExposure(data.table::copy(exp_dt), "ID", "startA", "endA")
  covariate1 <- setCovariate(data.table::copy(a1c_dt), "sporadic", "ID",
                              "A1cDate", "A1c", categorical = FALSE)
  covariate2 <- setCovariate(data.table::copy(egfr_dt), "sporadic", "ID",
                              "eGFRDate", "eGFR", categorical = TRUE)

  spec <- cohort + exposure + covariate1 + covariate2
  result <- construct(spec, time_unit = time_unit,
                      first_exp_rule = first_exp_rule,
                      exp_threshold = exp_threshold)
  result
}

# ============================================================
# Test configurations
# ============================================================
configs <- list(
  list(name = "bundled_t15_f1_p75",  time_unit = 15, first_exp_rule = 1, exp_threshold = 0.75),
  list(name = "bundled_t15_f0_p75",  time_unit = 15, first_exp_rule = 0, exp_threshold = 0.75),
  list(name = "bundled_t30_f1_p50",  time_unit = 30, first_exp_rule = 1, exp_threshold = 0.50),
  list(name = "bundled_t30_f0_p25",  time_unit = 30, first_exp_rule = 0, exp_threshold = 0.25)
)

# ============================================================
# MODE: generate
# ============================================================
do_generate <- function() {
  dir.create(fixtures_dir, recursive = TRUE, showWarnings = FALSE)

  # --- Bundled data (4 configs) ---
  message("=== Generating ground truth from bundled data ===")
  for (cfg in configs) {
    message(sprintf("  Config: %s (t=%d, f=%d, p=%.2f)",
                    cfg$name, cfg$time_unit, cfg$first_exp_rule,
                    cfg$exp_threshold))
    result <- run_pipeline(cohortDT, expDT, a1cDT, egfrDT,
                           cfg$time_unit, cfg$first_exp_rule,
                           cfg$exp_threshold)
    saveRDS(result, file.path(fixtures_dir, paste0(cfg$name, ".rds")))
    message(sprintf("    Saved: %d rows, %d cols", nrow(result), ncol(result)))
  }

  # --- Synthetic stress test ---
  message("\n=== Generating synthetic stress test data ===")
  syn <- generate_synthetic_data(n_subjects = 1000, seed = 42)
  saveRDS(syn, file.path(fixtures_dir, "synthetic_input.rds"))

  for (cfg in list(
    list(name = "synthetic_t15_f1_p75", time_unit = 15, first_exp_rule = 1, exp_threshold = 0.75),
    list(name = "synthetic_t30_f0_p50", time_unit = 30, first_exp_rule = 0, exp_threshold = 0.50)
  )) {
    message(sprintf("  Config: %s", cfg$name))
    timing <- system.time({
      result <- run_pipeline(syn$cohort, syn$exposure, syn$a1c, syn$egfr,
                             cfg$time_unit, cfg$first_exp_rule,
                             cfg$exp_threshold)
    })
    saveRDS(result, file.path(fixtures_dir, paste0(cfg$name, ".rds")))
    message(sprintf("    Saved: %d rows, %d cols (%.1f sec)",
                    nrow(result), ncol(result), timing["elapsed"]))
  }

  message("\nDone! Ground truth saved to: ", fixtures_dir)
}

# ============================================================
# MODE: verify
# ============================================================
do_verify <- function() {
  message("=== Verifying against ground truth snapshots ===\n")
  all_pass <- TRUE

  # --- Bundled data ---
  for (cfg in configs) {
    snapshot_file <- file.path(fixtures_dir, paste0(cfg$name, ".rds"))
    if (!file.exists(snapshot_file)) {
      message(sprintf("  SKIP %s: snapshot not found", cfg$name))
      next
    }
    expected <- readRDS(snapshot_file)
    result <- run_pipeline(cohortDT, expDT, a1cDT, egfrDT,
                           cfg$time_unit, cfg$first_exp_rule,
                           cfg$exp_threshold)
    comparison <- all.equal(expected, result, check.attributes = FALSE)
    if (isTRUE(comparison)) {
      message(sprintf("  PASS %s", cfg$name))
    } else {
      message(sprintf("  FAIL %s:", cfg$name))
      message(paste("    ", comparison, collapse = "\n"))
      all_pass <- FALSE
    }
  }

  # --- Synthetic data ---
  syn_input_file <- file.path(fixtures_dir, "synthetic_input.rds")
  if (file.exists(syn_input_file)) {
    syn <- readRDS(syn_input_file)
    for (cfg in list(
      list(name = "synthetic_t15_f1_p75", time_unit = 15, first_exp_rule = 1, exp_threshold = 0.75),
      list(name = "synthetic_t30_f0_p50", time_unit = 30, first_exp_rule = 0, exp_threshold = 0.50)
    )) {
      snapshot_file <- file.path(fixtures_dir, paste0(cfg$name, ".rds"))
      if (!file.exists(snapshot_file)) {
        message(sprintf("  SKIP %s: snapshot not found", cfg$name))
        next
      }
      expected <- readRDS(snapshot_file)
      result <- run_pipeline(syn$cohort, syn$exposure, syn$a1c, syn$egfr,
                             cfg$time_unit, cfg$first_exp_rule,
                             cfg$exp_threshold)
      comparison <- all.equal(expected, result, check.attributes = FALSE)
      if (isTRUE(comparison)) {
        message(sprintf("  PASS %s", cfg$name))
      } else {
        message(sprintf("  FAIL %s:", cfg$name))
        message(paste("    ", comparison, collapse = "\n"))
        all_pass <- FALSE
      }
    }
  }

  if (all_pass) {
    message("\nAll tests PASSED!")
  } else {
    message("\nSome tests FAILED!")
    quit(status = 1)
  }
}

# ============================================================
# MODE: benchmark
# ============================================================
do_benchmark <- function() {
  syn_input_file <- file.path(fixtures_dir, "synthetic_input.rds")
  if (!file.exists(syn_input_file)) {
    message("Generating synthetic data first...")
    syn <- generate_synthetic_data(n_subjects = 1000, seed = 42)
    dir.create(fixtures_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(syn, syn_input_file)
  } else {
    syn <- readRDS(syn_input_file)
  }

  message("=== Benchmarking assignAC() on 1000-subject synthetic data ===\n")
  for (cfg in list(
    list(name = "t15_f1_p75", time_unit = 15, first_exp_rule = 1, exp_threshold = 0.75),
    list(name = "t30_f0_p50", time_unit = 30, first_exp_rule = 0, exp_threshold = 0.50)
  )) {
    timing <- system.time({
      result <- run_pipeline(syn$cohort, syn$exposure, syn$a1c, syn$egfr,
                             cfg$time_unit, cfg$first_exp_rule,
                             cfg$exp_threshold)
    })
    message(sprintf("  %s: %.1f sec elapsed (%.1f user + %.1f system)",
                    cfg$name, timing["elapsed"], timing["user.self"],
                    timing["sys.self"]))
  }
}

# ============================================================
# Main dispatch
# ============================================================
args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[1] else "verify"

switch(mode,
  "generate" = do_generate(),
  "verify" = do_verify(),
  "benchmark" = do_benchmark(),
  stop("Unknown mode: ", mode, ". Use 'generate', 'verify', or 'benchmark'.")
)
