# helper-construct-original.R
# Runs the OLD (per-subject future_lapply) pipeline on arbitrary data,
# so we can compare its output to the current vectorized implementation.
#
# Usage:
#   source("tests/helper-construct-original.R")
#   result_old <- construct_original(cohort_dt, exp_dt, a1c_dt, egfr_dt,
#                                     time_unit, first_exp_rule, exp_threshold,
#                                     dates = FALSE)

# Helper: source the old dataClass.R into an isolated environment,
# skipping the utils::globalVariables() call (which fails because the
# package namespace is locked when already loaded).
.source_old_code <- function() {
  old_env <- new.env(parent = asNamespace("LtAtStructuR"))
  old_file <- file.path(this_dir, "..", "R", "dataClass OLD.R")
  lines <- readLines(old_file)
  # Remove the globalVariables() call that fails on a locked namespace
  lines <- lines[!grepl("^utils::globalVariables", lines)]
  # Fix difftime bug: unit_time is Date subtraction result (difftime) but
  # gets used as a numeric denominator. Wrap it in as.numeric() at definition.
  lines <- sub(
    "unit_time <- out_data_slice\\[, unique\\(intend - intstart \\+ 1\\)\\]",
    "unit_time <- as.numeric(out_data_slice[, unique(intend - intstart + 1)])",
    lines
  )
  tmp <- tempfile(fileext = ".R")
  writeLines(lines, tmp)
  source(tmp, local = old_env)
  unlink(tmp)
  old_env
}

# Helper: build spec objects using the current package API.
# Class identity is string-based ("cohortData" %in% class(spec)),
# so package-created spec objects work fine with the old LtAtData$addSpec().
.build_specs <- function(cohort_dt, exp_dt, a1c_dt, egfr_dt) {
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
  list(cohort = cohort, exposure = exposure,
       covariate1 = covariate1, covariate2 = covariate2)
}

construct_original <- function(cohort_dt, exp_dt, a1c_dt, egfr_dt,
                                time_unit, first_exp_rule, exp_threshold,
                                dates = FALSE) {
  old_env <- .source_old_code()
  specs <- .build_specs(cohort_dt, exp_dt, a1c_dt, egfr_dt)

  obj <- old_env$LtAtData$new()
  obj$addSpec(specs$cohort)
  obj$addSpec(specs$exposure)
  obj$addSpec(specs$covariate1)
  obj$addSpec(specs$covariate2)
  obj$construct(time_unit,
                first_exp_rule = first_exp_rule,
                exp_threshold = exp_threshold,
                dates = dates)
  return(obj$data)
}

# Variant that runs the old pipeline step-by-step, returning the object
# with intermediate state intact (case/caseExp columns not yet removed).
construct_original_stepwise <- function(cohort_dt, exp_dt, a1c_dt, egfr_dt,
                                         time_unit, first_exp_rule, exp_threshold) {
  old_env <- .source_old_code()
  specs <- .build_specs(cohort_dt, exp_dt, a1c_dt, egfr_dt)

  obj <- old_env$LtAtData$new()
  obj$addSpec(specs$cohort)
  obj$addSpec(specs$exposure)
  obj$addSpec(specs$covariate1)
  obj$addSpec(specs$covariate2)
  obj$setAlgoOptions(time_unit,
                     first_exp_rule = first_exp_rule,
                     exp_threshold = exp_threshold,
                     dates = TRUE)
  obj$createIntervals()
  obj$assignAC()
  obj$assignL()
  # Stop here — do NOT call imputeL() or cleanUp()
  # so 'case' and 'caseExp' columns are preserved.
  return(obj)
}
