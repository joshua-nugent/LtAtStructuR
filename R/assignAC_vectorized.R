# Vectorized assignAC helper function
# Called from the assignAC method in dataClass.R
# Replaces the per-subject future_lapply loop with vectorized data.table operations

.assignAC_vectorized <- function(outcome_data, exp_data, cohort_data,
                                  id_var, start_date, end_date, exp_level,
                                  exp_ref, eof_date, eof_type, y_name,
                                  firs_exp_rule, exp_threshold) {

  # ---- Step 1: Merge outcome_data with cohort_data (all subjects at once) ----
  out_data <- merge(outcome_data, cohort_data, by = id_var, allow.cartesian = TRUE)
  data.table::setkeyv(out_data, c(id_var, "intnum"))

  # unit of time (should be constant across all subjects)
  unit_time <- as.numeric(out_data[1, intend - intstart + 1])

  # is this subject exposed at all?
  exposed_ids <- exp_data[, unique(get(id_var))]
  out_data[, .has_exp := get(id_var) %in% exposed_ids]

  is_categorical <- exp_data[, length(unique(get(exp_level)))] > 1

  # ---- Step 2: For exposed subjects, compute exposure overlap ----
  out_exposed <- out_data[(.has_exp), ]
  out_unexposed <- out_data[!(.has_exp), ]

  z_exposure_times <- NULL

  if (nrow(out_exposed) > 0) {
    # --- Z-interval overlap (for cases 12a/13a / 5b/6b) ---
    exp_overlap_z <- merge(out_exposed, exp_data, by = id_var,
                           allow.cartesian = TRUE)

    # Z-interval overlap: [eof_date - unit_time + 1, eof_date] vs [start_date, end_date]
    z_int_start <- exp_overlap_z[, get(eof_date) - unit_time + 1]
    z_int_end <- exp_overlap_z[, get(eof_date)]
    z_exp_start <- exp_overlap_z[, get(start_date)]
    z_exp_end <- exp_overlap_z[, get(end_date)]
    exp_overlap_z[, z_overlap := z_int_start <= z_exp_end & z_exp_start <= z_int_end]

    exp_overlap_z[z_overlap == FALSE, z_exp_time := 0]
    exp_overlap_z[z_overlap == TRUE,
                  z_exp_time := as.numeric(
                    pmin(get(eof_date), get(end_date)) -
                    pmax(get(eof_date) - unit_time + 1, get(start_date)) + 1
                  )]

    exp_overlap_z[, z_exp_time_by_unique_exp := sum(z_exp_time),
                  by = c(id_var, "intnum", exp_level)]
    exp_overlap_z[z_overlap == FALSE, z_exp_time_by_unique_exp := 0]
    exp_overlap_z[, z_exp_time_by_freq_exp := max(z_exp_time_by_unique_exp),
                  by = c(id_var, "intnum")]
    exp_overlap_z[, z_exp_time := sum(z_exp_time), by = c(id_var, "intnum")]

    data.table::setorderv(exp_overlap_z, c(id_var, "intnum", start_date))

    exp_overlap_z[, z_tie := {
      tied_levels <- unique(get(exp_level)[z_exp_time_by_unique_exp == z_exp_time_by_freq_exp])
      as.integer(length(tied_levels) > 1)
    }, by = c(id_var, "intnum")]

    exp_overlap_z[z_tie == 0, z_max_freq_exp := {
      unique(get(exp_level)[z_exp_time_by_unique_exp == z_exp_time_by_freq_exp])
    }, by = c(id_var, "intnum")]

    exp_overlap_z[z_tie == 1, z_max_freq_exp := {
      tail(get(exp_level)[z_exp_time_by_unique_exp == z_exp_time_by_freq_exp], 1)
    }, by = c(id_var, "intnum")]

    z_exposure_times <- data.table::copy(exp_overlap_z[
      z_exp_time_by_unique_exp == z_exp_time_by_freq_exp
    ])
    z_exposure_times <- z_exposure_times[, .SD[intnum == intnum[.N]],
                                          by = id_var]

    z_exp_overlap <- unique(exp_overlap_z[, c(id_var, "intnum",
                                              "z_exp_time",
                                              "z_exp_time_by_freq_exp",
                                              "z_tie",
                                              "z_max_freq_exp"),
                                          with = FALSE])

    # --- Standard interval overlap ---
    exp_overlap <- merge(out_exposed, exp_data, by = id_var,
                         allow.cartesian = TRUE)

    bin_start <- exp_overlap[, intstart]
    bin_end <- exp_overlap[, pmin(intend, get(eof_date))]
    e_start <- exp_overlap[, get(start_date)]
    e_end <- exp_overlap[, get(end_date)]
    exp_overlap[, overlap := bin_start <= e_end & e_start <= bin_end]
    exp_overlap <- exp_overlap[overlap == TRUE]

    exp_overlap[, exp_intervals := as.numeric(
      pmin(intend, get(end_date)) - pmax(intstart, get(start_date)) + 1
    )]
    exp_overlap[, exp_time := sum(exp_intervals), by = c(id_var, "intnum")]
    exp_overlap[, exp_time_by_unique_exp := sum(exp_intervals),
                by = c(id_var, "intnum", exp_level)]
    exp_overlap[, exp_time_by_freq_exp := max(exp_time_by_unique_exp),
                by = c(id_var, "intnum")]

    data.table::setorderv(exp_overlap, c(id_var, "intnum", start_date))

    exp_overlap[, tie := {
      tied_levels <- unique(get(exp_level)[exp_time_by_unique_exp == exp_time_by_freq_exp])
      as.integer(length(tied_levels) > 1)
    }, by = c(id_var, "intnum")]

    exp_overlap[tie == 0, max_freq_exp := {
      unique(get(exp_level)[exp_time_by_unique_exp == exp_time_by_freq_exp])
    }, by = c(id_var, "intnum")]

    exp_overlap[tie == 1, max_freq_exp := {
      tail(get(exp_level)[exp_time_by_unique_exp == exp_time_by_freq_exp], 1)
    }, by = c(id_var, "intnum")]

    start_date_col <- start_date
    end_date_col <- end_date
    exp_overlap[, (start_date_col) := NULL]
    exp_overlap[, (end_date_col) := NULL]
    exp_overlap[, exp_intervals := NULL]
    exp_overlap <- unique(exp_overlap)
    data.table::setkey(exp_overlap, NULL)

    exp_overlap[, final := exp_time_by_unique_exp == exp_time_by_freq_exp]
    exp_overlap[intnum == 0, final := max_freq_exp == get(exp_level)]
    exp_overlap <- exp_overlap[(final)]
    exp_overlap[, final := NULL]
    exp_overlap[, A0.warn := 0L]

    data.table::setkey(out_exposed, NULL)
    overlap_exposed <- merge(out_exposed, exp_overlap, all.x = TRUE)
    overlap_exposed <- merge(overlap_exposed, z_exp_overlap,
                             by = c(id_var, "intnum"), all.x = TRUE)
  } else {
    overlap_exposed <- data.table::data.table()
  }

  # ---- Step 3: Handle unexposed subjects ----
  if (nrow(out_unexposed) > 0) {
    overlap_unexposed <- data.table::copy(out_unexposed)
    overlap_unexposed[, (exp_level) := NA]
    overlap_unexposed[, overLap := NA]
    overlap_unexposed[, exp_time := NA_real_]
    overlap_unexposed[, z_exp_time := NA_real_]
    overlap_unexposed[, z_exp_time_by_freq_exp := NA_real_]
    overlap_unexposed[, exp_time_by_unique_exp := NA_real_]
    overlap_unexposed[, exp_time_by_freq_exp := NA_real_]
    overlap_unexposed[, tie := 0L]
    overlap_unexposed[, A0.warn := 0L]
  } else {
    overlap_unexposed <- data.table::data.table()
  }

  # ---- Step 4: Combine and fill NAs ----
  # Ensure type consistency before rbindlist
  if (nrow(overlap_exposed) > 0) {
    for (col in c("exp_time", "z_exp_time", "z_exp_time_by_freq_exp",
                  "exp_time_by_unique_exp", "exp_time_by_freq_exp")) {
      if (col %in% names(overlap_exposed)) {
        overlap_exposed[, (col) := as.numeric(get(col))]
      }
    }
  }

  overlap_data <- data.table::rbindlist(list(overlap_exposed, overlap_unexposed),
                                        use.names = TRUE, fill = TRUE)

  overlap_data[is.na(exp_time), exp_time := 0]
  overlap_data[is.na(z_exp_time), z_exp_time := 0]
  overlap_data[is.na(exp_time_by_unique_exp), exp_time_by_unique_exp := 0]
  overlap_data[is.na(tie), tie := 0L]
  overlap_data[is.na(A0.warn), A0.warn := 0L]
  overlap_data[is.na(exp_time_by_freq_exp), exp_time_by_freq_exp := 0]
  overlap_data[is.na(z_exp_time_by_freq_exp), z_exp_time_by_freq_exp := 0]
  data.table::setkeyv(overlap_data, c(id_var, "intnum"))

  # ---- Step 5: Initialize columns ----
  overlap_data[, exposure := 0L]
  overlap_data[, censor := 0L]
  overlap_data[, case := "tmp"]

  # ---- Step 6: Censoring detection ----
  overlap_data[get(eof_type) != y_name,
               censor := as.integer(get(eof_date) >= intstart & get(eof_date) <= intend)]

  # ---- Step 7: Case assignment (per-subject) ----
  # The case logic is inherently sequential within each subject.
  # The expensive merge/overlap work is already done above (vectorized).
  # This loop only does lightweight case assignment on pre-merged data.
  id_col <- id_var
  split_data <- split(overlap_data, by = id_col)
  result_list <- data.table::rbindlist(lapply(split_data, function(od) {
    n <- nrow(od)

    # LOGIC 1
    if (firs_exp_rule == 1) {
      od[exp_time_by_freq_exp > 0, exposure := 1L]
      if (od[, sum(exposure)] > 0) {
        first_exp_intnum <- od[min(which(exposure == 1)), intnum]
        od[intnum == first_exp_intnum, case := "1a"]
      }
      od[, Part1 := !cumsum(data.table::shift(exposure, n = 1L, fill = 0L,
                                              type = "lag")) > 0]
      od[case == "1a", Part1 := TRUE]

      od[Part1 == TRUE & exposure != 1L, case := "2a"]

      # case 3a
      if (od[Part1 == TRUE, sum(outcome) > 0]) {
        outcome_rows <- which(od$outcome == 1)
        if (length(outcome_rows) > 0) {
          before_rows <- outcome_rows - 1
          before_rows <- before_rows[before_rows >= 1]
          if (length(before_rows) > 0) {
            od[before_rows, `:=`(exposure = 0L, censor = 0L, outcome = 0L, case = "3a")]
          }
          od[outcome == 1 & Part1 == TRUE, `:=`(exposure = NA_integer_,
                                                 censor = NA_integer_, case = "3a")]
        }
      }

      # case 4a
      if (od[Part1 == TRUE & exposure == 0, any(censor == 1, na.rm = TRUE)]) {
        od[as.logical(censor), `:=`(outcome = 0L, exposure = 0L, case = "4a")]
      }

      # case 5a
      if (od[Part1 == TRUE & exposure == 1, any(censor == 1, na.rm = TRUE)]) {
        od[censor == 1L, `:=`(outcome = 0L, exposure = 1L, case = "5a")]
      }

      # case 6a
      od[, lagPart1 := data.table::shift(Part1, n = 1L, fill = TRUE, type = "lag")]
      if (od[Part1 == TRUE, any(exposure == 1, na.rm = TRUE)] &&
          od[lagPart1 == TRUE, any(outcome == 1, na.rm = TRUE)]) {
        intnum_before_outcome <- od[which(outcome == 1), intnum - 1]
        od[intnum %in% intnum_before_outcome, `:=`(
          exposure = 1L, censor = 0L, outcome = 0L, case = "6a"
        )]
        od[outcome == 1, `:=`(exposure = NA_integer_, censor = NA_integer_, case = "6a")]
      }
      od[outcome == 1 & lagPart1 == TRUE, Part1 := TRUE]
    }

    if (firs_exp_rule == 0) {
      od[, Part1 := FALSE]
    }

    # Part 2
    od[, exp_beyond_threshold := as.numeric(exp_time_by_freq_exp) /
         as.numeric(pmin(intend, get(eof_date)) - intstart + 1) >= exp_threshold]
    od[, Awarn_exp_beyond_threshold := as.numeric(exp_time) /
         as.numeric(pmin(intend, get(eof_date)) - intstart + 1) >= exp_threshold]

    last_intnum <- od[n, intnum]

    od[intnum < last_intnum & exp_beyond_threshold & !Part1,
       `:=`(exposure = 1L, outcome = 0L, censor = 0L,
            case = ifelse(firs_exp_rule == 0, "1b", "8a"))]

    od[intnum < last_intnum & exp_beyond_threshold == FALSE & !Part1,
       `:=`(exposure = 0L, outcome = 0L, censor = 0L, tie = 0L,
            A0.warn = ifelse(Awarn_exp_beyond_threshold, 1L, 0L),
            case = ifelse(firs_exp_rule == 0, "2b", "9a"))]

    # cases 10a/11a / 3b/4b
    if (od[n, censor == 1 & Part1 == FALSE]) {
      od[intnum == last_intnum & exp_beyond_threshold == TRUE,
         `:=`(censor = 1L, outcome = 0L, exposure = 1L,
              case = ifelse(firs_exp_rule == 0, "3b", "10a"))]
      od[intnum == last_intnum & exp_beyond_threshold == FALSE,
         `:=`(censor = 1L, outcome = 0L, exposure = 0L, tie = 0L,
              A0.warn = ifelse(Awarn_exp_beyond_threshold, 1L, 0L),
              case = ifelse(firs_exp_rule == 0, "4b", "11a"))]
    }

    # cases 12a/13a / 5b/6b
    if (od[n, outcome == 1 & Part1 == FALSE]) {
      if (nrow(od) > 2) {
        od[, z_exp_beyond_threshold :=
             as.numeric(z_exp_time_by_freq_exp) / unit_time >= exp_threshold]
        od[n, z_exp_beyond_threshold := od[n - 1, z_exp_beyond_threshold]]
        od[, Awarn_z_exp_beyond_threshold :=
             as.numeric(z_exp_time) / unit_time >= exp_threshold]
      } else {
        ref_time <- as.numeric(od[1, get(eof_date) - intstart + 1])
        od[, z_exp_beyond_threshold :=
             as.numeric(z_exp_time_by_freq_exp) / ref_time >= exp_threshold]
        od[n, z_exp_beyond_threshold := od[n - 1, z_exp_beyond_threshold]]
        od[, Awarn_z_exp_beyond_threshold :=
             as.numeric(exp_time) / ref_time >= exp_threshold]
      }

      penult_intnum <- od[n - 1, intnum]

      od[intnum == penult_intnum & z_exp_beyond_threshold == TRUE,
         `:=`(censor = 0L, outcome = 0L, exposure = 1L,
              tie = unique(z_tie[!is.na(z_tie)])[1],
              A0.warn = 0L,
              case = ifelse(firs_exp_rule == 0, "5b", "12a"))]
      od[seq_len(n) == n & z_exp_beyond_threshold == TRUE,
         `:=`(censor = NA_integer_, outcome = 1L, exposure = NA_integer_,
              case = ifelse(firs_exp_rule == 0, "5b", "12a"))]

      od[intnum == penult_intnum & z_exp_beyond_threshold == FALSE,
         `:=`(censor = 0L, outcome = 0L, exposure = 0L, tie = 0L,
              A0.warn = ifelse(Awarn_z_exp_beyond_threshold, 1L, 0L),
              case = ifelse(firs_exp_rule == 0, "6b", "13a"))]
      od[seq_len(n) == n & z_exp_beyond_threshold == FALSE,
         `:=`(censor = NA_integer_, outcome = 1L, exposure = NA_integer_,
              tie = 0L,
              case = ifelse(firs_exp_rule == 0, "6b", "13a"))]
    }

    od
  }), use.names = TRUE, fill = TRUE)

  # ---- Step 8: Categorical exposure tie-breaking ----
  if (is_categorical) {
    result_list[exposure == 0, exposureTMP := exp_ref]
    result_list[exposure == 1, exposureTMP := get(exp_level)]
    result_list[outcome == 1, exposureTMP := ""]
    result_list[, (exp_level) := as.character(get(exp_level))]

    if (firs_exp_rule == 1) {
      result_list[tie == 1 & case %in% c("1a", "5a", "6a"),
                   exposureTMP := max_freq_exp]
      result_list[exposure == 1 & case == "12a" & tie != 1,
                   exposureTMP := unique(z_exposure_times[, get(exp_level)]),
                   by = id_var]

      result_list[, TIE_8A := case == "8a" & tie == 1]
      if (any(result_list$TIE_8A)) {
        for (tie_intnum in result_list[TIE_8A == TRUE, unique(intnum)]) {
          tied_ids <- result_list[TIE_8A == TRUE & intnum == tie_intnum,
                                   unique(get(id_var))]
          for (tid in tied_ids) {
            tie_test <- any(result_list[get(id_var) == tid & intnum == tie_intnum,
                                         exposureTMP] %in%
                            result_list[get(id_var) == tid & intnum == (tie_intnum - 1),
                                         exposureTMP])
            at_minus1 <- unique(result_list[get(id_var) == tid &
                                             intnum == (tie_intnum - 1), exposureTMP])
            result_list[get(id_var) == tid & intnum == tie_intnum,
                         exposureTMP := ifelse(tie_test, at_minus1,
                                               unique(max_freq_exp))]
          }
        }
      }

      result_list[, TIE_10A := case == "10a" & tie == 1]
      if (any(result_list$TIE_10A)) {
        tied_ids <- result_list[TIE_10A == TRUE, unique(get(id_var))]
        for (tid in tied_ids) {
          od_sub <- result_list[get(id_var) == tid]
          od_sub[, TIE_10A_LEAD := data.table::shift(TIE_10A, n = 1L,
                                                       fill = NA, type = "lead")]
          tie_test <- any(od_sub[TIE_10A == TRUE, exposureTMP] %in%
                          od_sub[!TIE_10A & TIE_10A_LEAD == TRUE, exposureTMP])
          at_minus1 <- od_sub[od_sub[, .N] - 1, exposureTMP]
          result_list[get(id_var) == tid & TIE_10A == TRUE,
                       exposureTMP := ifelse(tie_test, at_minus1,
                                             unique(max_freq_exp))]
        }
      }

      result_list[, TIE_12A := case == "12a" & tie == 1]
      if (any(result_list$TIE_12A)) {
        tied_ids <- result_list[TIE_12A == TRUE, unique(get(id_var))]
        for (tid in tied_ids) {
          od_sub <- result_list[get(id_var) == tid]
          od_sub[, TIE_12A_LEAD := data.table::shift(TIE_12A, n = 1L,
                                                       fill = NA, type = "lead")]
          zt_sub <- z_exposure_times[get(id_var) == tid]
          tie_test <- any(zt_sub[, get(exp_level)] %in%
                          od_sub[!TIE_12A & TIE_12A_LEAD == TRUE, exposureTMP])
          at_minus1 <- od_sub[od_sub[, .N] - 1, exposureTMP]
          result_list[get(id_var) == tid & TIE_12A == TRUE,
                       exposureTMP := ifelse(tie_test, at_minus1,
                                             unique(z_max_freq_exp))]
        }
      }

    } else if (firs_exp_rule == 0) {
      result_list[exposure == 1 & case == "5b" & tie != 1,
                   exposureTMP := ifelse(
                     (z_exp_time_by_freq_exp / z_exp_time) >= exp_threshold,
                     z_max_freq_exp, exposureTMP)]
      result_list[is.na(get(exp_level)) & is.na(exposureTMP) &
                   case == "5b" & tie != 1,
                   exposureTMP := z_max_freq_exp]

      result_list[, TIE_1B := case == "1b" & tie == 1]
      if (any(result_list$TIE_1B)) {
        for (tie_intnum in result_list[TIE_1B == TRUE, unique(intnum)]) {
          tied_ids <- result_list[TIE_1B == TRUE & intnum == tie_intnum,
                                   unique(get(id_var))]
          for (tid in tied_ids) {
            tie_test <- any(result_list[get(id_var) == tid & intnum == tie_intnum,
                                         exposureTMP] %in%
                            result_list[get(id_var) == tid & intnum == (tie_intnum - 1),
                                         exposureTMP])
            at_minus1 <- unique(result_list[get(id_var) == tid &
                                             intnum == (tie_intnum - 1), exposureTMP])
            result_list[get(id_var) == tid & intnum == tie_intnum,
                         exposureTMP := ifelse(tie_test, at_minus1,
                                               unique(max_freq_exp))]
          }
        }
      }

      result_list[, TIE_3B := case == "3b" & tie == 1]
      if (any(result_list$TIE_3B)) {
        tied_ids <- result_list[TIE_3B == TRUE, unique(get(id_var))]
        for (tid in tied_ids) {
          od_sub <- result_list[get(id_var) == tid]
          od_sub[, TIE_3B_LEAD := data.table::shift(TIE_3B, n = 1L,
                                                      fill = NA, type = "lead")]
          tie_test <- any(od_sub[TIE_3B == TRUE, exposureTMP] %in%
                          od_sub[!TIE_3B & TIE_3B_LEAD == TRUE, exposureTMP])
          at_minus1 <- od_sub[od_sub[, .N] - 1, exposureTMP]
          result_list[get(id_var) == tid & TIE_3B == TRUE,
                       exposureTMP := ifelse(tie_test, at_minus1,
                                             unique(max_freq_exp))]
        }
      }

      result_list[, TIE_5B := case == "5b" & tie == 1]
      if (any(result_list$TIE_5B)) {
        tied_ids <- result_list[TIE_5B == TRUE, unique(get(id_var))]
        for (tid in tied_ids) {
          od_sub <- result_list[get(id_var) == tid]
          od_sub[, TIE_5B_LEAD := data.table::shift(TIE_5B, n = 1L,
                                                      fill = NA, type = "lead")]
          zt_sub <- z_exposure_times[get(id_var) == tid]
          tie_test <- any(zt_sub[, get(exp_level)] %in%
                          od_sub[!TIE_5B & TIE_5B_LEAD == TRUE, exposureTMP])
          at_minus1 <- od_sub[od_sub[, .N] - 1, exposureTMP]
          result_list[get(id_var) == tid & TIE_5B == TRUE,
                       exposureTMP := ifelse(tie_test, at_minus1,
                                             unique(z_max_freq_exp))]
        }
      }
    }

    result_list[, exposure := NULL]
    result_list[, exposure := exposureTMP]
    result_list[, final := TRUE]
    result_list[duplicated(paste0(get(id_var), "_", intnum)), final := FALSE]
    result_list <- result_list[(final)]

    eof_type_col <- intersect(c(eof_type, "EOFtype"), names(result_list))[1]
    result_list[, c(id_var, "intnum", "intstart", "intend",
                     "exposure", "outcome", "censor", "case",
                     eof_type_col, "tie", "A0.warn"),
                 with = FALSE]
  } else {
    eof_type_col <- intersect(c(eof_type, "EOFtype"), names(result_list))[1]
    result_list[, c(id_var, "intnum", "intstart", "intend",
                     "exposure", "outcome", "censor", "case",
                     eof_type_col),
                 with = FALSE]
  }
}
