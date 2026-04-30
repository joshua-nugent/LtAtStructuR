# Vectorized assignL helper function
# Called from the assignL method in dataClass.R
# Replaces the per-subject future_lapply loop with vectorized data.table operations
# and replaces lubridate interval/within/intersect with plain date arithmetic

.assignL_vectorized <- function(outcome_data, cohort_data, exp_data, cov_data,
                                 id_var, index_date_var, eof_date_var, eof_type_var,
                                 start_date_var, end_date_var, A_level,
                                 cov_name, cov_date, cov_acute,
                                 first_exp_rule, exp_ref) {

  exp_character <- is.character(exp_data[[A_level]])

  # ---- Phase A: Vectorized covariate-to-interval matching ----

  # 1. Work with copies to avoid modifying originals
  od <- data.table::copy(outcome_data)
  data.table::setnames(od, "case", "caseExp")

  # 2. Merge outcome_data with cohort_data to get index_date, eof_date, baseline cov per row
  cohort_subset <- cohort_data[, c(id_var, index_date_var, eof_date_var, cov_name),
                                with = FALSE]
  od <- merge(od, cohort_subset, by = id_var, all.x = TRUE)
  data.table::setnames(od, c(index_date_var, eof_date_var, cov_name),
                        c(".index_date", ".eof_date", ".index_cov"))
  od[, .eof_date := lubridate::ymd(.eof_date)]

  # 3. Get time-dependent covariate data
  cov_dt <- data.table::copy(cov_data$data)
  cov_dt[, .cov_date_raw := get(cov_date)]
  cov_dt[, .cov_date := lubridate::ymd(get(cov_date))]
  cov_dt[, .cov_value := get(cov_name)]

  # 4. Keep only post-index measurements (per subject)
  index_lookup <- unique(od[, c(id_var, ".index_date"), with = FALSE])
  cov_dt <- merge(cov_dt, index_lookup, by = id_var, all.x = TRUE)
  cov_dt <- cov_dt[.cov_date > .index_date]

  # 5. Non-equi join: match covariates to intervals
  # Need: which interval does each covariate date fall in?
  od_for_join <- od[, c(id_var, "intnum", "intstart", "intend"), with = FALSE]
  data.table::setkey(od_for_join, NULL)

  # Perform the join: for each covariate measurement, find the interval it falls in
  overlaps <- od_for_join[cov_dt,
                           on = c(id_var, "intstart<=.cov_date", "intend>=.cov_date"),
                           nomatch = 0L, allow.cartesian = TRUE]
  # After join, intstart and intend are overwritten with the cov_date values
  # We need to keep the covariate date and the matched intnum
  # The join gives us: id_var, intnum (from od), intstart (=.cov_date), intend (=.cov_date)
  # Plus all columns from cov_dt
  overlaps[, .matched_cov_date := intstart]  # intstart was replaced with .cov_date in the join
  overlaps[, c("intstart", "intend") := NULL]

  # 6. Shift intnum + 1 (covariate in bin t -> L(t+1)) for subjects with >1 interval
  n_intervals <- od[, .N, by = id_var]
  data.table::setnames(n_intervals, "N", ".n_intervals")
  overlaps <- merge(overlaps, n_intervals, by = id_var, all.x = TRUE)
  overlaps[.n_intervals > 1, intnum := intnum + 1L]
  overlaps[, .n_intervals := NULL]

  # 7. Keep most recent measurement per (id, intnum)
  overlaps <- overlaps[, .SD[which.max(.matched_cov_date)], by = c(id_var, "intnum")]

  # 8. Merge back to get combined_data with covariate columns
  overlap_cols <- overlaps[, c(id_var, "intnum", ".matched_cov_date", ".cov_value"),
                            with = FALSE]
  combined <- merge(od, overlap_cols, by = c(id_var, "intnum"), all.x = TRUE)

  # Set the covariate columns using the overlap results
  combined[, (cov_name) := .cov_value]
  combined[, (cov_date) := .matched_cov_date]
  combined[, c(".cov_value", ".matched_cov_date") := NULL]

  # z_days: number of days in each interval (should be constant)
  z_days <- unique(combined[, intend - intstart + 1])[1]

  # ---- Phase B: Vectorized case flags ----
  combined[, case := NA_character_]
  combined[, exp_change := (exposure != data.table::shift(exposure, n = 1L, type = "lag")),
           by = id_var]
  # First row per subject: no change
  combined[combined[, .I[1], by = id_var]$V1, exp_change := FALSE]
  combined[intnum == 0 & (exposure == 1 | exposure != exp_ref), exp_change := TRUE]

  if (exp_character) {
    combined[, .exposure_binary := fifelse(exposure == exp_ref, 0L, 1L)]
    combined[, part1 := !(cumsum(data.table::shift(.exposure_binary, n = 1L, fill = 0L,
                                                     type = "lag")) > 0),
             by = id_var]
    combined[, .exposure_binary := NULL]
  } else {
    combined[, part1 := !(cumsum(data.table::shift(exposure, n = 1L, fill = 0L,
                                                     type = "lag")) > 0),
             by = id_var]
  }

  # ---- Phase C: Per-subject case assignment via split/lapply ----
  # Pre-convert exposure dates to Date objects ONCE (avoids lubridate::ymd per subject)
  exp_dt_prepped <- data.table::copy(exp_data)
  exp_dt_prepped[, .exp_start := lubridate::ymd(get(start_date_var))]
  exp_dt_prepped[, .exp_end := lubridate::ymd(get(end_date_var))]

  # Pre-compute exposure data per subject for fast lookup
  exp_by_id <- split(exp_dt_prepped, by = id_var)

  # Pre-compute covariate dates/values per subject (post-index only)
  cov_by_id <- split(cov_dt, by = id_var)

  # Pre-extract exposure level column as a vector for fast subsetting
  exp_a_levels <- if (nrow(exp_dt_prepped) > 0) exp_dt_prepped[[A_level]] else character(0)

  split_data <- split(combined, by = id_var)

  result_list <- data.table::rbindlist(lapply(split_data, function(cd) {
    this_id <- cd[[id_var]][1]
    index_date_this_id <- cd$.index_date[1]
    eof_date_this_id <- cd$.eof_date[1]
    index_cov_this_id <- cd$.index_cov[1]

    # Extract key vectors for fast positional access (avoid [.data.table overhead)
    v_intnum <- cd$intnum
    v_intstart <- cd$intstart
    v_intend <- cd$intend
    v_exposure <- cd$exposure
    v_outcome <- cd$outcome
    v_censor <- cd$censor
    v_case <- cd$case
    v_exp_change <- cd$exp_change
    v_part1 <- cd$part1
    v_cov_val <- cd[[cov_name]]
    v_cov_dt <- cd[[cov_date]]

    n <- length(v_intnum)

    # Helper: intnum -> row index (intnum is 0-indexed, contiguous)
    # Since intnum may not start at 0 for all subjects, use match
    # But typically intnum = 0, 1, 2, ... so row = intnum + 1
    idx <- function(intnum_val) which(v_intnum == intnum_val)

    # Subject's exposure data (dates already converted)
    exp_this <- exp_by_id[[as.character(this_id)]]
    if (!is.null(exp_this)) {
      exp_start_this_id <- exp_this$.exp_start
      exp_end_this_id <- exp_this$.exp_end
      exp_levels_this <- exp_this[[A_level]]
    } else {
      exp_start_this_id <- as.Date(character(0))
      exp_end_this_id <- as.Date(character(0))
      exp_levels_this <- character(0)
    }

    # Subject's covariate data (already filtered to post-index)
    cov_this <- cov_by_id[[as.character(this_id)]]
    if (!is.null(cov_this) && nrow(cov_this) > 0) {
      cov_dates_this_id <- cov_this$.cov_date
      cov_values_this_id <- cov_this$.cov_value
    } else {
      cov_dates_this_id <- as.Date(character(0))
      cov_values_this_id <- vector(mode = typeof(index_cov_this_id), length = 0)
    }

    # ======== CASES 1/2: t=0, first and not last interval ========
    if (n > 1) {
      i0 <- idx(0)
      if (length(i0) == 1 && v_part1[i0]) {
        if (v_exposure[i0] == 0 || v_exposure[i0] == exp_ref) {
          # Case 1: unexposed at t=0
          v_case[i0] <- "1"
          v_cov_val[i0] <- index_cov_this_id
          v_cov_dt[i0] <- index_date_this_id
        } else {
          # Case 2: exposed at t=0
          v_case[i0] <- "2"
          if (exp_character) {
            At.0 <- v_exposure[i0]
            exp_start_j <- exp_start_this_id[exp_levels_this %in% At.0][1]
          } else {
            exp_start_j <- exp_start_this_id[1]
          }
          int_start_d <- index_date_this_id + 1L
          int_end_d <- exp_start_j - cov_acute
          interval_valid <- length(int_start_d) > 0 && length(int_end_d) > 0 &&
            !is.na(int_start_d) && !is.na(int_end_d) && int_start_d <= int_end_d
          if (interval_valid && any(cov_dates_this_id >= int_start_d &
                                     cov_dates_this_id <= int_end_d)) {
            in_range <- cov_dates_this_id >= int_start_d & cov_dates_this_id <= int_end_d
            v_cov_dt[i0] <- tail(cov_dates_this_id[in_range], 1)
            v_cov_val[i0] <- tail(cov_values_this_id[in_range], 1)
          } else {
            v_cov_dt[i0] <- index_date_this_id
            v_cov_val[i0] <- index_cov_this_id
          }
        }
      }
    }

    # ======== CASES 3/4: middle intervals, part1 ========
    has_outcome <- any(v_outcome == 1)
    has_censor <- any(v_censor == 1, na.rm = TRUE)
    if (has_outcome) {
      exclude <- c(0, n - 2, n - 1)  # intnum values (.N-2, .N-1 in 0-indexed = n-2, n-1)
      mid <- v_part1 & !(v_intnum %in% exclude)
      v_case[mid & !v_exp_change] <- "3"
      v_case[mid & v_exp_change] <- "4"
    } else if (has_censor) {
      exclude <- c(0, n - 1)
      mid <- v_part1 & !(v_intnum %in% exclude)
      v_case[mid & !v_exp_change] <- "3"
      v_case[mid & v_exp_change] <- "4"
    }

    # Case 4: search [L(t-1).date+1 or intstart, exp_start_in_interval - acute]
    if (any(v_case %in% "4")) {
      i4 <- which(v_case == "4")
      t.int <- v_intnum[i4]
      im1 <- idx(t.int - 1)
      int_start_d <- if (is.na(v_cov_val[im1])) v_intstart[im1] else v_cov_dt[im1] + 1L

      if (exp_character) {
        At <- v_exposure[i4]
        bin_start <- v_intstart[i4]
        bin_end <- v_intend[i4]
        sel <- exp_levels_this %in% At
        exp_starts_j <- exp_start_this_id[sel]
        exp_ends_j <- exp_end_this_id[sel]
      } else {
        bin_start <- v_intstart[i4]
        bin_end <- v_intend[i4]
        exp_starts_j <- exp_start_this_id
        exp_ends_j <- exp_end_this_id
      }

      d1_case4 <- .earliest_overlap_start(bin_start, bin_end, exp_starts_j, exp_ends_j)
      int_end_d <- d1_case4 - cov_acute

      res <- .search_cov_vec(int_start_d, int_end_d, cov_dates_this_id, cov_values_this_id)
      v_cov_val[i4] <- res$val
      v_cov_dt[i4] <- res$dt
    }

    # ======== CASES 5/7: last interval with censoring, part1 ========
    last_idx <- n
    v_case[v_intnum == (n - 1) & !v_exp_change & v_part1 & v_censor == 1] <- "5"
    v_case[v_intnum == (n - 1) & v_exp_change & v_part1 & v_censor == 1] <- "7"

    if (any(v_case %in% c("5", "7"))) {
      i57 <- which(v_case %in% c("5", "7"))[1]
      t.int <- v_intnum[i57]
      if (t.int == 0) {
        int_start_d <- index_date_this_id
      } else {
        im1 <- idx(t.int - 1)
        int_start_d <- if (is.na(v_cov_val[im1]) && is.na(v_cov_dt[im1])) {
          v_intstart[im1]
        } else {
          v_cov_dt[im1] + 1L
        }
      }
      int_end_d <- eof_date_this_id[1] - cov_acute

      cov_dates_t0 <- c(index_date_this_id, cov_dates_this_id)
      cov_values_t0 <- c(index_cov_this_id, cov_values_this_id)
      not_dup <- !duplicated(cov_dates_t0)
      cov_dates_t0 <- cov_dates_t0[not_dup]
      cov_values_t0 <- cov_values_t0[not_dup]

      res <- .search_cov_vec(int_start_d, int_end_d, cov_dates_t0, cov_values_t0)
      v_cov_val[i57] <- res$val
      v_cov_dt[i57] <- res$dt
    }

    # ======== CASES 6/8: penultimate interval with outcome, part1 ========
    if (has_outcome) {
      v_case[v_intnum == (n - 2) & !v_exp_change & v_part1] <- "6"
      v_case[v_intnum == (n - 2) & v_exp_change & v_part1] <- "8"
    }

    # Case 8 with first_exp_rule == 1
    if (any(v_case %in% "8") & first_exp_rule == 1) {
      i8 <- which(v_case == "8")[1]
      t.int <- v_intnum[i8]

      int_start_d <- if (t.int == 0) {
        index_date_this_id
      } else {
        im1 <- idx(t.int - 1)
        if (is.na(v_cov_val[im1])) v_intstart[im1] else v_cov_dt[im1] + 1L
      }

      if (exp_character) {
        At <- v_exposure[i8]
        exp_start_j <- exp_start_this_id[exp_levels_this %in% At][1]
        int_end_d <- exp_start_j - cov_acute
      } else {
        int_end_d <- exp_start_this_id[1] - cov_acute
      }

      cov_dates_t0 <- c(index_date_this_id, cov_dates_this_id)
      cov_values_t0 <- c(index_cov_this_id, cov_values_this_id)
      not_dup <- !duplicated(cov_dates_t0)
      cov_dates_t0 <- cov_dates_t0[not_dup]
      cov_values_t0 <- cov_values_t0[not_dup]

      if (t.int == 0) {
        res <- .search_cov_vec(int_start_d, int_end_d, cov_dates_t0, cov_values_t0)
        if (is.na(res$val) && !is.na(int_start_d) && !is.na(int_end_d) && int_start_d > int_end_d) {
          v_cov_val[i8] <- index_cov_this_id
          v_cov_dt[i8] <- index_date_this_id
        } else {
          v_cov_val[i8] <- res$val
          v_cov_dt[i8] <- res$dt
        }
      } else {
        res <- .search_cov_vec(int_start_d, int_end_d, cov_dates_this_id, cov_values_this_id)
        v_cov_val[i8] <- res$val
        v_cov_dt[i8] <- res$dt
      }

      # L(t+1) assignment
      acute_exp_date <- exp_start_this_id[1] + !cov_acute
      lt_start <- if (is.na(v_cov_dt[i8])) acute_exp_date else v_cov_dt[i8] + 1L
      lt_end <- eof_date_this_id
      i8p1 <- idx(t.int + 1)
      if (length(i8p1) == 1) {
        res <- .search_cov_vec(lt_start, lt_end, cov_dates_this_id, cov_values_this_id)
        v_cov_val[i8p1] <- res$val
        v_cov_dt[i8p1] <- res$dt
      }
    }

    # Single interval special case
    if (n == 1 & !(v_case[1] %in% c("5", "7", "8"))) {
      all_dates <- c(index_date_this_id, cov_dates_this_id)
      all_vals <- c(index_cov_this_id, cov_values_this_id)
      before_eof <- all_dates < eof_date_this_id
      if (any(before_eof)) {
        best <- which.max(all_dates[before_eof])
        v_cov_val[1] <- all_vals[before_eof][best]
        v_cov_dt[1] <- all_dates[before_eof][best]
      }
    }

    # ======== PART 2: Cases 10-14 (after first exposure change) ========
    if (has_censor) {
      v_case[v_intnum != (n - 1) & !v_exp_change & !v_part1] <- "10"
      v_case[v_intnum != (n - 1) & v_exp_change & !v_part1] <- "11"
    } else {
      exclude2 <- c(n - 2, n - 1)
      v_case[!(v_intnum %in% exclude2) & !v_exp_change & !v_part1] <- "10"
      v_case[!(v_intnum %in% exclude2) & v_exp_change & !v_part1] <- "11"
    }

    if (z_days != 1) {
      case1011 <- which(v_case %in% c("10", "11"))
      if (length(case1011) > 0) {
        for (ii in case1011) {
          t.i <- v_intnum[ii]
          im1 <- idx(t.i - 1)
          if (length(im1) == 0) next

          if (v_case[ii] == "10") {
            int_start_d <- if (is.na(v_cov_val[im1]) && is.na(v_cov_dt[im1])) {
              v_intstart[im1]
            } else {
              v_cov_dt[im1] + 1L
            }
            int_end_d <- v_intend[im1]
            res <- .search_cov_vec(int_start_d, int_end_d, cov_dates_this_id, cov_values_this_id)
            v_cov_val[ii] <- res$val
            v_cov_dt[ii] <- res$dt
          } else {
            # Case 11 with z_days != 1
            int_start_d <- if (is.na(v_cov_val[im1]) && is.na(v_cov_dt[im1])) {
              v_intstart[im1]
            } else {
              v_cov_dt[im1] + 1L
            }

            bin_start <- v_intstart[ii]
            bin_end <- v_intend[ii]
            A.t <- v_exposure[ii]

            if (exp_character) {
              sel <- exp_levels_this == A.t
              exp_starts_j <- exp_start_this_id[sel]
              exp_ends_j <- exp_end_this_id[sel]
            } else {
              exp_starts_j <- exp_start_this_id
              exp_ends_j <- exp_end_this_id
            }

            # Build non-exposure intervals
            exp0_starts <- c(index_date_this_id, exp_end_this_id + 1L)
            exp0_ends <- c(exp_start_this_id[1] - 1L,
                           data.table::shift(exp_start_this_id - 1L, n = 1L,
                                             fill = eof_date_this_id, type = "lead"))
            valid_exp0 <- exp0_ends >= exp0_starts
            exp0_starts <- exp0_starts[valid_exp0]
            exp0_ends <- exp0_ends[valid_exp0]

            d1_case11 <- .earliest_overlap_start(bin_start, bin_end, exp_starts_j, exp_ends_j)
            d0_case11 <- .earliest_overlap_start(bin_start, bin_end, exp0_starts, exp0_ends)

            d_case11 <- if (v_exposure[ii] == 0 || v_exposure[ii] == exp_ref) d0_case11 else d1_case11

            if (is.infinite(d_case11) || is.na(as.character(d_case11))) {
              d_case11 <- bin_start
            }

            int_end_d <- d_case11 - cov_acute
            res <- .search_cov_vec(int_start_d, int_end_d, cov_dates_this_id, cov_values_this_id)
            v_cov_val[ii] <- res$val
            v_cov_dt[ii] <- res$dt
          }
        }
      }
    } else {
      # z_days == 1 path
      case10 <- which(v_case == "10")
      if (length(case10) > 0) {
        t.int <- v_intnum[case10[1]]
        if (exp_character) {
          sel10 <- case10[v_exposure[case10] != exp_ref]
          if (length(sel10) > 0) t.int <- v_intnum[sel10[1]]
        }
        if (!is.na(t.int)) {
          im1 <- idx(t.int - 1)
          int_start_d <- if (is.na(v_cov_val[im1]) && is.na(v_cov_dt[im1])) {
            v_intstart[im1]
          } else {
            v_cov_dt[im1] + 1L
          }
          int_end_d <- v_intend[im1]
          ii <- idx(t.int)
          res <- .search_cov_vec(int_start_d, int_end_d, cov_dates_this_id, cov_values_this_id)
          v_cov_val[ii] <- res$val
          v_cov_dt[ii] <- res$dt
        }
      }

      # Case 11 with z_days == 1
      case11 <- which(v_case == "11")
      if (length(case11) > 0) {
        bin_starts <- v_intstart[case11]
        bin_ends <- v_intend[case11]

        exp_starts_all <- exp_start_this_id
        exp_ends_all <- exp_end_this_id

        # Build non-exposure intervals
        exp0_starts <- c(index_date_this_id, exp_end_this_id + 1L)
        exp0_ends <- c(exp_start_this_id[1] - 1L,
                       data.table::shift(exp_start_this_id - 1L, n = 1L,
                                         fill = eof_date_this_id, type = "lead"))
        valid_exp0 <- exp0_ends >= exp0_starts
        exp0_starts <- exp0_starts[valid_exp0]
        exp0_ends <- exp0_ends[valid_exp0]

        d1_case11 <- vapply(seq_along(bin_starts), function(j) {
          .earliest_overlap_start_scalar(bin_starts[j], bin_ends[j],
                                          exp_starts_all, exp_ends_all)
        }, as.Date(NA))

        d0_case11 <- vapply(seq_along(bin_starts), function(j) {
          .earliest_overlap_start_scalar(bin_starts[j], bin_ends[j],
                                          exp0_starts, exp0_ends)
        }, as.Date(NA))

        d_case11 <- ifelse(
          v_exposure[case11] == 0 | v_exposure[case11] == exp_ref,
          d0_case11, d1_case11
        )
        d_case11 <- as.Date(d_case11, origin = "1970-01-01")

        case11_intnums <- v_intnum[case11]
        ints_starts <- bin_starts
        ints_ends <- d_case11 - cov_acute

        for (j in seq_along(case11)) {
          s <- ints_starts[j]
          e <- ints_ends[j]
          res <- .search_cov_vec(s, e, cov_dates_this_id, cov_values_this_id)
          v_cov_val[case11[j]] <- res$val
          v_cov_dt[case11[j]] <- res$dt
        }

        # Handle duplicated dates
        non_na <- !is.na(v_cov_dt)
        dups <- non_na & duplicated(v_cov_dt)
        v_cov_dt[dups] <- as.Date(NA)
        v_cov_val[dups] <- NA
      }
    }

    # ======== CASE 12: last interval with censoring, part2 ========
    v_case[v_intnum == (n - 1) & !v_part1 & v_censor == 1] <- "12"

    if (any(v_case %in% "12")) {
      # Use second-to-last row for the L(t-1) lookup
      im1 <- n - 1  # row index (n is last row, n-1 is second-to-last)
      if (im1 >= 1) {
        int_start_d <- if (is.na(v_cov_dt[im1])) v_intstart[im1] else v_cov_dt[im1] + 1L
      } else {
        int_start_d <- v_intstart[n]
      }
      int_end_d <- eof_date_this_id - cov_acute
      i12 <- which(v_case == "12")[1]
      res <- .search_cov_vec(int_start_d, int_end_d, cov_dates_this_id, cov_values_this_id)
      v_cov_val[i12] <- res$val
      v_cov_dt[i12] <- res$dt
    }

    # ======== CASES 13/14: penultimate interval with outcome, part2 ========
    if (v_outcome[n] == 1) {
      v_case[v_intnum == (n - 2) & !v_exp_change & !v_part1] <- "13"
      v_case[v_intnum == (n - 2) & v_exp_change & !v_part1] <- "14"
    }

    # Case 13 or (case 6 with first_exp_rule==0)
    if (any(v_case %in% "13") | (any(v_case %in% "6") & first_exp_rule == 0)) {
      i_case <- which(v_case %in% c("13", "6"))[1]
      t.int <- v_intnum[i_case]
      if (t.int != 0) {
        Zpen_start <- eof_date_this_id - (2 * z_days) + 1
        Zpen_end <- eof_date_this_id - z_days

        im1 <- idx(t.int - 1)
        if (length(im1) > 0 && !is.na(v_cov_val[im1])) {
          Zpen_start <- v_cov_dt[im1] + 1
        }
        # Bug fix: check for non-missing earlier measurements in Zpenultimate
        prev_mask <- !is.na(v_cov_dt) & v_intnum < t.int
        prev_dates <- v_cov_dt[prev_mask]
        prev_in_zpen <- prev_dates[prev_dates >= Zpen_start & prev_dates <= Zpen_end]
        if (length(prev_in_zpen) > 0) {
          Zpen_start <- tail(prev_in_zpen, 1) + 1
        }

        res <- .search_cov_vec(Zpen_start, Zpen_end, cov_dates_this_id, cov_values_this_id)
        v_cov_val[i_case] <- res$val
        v_cov_dt[i_case] <- res$dt

        # L(t+1) assignment
        ip1 <- idx(t.int + 1)
        if (length(ip1) == 1) {
          if (is.na(v_cov_dt[i_case])) {
            last_known_dt <- prev_dates[!is.na(prev_dates)]
            if (length(last_known_dt) > 0) {
              lt_start <- max(Zpen_end + 1, tail(last_known_dt, 1) + 1)
            } else {
              lt_start <- Zpen_end + 1
            }
          } else {
            lt_start <- v_cov_dt[i_case] + 1
          }
          lt_end <- eof_date_this_id
          res <- .search_cov_vec(lt_start, lt_end, cov_dates_this_id, cov_values_this_id)
          v_cov_val[ip1] <- res$val
          v_cov_dt[ip1] <- res$dt
        }
      }
    }

    # Case 14 or (case 8 with first_exp_rule==0)
    if (any(v_case %in% "14") | (any(v_case %in% "8") & first_exp_rule == 0)) {
      i_case <- which(v_case %in% c("14", "8"))[1]
      t.int <- v_intnum[i_case]

      if (any(v_case %in% "8") & t.int == 0) {
        # Special case: case 8 at t=0 with first_exp_rule==0
        int_start_d <- index_date_this_id
        if (exp_character) {
          At <- v_exposure[i_case]
          exp_start_j <- exp_start_this_id[exp_levels_this %in% At][1]
          int_end_d <- exp_start_j - cov_acute
        } else {
          int_end_d <- exp_start_this_id[1] - cov_acute
        }

        cov_dates_t0 <- c(index_date_this_id, cov_dates_this_id)
        cov_values_t0 <- c(index_cov_this_id, cov_values_this_id)
        not_dup <- !duplicated(cov_dates_t0)
        cov_dates_t0 <- cov_dates_t0[not_dup]
        cov_values_t0 <- cov_values_t0[not_dup]

        interval_valid <- !is.na(int_start_d) && !is.na(int_end_d) && int_start_d <= int_end_d
        if (interval_valid) {
          res <- .search_cov_vec(int_start_d, int_end_d, cov_dates_t0, cov_values_t0)
          v_cov_val[i_case] <- res$val
          v_cov_dt[i_case] <- res$dt
        } else if (!is.na(int_start_d) && !is.na(int_end_d) && int_start_d > int_end_d) {
          v_cov_val[i_case] <- index_cov_this_id
          v_cov_dt[i_case] <- index_date_this_id
        } else {
          v_cov_val[1] <- NA
          v_cov_dt[1] <- as.Date(NA)
        }

        # L(t+1) assignment
        ip1 <- idx(t.int + 1)
        if (length(ip1) == 1) {
          lt_start <- if (is.na(v_cov_dt[i_case])) {
            exp_start_this_id[1] + !cov_acute
          } else {
            v_cov_dt[i_case] + 1L
          }
          lt_end <- eof_date_this_id
          res <- .search_cov_vec(lt_start, lt_end, cov_dates_this_id, cov_values_this_id)
          v_cov_val[ip1] <- res$val
          v_cov_dt[ip1] <- res$dt
        }
      } else {
        # Case 14 (or case 8 with t.int != 0)
        # Build non-exposure intervals
        exp0_starts <- c(index_date_this_id, exp_end_this_id + 1L)
        exp0_ends <- c(exp_start_this_id[1] - 1L,
                       data.table::shift(exp_start_this_id - 1L, n = 1L,
                                         fill = eof_date_this_id, type = "lead"))
        valid_exp0 <- exp0_ends >= exp0_starts
        exp0_starts <- exp0_starts[valid_exp0]
        exp0_ends <- exp0_ends[valid_exp0]

        Zlast_start <- eof_date_this_id - z_days + 1
        Zlast_end <- eof_date_this_id
        Zpen_start <- eof_date_this_id - (2 * z_days) + 1
        Zpen_end <- eof_date_this_id - z_days

        # Find d.day
        d.day <- as.Date(NA)
        if (v_exposure[i_case] == 0 || v_exposure[i_case] == exp_ref) {
          in_exp0 <- Zlast_start >= exp0_starts & Zlast_start <= exp0_ends
          if (any(in_exp0)) {
            d.day <- Zlast_start
          } else {
            starts_in_zlast <- exp0_starts[exp0_starts >= Zlast_start & exp0_starts <= Zlast_end]
            if (length(starts_in_zlast) > 0) d.day <- starts_in_zlast[1]
          }
        }
        if (v_exposure[i_case] == 1 || v_exposure[i_case] != exp_ref) {
          if (exp_character) {
            A.t <- v_exposure[i_case]
            sel <- exp_levels_this == A.t
            exp_j_starts <- exp_start_this_id[sel]
            exp_j_ends <- exp_end_this_id[sel]
          } else {
            exp_j_starts <- exp_start_this_id
            exp_j_ends <- exp_end_this_id
          }
          in_exp <- Zlast_start >= exp_j_starts & Zlast_start <= exp_j_ends
          if (any(in_exp)) {
            d.day <- Zlast_start
          } else {
            starts_in_zlast <- exp_j_starts[exp_j_starts >= Zlast_start & exp_j_starts <= Zlast_end]
            if (length(starts_in_zlast) > 0) d.day <- starts_in_zlast[1]
          }
        }

        d.day <- d.day - cov_acute
        if (is.na(d.day)) d.day <- Zpen_end

        Zpen_to_d_start <- Zpen_start
        Zpen_to_d_end <- d.day

        im1 <- idx(t.int - 1)
        if (length(im1) > 0 && !is.na(v_cov_val[im1])) {
          Zpen_to_d_start <- v_cov_dt[im1] + 1
        }
        prev_mask <- !is.na(v_cov_dt) & v_intnum < t.int
        prev_dates <- v_cov_dt[prev_mask]
        prev_in_zpen <- prev_dates[prev_dates >= Zpen_start & prev_dates <= Zpen_end]
        if (length(prev_in_zpen) > 0) {
          Zpen_to_d_start <- tail(prev_in_zpen, 1) + 1
        }

        res <- .search_cov_vec(Zpen_to_d_start, Zpen_to_d_end, cov_dates_this_id, cov_values_this_id)
        v_cov_val[i_case] <- res$val
        v_cov_dt[i_case] <- res$dt

        # L(t+1) assignment
        ip1 <- idx(t.int + 1)
        if (length(ip1) == 1) {
          if (is.na(v_cov_dt[i_case])) {
            last_known_dt <- prev_dates[!is.na(prev_dates)]
            if (length(last_known_dt) > 0) {
              lt_start <- max(Zpen_to_d_end + 1, tail(last_known_dt, 1) + 1)
            } else {
              lt_start <- Zpen_to_d_end + 1
            }
          } else {
            lt_start <- v_cov_dt[i_case] + 1
          }
          lt_end <- eof_date_this_id
          res <- .search_cov_vec(lt_start, lt_end, cov_dates_this_id, cov_values_this_id)
          v_cov_val[ip1] <- res$val
          v_cov_dt[ip1] <- res$dt
        }
      }
    }

    # Write vectors back to data.table
    data.table::set(cd, j = "case", value = suppressWarnings(as.numeric(v_case)))
    data.table::set(cd, j = cov_name, value = v_cov_val)
    data.table::set(cd, j = cov_date, value = v_cov_dt)
    data.table::set(cd, j = "exp_change", value = NULL)
    data.table::set(cd, j = "part1", value = NULL)

    cd
  }), use.names = TRUE, fill = TRUE)

  # Remove temp columns added during merge
  result_list[, c(".index_date", ".eof_date", ".index_cov") := NULL]

  result_list
}


# ---- Helper functions ----

# Find earliest start of any [starts, ends] interval that overlaps with [bin_start, bin_end]
# Returns the earliest pmax(bin_start, starts[i]) for overlapping intervals
.earliest_overlap_start <- function(bin_start, bin_end, starts, ends) {
  if (length(starts) == 0) return(as.Date(NA))
  # Check which intervals overlap
  overlaps <- starts <= bin_end & ends >= bin_start
  if (!any(overlaps)) return(as.Date(NA))
  # The intersection start is pmax(bin_start, starts[i])
  int_starts <- pmax(bin_start, starts[overlaps])
  as.Date(min(int_starts, na.rm = TRUE), origin = "1970-01-01")
}

# Scalar version for vapply
.earliest_overlap_start_scalar <- function(bin_start, bin_end, starts, ends) {
  if (length(starts) == 0) return(as.Date(NA))
  overlaps <- starts <= bin_end & ends >= bin_start
  if (!any(overlaps)) return(as.Date(NA))
  int_starts <- pmax(bin_start, starts[overlaps])
  as.Date(min(int_starts, na.rm = TRUE), origin = "1970-01-01")
}

# Search for most recent covariate in date range [start, end]
# Returns list(val=, dt=) with NA if none found (vector-optimized, no data.table)
.search_cov_vec <- function(int_start_d, int_end_d, cov_dates, cov_values) {
  if (length(int_start_d) == 0 || length(int_end_d) == 0 ||
      is.na(int_start_d) || is.na(int_end_d) || int_start_d > int_end_d ||
      length(cov_dates) == 0) {
    return(list(val = NA, dt = as.Date(NA)))
  }
  in_range <- cov_dates >= int_start_d & cov_dates <= int_end_d
  if (!any(in_range)) return(list(val = NA, dt = as.Date(NA)))
  idx <- which(in_range)
  best <- idx[which.max(cov_dates[idx])]
  list(val = cov_values[best], dt = cov_dates[best])
}

# Search for most recent covariate in date range [start, end]
# Returns 1-row data.table with cov_name and cov_date columns, or 0-row if none found
.search_cov_in_range <- function(int_start_d, int_end_d,
                                  cov_dates, cov_values,
                                  cov_name_col, cov_date_col) {
  if (length(int_start_d) == 0 || length(int_end_d) == 0 ||
      is.na(int_start_d) || is.na(int_end_d) || int_start_d > int_end_d) {
    return(data.table::data.table())
  }
  in_range <- cov_dates >= int_start_d & cov_dates <= int_end_d
  if (!any(in_range)) return(data.table::data.table())
  idx <- which(in_range)
  best <- idx[which.max(cov_dates[idx])]
  result <- data.table::data.table(V1 = cov_values[best], V2 = cov_dates[best])
  data.table::setnames(result, c(cov_name_col, cov_date_col))
  result
}
