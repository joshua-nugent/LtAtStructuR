# helper-case-subjects.R
# Hand-crafted subjects for case-by-case verification of assignL().
#
# All subjects use:
#   - Binary exposure
#   - time_unit = 30
#   - Different IndexDates (spread across 2010-2013), ages, sexes, and races
#   - 1 continuous sporadic covariate: A1c
#   - 1 categorical sporadic covariate: eGFR (minimal data, required by schema)
#
# Interval count formula: ceiling((EOFDate - IndexDate + 1) / time_unit)
# For AMI (outcome), an extra outcome row is added.
# To get N intervals: EOFDate = IndexDate + N*30 - 1
#
# All exposure and covariate dates are expressed as day-offsets from each
# subject's own IndexDate, so subjects can have different index dates.
#
# Returns a list with components: cohort, exposure, a1c, egfr
# Subjects S01-S09 are designed to cover every assignL case (1-14).

make_case_subjects <- function() {
  library(data.table)

  ids <- paste0("S", sprintf("%02d", 1:9))

  # Per-subject index dates (spread across 2010-2013)
  index_dates <- as.Date(c(
    "2010-03-15",  # S01
    "2011-07-22",  # S02
    "2010-11-01",  # S03
    "2012-01-10",  # S04
    "2013-05-03",  # S05
    "2011-02-14",  # S06
    "2012-08-30",  # S07
    "2010-06-17",  # S08
    "2013-09-25"   # S09
  ))

  # Follow-up durations (days past index date to EOF)
  eof_offsets <- c(
    119,   # S01: censor -> 4 intervals (intnum 0-3)
    119,   # S02: censor -> 4 intervals
     89,   # S03: AMI    -> 3 + 1 outcome = 4 rows (intnum 0-3)
    119,   # S04: censor -> 4 intervals
    119,   # S05: censor -> 4 intervals
     59,   # S06: censor -> 2 intervals (intnum 0-1) [for case 7]
     89,   # S07: AMI    -> 3 + 1 outcome = 4 rows  [for case 8]
    149,   # S08: AMI    -> 5 + 1 outcome = 6 rows (intnum 0-5)
    149    # S09: AMI    -> 5 + 1 outcome = 6 rows
  )

  # ====================================================================
  # COHORT DATA
  # ====================================================================
  cohort <- data.table(
    ID        = ids,
    IndexDate = index_dates,
    EOFDate   = index_dates + eof_offsets,
    ageEntry  = c(62, 45, 73, 51, 38, 67, 55, 41, 58),
    sex       = c("Male", "Female", "Male", "Female", "Male",
                  "Female", "Male", "Female", "Male"),
    race      = c("White", "Black", "Hispanic", "Asian", "White",
                  "Black", "Hispanic", "Asian", "White"),
    A1c       = c(6.5, 7.0, 8.0, 6.0, 7.5, 6.2, 7.8, 5.5, 6.8),
    eGFR      = c("stage2", "stage3a", "stage1", "stage2", "stage4",
                  "stage3b", "stage2", "stage1", "stage3a"),
    EOFtype   = c("Censor", "Censor", "AMI", "Censor", "Censor",
                  "Censor", "AMI", "AMI", "AMI")
  )

  # Helper: look up index date by ID
  idx_of <- function(id) index_dates[match(id, ids)]

  # ====================================================================
  # EXPOSURE DATA
  # ====================================================================
  # Binary exposure episodes: rows only for exposed periods.
  # Day offsets are relative to each subject's own IndexDate.
  #
  # Key: for case 7 (S06), exposure must start in the LAST interval while
  # still in part1 (no prior exposure). For case 8 (S07), exposure must
  # start in the PENULTIMATE interval with outcome, still in part1.
  exposure <- data.table(
    ID     = c("S02",       # exposed day 1-25 (within t=0 only)
               "S04",       # exposed day 35-65 (starts in t=1)
               "S05", "S05",# two episodes: day 1-25 and day 65-90
               "S06",       # exposed day 35-55 (starts in t=1=last for 2-interval subj)
               "S07",       # exposed day 61-85 (starts in t=2=penultimate for AMI)
               "S08",       # exposed day 35-65
               "S09", "S09" # two episodes: day 35-65 and day 125-145
    ),
    startA = c(idx_of("S02") + 1,   idx_of("S04") + 35,
               idx_of("S05") + 1,   idx_of("S05") + 65,
               idx_of("S06") + 35,  idx_of("S07") + 61,
               idx_of("S08") + 35,
               idx_of("S09") + 35,  idx_of("S09") + 125),
    endA   = c(idx_of("S02") + 25,  idx_of("S04") + 65,
               idx_of("S05") + 25,  idx_of("S05") + 90,
               idx_of("S06") + 55,  idx_of("S07") + 85,
               idx_of("S08") + 65,
               idx_of("S09") + 65,  idx_of("S09") + 145)
  )

  # ====================================================================
  # A1c COVARIATE DATA (sporadic, continuous)
  # ====================================================================
  # Measurements placed strategically after IndexDate to verify each case.
  # NOTE: only measurements AFTER IndexDate (strictly >) are used as
  # time-dependent covariate values. Baseline is from cohort.
  #
  # The shift rule: measurement at date d in interval t gets assigned to
  # L(t+1) for subjects with >1 interval.
  a1c <- data.table(
    ID = c(
      # S01: Never exposed, censor, 4 intervals (intnum 0-3)
      # Expected assignL cases: 1, 3, 3, 5
      "S01", "S01", "S01",

      # S02: Exposed day 1-25 (within t=0), censor, 4 intervals
      # part1 only at intnum 0 (exposed right away)
      # Expected assignL cases: 2, 11, 10, 12
      "S02", "S02",

      # S03: Never exposed, AMI, 4 rows (intnum 0-3; row 3 is outcome)
      # Expected assignL cases: 1, 3, 6, NA(outcome)
      "S03", "S03",

      # S04: Exposed day 35-65 (starts in t=1), censor, 4 intervals
      # part1 at intnum 0,1; part2 at intnum 2,3
      # Expected assignL cases: 1, 4, 11, 12
      "S04", "S04", "S04",

      # S05: Two exp episodes (1-25, 65-90), censor, 4 intervals
      # part1 only at intnum 0
      # Expected assignL cases: 2, 11, 11, 12
      "S05", "S05", "S05",

      # S06: Exposed day 35-55 (starts in last interval), censor, 2 intervals
      # part1 at both intervals (no prior exposure)
      # Expected assignL cases: 1, 7
      "S06",

      # S07: Exposed day 61-85 (starts in penultimate), AMI, 4 rows
      # part1 at intnum 0,1,2; part2 at intnum 3
      # Expected assignL cases: 1, 3, 8, NA(outcome)
      "S07", "S07",

      # S08: Exposed day 35-65, AMI, 6 rows (intnum 0-5)
      # part1 at intnum 0,1; part2 at intnum 2,3,4,5
      # Expected assignL cases: 1, 4, 11, 10, 13, NA(outcome)
      "S08", "S08", "S08",

      # S09: Two exp episodes (35-65, 125-145), AMI, 6 rows
      # Expected assignL cases: 1, 4, 11, 10, 14, NA(outcome)
      "S09", "S09", "S09"
    ),
    A1cDate = c(
      # S01: measurements at day 10, day 40, day 70
      idx_of("S01") + c(10, 40, 70),

      # S02: measurement at day 2 (in intnum 0) and day 45 (in intnum 1)
      idx_of("S02") + c(2, 45),

      # S03: measurements at day 10, day 50
      idx_of("S03") + c(10, 50),

      # S04: measurements at day 10, day 33, day 80
      idx_of("S04") + c(10, 33, 80),

      # S05: measurements at day 5, day 40, day 62
      idx_of("S05") + c(5, 40, 62),

      # S06: measurement at day 20 (in intnum 0)
      idx_of("S06") + 20,

      # S07: measurements at day 10 and day 50
      idx_of("S07") + c(10, 50),

      # S08: measurements at day 10, day 80, day 130
      idx_of("S08") + c(10, 80, 130),

      # S09: measurements at day 10, day 80, day 123
      idx_of("S09") + c(10, 80, 123)
    ),
    A1c = c(
      # S01
      6.8, 7.1, 7.5,
      # S02
      7.2, 7.6,
      # S03
      8.5, 9.0,
      # S04
      6.3, 6.7, 7.0,
      # S05
      7.8, 8.0, 8.3,
      # S06
      6.5,
      # S07
      8.1, 8.4,
      # S08
      5.9, 6.2, 6.5,
      # S09
      7.1, 7.4, 7.7
    )
  )

  # ====================================================================
  # eGFR COVARIATE DATA (sporadic, categorical)
  # ====================================================================
  # Minimal data: one measurement per subject shortly after index.
  # Required by the schema but not the focus of verification.
  egfr <- data.table(
    ID       = ids,
    eGFRDate = index_dates + 10,  # day 10 for each subject
    eGFR     = c("stage3a", "stage2", "stage3b", "stage1", "stage3a",
                 "stage2", "stage4", "stage2", "stage3a")
  )

  list(
    cohort   = cohort,
    exposure = exposure,
    a1c      = a1c,
    egfr     = egfr
  )
}
