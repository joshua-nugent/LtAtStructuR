utils::globalVariables(c("private"))

`%+%` <- function(a, b) paste0(a, b)

readOnly <- function(privateFieldName) {
  err_name <- gsub(".", "", privateFieldName, fixed = TRUE)
  fn <- function(value) {}
  body(fn) <- bquote({
    if (missing(value)) {
      private[[.(privateFieldName)]]
    } else {
      stop(.(err_name), " is read only", call. = FALSE)
    }
  })
  fn
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Data Storage Class for cohort dataset
#'
#'
#' Class that defines the standard format for the cohort dataset, i.e. the table
#' that specifies for each subject in the cohort:
#' 1) a unique subject identifier,
#' 2) the date of study entry,
#' 3) the date of end of follow-up,
#' 4) the reason for end of follow-up (failure or right-censoring), and
#' 5) baseline measurements of time-dependent or time-independent covariates.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom data.table is.data.table copy setkeyv
#' @importFrom lubridate is.Date
#' @importFrom assertthat assert_that is.string
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{cohortData} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Fields:
#' \describe{
#'     \item{\code{data}:}{\code{data.table} containing
#'           the input cohort dataset to be wrapped in and processed.
#'           The table must contain a single row for each subject in the cohort.
#'           Cannot have columns named 'IDvar', 'index_date', 'EOF_date',
#'           'EOF_type', or 'L0'.
#'     }
#'     \item{\code{IDvar}:}{\code{character} providing the name of the column of
#'           \code{data} that contains the unique subject identifier.
#'     }
#'     \item{\code{index_date}:}{\code{character} providing the name of the
#'           column of \code{data} that contains the date of study entry.
#'     }
#'     \item{\code{EOF_date}:}{\code{character} providing the name of the column
#'           of \code{data} that contains the date of end of follow-up. All observations
#'           with the end of follow-up date equal to the study entry date will be ignored
#'           (i.e., excluded from the cohort).
#'     }
#'     \item{\code{EOF_type}:}{\code{character} providing the name of the column
#'           of \code{data} corresponding to the reason for end of follow-up.
#'     }
#'     \item{\code{Y_name}:}{\code{character} or \code{integer}  providing the
#'           unique value in column \code{EOF_type} that encodes the end of
#'           follow-up due to failure (i.e., occurrence of the outcome event of
#'           interest).
#'     }
#'     \item{\code{L0}:}{vector of \code{character} providing the names of the
#'           columns of \code{data} that contain baseline covariate
#'           measurements. Covariate values must be encoded by a character or
#'           numeric vector (e.g., factors are not allowed).
#'     }
#'     \item{\code{L0_timeIndep}:}{named list specifying, for each time-independent
#'           covariates in \code{L0}, a sublist with only the following three named
#'           elements:
#'           \enumerate{
#'           \item \code{categorical}: \code{logical} indicating
#'           whether the time-independent covariate is
#'           continuous ('FALSE') or categorical ('TRUE'). Cannot be missing.
#'           \item \code{impute}: \code{character} specifying the imputation
#'            method for
#'            missing measurements of the time-independent covariate. Possible values are
#'            'default', 'mean', 'mode', 'median'.
#'            If missing, imputation with
#'            the 'mean' and 'mode' is used for continuous and categorical covariates,
#'            respectively. Imputation with 'mean', 'mode', or 'median' is based on
#'            measurements in \code{data} from subjects with observed covariate values.
#'            'mean' and 'median' can only be used for continuous covariates.
#'            'mode' can only be used for categorical covariates. Imputation with 'default'
#'            replaces missing values with 0 if the covariate is numeric and with 'Unknown'
#'            otherwise. 
#'           \item \code{impute_default_level}: \code{character} or 
#'            \code{numeric} specifying the
#'            imputation value to be used when \code{impute}='default'. The value must be
#'            a length 1 \code{character} (resp. \code{numeric}) for a covariate encoded by a
#'            \code{character} (resp. \code{numeric}) vector.
#'            If missing, the default
#'            values 0 and 'Unknown' are used for numeric and character covariates,
#'            respectively.
#'           }
#'        Each element of the list \code{L0_timeIndep} must be named with the time-independent
#'        covariate in \code{L0} to which the sublist information applies. \code{L0_timeIndep}
#'        can be missing if there is no time-independent covariate in \code{data}.
#'     }
#' }

cohortData <- R6::R6Class(
  classname = "cohortData",
  private = list(
    .data = NA,
    .IDvar = NA,
    .index_date = NA,
    .EOF_date = NA,
    .EOF_type = NA,
    .Y_name = NA,
    .L0 = NA,
    .L0_timeIndep = NA,
    .IDs_ignore = NA
  ),
  active = list(
    data = readOnly(".data"),
    IDvar = readOnly(".IDvar"),
    index_date = readOnly(".index_date"),
    EOF_date = readOnly(".EOF_date"),
    EOF_type = readOnly(".EOF_type"),
    Y_name = readOnly(".Y_name"),
    L0 = readOnly(".L0"),
    L0_timeIndep = readOnly(".L0_timeIndep"),
    IDs_ignore = readOnly(".IDs_ignore")
  ),
  public = list(
    initialize = function(data, IDvar, index_date, EOF_date, EOF_type, Y_name,
                              L0, L0_timeIndep) {

      ## Check valid argument types and make copy of data to avoid overwritting
      ## user data
      assertthat::assert_that(data.table::is.data.table(data),
        msg = "data is not a data.table object."
      )
      dataDT <- data.table::copy(data)

      assertthat::assert_that(is.string(IDvar))
      assertthat::assert_that(is.string(index_date))
      assertthat::assert_that(is.string(EOF_date))
      assertthat::assert_that(is.string(EOF_type))
      assertthat::assert_that(is.string(Y_name) |
        (is.integer(Y_name) & length(Y_name) == 1),
      msg = paste(
        "Y_name is neither a length 1",
        "character nor a length 1 integer."
      )
      )
      assertthat::assert_that(is.character(L0))
      ## Check table contains all columns referenced
      assertthat::assert_that(all(c(IDvar, index_date, EOF_date, EOF_type) %in%
        names(dataDT)))
      assertthat::assert_that(all(L0 %in% names(dataDT)))
      ## Check table contains no unallowed columns names
      assertthat::assert_that(!any(c(
        "IDvar", "index_date", "EOF_date",
        "EOF_type", "L0"
      ) %in% names(dataDT)),
      msg = paste(
        "Columns of data cannot be named",
        "'IDvar', 'index_date', 'EOF_date',",
        "'EOF_type', or 'L0'."
      )
      )
      ## delete rows with missing IDvar, index date, eof date, eof type
      ignoredRow <- dataDT[, which(is.na(get(IDvar)) | is.na(get(index_date)) |
        is.na(get(EOF_date)) | is.na(get(EOF_type)))]
      if (length(ignoredRow) > 0) {
        dataDT <- dataDT[-ignoredRow, ]
        warning(
          paste(
            "Ignoring the following row(s) in data due to missing",
            "values in IDvar, index_date, EOF_date, or EOF_type:"
          ),
          paste(ignoredRow, collapse = ","), "."
        )
      }

      ## Check valid IDvar column content and standardize if not
      if (!is.character(dataDT[, get(IDvar)])) {
        IDvarClass <- class(dataDT[, get(IDvar)])
        assert_that(IDvarClass %in% c(
          "factor", "numeric", "integer",
          "character"
        ),
        msg = paste(
          "Class of IDvar column in data is not valid:",
          "unique subject identifiers must be encoded",
          "by a factor, numeric, integer, or character",
          "vector."
        )
        )
        dataDT[, eval(IDvar) := as.character(get(IDvar))]
        warning("Coercing elements of IDvar column from ", IDvarClass,
          " to character\n",
          call. = FALSE
        )
      }
      ## Check one row per ID requirement
      assertthat::assert_that(dataDT[, length(unique(get(IDvar)))] ==
        dataDT[, length(get(IDvar))],
      msg = "Duplicate values in IDvar are not allowed."
      )

      assertthat::assert_that(lubridate::is.Date(dataDT[, get(index_date)]),
        msg = paste(
          "Class of index_date in data is not",
          "valid: dates of study entry must be",
          "encoded by a date vector."
        )
      )
      assertthat::assert_that(lubridate::is.Date(dataDT[, get(EOF_date)]),
        msg = paste(
          "Class of EOF_date in data is not",
          "valid: dates of end of follow-up",
          "must be encoded by a date vector."
        )
      )
      assertthat::assert_that(all(dataDT[, get(index_date)] <=
        dataDT[, get(EOF_date)]),
      msg = paste(
        "Each value in the index_date column",
        "of data must be lower than or equal",
        "to that in the same row of the",
        "EOF_date colummn of data."
      )
      )
      ignoredObs <- dataDT[, which(get(index_date) == get(EOF_date))]
      if (length(ignoredObs) > 0) {
        IDs_ignore <- as.vector(dataDT[ignoredObs,get(IDvar)])
        dataDT <- dataDT[-ignoredObs, ]
        warning(paste("Removing", length(ignoredObs), "observations because end of follow-up date is equal to study entry date"))
      } else IDs_ignore <- NA
      assertthat::assert_that(class(dataDT[, get(EOF_type)]) %in% c("character", "integer"),
        msg = paste(
          "Class of EOF_type column in data is",
          "not valid: reasons for end of",
          "follow-up must be encoded by a",
          "character or integer vector."
        )
      )
      assertthat::assert_that(Y_name %in% dataDT[, unique(get(EOF_type))],
        msg = paste(
          "Y_name must be a value of the",
          "EOF_type column of data."
        )
      )
      for(L0.i in L0)assert_that(class(dataDT[, get(L0.i)]) %in% c("numeric", "character"), msg = "Class of "%+%L0.i%+%" in data is not valid: covariate values must be encoded by a numeric or character vector.")

      ## Validate L0_timeIndep
      if(noNA(L0_timeIndep)){
          assertthat::assert_that(all(names(L0_timeIndep)%in%L0))
          for(L0.i in seq_along(L0_timeIndep)){
              assert_that( identical( sort(names(L0_timeIndep[[L0.i]])) , sort(c("categorical","impute","impute_default_level")) ) ,msg = "The sublist of L0_timeIndep for "%+%names(L0_timeIndep)[L0.i]%+%" must contain exactly three elements wih names 'categorical', 'impute, and 'impute_default_level'.")
              assert_that(noNA(L0_timeIndep[[L0.i]]$categorical), msg = "categorical must be set to TRUE or FALSE for "%+%names(L0_timeIndep)[L0.i])
              assert_that(is.flag(L0_timeIndep[[L0.i]]$categorical), msg = "categorical must be a logical value for "%+%names(L0_timeIndep)[L0.i])
              if (noNA(L0_timeIndep[[L0.i]]$impute)) {
                  assert_that(is.string(L0_timeIndep[[L0.i]]$impute), msg = "impute must be set to a character string for "%+%names(L0_timeIndep)[L0.i])
                  assert_that(L0_timeIndep[[L0.i]]$impute %in% c("default", "mean", "mode", "median"), msg = "impute must be set to 'default', 'mean', 'mode', or 'median' for "%+%names(L0_timeIndep)[L0.i])
              }
              if (noNA(L0_timeIndep[[L0.i]]$impute_default_level)) assert_that(is.string(L0_timeIndep[[L0.i]]$impute_default_level) | (is.numeric(L0_timeIndep[[L0.i]]$impute_default_level) & length(L0_timeIndep[[L0.i]]$impute_default_level) == 1), msg = "impute_default_level is neither a length 1 character nor a length 1 numeric for "%+%names(L0_timeIndep)[L0.i])              
              if (dataDT[, class(get(names(L0_timeIndep)[L0.i]))] == "character") {
                  assert_that(L0_timeIndep[[L0.i]]$categorical, msg = "a covariate ("%+%names(L0_timeIndep)[L0.i]%+%") encoded by a character vector must be categorical (categorical cannot be set to FALSE).")
              }
              if (is.na(L0_timeIndep[[L0.i]]$impute) ) {
                  if (L0_timeIndep[[L0.i]]$categorical) {
                      L0_timeIndep[[L0.i]]$impute <- "mode"
                  } else {
                      L0_timeIndep[[L0.i]]$impute <- "mean"
                  }
              }
              if (L0_timeIndep[[L0.i]]$categorical) {
                  assert_that(!L0_timeIndep[[L0.i]]$impute %in% c("mean", "median"), msg = "Cannot use imputation with mean or median for a categorical covariate ("%+%names(L0_timeIndep)[L0.i]%+%").")
              } else {
                  assert_that(!L0_timeIndep[[L0.i]]$impute %in% c("mode"), msg = "Cannot use imputation with mode for a continuous covariate ("%+%names(L0_timeIndep)[L0.i]%+%").")
              }
              if (is.na(L0_timeIndep[[L0.i]]$impute_default_level)) {
                  if (dataDT[, class(get(names(L0_timeIndep)[L0.i]))] == "character") {
                      L0_timeIndep[[L0.i]]$impute_default_level <- "Unknown"
                  } else {
                      L0_timeIndep[[L0.i]]$impute_default_level <- 0
                  }
              }
              if (noNA(L0_timeIndep[[L0.i]]$impute_default_level)) {
                  if (dataDT[, class(get(names(L0_timeIndep)[L0.i]))] == "character") assert_that(is.character(L0_timeIndep[[L0.i]]$impute_default_level), msg = "impute_level_default specifies a value that is not a string while "%+%names(L0_timeIndep)[L0.i]%+%" points to a character vector.")
                  if (dataDT[, class(get(names(L0_timeIndep)[L0.i]))] == "numeric") assert_that(is.numeric(L0_timeIndep[[L0.i]]$impute_default_level), msg = "impute_level_default specifies a value that is not a real scalar while "%+%names(L0_timeIndep)[L0.i]%+%" points to a numeric vector.")
              }
          }
      }else{
        warning("No time-independent covariates were specified.")
      }

      ## remove all unnecessary columns, point out what will be ignored, and standardize ordering in process (no need to use setcolorder since subsetting)
      dataDT <- dataDT[, c(IDvar, index_date, EOF_date, EOF_type, sort(L0)), with = FALSE]
      ## order by ID
      data.table::setkeyv(dataDT, IDvar)
      ## warn which columns where ignored
      ignoredCol <- names(data)[!names(data) %in% intersect(
        names(dataDT),
        names(data)
      )]
      if (length(ignoredCol) > 0) {
        warning(
          "Ignoring the following column(s) in data: ",
          paste(ignoredCol, collapse = " "), "."
        )
      }

      private$.data <- dataDT
      private$.IDvar <- IDvar
      private$.index_date <- index_date
      private$.EOF_date <- EOF_date
      private$.EOF_type <- EOF_type
      private$.Y_name <- Y_name
      private$.L0 <- L0
      private$.L0_timeIndep <- L0_timeIndep
      private$.IDs_ignore <- IDs_ignore
    }
  )
)

#' Data Storage Class for exposure dataset
#'
#'
#' Class that defines the standard format for the exposure dataset, i.e. the
#' table that specifies all follow-up time intervals during which a study
#' subject is exposed to an exposure level other than a reference level
#' specified by the analyst. For each episode of exposure to a non-reference
#' level, the table specifies:
#' 1) a unique subject identifier,
#' 2) the exposure episode start date,
#' 3) the exposure episode end date, and
#' 4) the non-reference exposure level (required only when the exposure is not
#'    binary).
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom data.table is.data.table copy setkeyv shift
#' @importFrom lubridate is.Date now
#' @importFrom assertthat assert_that is.string noNA
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{expData} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Fields:
#' \describe{
#'     \item{\code{data}:}{\code{data.table} containing
#'           the input exposure dataset to be wrapped in and processed.
#'           The table can contain multiple rows per subject. There
#'           should be no row for subjects who are only
#'           exposed to the reference exposure level during follow-up.
#'           Exposure levels must be encoded by a character vector or an integer
#'           vector. There cannot be any overlapping exposure episodes. Cannot
#'           contain missing values. Cannot have columns named 'IDvar',
#'           'start_date', 'end_date', 'exposure', or 'exp_level'.
#'     }
#'     \item{\code{IDvar}:}{\code{character} providing the name of the column of
#'           \code{data} that contains the unique subject identifier.
#'     }
#'     \item{\code{start_date}:}{\code{character} providing the name of the
#'           column of \code{data} that contains the exposure episode start
#'           date.
#'     }
#'     \item{\code{end_date}:}{\code{character} providing the name of the column
#'           of \code{data} that contains the exposure episode end date.
#'     }
#'     \item{\code{exp_level}:}{\code{character} providing the name of the
#'           column of \code{data} that contains the non-reference exposure
#'           level. Can be missing only if \code{exp_ref} is also missing. If
#'           missing, the exposure is assumed to be binary and its reference
#'           level is encoded by 0.
#'    }
#'    \item{\code{exp_ref}:}{\code{character} or \code{integer} identifying the
#'          exposure reference level. Cannot be a value of the exp_level column
#'          of data. Can be missing only if \code{exp_level} is also missing.
#'     }
#'     }

expData <- R6::R6Class(
  classname = "expData",
  private = list(
    .data = NA,
    .IDvar = NA,
    .start_date = NA,
    .end_date = NA,
    .exp_level = NA,
    .exp_ref = NA
  ),
  active = list(
    data = readOnly(".data"),
    IDvar = readOnly(".IDvar"),
    start_date = readOnly(".start_date"),
    end_date = readOnly(".end_date"),
    exp_level = readOnly(".exp_level"),
    exp_ref = readOnly(".exp_ref")
  ),
  public = list(
    initialize = function(data, IDvar, start_date, end_date, exp_level = NA,
                              exp_ref = NA) {
      
      ## Check valid argument types and make copy of data to avoid overwritting user data
      assertthat::assert_that(data.table::is.data.table(data),
        msg = "data is not a data.table object."
      )
      dataDT <- data.table::copy(data)

      assert_that(is.string(IDvar))
      assert_that(is.string(start_date))
      assert_that(is.string(end_date))
      if (noNA(exp_level)) {
        assert_that(is.string(exp_level))
      } else { # define a unique column name for exp_level
        expCol <- "expLevel"
        while (expCol %in% names(dataDT))
          expCol <- expCol %+% as.integer(now("UTC")) %+% "UTC"
      }
      if (noNA(exp_ref)) {
        assert_that(is.string(exp_ref) | (is.integer(exp_ref) &
          length(exp_ref) == 1),
        msg = paste(
          "exp_ref is neither a length 1 character nor",
          "a length 1 integer."
        )
        )
      }
      
      ## Handle missing elements
      if (is.na(exp_level) & is.na(exp_ref)) {
        exp_ref <- 0L
        exp_level <- expCol
        dataDT[, eval(exp_level) := 1L]
        warning("exp_level and exp_ref were missing: the reference and non-reference levels for the binary exposure are set to 0 and 1, respectively.")
      } else {
        assert_that(noNA(c(exp_level, exp_ref)), msg = "exp_level cannot be missing if exp_ref is missing and vice versa.")
      }

      ## Check table contains all columns referenced
      assert_that(all(c(IDvar, start_date, end_date, exp_level) %in% names(dataDT)))
      ## Check table contains no unallowed columns names
      assert_that(!any(c("IDvar", "start_date", "end_date", "exp_level", "exposure") %in% names(dataDT)), msg = "Columns of data cannot be named 'IDvar', 'start_date', 'end_date', 'exposure', or 'exp_level'.")
        
      ## Check table content
      assert_that(noNA(dataDT), msg = "data contains missing values.")
      ## Check valid IDvar column content and standardize if not
      if (!is.character(dataDT[, get(IDvar)])) {
        IDvarClass <- class(dataDT[, get(IDvar)])
        assert_that(IDvarClass %in% c("factor", "numeric", "integer", "character"), msg = "Class of IDvar column in data is not valid: unique subject identifiers must be encoded by a factor, numeric, integer, or character vector.")
        dataDT[, eval(IDvar) := as.character(get(IDvar))]
        warning("Coercing elements of IDvar column from ", IDvarClass, " to character\n", call. = FALSE)
      }
      assert_that(lubridate::is.Date(dataDT[, get(start_date)]), msg = "Class of start_date in data is not valid: start dates of exposure episodes must be encoded by a date vector.")
      assert_that(lubridate::is.Date(dataDT[, get(end_date)]), msg = "Class of end_date in data is not valid: end dates of exposure episodes must be encoded by a date vector.")
      assert_that(all(dataDT[, get(start_date)] <= dataDT[, get(end_date)]), msg = "The start date of an exposure episode must precede or equal its end date. Each value in the start_date column of data must be lower than or equal to that in the same row of the end_date column of data.")
      assert_that(class(dataDT[, get(exp_level)]) %in% c("character", "integer"), msg = "Class of exp_level column in data is not valid: non-reference exposure levels must be encoded by a character or integer vector.")
      ## consistency between the class and values of the ref and nonRef exposure levels
      if (is.integer(dataDT[, get(exp_level)])) assert_that(is.integer(exp_ref), msg = "exp_ref must be an integer when the column exp_level of data is an integer vector.")
      if (is.character(dataDT[, get(exp_level)])) assert_that(is.string(exp_ref), msg = "exp_ref must be a string (a length one character vector) when the column exp_level of data is a character vector.")
      assert_that(!exp_ref %in% dataDT[, unique(get(exp_level))], msg = "exp_ref cannot be a value of the exp_level column of data.")

      ## remove all unnecessary columns, point out what will be ignored, and standardize ordering in process (no need to use setcolorder since subsetting)
      dataDT <- dataDT[, c(IDvar, start_date, end_date, exp_level), with = FALSE]
      ## order by IDvar and start date
      data.table::setkeyv(dataDT, c(IDvar, start_date))
      ## warn which columns where ignored
      ignoredCol <- names(data)[!names(data) %in% intersect(names(dataDT), names(data))]
      if (length(ignoredCol) > 0) warning("Ignoring the following column(s) in data: ", paste(ignoredCol, collapse = " "), ".")

      ## check no overlapping columns - works only because we orderd by ID and start date above
      end_date_lag <- end_date %+% "_lag"
      dataDT[, eval(end_date_lag) := shift(.SD, n = 1L, fill = NA, type = "lag"), by = eval(IDvar), .SDcols = eval(end_date)]
      assert_that(dataDT[!is.na(get(end_date_lag)), all(get(start_date) > get(end_date_lag))], msg = "Overlapping exposure episodes are not allowed.")
      dataDT[, eval(end_date_lag) := NULL]

      private$.data <- dataDT
      private$.IDvar <- IDvar
      private$.start_date <- start_date
      private$.end_date <- end_date
      private$.exp_level <- exp_level
      private$.exp_ref <- exp_ref
    },
    checkAgainst = function(otherData) {
      if ("cohortData" %in% class(otherData)) {
        assert_that(private$.IDvar == otherData$IDvar, msg = "exposure and cohort datasets must use the same column name to store unique subject identifiers.")
        assert_that(all(private$.data[!get(private$.IDvar)%in%otherData$IDs_ignore, get(private$.IDvar)] %in% otherData$data[, get(otherData$IDvar)]), msg = "Subject identifiers in the exposure dataset must also be found in the cohort dataset.")
        ## Identify exposure rows that need to be modified or removed (index/eof date):
        private$.data <- merge(private$.data, otherData$data[, c(otherData$IDvar, otherData$index_date, otherData$EOF_date), with = FALSE], by = private$.IDvar, all.x = TRUE, all.y = FALSE)
        expEpisode.ignored <- private$.data[get(private$.end_date) < get(otherData$index_date) | get(private$.start_date) > get(otherData$EOF_date) | (get(private$.start_date) < get(otherData$index_date) & get(private$.end_date) >= get(otherData$index_date)) | (get(private$.start_date) <= get(otherData$EOF_date) & get(private$.end_date) > get(otherData$EOF_date)), .N]
        if (expEpisode.ignored > 0) {
          warning("Found at least one exposure episode that indicates exposed days before a subject's index date or after a subject's end of follow-up. Information on such exposed days will be ignored.")
          ## remove episodes with end dates strictly before index dates
          private$.data <- private$.data[get(private$.end_date) >= get(otherData$index_date), ]
          ## remove episodes with start dates strictly after EOF dates
          private$.data <- private$.data[get(private$.start_date) <= get(otherData$EOF_date), ]
          ## remove days prior to index date from episodes that straddle index date
          private$.data[(get(private$.start_date) < get(otherData$index_date) & get(private$.end_date) >= get(otherData$index_date)), eval(private$.start_date) := get(otherData$index_date)]
          ## remove days after EOF date from episodes that straddle EOF date
          private$.data[(get(private$.start_date) <= get(otherData$EOF_date) & get(private$.end_date) > get(otherData$EOF_date)), eval(private$.end_date) := get(otherData$EOF_date)]

          assert_that(private$.data[get(private$.end_date) < get(otherData$index_date) | get(private$.start_date) > get(otherData$EOF_date) | (get(private$.start_date) < get(otherData$index_date) & get(private$.end_date) >= get(otherData$index_date)) | (get(private$.start_date) <= get(otherData$EOF_date) & get(private$.end_date) > get(otherData$EOF_date)), .N] == 0)
          data.table::setkeyv(private$.data, c(private$.IDvar, private$.start_date))
        }
        ## remove columns added from merge
        private$.data[, ":="(c(otherData$index_date, otherData$EOF_date), vector("list", 2))]
      }
      invisible(self)
    }
  )
)

#' Data Storage Class for instant exposure dataset
#'
#'
#' Class that defines the standard format for the instant exposure dataset, i.e. the
#' table that specifies all follow-up times during which a study
#' subject is exposed to a non-reference exposure level. For each instantaneous
#' exposure, the table specifies:
#' 1) a unique subject identifier,
#' 2) the exposure time,
#' 3) the exposure level (required only when the exposure is not
#'    binary).
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom data.table is.data.table copy setkeyv shift
#' @importFrom lubridate is.Date now
#' @importFrom assertthat assert_that is.string are_equal noNA
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{instExpData} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Fields:
#' \describe{
#'     \item{\code{data}:}{\code{data.table} containing
#'           the input exposure dataset to be wrapped in and processed.
#'           The table can contain multiple rows per subject. There
#'           should be no row for subjects who are only
#'           exposed to the reference exposure level during follow-up.
#'           Exposure levels must be encoded by one or more character, integer,
#'           or numeric vector(s). Multiples rows per subject and time are permitted.
#'           Cannot
#'           contain missing values. Cannot have columns named 'IDvar',
#'           'start_date', or 'exp_level'.
#'     }
#'     \item{\code{IDvar}:}{\code{character} providing the name of the column of
#'           \code{data} that contains the unique subject identifier.
#'     }
#'     \item{\code{start_date}:}{\code{character} providing the name of the
#'           column of \code{data} that contains the exposure 
#'           date.
#'     }
#'     \item{\code{exp_level}:}{\code{character} vector providing the name(s) of the
#'           column(s) of \code{data} that contain(s) the non-reference exposure
#'           level(s). Can be missing if there is only one non-reference exposure level.
#'           If missing, the exposure is assumed to be binary and its reference
#'           level is encoded by 0.
#'    }
#'    }

instExpData <- R6::R6Class(
  classname = "instExpData",                       
  inherit = expData,
  public = list(
    initialize = function(data, IDvar, start_date, exp_level = NA) {
      
      ## Check valid argument types and make copy of data to avoid overwritting user data
      assertthat::assert_that(data.table::is.data.table(data),
        msg = "data is not a data.table object."
      )
      dataDT <- data.table::copy(data)

      assert_that(is.string(IDvar))
      assert_that(is.string(start_date))
      if (!are_equal(exp_level,NA)) {
          assert_that(is.character(exp_level))
          assert_that(all(!is.na(exp_level)), msg = "exp_level cannot contain a missing value")
      } else { # Handle missing elements: define a unique column name for exp_level
        expCol <- "expLevel"
        while (expCol %in% names(dataDT))
            expCol <- expCol %+% as.integer(now("UTC")) %+% "UTC"
        exp_level <- expCol
        dataDT[, eval(exp_level) := 1L]
        warning("exp_level was missing: the reference and non-reference levels for the binary exposure are set to 0 and 1, respectively.")
      }
            
      ## Check table contains all columns referenced
      assert_that(all(c(IDvar, start_date, exp_level) %in% names(dataDT)))
      ## Check table contains no unallowed columns names
      assert_that(!any(c("IDvar", "start_date", "exp_level") %in% names(dataDT)), msg = "Columns of data cannot be named 'IDvar', 'start_date', or 'exp_level'.")
        
      ## Check table content
      assert_that(noNA(dataDT), msg = "data contains missing values.")
      ## Check valid IDvar column content and standardize if not
      if (!is.character(dataDT[, get(IDvar)])) {
        IDvarClass <- class(dataDT[, get(IDvar)])
        assert_that(IDvarClass %in% c("factor", "numeric", "integer", "character"), msg = "Class of IDvar column in data is not valid: unique subject identifiers must be encoded by a factor, numeric, integer, or character vector.")
        dataDT[, eval(IDvar) := as.character(get(IDvar))]
        warning("Coercing elements of IDvar column from ", IDvarClass, " to character\n", call. = FALSE)
      }
      assert_that(lubridate::is.Date(dataDT[, get(start_date)]), msg = "Class of start_date in data is not valid: start dates of exposure episodes must be encoded by a date vector.")
      assert_that( all(lapply(dataDT[, mget(exp_level)],class) %in% c("character", "integer","numeric")) , msg = "A class of an exp_level column in data is not valid: non-reference exposure levels must be encoded by character, integer, or numeric vector(s).")

      ## remove all unnecessary columns, point out what will be ignored, and standardize ordering in process (no need to use setcolorder since subsetting)
      dataDT <- dataDT[, c(IDvar, start_date, exp_level), with = FALSE]
      ## order by IDvar and start date
      data.table::setkeyv(dataDT, c(IDvar, start_date))
      ## warn which columns where ignored
      ignoredCol <- names(data)[!names(data) %in% intersect(names(dataDT), names(data))]
      if (length(ignoredCol) > 0) warning("Ignoring the following column(s) in data: ", paste(ignoredCol, collapse = " "), ".")

      private$.data <- dataDT
      private$.IDvar <- IDvar
      private$.start_date <- start_date
      private$.exp_level <- exp_level
    },
    checkAgainst = function(otherData) {
      if ("cohortData" %in% class(otherData)) {
        assert_that(private$.IDvar == otherData$IDvar, msg = "exposure and cohort datasets must use the same column name to store unique subject identifiers.")
        assert_that(all(private$.data[!get(private$.IDvar)%in%otherData$IDs_ignore, get(private$.IDvar)] %in% otherData$data[, get(otherData$IDvar)]), msg = "Subject identifiers in the exposure dataset must also be found in the cohort dataset.")
        ## Identify exposure rows that need to be modified or removed (index/eof date):
        private$.data <- merge(private$.data, otherData$data[, c(otherData$IDvar, otherData$index_date, otherData$EOF_date), with = FALSE], by = private$.IDvar, all.x = TRUE, all.y = FALSE)
        expEpisode.ignored <- private$.data[get(private$.start_date) < get(otherData$index_date) | get(private$.start_date) > get(otherData$EOF_date) , .N]
        if (expEpisode.ignored > 0) {
          warning("Found at least one exposure date before a subject's index date or after a subject's end of follow-up. Information on such exposure will be ignored.")
          ## remove episodes with end dates strictly before index dates
          private$.data <- private$.data[get(private$.start_date) >= get(otherData$index_date), ]
          ## remove episodes with start dates strictly after EOF dates
          private$.data <- private$.data[get(private$.start_date) <= get(otherData$EOF_date), ]

          assert_that(private$.data[get(private$.start_date) < get(otherData$index_date) | get(private$.start_date) > get(otherData$EOF_date) , .N] == 0)
          data.table::setkeyv(private$.data, c(private$.IDvar, private$.start_date))
        }
        ## remove columns added from merge
        private$.data[, ":="(c(otherData$index_date, otherData$EOF_date), vector("list", 2))]
      }
      invisible(self)
    }
  )
)


#' Data Storage Class for a time-dependent covariate dataset
#'
#'
#' Class that defines the standard format for a dataset that encodes all follow-up measurements for
#' a single time-dependent covariate, i.e. the table that specifies for each subject in the
#' cohort: 1) a unique subject identifier,  2) the date of the covariate measurement, 3) the
#' covariate value.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom data.table is.data.table copy setkeyv shift
#' @importFrom lubridate is.Date
#' @importFrom assertthat assert_that is.string noNA is.flag
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{timeDepCovData} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Fields:
#' \describe{
#'     \item{\code{data}:}{\code{data.table} containing
#'           an input covariate dataset to be wrapped in and processed.
#'           The table can contain multiple rows per subject. There
#'           should be no row for subjects with no follow-up covariate measurements. The table
#'           must contain at most one covariate measurement on a given date for any given subject.
#'           Cannot contain missing values. Covariate values must be encoded by a character or
#'           numeric vector (e.g., factors are not allowed).
#'           Cannot have columns named 'IDvar', 'L_date', or 'L_name'.
#'     }
#'     \item{\code{type}:}{\code{character} specifying the covariate type: 'binary monotone
#'           increasing' (e.g., history of a diagnosis or procedure), 'interval' (e.g., hospital
#'           stay or prescription coverage), 'sporadic' (e.g., laboratory measurements),
#'           'indicator' (e.g., occurrence of a repeatable event).
#'     }
#'     \item{\code{IDvar}:}{\code{character} providing the name of the column of
#'           \code{data} that contains the unique subject identifier.
#'     }
#'     \item{\code{L_date}:}{\code{character} providing the name of the column of
#'           \code{data} that contains the date of the follow-up covariate measurement.
#'     }
#'     \item{\code{L_name}:}{\code{character} providing the name of
#'          the column of \code{data} that contains the covariate values. For all covariate types,
#'          \code{L_name} must also be the name of the column of the cohort dataset that contains
#'          the baseline measurements of the time-dependent covariate. Baseline measurements of a
#'          covariate of type 'binary monotone increasing' can only be encoded with values 0 and 1 in
#'          the cohort dataset. All values in the column \code{L_name} of \code{data} must be set
#'          to 1 for a covariate of type 'binary monotone increasing'.
#'          The column \code{L_name} in \code{data} cannot contain
#'          the value 'None' for a covariate of type 'indicator' that is \code{character}.
#'          The column \code{L_name} in \code{data} and in the cohort dataset cannot contain
#'          the value 0 for a covariate of type 'indicator' that is \code{numeric}.
#'    }
#'     \item{\code{categorical}:}{\code{logical} indicating whether the covariate is continuous
#'           ('FALSE') or categorical ('TRUE'). Must be 'TRUE' for a covariate of type 'binary
#'           monotone increasing' or 'indicator'. Cannot be missing.
#'     }
#'     \item{\code{impute}:}{\code{character} specifying imputation method for missing baseline
#'             measurements: 'default', 'mean', 'mode', 'median'. If missing, imputation with
#'             the 'mean' and 'mode' is used for continuous and categorical covariates,
#'             respectively. Imputation with 'mean', 'mode', or 'median' is based on baseline
#'             measurements from subjects with observed baseline covariate values (stored in the
#'             cohort dataset). 'mean' and 'median' can only be used for continuous covariates.
#'             'mode' can only be used for categorical covariates. Imputation with 'default'
#'             replaces missing values with 0 if the covariate is numeric and with 'Unknown'
#'             otherwise. Ignored for a covariate of type 'binary monotone increasing', 'interval',
#'             or 'indicator'.
#'    }
#'    \item{\code{impute_default_level}:}{ \code{character} or \code{numeric} specifying the
#'          imputation value to be used when \code{impute}='default'. The value must be
#'          a length 1 \code{character} (resp. \code{numeric}) for a covariate encoded by a
#'          \code{character} (resp. \code{numeric}) vector.
#'          If missing, the default
#'          values 0 and 'Unknown' are used for numeric and character covariates,
#'          respectively. Ignored for a
#'          covariate of type 'binary monotone increasing', 'interval', or 'indicator'.
#'    }
#'     \item{\code{acute_change}:}{\code{logical} indicating whether a covariate measurement
#'           collected on the date of an exposure change can be impacted by the change.
#'           The default value 'FALSE' indicates that the covariate measurement can be assumed to
#'           have preceded and possibly triggered the change in exposure. Cannot be missing.
#'     }
#'    }

timeDepCovData <- R6::R6Class(
  classname = "timeDepCovData",
  private = list(
    .data = NA,
    .type = NA,
    .IDvar = NA,
    .L_date = NA,
    .L_name = NA,
    .categorical = NA,
    .impute = NA,
    .impute_default_level = NA,
    .acute_change = NA
  ),
  active = list(
    data = readOnly(".data"),
    type = readOnly(".type"),
    IDvar = readOnly(".IDvar"),
    L_date = readOnly(".L_date"),
    L_name = readOnly(".L_name"),
    categorical = readOnly(".categorical"),
    impute = readOnly(".impute"),
    impute_default_level = readOnly(".impute_default_level"),
    acute_change = readOnly(".acute_change")
  ),
  public = list(
    initialize = function(data, type, IDvar, L_date, L_name, categorical, impute = NA, impute_default_level = NA, acute_change = FALSE) {

      ## Check valid argument types and make copy of data to avoid overwritting user data
      assert_that(data.table::is.data.table(data), msg = "data is not a data.table object.")
      dataDT <- data.table::copy(data)

      assert_that(is.string(type))
      assert_that(type %in% c("binary monotone increasing", "interval", "sporadic", "indicator"), msg = "type must be set to 'binary monotone increasing', 'interval', 'sporadic', or 'indicator'.")
      assert_that(is.string(IDvar))
      assert_that(is.string(L_date))
      assert_that(is.string(L_name))
      assert_that(noNA(categorical))
      assert_that(is.flag(categorical))
      if (noNA(impute)) {
        assert_that(is.string(impute))
        assert_that(impute %in% c("default", "mean", "mode", "median"), msg = "impute must be set to 'default', 'mean', 'mode', or 'median'.")
      }
      if (noNA(impute_default_level)) assert_that(is.string(impute_default_level) | (is.numeric(impute_default_level) & length(impute_default_level) == 1), msg = "impute_default_level is neither a length 1 character nor a length 1 numeric.")
      assert_that(noNA(acute_change))
      assert_that(is.flag(acute_change))

      ## Check table contains all columns referenced
      assert_that(all(c(IDvar, L_date, L_name) %in% names(dataDT)))
      ## Check table contains no unallowed columns names
      assert_that(!any(c("IDvar", "L_date", "L_name") %in% names(dataDT)), msg = "Columns of data cannot be named 'IDvar', 'L_date', or 'L_name'.")

      ## Check table content
      assert_that(noNA(dataDT), msg = "data contains missing values.")
      ## Check valid IDvar column content and standardize if not
      if (!is.character(dataDT[, get(IDvar)])) {
        IDvarClass <- class(dataDT[, get(IDvar)])
        assert_that(IDvarClass %in% c("factor", "numeric", "integer", "character"), msg = "Class of IDvar column in data is not valid: unique subject identifiers must be encoded by a factor, numeric, integer, or character vector.")
        dataDT[, eval(IDvar) := as.character(get(IDvar))]
        warning("Coercing elements of IDvar column from ", IDvarClass, " to character\n", call. = FALSE)
      }
      assert_that(lubridate::is.Date(dataDT[, get(L_date)]), msg = "Class of L_date in data is not valid: dates of covariate measurements must be encoded by a date vector.")
      assert_that(class(dataDT[, get(L_name)]) %in% c("numeric", "character"), msg = "Class of L_name in data is not valid: covariate values must be encoded by a numeric or character vector.")
      if (type == "binary monotone increasing") assert_that(identical(dataDT[, unique(get(L_name))], 1), msg = "For a covariate of type 'binary monotone increasing', the column L_name of data cannot contain values other than 1.")
      if (type == "indicator" & dataDT[, class(get(L_name))] == "character") assert_that(!"None" %in% dataDT[, unique(get(L_name))], msg = "For a covariate with character values of type 'indicator', the column L_name of data cannot contain the value 'None'.")
      if (type == "indicator" & dataDT[, class(get(L_name))] == "numeric") assert_that(!0 %in% dataDT[, unique(get(L_name))], msg = "For a covariate with numeric values of type 'indicator', the column L_name of data cannot contain the value 0.")
      if (type %in% c("binary monotone increasing", "indicator")) assert_that(categorical, msg = "categorical must be set to 'TRUE' for a covariate of type 'binary monotone increasing' or 'indicator'.")
      if (dataDT[, class(get(L_name))] == "character") {
        assert_that(categorical, msg = "a covariate encoded by a character vector must be categorical (categorical cannot be set to FALSE).")
      }

      if (noNA(impute) & type %in% c("binary monotone increasing", "interval", "indicator")) {
        warning("impute was specified for a variable of type 'binary monotone increasing','interval', or 'indicator': argument is ignored.")
        impute <- NA
      }
      if (is.na(impute) & (!type %in% c("binary monotone increasing", "interval", "indicator"))) {
        if (categorical) {
          impute <- "mode"
        } else {
          impute <- "mean"
        }
      }
      if (categorical) {
        assert_that(!impute %in% c("mean", "median"), msg = "Cannot use imputation with mean or median for a categorical covariate.")
      } else {
        assert_that(!impute %in% c("mode"), msg = "Cannot use imputation with mode for a continuous covariate.")
      }

      if (noNA(impute_default_level) & type %in% c("binary monotone increasing", "interval", "indicator")) {
        warning("impute_default_level was specified for a variable of type 'binary monotone increasing','interval', or 'indicator': argument is ignored.")
        impute_default_level <- NA
      }
      if (is.na(impute_default_level) & (!type %in% c("binary monotone increasing", "interval", "indicator"))) {
        if (dataDT[, class(get(L_name))] == "character") {
          impute_default_level <- "Unknown"
        } else {
          impute_default_level <- 0
        }
      }
      if (noNA(impute_default_level)) {
        if (dataDT[, class(get(L_name))] == "character") assert_that(is.character(impute_default_level), msg = "impute_level_default specifies a value that is not a string while L_name points to a character vector.")
        if (dataDT[, class(get(L_name))] == "numeric") assert_that(is.numeric(impute_default_level), msg = "impute_level_default specifies a value that is not a real scalar while L_name points to a numeric vector.")
      }

      ## remove all unnecessary columns, point out what will be ignored, and standardize ordering in process (no need to use setcolorder since subsetting)
      dataDT <- dataDT[, c(IDvar, L_date, L_name), with = FALSE]
      ## order by IDvar and L_date
      data.table::setkeyv(dataDT, c(IDvar, L_date))
      ## warn which columns where ignored
      ignoredCol <- names(data)[!names(data) %in% intersect(names(dataDT), names(data))]
      if (length(ignoredCol) > 0) warning("Ignoring the following column(s) in data: ", paste(ignoredCol, collapse = " "), ".")

      ## Check that there is no more than one row with the same IDvar and L_date
      assert_that(anyDuplicated(dataDT[, .(get(IDvar), get(L_date))]) == 0, msg = "data must contain at most one covariate measurement on a given date for any given subject.")

      private$.data <- dataDT
      private$.type <- type
      private$.IDvar <- IDvar
      private$.L_date <- L_date
      private$.L_name <- L_name
      private$.categorical <- categorical
      private$.impute <- impute
      private$.impute_default_level <- impute_default_level
      private$.acute_change <- acute_change
    },
    checkAgainst = function(otherData) {
      if ("list" %in% class(otherData) && "timeDepCovData" %in% class(otherData[[1]])) {
        assert_that(!private$.L_name %in% names(otherData), msg = "Time-varying covariates cannot have the same name. Check that the same covariate dataset was not added twice.")
      }

      if ("cohortData" %in% class(otherData)) {
        assert_that(private$.L_name %in% otherData$L0, msg = "The time-dependent covariate " %+% private$.L_name %+% " must have its baseline measurement stored in the cohort dataset.")
        assert_that(!private$.L_name %in% names(otherData$L0_timeIndep), msg = "The time-dependent covariate " %+% private$.L_name %+% " was specified as a time-independent covariate in the cohort dataset.")
        assert_that( class(private$.data[,get(private$.L_name)]) == class(otherData$data[,get(private$.L_name)]) , msg = "The type (numeric or character) of the time-dependent covariate " %+% private$.L_name %+% " does not match the type for its baseline values stored in the cohort dataset")

        if(private$.type=="interval") assertthat::assert_that(otherData$data[is.na(get(private$.L_name)),.N]==0, msg = "Covariate "%+%private$.L_name%+% " of type 'interval' is not allowed to have missing baseline values stored in the cohort dataset.")
        if(private$.type=="binary monotone increasing") assertthat::assert_that(otherData$data[is.na(get(private$.L_name)),.N]==0, msg = "Covariate "%+%private$.L_name%+% " of type 'binary monotone increasing' is not allowed to have missing baseline values stored in the cohort dataset.")
        if(private$.type=="indicator") assertthat::assert_that(otherData$data[is.na(get(private$.L_name)),.N]==0, msg = "Covariate "%+%private$.L_name%+% " of type 'indicator' is not allowed to have missing baseline values stored in the cohort dataset.")

        if(private$.type=="binary monotone increasing") assert_that(otherData$data[, all(unique(get(private$.L_name))%in%c(0,1))], msg = "Baseline values for the covariate "%+%private$.L_name%+%" of type 'binary monotone increasing' that are stored in the cohort dataset must be either 1 or 0 (no other values are allowed).")
        
        assert_that(private$.IDvar == otherData$IDvar, msg = "The cohort dataset and the time-dependent covariate dataset for " %+% private$.L_name %+% " must use the same column name to store unique subject identifiers.")
        assert_that(all(private$.data[!get(private$.IDvar)%in%otherData$IDs_ignore, get(private$.IDvar)] %in% otherData$data[, get(otherData$IDvar)]), msg = "Subject identifiers in the time-dependent covariate dataset for " %+% private$.L_name %+% " must also be found in the cohort dataset.")

        ## Identify covariate rows that need to be modified or removed (index/eof date):
        private$.data <- merge(private$.data, otherData$data[, c(otherData$IDvar, otherData$index_date, otherData$EOF_date), with = FALSE], by = private$.IDvar, all.x = TRUE, all.y = FALSE)
        covVal.ignored <- private$.data[get(private$.L_date) < get(otherData$index_date) | get(private$.L_date) > get(otherData$EOF_date), .N]

        if (covVal.ignored > 0) {
          warning("Found one or more ", private$.L_name, " measurement(s) before a subject's index date or after a subject's end of follow-up. Such measurements will be ignored.")

          if (private$.type %in% c("sporadic", "indicator", "binary monotone increasing")) {
            if (private$.type %in% c("binary monotone increasing")) {
              subject.Lt.before.index <- private$.data[get(private$.L_date) < get(otherData$index_date), unique(get(private$.IDvar))]
              if (any(otherData$data[ get(otherData$IDvar) %in% subject.Lt.before.index, get(private$.L_name)] %in% 0)) warning("Found at least one subject with a measurement of the time-dependent covariate dataset for ", private$.L_name, " (binary monotone increasing) which was collected before the subject's index date while its baseline measurement in the cohort dataset is 0. This may indicate an error in the information encoded for ", private$.L_name, ". Time-dependent measurements before the index date are ignored.")
            }
            ## remove measurements with dates strictly before index dates
            private$.data <- private$.data[get(private$.L_date) >= get(otherData$index_date), ]
            ## remove measurements with dates strictly after EOF dates
            private$.data <- private$.data[get(private$.L_date) <= get(otherData$EOF_date), ]
          }

          if (private$.type %in% c("interval")) {
            Lt.before.or.at.index <- private$.data[get(private$.L_date) <= get(otherData$index_date), ]
            if(nrow(Lt.before.or.at.index) > 0) {
              Lt.keep <- Lt.before.or.at.index[, closest.before.index := as.numeric(get(private$.L_date) == max(get(private$.L_date))), by = c(private$.IDvar)][closest.before.index == 1, ]
              Lt.keep[, eval(private$.L_date) := get(otherData$index_date)][, closest.before.index := NULL]
              ## remove Lts before/on index and bring back Lts closest before or on index
              assert_that(identical(names(private$.data), names(Lt.keep)))
              private$.data <- rbind(private$.data[get(private$.L_date) > get(otherData$index_date), ], Lt.keep)
            }
            ## remove measurements with dates strictly after EOF dates
            private$.data <- private$.data[get(private$.L_date) <= get(otherData$EOF_date), ]
          }

          assert_that(private$.data[get(private$.L_date) < get(otherData$index_date) | get(private$.L_date) > get(otherData$EOF_date), .N] == 0)
          data.table::setkeyv(private$.data, c(private$.IDvar, private$.L_date))
        }
        ## remove columns added from merge
        private$.data[, ":="(c(otherData$index_date, otherData$EOF_date), vector("list", 2))]
      }
      invisible(self)
    }
  )
)

#' Data Storage Class for an LtAt dataset
#'
#'
#' Class that defines the specifications for constructing an LtAt dataset and the dataset itself.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom data.table is.data.table copy setkeyv shift rbindlist setcolorder setorderv data.table merge.data.table
#' @importFrom lubridate is.Date %within% interval as_date int_start int_end ymd interval int_overlaps intersect
#' @importFrom assertthat assert_that is.string noNA is.flag are_equal
#' @importFrom future.apply future_lapply
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{LtAtData} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Fields:
#' \describe{
#'     \item{\code{data}:}{\code{data.table} containing
#'           the LtAt dataset. NULL by default before construction.
#'     }
#'     \item{\code{cohort_data}:}{\code{cohortData} specifying the cohort dataset.
#'     }
#'     \item{\code{exp_data}:}{\code{expData} specifying the time-varying exposure dataset.
#'           Its \code{IDvar} field must contain the same value as that of the
#'           \code{cohort_data} (i.e.,
#'           the same column name must be used for the ID column of the exposure and cohort
#'           datasets). The unique subject identifiers in the \code{data} field of \code{exp_data}
#'           must be a subset of those founds in the \code{data} field of \code{cohort_data} (i.e.,
#'           all IDs in the exposure dataset must also be present in the cohort dataset.)
#'           An exposure measurement stored in the \code{data} field of \code{exp_data}
#'           will be ignored
#'           if its date is either strictly before the subject's index date stored in the
#'           \code{data} field of \code{cohort_data} or strictly after the subject's end of
#'           follow-up date stored in the \code{data} field of \code{cohort_data} (i.e., only
#'           exposure measurements collected between the start and end of follow-up will be
#'           retained).
#'     }
#'     \item{\code{cov_data}:}{a list of \code{timeDepCovData} that
#'           specifies time-varying covariate(s). Each element of the list must have a distinct
#'           value stored in its \code{L_name} field and this value must also be found in the
#'           \code{L0} field of \code{cohort_data} (i.e., time-varying covariates cannot have
#'           the same name and must have their baseline measurements stored in the cohort dataset).
#'           The \code{IDvar} field of each element \code{timeDepCovData} of \code{cov_data}
#'           must contain the same value as
#'           that of the \code{cohort_data} (i.e.,
#'           the same column name must be used for the ID column of each covariate dataset and
#'           that of the cohort dataset). The unique subject identifiers in the \code{data}
#'           field of each element
#'           \code{timeDepCovData} of \code{cov_data}
#'           must be a subset of those founds in the \code{data} field of \code{cohort_data} (i.e.,
#'           all IDs in a covariate dataset must also be present in the cohort dataset.)
#'           A covariate measurement stored in the \code{data} field of an element
#'           \code{timeDepCovData}
#'           of \code{cov_data} will be ignored
#'           if its date is either strictly before the subject's index date stored in the
#'           \code{data} field of \code{cohort_data} or strictly after the subject's end of
#'           follow-up date stored in the \code{data} field of \code{cohort_data}. (i.e., only
#'           covariate measurements collected between the start and end of follow-up will be
#'           retained).
#'     }
#'    }

LtAtData <- R6::R6Class(
  classname = "LtAtData",
  private = list(
    .data = NA,
    .cohort_data = NA,
    .exp_data = NA,
    .cov_data = NA,
    .time_unit = NA, 
    .format=NA,
    .dates=NA,
    .first_exp_rule = NA,
    .exp_threshold = NA
  ),
  active = list(
    data = readOnly(".data"),
    cohort_data = readOnly(".cohort_data"),
    exp_data = readOnly(".exp_data"),
    cov_data = readOnly(".cov_data"),
    time_unit = readOnly(".time_unit"),
    format = readOnly(".format"),
    dates = readOnly(".dates"),
    first_exp_rule = readOnly(".first_exp_rule"),
    exp_threshold = readOnly(".exp_threshold")    
  ),
  public = list(
    initialize = function() {
      private$.data <- NA
      private$.cohort_data <- NA
      private$.exp_data <- NA
      private$.cov_data <- NA
      private$.time_unit <- NA
      private$.first_exp_rule <- NA
      private$.exp_threshold <- NA
      private$.format <- NA
      private$.dates <- NA
      invisible(self) # critical to allow method chaining
    },
    addSpec = function(spec) {
      assertthat::assert_that(any(class(spec) %in%
        c("cohortData", "expData", "timeDepCovData")))
      if ("cohortData" %in% class(spec)) {
        assertthat::assert_that(!"cohortData" %in% class(private$.cohort_data),
          msg = paste(
            "cohort_data field already defined",
            "- will not overwrite it."
          )
        )
        if ("expData" %in% class(private$.exp_data)) {
          private$.exp_data <- private$.exp_data$checkAgainst(spec)
        }
        if ("list" %in% class(private$.cov_data) && "timeDepCovData" %in%
          class(private$.cov_data[[1]])) {
          for (cov.i in names(private$.cov_data)) {
            private$.cov_data[[cov.i]] <-
              private$.cov_data[[cov.i]]$checkAgainst(spec)
          }
        }
        private$.cohort_data <- spec
      }
      if ("expData" %in% class(spec)) {
        assertthat::assert_that(!"expData" %in% class(private$.exp_data),
          msg = paste(
            "exp_data field already defined",
            "- will not overwrite it."
          )
        )
        private$.exp_data <- spec$checkAgainst(private$.cohort_data)
      }
      if ("timeDepCovData" %in% class(spec)) {
        if ("timeDepCovData" %in% class(private$.cov_data[[1]])) {
          spec$checkAgainst(private$.cov_data)
          if ("cohortData" %in% class(private$.cohort_data)) {
            spec$checkAgainst(private$.cohort_data)
          }
          spec.list <- list(spec)
          names(spec.list) <- spec$L_name
          spec.list <- c(private$.cov_data, spec.list)
          spec.list <- spec.list[order(names(spec.list))]
          private$.cov_data <- spec.list
        } else {
          if ("cohortData" %in% class(private$.cohort_data)) {
            spec$checkAgainst(private$.cohort_data)
          }
          spec.list <- list(spec)
          names(spec.list) <- spec$L_name
          private$.cov_data <- spec.list
        }
      }
      invisible(self) # critical to allow method chaining
    },
    construct = function(time_unit, ...) {

        self$setAlgoOptions(time_unit, ...)
        self$createIntervals()
        self$assignAC()
        self$assignL()
        self$imputeL()
        self$cleanUp()
        
        invisible(self)
    },
    setAlgoOptions = function(time_unit, ...){
        ## Validate named argument
        assert_that( length(time_unit)==1 && !is.na(time_unit) & class(time_unit)%in%c("integer","numeric")
                    && floor(time_unit)==time_unit , msg = "time_unit must be a non-missing integer" )
        assert_that( time_unit>0 , msg = "time_unit must be strictly positive" )    
        private$.time_unit <- as.integer(time_unit) 
        ## Handle all other arguments
        inputArgs <- list(...)
        if(length(inputArgs)>0){
            inputArgsName <- sort(names(inputArgs))
            requiredArgsName <- sort(c("first_exp_rule", "exp_threshold", "format", "dates"))
            ## Warm user of all ... arguments that will be ignored (i.e., all the ones not required by the doc)
            notSureWhat2toDoWith <- inputArgsName[!inputArgsName%in%requiredArgsName]
            if(length(notSureWhat2toDoWith)>0)warning("The following arguments will be ignored: "%+%notSureWhat2toDoWith%+%". ")
        }
        ## set required ... arguments to stated default
        private$.first_exp_rule <- 1
        private$.exp_threshold <- 0.5
        private$.format <- "standard"
        private$.dates <- FALSE
        ## Overwrite default values for required ... argument with user-provided ones but give error if provided values are not allowed
        if(length(inputArgs)>0){
            if("first_exp_rule"%in%inputArgsName){
                first_exp_rule <- inputArgs$first_exp_rule
                assert_that( length(first_exp_rule)==1 && class(first_exp_rule)%in%c("integer","numeric")
                            && first_exp_rule%in%c(0,1), msg = "first_exp_rule must be 0 or 1" )
                private$.first_exp_rule <- first_exp_rule
            }
            if("exp_threshold"%in%inputArgsName){
                exp_threshold <- inputArgs$exp_threshold
                assert_that( length(exp_threshold)==1 && !is.na(exp_threshold) & class(exp_threshold)%in%c("integer","numeric")
                            && (exp_threshold>=0 & exp_threshold<=1), msg = "exp_threshold must be a non-missing value between 0 and 1" )
                private$.exp_threshold <- exp_threshold
            }
            if("format"%in%inputArgsName){
                format <- inputArgs$format
                assert_that( length(format)==1 && class(format)%in%c("character")
                            && format%in%c("standard","MSM SAS macro"), msg = "format must be 'standard' or 'MSM SAS macro'" )
                private$.format <- format
            }
            if("dates"%in%inputArgsName){
                dates <- inputArgs$dates
                assert_that( length(dates)==1 && !is.na(dates) & class(dates)%in%c("logical"), msg = "dates must be TRUE or FALSE" )
                private$.dates <- dates
            }
        }
        invisible(self)
       },
    createIntervals = function() {

        time_unit <-  private$.time_unit
      ## Confirm that all time-dependent variables were added
      missingTimeDep <-   sort(private$.cohort_data$L0)[!sort(private$.cohort_data$L0)%in%sort(c(names(private$.cohort_data$L0_timeIndep),names(private$.cov_data)))]
      assertthat::assert_that( identical( sort(private$.cohort_data$L0),
                                         sort(c(names(private$.cohort_data$L0_timeIndep),names(private$.cov_data)))
                                         ),
                              msg = "Data construction cannot start until the measurements post index date for all time-dependent covariates included in the cohort dataset are specified. Either correct the value for L0_timeIndep used to specify the cohort dataset or add the time-dependent measurements for the following covariates: "%+%paste(missingTimeDep,collapse=", ")%+%".")
        
      ## Gather relevant elements for long-format construction (for readability)
      IDvar <- private$.cohort_data$IDvar
      index_date <- private$.cohort_data$index_date
      EOF_date <- private$.cohort_data$EOF_date
      EOF_type <- private$.cohort_data$EOF_type
      Y_name <- private$.cohort_data$Y_name
      outData <- private$.cohort_data$data[, c(
        IDvar, index_date, EOF_date,
        EOF_type
      ), with = FALSE]
      ## Construct long-format
      outData[, maxInt := ceiling((julian(get(EOF_date)) -
        julian(get(index_date)) + 1) / time_unit)]

      ## add extra row for outcome
      outData[get(EOF_type) == Y_name, maxInt := maxInt + 1]

      ## duplicate rows based on max f/up by patient
      outData <- outData[rep(1:.N, maxInt)][, intnum := 0:(.N - 1),
        by = eval(IDvar)
      ]
      ## set up start and end date for each consecutive bins of size t_unit
      outData[, intstart := get(index_date) + time_unit * intnum]
      outData[, intend := intstart + time_unit - 1]

      ## add outcome flag and clean-up
      outData[, outcome := 0]
      outData[get(EOF_type) == Y_name & (intnum + 1) == maxInt, outcome := 1]
      assert_that(identical(
        outData[get(EOF_type) == Y_name & (intnum + 1) ==
          (maxInt - 1), all(get(EOF_date) %within%
          interval(intstart, intend))],
        TRUE
      ))

      outData[, `:=`(
        c(index_date, EOF_date, EOF_type, "maxInt"),
        vector("list", 4)
      )]
      ## order by ID and intnum
      setkeyv(outData, c(IDvar, "intnum"))
      ## save long-format data structure
      private$.data <- outData
      ##
      invisible(self)
    },
    assignAC = function() {

      firs_exp_rule <- private$.first_exp_rule
      exp_threshold <- private$.exp_threshold
      # pointer to data sources for convenience
      exp_data <- private$.exp_data$data
      cohort_data <- private$.cohort_data$data
      outcome_data <- private$.data

      # pointers for exposure data
      start_date <- private$.exp_data$start_date
      end_date <- private$.exp_data$end_date
      exp_level <- private$.exp_data$exp_level
      exp_ref <- private$.exp_data$exp_ref

      # pointers for cohort data
      id_var <- private$.cohort_data$IDvar
      eof_date <- private$.cohort_data$EOF_date
      eof_type <- private$.cohort_data$EOF_type
      y_name <- private$.cohort_data$Y_name

      # Vectorized implementation (replaces per-subject future_lapply loop)
      data_assigned_ac <- .assignAC_vectorized(
        outcome_data, exp_data, cohort_data,
        id_var, start_date, end_date, exp_level,
        exp_ref, eof_date, eof_type, y_name,
        firs_exp_rule, exp_threshold
      )

      # Sort and set column order
      if (exp_data[, length(unique(get(exp_level)))] > 1) {
        data.table::setcolorder(data_assigned_ac, c(
          id_var, "intnum", "intstart", "intend",
          "exposure", "outcome", "censor",
          "case", eof_type, "tie", "A0.warn"
        ))
      } else {
        data.table::setcolorder(data_assigned_ac, c(
          id_var, "intnum", "intstart", "intend",
          "exposure", "outcome", "censor",
          "case", eof_type
        ))
      }
      data.table::setorderv(data_assigned_ac, id_var)[]

      # output
      private$.data <- data_assigned_ac
      invisible(self)

      # model_output <- expDT_15_f1_p75[, c(
      # "ID", "intnum", "intstart", "intend",
      # "exposure", "outcome", "censor",
      # "case", "EOFtype",
      # "daysexposed1", "intervalpercent1", "lastintpercent1"
      # ), with = FALSE]

      # print(sum(model_output$case != data_assigned_ac$case) /
      # length(data_assigned_ac$case) * 100)
    },
    assignL = function() {

      first_exp_rule <- private$.first_exp_rule
      # pointer to data sources for convenience
      outcome_data <- private$.data
      cohort_data <- private$.cohort_data$data
      exp_data <- private$.exp_data$data
      id_var <- private$.cohort_data$IDvar
      eof_date_var <- private$.cohort_data$EOF_date
      eof_type_var <- private$.cohort_data$EOF_type
      index_date_var <- private$.cohort_data$index_date
      start_date_var <- private$.exp_data$start_date
      end_date_var <- private$.exp_data$end_date
      A_level <- private$.exp_data$exp_level
      t_ind_cov <- private$.cohort_data$L0[!private$.cohort_data$L0%in%names(private$.cov_data)]

      # Eventually we will loop over elements of the covariate list:
      # private$.cov_data. For now, we focus on a single covariate of type
      # "sporadic": A1c in GS2

        ## looping over time-dependent covariates
        if(any(!is.na(private$.cov_data))){ ### ATTENTION: If code produced extra warnings, try !is.na(private$.cov_data)[1]
              
          for(cov_position in seq_along(private$.cov_data)){
              
            # extract data for the current covariate only
            cov_data <- private$.cov_data[[cov_position]]
            cov_name <- private$.cov_data[[cov_position]]$L_name
            cov_acute <- private$.cov_data[[cov_position]]$acute_change
            cov_date <- private$.cov_data[[cov_position]]$L_date

            # Vectorized implementation (replaces per-subject future_lapply loop)
            exp_ref <- private$.exp_data$exp_ref
            data_assigned_lt_by_id <- .assignL_vectorized(
              outcome_data, cohort_data, exp_data, cov_data,
              id_var, index_date_var, eof_date_var, eof_type_var,
              start_date_var, end_date_var, A_level,
              cov_name, cov_date, cov_acute,
              first_exp_rule, exp_ref
            )

            ## merge ID-specific data sets            
            if(cov_position==1){
              data_assigned_lt <- copy(data_assigned_lt_by_id)
              data.table::setcolorder(data_assigned_lt, c(id_var, "intnum", "intstart", "intend",
                                                          "exposure", "outcome", "censor",
                                                          "caseExp", eof_type_var, "case", cov_name, cov_date
              ))
              data.table::setnames(data_assigned_lt, cov_date,
                                   paste0("dt", cov_name))
            }else{
              data_assigned_lt_by_id <- data_assigned_lt_by_id[,c(id_var,"intnum",cov_name,cov_date),with=FALSE]
              data.table::setnames(data_assigned_lt_by_id, cov_date,
                                   paste0("dt", cov_name))
              data_assigned_lt <- data.table::merge.data.table(data_assigned_lt,data_assigned_lt_by_id,by=c(id_var,"intnum"),all=TRUE)
            }
          } # END: loop over covariates, i.e. end of for(cov_position in seq_along(private$.cov_data)){...
        } else { ## if there is no time-dep covariates, i.e. else following if(any(!is.na(private$.cov_data))){..}
          data_assigned_lt <- outcome_data
        }
    
      ## For loop for time-indepent
      data.table::setkeyv(data_assigned_lt, c(id_var,"intnum"))
      
      # Assign time-independent covariates
      t_ind_cov_data <- cohort_data[,c(id_var,t_ind_cov),with=FALSE]
      data_assigned_lt <- merge(data_assigned_lt, t_ind_cov_data, by=id_var, all.x = TRUE, all.y=FALSE)
      for(cov.i in t_ind_cov){
        data_assigned_lt[get(cov.i)%in%"",eval(cov.i):=NA]
      }

      # output
      private$.data <- data_assigned_lt
      invisible(self)
    },
    imputeL = function() {
        ## identify all time-dep and time-indep covariates
        if(any(!is.na(private$.cov_data))){ ### ATTENTION: If code produced extra warnings, try !is.na(private$.cov_data)[1]
          covariate_data <- c(private$.cov_data,private$.cohort_data$L0_timeIndep)
        } else{
          covariate_data <- private$.cohort_data$L0_timeIndep
        }
        #covariate_data <- c(private$.cov_data,private$.cohort_data$L0_timeIndep)

        ## Only sporadic time-dep and time-indep variables first need baseline imputation and imputation indicators
        ## All time-indep variables need LOVCF except "indicator" variables
        for(cov.i in seq_along(covariate_data)){
            cov_name <- names(covariate_data)[cov.i]
            ## Baseline imputation and creation of imputation indicator
            if(is.null(covariate_data[[cov.i]]$type) || covariate_data[[cov.i]]$type=="sporadic"){ # null is only possible for time-indep vars
                ## add indicator of imputation to output data
                private$.data[,"I."%+%cov_name:=0]
                private$.data[is.na(get(cov_name)),eval("I."%+%cov_name):=1]
                ## figure out baseline imputation value
                impute <- covariate_data[[cov.i]]$impute
                if(impute=="default") {
                    imputed_value <- covariate_data[[cov.i]]$impute_default_level
                } else if(impute=="mode"){
                    imputed_value <- private$.data[intnum==0 & !is.na(get(cov_name)),getmode(get(cov_name))]
                } else if(impute=="mean"){
                    imputed_value <- private$.data[intnum==0 & !is.na(get(cov_name)),mean(get(cov_name))]
                } else if(impute=="median"){
                    imputed_value <- private$.data[intnum==0 & !is.na(get(cov_name)),median(get(cov_name))]
                }
                ## impute baseline values 
                private$.data[intnum==0 & is.na(get(cov_name)) , eval(cov_name):=imputed_value]
            }
            if(!is.null(covariate_data[[cov.i]]$type)){ # null is only possible for time-indep vars
              ## Implement LOVCF 
              if(covariate_data[[cov.i]]$type!="indicator"){ 
                private$.data[, eval(cov_name):= get(cov_name)[1], by = .(get(private$.cohort_data$IDvar), cumsum(!is.na(get(cov_name)))) ]
                ## private$.data[,eval(cov_name):=zoo::na.locf(get(cov_name)),by=id_var]
              }else{
                if(private$.data[ , class(get(cov_name)) ]=="character")private$.data[ is.na(get(cov_name)) , eval(cov_name) := "None" ]
                else private$.data[ is.na(get(cov_name)) , eval(cov_name) := 0 ]
              }
            }else{
              ## Override missing time-independent values with imputed_value
              private$.data[get("I."%+%cov_name)==1, eval(cov_name) := imputed_value]
            }
        }
        ## reorder data before exiting
        data.table::setkeyv(private$.data, c(private$.cohort_data$IDvar,"intnum"))
        invisible(self)
    },
    cleanUp = function(){

        format <- private$.format
        dates <- private$.dates
        ## Create object that will be used to reorder covariates in a standardized fashion
        timeIndep <- names(private$.cohort_data$L0_timeIndep)
        names(timeIndep) <- timeIndep
        I.timeIndep <- "I."%+%timeIndep
        names(I.timeIndep) <- timeIndep%+%".1"
        timeIndepOrdered <- as.character(c(timeIndep,I.timeIndep)[sort(names(c(timeIndep,I.timeIndep)))])
            
        timeDep <- names(private$.cov_data)
        names(timeDep) <- timeDep
        dt.timeDep <- "dt"%+%timeDep
        names(dt.timeDep) <- timeDep%+%".1"
        I.timeDep <- "I."%+%timeDep
        names(I.timeDep) <- timeDep%+%".2"
        timeDepOrdered <- as.character(c(timeDep,dt.timeDep,I.timeDep)[sort(names(c(timeDep,dt.timeDep,I.timeDep)))])
        timeDepOrdered <- timeDepOrdered[timeDepOrdered%in%names(private$.data)]

        orderedCol <- c(private$.cohort_data$IDvar,"intnum","intstart","intend",private$.cohort_data$EOF_type,"outcome","censor",timeIndepOrdered,timeDepOrdered,"exposure")
        if(all(c("tie","A0.warn")%in%names(private$.data)))orderedCol <- c(orderedCol,"tie","A0.warn")
        
        if(any(!is.na(private$.cov_data))){ ### ATTENTION: If code produced extra warnings, try !is.na(private$.cov_data)[1]
            assert_that( all(sort(names(private$.data)[ !names(private$.data)%in%orderedCol ])==c("case","caseExp")) , msg = "some columns might need exporting")
        } else{
            assert_that( all(sort(names(private$.data)[ !names(private$.data)%in%orderedCol ])==c("case")) , msg = "some columns might need exporting")
        }
        private$.data <- suppressWarnings(private$.data[ , -c("caseExp","case"), with=FALSE])            
        
        ## Remove unneeded columns
         if(!dates){
            col2remove <- unlist(lapply(private$.data,class))=="Date"
            col2remove <- names(col2remove)[col2remove]
            private$.data <- private$.data[ , -col2remove, with=FALSE]
            orderedCol <- orderedCol[!orderedCol%in%c("intstart","intend",dt.timeDep)]
        }

        ## Reformat
        if(format=="MSM SAS macro"){
            ## remove patients censored in first f/up interval
            IDsCensoredBin0 <- private$.data[ censor==1 & intnum==0, unique(get(private$.cohort_data$IDvar)) ]
            private$.data <- private$.data[ !get(private$.cohort_data$IDvar)%in%IDsCensoredBin0,  ]
            ## shift outcome and censor up by one row
            private$.data[ , outcome := shift(outcome, 1L, fill=NA, type = "lead") , by = get(private$.cohort_data$IDvar) ]
            private$.data[ , censor := shift(censor, 1L, fill=NA, type = "lead") , by = get(private$.cohort_data$IDvar) ]
            ## remove last row of each ID
            private$.data <- private$.data[ , .SD[-.N,] , by = get(private$.cohort_data$IDvar) ][, -"get", with=FALSE ]
            ## set outcome to NA in last row when censor = 1
            private$.data[ censor %in% 1, outcome := NA ]
        }
        
        ## Reorder data before exiting
        setcolorder(private$.data, orderedCol)
        data.table::setkeyv(private$.data, c(private$.cohort_data$IDvar,"intnum"))
        invisible(self)
    }
  )
)

#' Data Storage Class for LtAt dataset based on instant exposure
#'
#'
#' Class that defines the specifications for constructing an LtAt dataset and the dataset itself
#' using an instant exposure dataset.
#'
#' @docType class
#'
#' @importFrom data.table foverlaps
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{LtAtDataInst} object
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Fields:
#' \describe{
#'     \item{\code{data}:}{\code{data.table} containing
#'           the LtAt dataset. NULL by default before construction.
#'     }
#'     \item{\code{cohort_data}:}{\code{cohortData} specifying the cohort dataset.
#'     }
#'     \item{\code{exp_data}:}{\code{expData} specifying the time-varying exposure dataset.
#'           Its \code{IDvar} field must contain the same value as that of the
#'           \code{cohort_data} (i.e.,
#'           the same column name must be used for the ID column of the exposure and cohort
#'           datasets). The unique subject identifiers in the \code{data} field of \code{exp_data}
#'           must be a subset of those founds in the \code{data} field of \code{cohort_data} (i.e.,
#'           all IDs in the exposure dataset must also be present in the cohort dataset.)
#'           An exposure measurement stored in the \code{data} field of \code{exp_data}
#'           will be ignored
#'           if its date is either strictly before the subject's index date stored in the
#'           \code{data} field of \code{cohort_data} or strictly after the subject's end of
#'           follow-up date stored in the \code{data} field of \code{cohort_data} (i.e., only
#'           exposure measurements collected between the start and end of follow-up will be
#'           retained).
#'     }
#'     \item{\code{cov_data}:}{a list of \code{timeDepCovData} that
#'           specifies time-varying covariate(s). Each element of the list must have a distinct
#'           value stored in its \code{L_name} field and this value must also be found in the
#'           \code{L0} field of \code{cohort_data} (i.e., time-varying covariates cannot have
#'           the same name and must have their baseline measurements stored in the cohort dataset).
#'           The \code{IDvar} field of each element \code{timeDepCovData} of \code{cov_data}
#'           must contain the same value as
#'           that of the \code{cohort_data} (i.e.,
#'           the same column name must be used for the ID column of each covariate dataset and
#'           that of the cohort dataset). The unique subject identifiers in the \code{data}
#'           field of each element
#'           \code{timeDepCovData} of \code{cov_data}
#'           must be a subset of those founds in the \code{data} field of \code{cohort_data} (i.e.,
#'           all IDs in a covariate dataset must also be present in the cohort dataset.)
#'           A covariate measurement stored in the \code{data} field of an element
#'           \code{timeDepCovData}
#'           of \code{cov_data} will be ignored
#'           if its date is either strictly before the subject's index date stored in the
#'           \code{data} field of \code{cohort_data} or strictly after the subject's end of
#'           follow-up date stored in the \code{data} field of \code{cohort_data}. (i.e., only
#'           covariate measurements collected between the start and end of follow-up will be
#'           retained).
#'     }
#'    }

LtAtDataInst <- R6::R6Class(
  classname = "LtAtDataInst",                       
  inherit = LtAtData,
  private = list(
      .max_exp_var = NA,
      .max_cov_var = NA,
      .summary_cov_var = NA
  ),
  active = list(
      max_exp_var = readOnly(".max_exp_var"),
      max_cov_var = readOnly(".max_cov_var"),
      summary_cov_var = readOnly(".summary_cov_var")
  ),
  public = list(
    initialize = function() {
      private$.max_exp_var <- NA
      private$.max_cov_var <- NA
      private$.summary_cov_var <- NA
      invisible(self) # critical to allow method chaining
    },
    copyLtAtData = function(LtAtObject) {
        assert_that("LtAtData"%in%class(LtAtObject) & !"LtAtDataInst"%in%class(LtAtObject))
        private$.data <- LtAtObject$data
        private$.cohort_data <- LtAtObject$cohort_data
        private$.exp_data <- LtAtObject$exp_data
        private$.cov_data <- LtAtObject$cov_data
        private$.summary_cov_var <- LtAtObject$summary_cov_var
        private$.time_unit <- LtAtObject$time_unit
        private$.first_exp_rule <- LtAtObject$first_exp_rule
        private$.exp_threshold <- LtAtObject$exp_threshold
        private$.format <- LtAtObject$format
        private$.dates <- LtAtObject$dates
        invisible(self) # critical to allow method chaining
    },
    construct = function(time_unit, ...) {

        self$setAlgoOptions(time_unit, ...)
        super$createIntervals()
        self$assignAC()
        self$assignL()
        super$imputeL()
        self$cleanUp()
        
        invisible(self)
    },
    setAlgoOptions = function(time_unit, ...){
        ## Validate named argument
        assert_that( length(time_unit)==1 && !is.na(time_unit) & class(time_unit)%in%c("integer","numeric")
                    && floor(time_unit)==time_unit , msg = "time_unit must be a non-missing integer" )
        assert_that( time_unit>0 , msg = "time_unit must be strictly positive" )    
        private$.time_unit <- as.integer(time_unit) 
        ## Handle all other arguments
        inputArgs <- list(...)
        if(length(inputArgs)>0){
            inputArgsName <- sort(names(inputArgs))
            requiredArgsName <- sort(c("max_exp_var", "max_cov_var","summary_cov_var", "format", "dates"))
            ## Warm user of all ... arguments that will be ignored (i.e., all the ones not required by the doc)
            notSureWhat2toDoWith <- inputArgsName[!inputArgsName%in%requiredArgsName]
            if(length(notSureWhat2toDoWith)>0)warning("The following arguments will be ignored: "%+%notSureWhat2toDoWith%+%". ")
        }
        ## set required ... arguments to stated default
        private$.max_exp_var <- 100L
        private$.max_cov_var <- 100L
        private$.summary_cov_var <- "last"
        private$.format <- "standard"
        private$.dates <- FALSE
        ## Overwrite default values for required ... argument with user-provided ones but give error if provided values are not allowed
        if(length(inputArgs)>0){
            if("max_exp_var"%in%inputArgsName){
                max_exp_var <- inputArgs$max_exp_var
                assert_that( length(max_exp_var)==1 && !is.na(max_exp_var) & class(max_exp_var)%in%c("integer","numeric")
                            && floor(max_exp_var)==max_exp_var , msg = "max_exp_var must be a non-missing integer" )
                assert_that( max_exp_var>0 , msg = "max_exp_var must be strictly positive" )    
                max_exp_var <- as.integer(max_exp_var) 
                private$.max_exp_var <- max_exp_var
            }
            if("max_cov_var"%in%inputArgsName){
                max_cov_var <- inputArgs$max_cov_var
                assert_that( length(max_cov_var)==1 && !is.na(max_cov_var) & class(max_cov_var)%in%c("integer","numeric")
                            && floor(max_cov_var)==max_cov_var , msg = "max_cov_var must be a non-missing integer" )
                assert_that( max_cov_var>0 , msg = "max_cov_var must be strictly positive" )    
                max_cov_var <- as.integer(max_cov_var) 
                private$.max_cov_var <- max_cov_var
            }
            if("summary_cov_var"%in%inputArgsName){
                summary_cov_var <- inputArgs$summary_cov_var
                assert_that( length(summary_cov_var)==1 && class(summary_cov_var)%in%c("character")
                            && summary_cov_var%in%c("last","all"), msg = "summary_cov_var must be 'last' or 'all'" )
                private$.summary_cov_var <- summary_cov_var
            }
            if("format"%in%inputArgsName){
                format <- inputArgs$format
                assert_that( length(format)==1 && class(format)%in%c("character")
                            && format%in%c("standard","MSM SAS macro"), msg = "format must be 'standard' or 'MSM SAS macro'" )
                private$.format <- format
            }
            if("dates"%in%inputArgsName){
                dates <- inputArgs$dates
                assert_that( length(dates)==1 && !is.na(dates) & class(dates)%in%c("logical"), msg = "dates must be TRUE or FALSE" )
                private$.dates <- dates
            }
        }
        invisible(self)
    },
    assignAC = function() {

        max_exp_var <- private$.max_exp_var
        ## pointer to data sources for convenience
        exp_data <- private$.exp_data$data
        cohort_data <- private$.cohort_data$data
        outcome_data <- private$.data

        ## pointers for exposure data
        start_date <- private$.exp_data$start_date
        exp_level <- private$.exp_data$exp_level

        ## pointers for cohort data
        id_var <- private$.cohort_data$IDvar
        eof_type <- private$.cohort_data$EOF_type
        
        index_date <- private$.cohort_data$index_date
        time_unit <- as.numeric(outcome_data[1,intend-intstart+1])
        data_assigned_ac <- merge(exp_data, cohort_data[, c(id_var,index_date), with = FALSE], by = id_var, all.x = TRUE,all.y = FALSE) # data_assigned_ac <- merge(exp_data,cohort_data[,c(id_var,index_date,eof_type),with=FALSE],by=id_var,all.x=TRUE,all.y=FALSE)
        outcome_data <- merge(outcome_data, cohort_data[, c(id_var,eof_type), with = FALSE], by = id_var, all.x = TRUE, all.y = FALSE)
        ## Assign each exposure observation to the interval number in which it occurs
        data_assigned_ac[,intnum:=as.numeric(ceiling((get(start_date)-get(index_date)+1)/time_unit-1))]
        data_assigned_ac[,eval(index_date):=NULL]
        ## Validate correct intnum assignment - only run if need to debug:
        ## data_assigned_ac <- merge(data_assigned_ac,outcome_data[,c(id_var,"intnum","intstart","intend"),with=FALSE],by=c(id_var,"intnum"),all.x=TRUE,all.y=FALSE)
        ## assert_that(data_assigned_ac[,all(get(start_date)>=intstart & get(start_date)<=intend)])

        ## assign C
        outcome_data[,maxIntnum:=max(intnum),by=id_var]
        outcome_data[,censor:=0] # initialize
        outcome_data[intnum==maxIntnum & outcome==0, censor:=1]
        outcome_data[,maxIntnum:=NULL]
        
        ## Merge exposure information to outcome data and reformat from long format (repeated exposures by interval) into wide format (1 obs per ID and intnum)
        data_assigned_ac <- merge(outcome_data,data_assigned_ac,by=c(id_var,"intnum"),all.x=TRUE,all.y=FALSE)
        ## Identify maximum number of exposure per ID and intnum and order data by start_date and exp_level within ID and intnum so that wide format correctly captures temporal ordering of fills in each interval
        setkeyv(data_assigned_ac, c(id_var,"intnum",start_date,exp_level))
        data_assigned_ac[,cntExp:=(1:.N),by=c(id_var,"intnum")]
        data_assigned_ac[,maxExp:=max(cntExp),by=c(id_var,"intnum")]
        nExp <- data_assigned_ac[,max(cntExp)]
        ## Define total number of exposures and fill out their values after figuring out what class each exposure column is
        NAexp <- c("NA_integer_","as_date(NA)")
        for(exp.i in exp_level)NAexp <- c(NAexp, ifelse(is.integer(data_assigned_ac[,get(exp.i)]),"NA_integer_", ifelse(is.character(data_assigned_ac[,get(exp.i)]), "NA_character_", "NA_real_")))
        initializeExp <- eval(parse(text=("data.table("%+%paste(rep(NAexp,nExp),collapse=",")%+%")")))
        assert_that(ncol(initializeExp)<=max_exp_var, msg = "The number of exposure variables required to encode all possible exposure levels exceed the maximum number allowed, i.e. "%+%max_exp_var%+%". If this is expected, set 'max_exp_var' to a larger value to prevent this error message.")
        data_assigned_ac[,`:=`( c("exposure",start_date,exp_level)%+%"."%+%rep(1:nExp,each=length(exp_level)+2), initializeExp )]
        assert_that(identical(key(data_assigned_ac),c(id_var,"intnum",start_date,exp_level)))
        for(i.exp in 1:nExp){
            data_assigned_ac[cntExp==i.exp, "exposure."%+%i.exp := ifelse(is.na(get(start_date)), 0L, 1L)]
            data_assigned_ac[cntExp==i.exp & get("exposure."%+%i.exp)==1L , `:=`( start_date%+%"."%+%i.exp , get(start_date) ) ]
            data_assigned_ac[cntExp==i.exp & get("exposure."%+%i.exp)==1L , `:=`( exp_level%+%"."%+%i.exp , mget(exp_level) ) ]
            if(i.exp>1)for(i.level in c("exposure",start_date,exp_level)){
                           data_assigned_ac[ maxExp>=i.exp & cntExp<=i.exp , LOVCB := rev(cumsum(!is.na(rev(get(i.level%+%"."%+%i.exp))))) , by = c(id_var, "intnum") ]
                           data_assigned_ac[ maxExp>=i.exp & cntExp<=i.exp , i.level%+%"."%+%i.exp := get(i.level%+%"."%+%i.exp)[.N] , by = c(id_var, "intnum", "LOVCB") ] ## implement last observed value carried backward ny ID and intnum so that we can dedup later on each column
                       }
        }
        data_assigned_ac <- data_assigned_ac[cntExp==1,] # dedup
        assert_that(nrow(data_assigned_ac)==nrow(outcome_data))
        for(i.exp in 1:nExp)data_assigned_ac[ is.na(get("exposure."%+%i.exp)), "exposure."%+%i.exp := 0L]
        cleanUp <- c(exp_level,start_date,"cntExp")
        if(nExp>1)cleanUp <- c(cleanUp,"LOVCB")
        data_assigned_ac[,`:=`( eval(cleanUp) , vector("list",length(cleanUp)) )]
        ## replace maxExp so it encodes the number of non-reference level(s) experience at time t
        data_assigned_ac[maxExp%in%1 & exposure.1%in%0, maxExp:=0]
        ## output
        private$.data <- data_assigned_ac
        invisible(self)
    },
    assignL = function() {

        max_cov_var <- private$.max_cov_var
        ## pointer to data sources for convenience
        outcome_data <- private$.data
        cohort_data <- private$.cohort_data$data
        id_var <- private$.cohort_data$IDvar
        eof_date_var <- private$.cohort_data$EOF_date
        eof_type_var <- private$.cohort_data$EOF_type
        index_date_var <- private$.cohort_data$index_date
        start_date_var <- private$.exp_data$start_date
        t_ind_cov <- private$.cohort_data$L0[!private$.cohort_data$L0%in%names(private$.cov_data)]

        ## looping over time-dependent covariates
        if(any(!is.na(private$.cov_data))){ ### ATTENTION: If code produced extra warnings, try !is.na(private$.cov_data)[1]
        data_assigned_lt_bycov <- future.apply::future_lapply(seq_along(private$.cov_data),function(cov_position){
            ## for(cov_position in seq_along(private$.cov_data)){
                
                ## extract data for the current covariate only
                cov_data <- private$.cov_data[[cov_position]]
                cov_name <- private$.cov_data[[cov_position]]$L_name
                cov_acute <- private$.cov_data[[cov_position]]$acute_change
                cov_date <- private$.cov_data[[cov_position]]$L_date

                time_unit <- as.numeric(outcome_data[1,intend-intstart+1])
                ## Define for each interval the window of time for which L(t)'s should be searched for each subject
                assignment_conditions <- outcome_data[,c(id_var,"intnum","intstart","intend","outcome","censor","exposure.1",start_date_var %+% ".1"), with=FALSE]
                assignment_conditions <- merge(assignment_conditions,cohort_data[,c(id_var,eof_date_var),with=FALSE],by=id_var,all.x=TRUE,all.y=FALSE)
                ## handle t==0
                assignment_conditions[ intnum==0 , Ldt_low:=intstart ]
                assignment_conditions[ intnum==0 & exposure.1==1, Ldt_up:= pmax( Ldt_low, get(start_date_var %+% ".1")-as.numeric(cov_acute) ) ]
                assignment_conditions[ intnum==0 & exposure.1==0, Ldt_up:= intstart ]
                assignment_conditions[ intnum==0 & censor%in%1 , Ldt_up:= pmax( Ldt_low, get(eof_date_var)-as.numeric(cov_acute) )]
                ## handle t>0
                assignment_conditions[ intnum>0 , Ldt_low:=intstart-time_unit ]
                assignment_conditions[ intnum>0 & exposure.1==1, Ldt_up:= pmax( Ldt_low, get(start_date_var %+% ".1")-as.numeric(cov_acute) ) ]
                assignment_conditions[ intnum>0 & exposure.1==0, Ldt_up:= intstart-1 ]
                assignment_conditions[ intnum>0 & censor%in%1 , Ldt_up:= pmax( Ldt_low, get(eof_date_var)-as.numeric(cov_acute) ) ]
                ## search window for when outcome==1
                assignment_conditions[ outcome%in%1 , Ldt_low:=intstart-time_unit ]
                assignment_conditions[ outcome%in%1, Ldt_up := get(eof_date_var) ]
                ## finalize search windows
                assignment_conditions <- assignment_conditions[, c(id_var,"intnum","Ldt_low","Ldt_up"), with=FALSE]
                assert_that(assignment_conditions[,all(Ldt_low<=Ldt_up)])

                ## Bring in covariate data (including baseline value from cohort, note that we might end up with 2 measures on index, will
                ## need to order these 2 as cohort first then time-dep measurement next)
                covData2 <- cohort_data[, c(id_var, index_date_var ,cov_name), with=FALSE]
                covData2[, fromTimeDep:=0]
                setnames(covData2, index_date_var, cov_date)
                covData <- copy(cov_data$data)
                covData[, fromTimeDep:=1]
                setcolorder(covData, c(id_var, cov_date ,cov_name, "fromTimeDep"))
                covData <- rbind(covData,covData2)
                covData[,cov_date%+%"_cp":=get(cov_date)]
                setkeyv(covData ,c(id_var,cov_date,cov_date%+%"_cp"))
                assignment_conditions <- data.table::foverlaps(assignment_conditions, covData, by.x=c(id_var,"Ldt_low", "Ldt_up"), by.y=c(id_var,cov_date,cov_date%+%"_cp"), type="any", mult="all", nomatch=NA)
                assignment_conditions[,cov_date%+%"_cp":=NULL]
                assert_that(assignment_conditions[ !is.na(get(cov_date)) , all(get(cov_date)>=Ldt_low)])
                assert_that(assignment_conditions[ !is.na(get(cov_date)) , all(get(cov_date)<=Ldt_up)])
                ## Remove covariate values assigned more than once (remove all but the first of the redundant information)
                setkeyv(assignment_conditions, c(id_var,"intnum"))
                assignment_conditions[ !is.na(get(cov_date)) , ncount:=1:.N, by=c(id_var,cov_date,cov_name)]
                ## validate that redundant assignment of same L indexed by ncount>1 have larger intnum values that for the first assignment of L with ncount=1
                assignment_conditions[ !is.na(get(cov_date)) , intcheck := as.integer(intnum[1]+(ncount-1)), by=c(id_var,cov_date,cov_name)]
                assert_that(identical(assignment_conditions[ !is.na(get(cov_date)) , intcheck],assignment_conditions[ !is.na(get(cov_date)) , intnum] ))
                assignment_conditions[ , intcheck:=NULL ]
                ## last assert indicates that we can delete redudant L assignent in bins with ncount>1
                assignment_conditions[ !is.na(get(cov_date)) & ncount>1, `:=`( c(cov_date, cov_name), NA ) ]
                assignment_conditions[ , ncount:=NULL ]
                ## remove rows with cov_date = NA if indexed by intnum value for that patient has already has another row with cov_date!=NA
                assignment_conditions[  , ncount:=.N, by=c(id_var,"intnum")]
                assignment_conditions[ ncount>1 & is.na(get(cov_date)), ncountNA:=.N, by=c(id_var,"intnum") ]
                ## remove NA rows for (ID,t) combo that have strictly fewer NA rows for L than total number of rows
                removeNAs <- assignment_conditions[ , which(ncount>1 & is.na(get(cov_date)) & ncount>ncountNA) ]
                if(length(removeNAs)>0)assignment_conditions <- assignment_conditions[ -removeNAs ,]
                assignment_conditions[, rowKeep:=1] # initialize
                if(nrow(assignment_conditions[ ncount>1 & is.na(get(cov_date)) & ncount==ncountNA])>0)assignment_conditions[ ncount>1 & is.na(get(cov_date)) & ncount==ncountNA, rowKeep:=1:.N , by=c(id_var,"intnum") ]
                ## remove all but 1 of the NA rows for (ID,t) combo that have all NA rows for L
                removeNAs <- assignment_conditions[ , which(ncount>1 & is.na(get(cov_date)) & ncount==ncountNA  & rowKeep!=1)]
                if(length(removeNAs)>0)assignment_conditions <- assignment_conditions[ -removeNAs ,]
                ## Confirm that no (ID,t) combo with >1 row of L's have a row with L=NA
                assignment_conditions[  , ncount:=.N, by=c(id_var,"intnum")] ## need to recompute ncount since deleted some rows
                if(assignment_conditions[ ncount>1,.N]>0)assert_that(identical(unique(assignment_conditions[ ncount>1 , sum(is.na(get(cov_date))), by=c(id_var,"intnum")]$V1),0L))
                assignment_conditions[  , `:=`( c("ncount", "ncountNA","rowKeep"), vector("list",3) )]
                ## Dedup and check nrow is same as outcome_data
                assert_that(identical(assignment_conditions[fromTimeDep==0 & !is.na(get(cov_date)),unique(intnum)],0L))
                ## Identify maximum number of covariates per ID and intnum and order data by cov_date and fromTimeDep within ID,intnum so that wide format correctly captures temporal ordering of fills in each interval
                setkeyv(assignment_conditions, c(id_var,"intnum",cov_date,"fromTimeDep"))
                assignment_conditions[,cntCov:=(1:.N),by=c(id_var,"intnum")]
                assignment_conditions[,maxCov:=max(cntCov),by=c(id_var,"intnum")]
                nCov <- assignment_conditions[,max(cntCov)]
                ## Define total number of covariates and fill out their values after figuring out the class of the covariate column
                NAcov <- c("as_date(NA)", ifelse(is.integer(assignment_conditions[,get(cov_name)]),"NA_integer_", ifelse(is.character(assignment_conditions[,get(cov_name)]), "NA_character_", "NA_real_")) )
                initializeCov <- eval(parse(text=("data.table("%+%paste(rep(NAcov,nCov),collapse=",")%+%")")))
                assert_that(ncol(initializeCov)<=max_cov_var, msg = "The number of covariate variables required to encode all possible "%+%cov_name%+%" covariate levels exceed the maximum number allowed, i.e. "%+%max_cov_var%+%". If this is expected, set 'max_cov_var' to a larger value to prevent this error message.")
                assignment_conditions[,`:=`( c(cov_date,cov_name)%+%"."%+%rep(1:nCov,each=2), initializeCov )]
                assert_that(identical(key(assignment_conditions),c(id_var,"intnum",cov_date,"fromTimeDep")))
                for(i.cov in 1:nCov){
                    assignment_conditions[cntCov==i.cov & !is.na(get(cov_date)) , `:=`( cov_date%+%"."%+%i.cov , get(cov_date) ) ]
                    assignment_conditions[cntCov==i.cov & !is.na(get(cov_date)) , `:=`( cov_name%+%"."%+%i.cov , get(cov_name) ) ]
                    if(i.cov>1)for(i.level in c(cov_name,cov_date)){
                                   assignment_conditions[ maxCov>=i.cov & cntCov<=i.cov , LOVCB := rev(cumsum(!is.na(rev(get(i.level%+%"."%+%i.cov))))) , by = c(id_var, "intnum") ]
                                   assignment_conditions[ maxCov>=i.cov & cntCov<=i.cov , i.level%+%"."%+%i.cov := get(i.level%+%"."%+%i.cov)[.N] , by = c(id_var, "intnum", "LOVCB") ] ## implement last observed value carried backward by ID and intnum so that we can dedup later on each column
                               }
                }
                ## Set cov_name and cov_date to last value assigned to interval to match old LtAt approach
                assignment_conditions[ , `:=`( c(cov_date, cov_name), list(get(cov_date)[.N],get(cov_name)[.N]) ), by=c(id_var,"intnum") ]
                assignment_conditions <- assignment_conditions[cntCov==1,] # dedup
                ## Validate that we picked the last cov measurement for that bin
                for(i.cov in 1:nCov)assert_that( assignment_conditions[ !is.na(get(cov_date)) & !is.na(get(cov_date%+%"."%+%i.cov)) , all(get(cov_date)>=get(cov_date%+%"."%+%i.cov)) ])
                assignment_conditions[maxCov%in%1 & is.na(get(cov_name)), maxCov:=0] ## replace maxCov so it encodes the number of measurement at time t
                setnames(assignment_conditions, "maxCov", "max."%+%cov_name)
                assignment_conditions[,`:=`( c("fromTimeDep","cntCov","Ldt_low","Ldt_up") , vector("list",4) )]
                if("LOVCB"%in%names(assignment_conditions))assignment_conditions[, "LOVCB":=NULL]
                setkeyv(assignment_conditions,c(id_var,"intnum"))

                ## merge this cov data with prior assembled data
                assert_that( identical(assignment_conditions[,c(id_var,"intnum"), with=FALSE],outcome_data[,c(id_var,"intnum"), with=FALSE]) )
                ## if(cov_position==1){
                ##     data_assigned_lt <- merge(outcome_data, assignment_conditions, by=c(id_var,"intnum"), all=TRUE)
                ##     expNames <- names(outcome_data)[!names(outcome_data)%in%c(id_var, "intnum", "intstart", "intend","outcome", "censor",eof_type_var)]
                ##     covNames <- names(assignment_conditions)[!names(assignment_conditions)%in%c(id_var, "intnum", cov_name, cov_date)]
                ##     data.table::setcolorder(data_assigned_lt, c(id_var, "intnum", "intstart", "intend",
                ##                                                 expNames, "outcome", "censor",eof_type_var, 
                ##                                                 cov_name, cov_date, covNames
                ##                                                 ))
                ##     data.table::setnames(data_assigned_lt, c(cov_date,covNames[grep(cov_date,covNames)]) ,
                ##                          c("dt"%+%cov_name,"dt"%+%cov_name%+%gsub(cov_date,"",covNames[grep(cov_date,covNames)])) )
                ## }else{
                ##     covNames <- names(assignment_conditions)[!names(assignment_conditions)%in%c(id_var, "intnum", cov_name, cov_date)]
                ##     data.table::setnames(assignment_conditions, c(cov_date,covNames[grep(cov_date,covNames)]) ,
                ##                          c("dt"%+%cov_name,"dt"%+%cov_name%+%gsub(cov_date,"",covNames[grep(cov_date,covNames)])) )
                ##     assert_that( all.equal(assignment_conditions[,c(id_var,"intnum"), with=FALSE],data_assigned_lt[,c(id_var,"intnum"), with=FALSE]) )
                ##     data_assigned_lt <- data.table::merge.data.table(data_assigned_lt,assignment_conditions,by=c(id_var,"intnum"),all=TRUE)
                ## }
                return(assignment_conditions)
            }) # END: loop over covariates, i.e. end of future_lapply or for(cov_position in seq_along(private$.cov_data)){...
        ## combined list of data.tables together and sort by ID
        for(cov_position in seq_along(private$.cov_data)){
            ## extract data for the current covariate only
            cov_name <- private$.cov_data[[cov_position]]$L_name
            cov_date <- private$.cov_data[[cov_position]]$L_date
            
            if(cov_position==1){
                data_assigned_lt <- merge(outcome_data, data_assigned_lt_bycov[[cov_position]], by=c(id_var,"intnum"), all=TRUE)
                expNames <- names(outcome_data)[!names(outcome_data)%in%c(id_var, "intnum", "intstart", "intend","outcome", "censor",eof_type_var)]
                covNames <- names(data_assigned_lt_bycov[[cov_position]])[!names(data_assigned_lt_bycov[[cov_position]])%in%c(id_var, "intnum", cov_name, cov_date)]
                data.table::setcolorder(data_assigned_lt, c(id_var, "intnum", "intstart", "intend",
                                                            expNames, "outcome", "censor",eof_type_var, 
                                                            cov_name, cov_date, covNames
                                                            ))
                data.table::setnames(data_assigned_lt, c(cov_date,covNames[grep(cov_date,covNames)]) ,
                                     c("dt."%+%cov_name,"dt."%+%cov_name%+%gsub(cov_date,"",covNames[grep(cov_date,covNames)])) )
            }else{
                covNames <- names(data_assigned_lt_bycov[[cov_position]])[!names(data_assigned_lt_bycov[[cov_position]])%in%c(id_var, "intnum", cov_name, cov_date)]
                data.table::setnames(data_assigned_lt_bycov[[cov_position]], c(cov_date,covNames[grep(cov_date,covNames)]) ,
                                     c("dt."%+%cov_name,"dt."%+%cov_name%+%gsub(cov_date,"",covNames[grep(cov_date,covNames)])) )
                assert_that( identical(data_assigned_lt_bycov[[cov_position]][,c(id_var,"intnum"), with=FALSE],data_assigned_lt[,c(id_var,"intnum"), with=FALSE]) )
                data_assigned_lt <- data.table::merge.data.table(data_assigned_lt,data_assigned_lt_bycov[[cov_position]],by=c(id_var,"intnum"),all=TRUE)
            }
        }
        
        } else { ## if there is no time-dep covariates, i.e. else following if(any(!is.na(private$.cov_data))){..}
            data_assigned_lt <- outcome_data
        }
        
        ## For loop for time-indepent
        data.table::setkeyv(data_assigned_lt, c(id_var,"intnum"))
        
        ## Assign time-independent covariates
        t_ind_cov_data <- cohort_data[,c(id_var,t_ind_cov),with=FALSE]
        data_assigned_lt <- merge(data_assigned_lt, t_ind_cov_data, by=id_var, all.x = TRUE, all.y=FALSE)
        for(cov.i in t_ind_cov){
            data_assigned_lt[get(cov.i)%in%"",eval(cov.i):=NA]
        }

        ## output
        private$.data <- data_assigned_lt
        invisible(self)
    },
    cleanUp = function(){

        summary_cov_var <- private$.summary_cov_var
        format <- private$.format
        dates <- private$.dates
        ## Create object that will be used to reorder covariates in a standardized fashion
        timeIndep <- names(private$.cohort_data$L0_timeIndep)
        names(timeIndep) <- timeIndep
        I.timeIndep <- "I."%+%timeIndep
        names(I.timeIndep) <- timeIndep%+%".1"
        timeIndepOrdered <- as.character(c(timeIndep,I.timeIndep)[sort(names(c(timeIndep,I.timeIndep)))])
            
        timeDep <- names(private$.cov_data)
        if(length(timeDep)>0){
            names(timeDep) <- timeDep
            dt.timeDep <- "dt."%+%timeDep
            names(dt.timeDep) <- timeDep%+%".1"
            I.timeDep <- "I."%+%timeDep
            names(I.timeDep) <- timeDep%+%".2"
            timeDepOrdered <- as.character(c(timeDep,dt.timeDep,I.timeDep)[sort(names(c(timeDep,dt.timeDep,I.timeDep)))])
            timeDepOrdered <- timeDepOrdered[timeDepOrdered%in%names(private$.data)]


            maxVal <- unlist(lapply(private$.data[, mget("max."%+%timeDep)],max))
            other.timeDep <- unlist(sapply(timeDep, function(x,y){
                res <- c( "max."%+%x, rep( c(x, "dt."%+%x), y["max."%+%x] )%+%"."%+%rep( 1:y["max."%+%x], each=2) )
                return(res)
            },y=maxVal,simplify=FALSE,USE.NAMES=FALSE),use.names=FALSE)
            timeDepOrdered <- c(timeDepOrdered,other.timeDep)
            timeDepOrdered <- timeDepOrdered[timeDepOrdered%in%names(private$.data)]
        }else{
            timeDepOrdered <- NULL
            other.timeDep <- NULL
        }
        expNames <- c("exposure",private$.exp_data$start_date,private$.exp_data$exp_level)%+%"."%+%rep(1:private$.data[, max(maxExp)],each=length(private$.exp_data$exp_level)+2)
        orderedCol <- c(private$.cohort_data$IDvar,"intnum","intstart","intend",private$.cohort_data$EOF_type,"outcome","censor",timeIndepOrdered,timeDepOrdered,"maxExp",expNames)
        assert_that( length(names(private$.data)[ !names(private$.data)%in%orderedCol ])==0 , msg = "some columns might need exporting")
        
        ## Remove unneeded columns
        col2remove <- ""
        if(!dates){
            col2remove <- unlist(lapply(private$.data,class))=="Date"
            col2remove <- names(col2remove)[col2remove]
            private$.data <- private$.data[ , -col2remove, with=FALSE]
            orderedCol <- orderedCol[!orderedCol%in%col2remove]
         }
        if(!is.null(other.timeDep) & summary_cov_var=="last"){
            col2remove2 <- other.timeDep[!other.timeDep%in%col2remove]
            private$.data <- private$.data[ , -col2remove2, with=FALSE]
            orderedCol <- orderedCol[!orderedCol%in%col2remove2]
        }

        ## Reformat
        if(format=="MSM SAS macro"){
            ## remove patients censored in first f/up interval
            IDsCensoredBin0 <- private$.data[ censor==1 & intnum==0, unique(get(private$.cohort_data$IDvar)) ]
            private$.data <- private$.data[ !get(private$.cohort_data$IDvar)%in%IDsCensoredBin0,  ]
            ## shift outcome and censor up by one row
            private$.data[ , outcome := shift(outcome, 1L, fill=NA, type = "lead") , by = get(private$.cohort_data$IDvar) ]
            private$.data[ , censor := shift(censor, 1L, fill=NA, type = "lead") , by = get(private$.cohort_data$IDvar) ]
            ## remove last row of each ID
            private$.data <- private$.data[ , .SD[-.N,] , by = get(private$.cohort_data$IDvar) ][, -"get", with=FALSE ]
            ## set outcome to NA in last row when censor = 1
            private$.data[ censor %in% 1, outcome := NA ]
        }
        
        ## Reorder data before exiting
        setcolorder(private$.data, orderedCol)
        data.table::setkeyv(private$.data, c(private$.cohort_data$IDvar,"intnum"))
        invisible(self)
    }    
  )
)
