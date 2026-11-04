# ==============================================================================
# 4. HIGH-PERFORMANCE DATA WRANGLING & EDA WITH DATA.TABLE
# ==============================================================================
# OUTLINE:
#   1. The data.table Mental Model: DT[i, j, by]
#   2. Zero-Copy Ingestion: setDT(), setnames(), tables()
#   3. Reference Semantics: The let Operator & Memory Management
#   4. High-Performance Date Parsing: as.IDate vs as.POSIXct
#   5. Row Filtering (i): Expressions, %between%, and %chin%
#   6. Column Selection & Summaries (j): .(), .N, uniqueN()
#   7. Grouped Computations: by vs. keyby
#   8. Advanced Group-Level Operations: .SD and .SDcols
#   9. Fast Binary Search Lookups: setkey() & Keys
#  10. Bidirectional Reshaping: dcast() and melt() (with patterns())
#  11. Time Series Windows: shift(), frollmean(), and Missing Gap Detection
#  12. Table Joins: Syntax DT[X, on = ...] vs merge()
# ==============================================================================

# Load required package
# install.packages("data.table")
library(data.table)


# ==============================================================================
# 1. THE CORE MENTAL MODEL & ZERO-COPY INGESTION
# ==============================================================================

## THE DATA.TABLE QUERY PARADIGM:
#
#       DT[    i,            j,             by      ]
#              |             |              |
#            WHERE        SELECT /       GROUP BY
#          (filter)     UPDATE (:=)
#
# - 'i': Which rows do I want? (Filtering, ordering, or indexed joins)
# - 'j': What do I want to do to columns? (Select, compute metrics, assign in-place)
# - 'by': How should rows be grouped? (Categories, dates, factors)

# Assume 'dta_prec' is loaded in the workspace from the previous ingestion step.
class(x = dta_prec)

# METHOD: setDT() converts a standard data.frame into a data.table IN PLACE.
# It modifies the object's pointer in C memory without copying 800k+ rows in RAM.
setDT(x = dta_prec)

# Check all active data.tables in RAM, their sizes, row counts, and keys:
tables()

# Select the target columns using a character vector with the '..' prefix.
# The '..' prefix tells data.table to look ONE SCOPE LEVEL UP in R for the vector,
# rather than looking for a column named 'cols_keep' inside the table.
cols_keep <- c("START_TIME", "VALUE", "ID_REGISTRACE", "KRAJ_NAZEV", 
               "LOKALITA_NAZEV", "VELICINA_ZKRATKA", "METODA_ZKRATKA", 
               "ZEMEPISNA_DELKA", "ZEMEPISNA_SIRKA", "NADMORSKA_VYSKA", 
               "JEDNOTKA_ZKRATKA", "DATA_INTERVAL")

dt <- dta_prec[, ..cols_keep]

# Standardize column headers in place without allocating memory:
setnames(x = dt, 
         old = cols_keep, 
         new = c("date_raw", "val", "reg_id", "region", 
                 "location", "variable", "method", 
                 "lon", "lat", "elevation", 
                 "unit", "interval"))

str(object = dt)


# ==============================================================================
# 2. REFERENCE SEMANTICS (`let`) & FAST INTEGER DATES
# ==============================================================================

## THE `let` OPERATOR (Assign by Reference)
# In Base R, `df$col <- x` schedules a copy-on-write, duplicating entire columns.
# In data.table, `let` directly modifies values at their exact memory address.

# PERFORMANCE GOTCHA: Dates
# POSIXct stores timestamps as 8-byte floating-point doubles with timezone attributes.
# data.table provides 'as.IDate', an integer date representing elapsed days since 
# 1970-01-01. It uses 4 bytes (50% less RAM) and hashes significantly faster in 'by'.

dt[, let(
  # High-precision UTC timestamp
  datetime = as.POSIXct(x = date_raw, 
                        format = "%Y-%m-%dT%H:%M:%OSZ", 
                        tz = "UTC"),
  # Lightweight integer calendar date
  date = as.IDate(x = substr(x = date_raw, 
                             start = 1, 
                             stop = 10)),
  # Temporal components
  year = as.integer(x = substr(x = date_raw, 
                               start = 1, 
                               stop = 4)),
  month = as.integer(x = substr(x = date_raw, 
                                start = 6, 
                                stop = 7)),
  # Clean text factors in place
  region = as.factor(x = region),
  variable = as.factor(x = variable),
  unit = as.factor(x = unit),
  # Clean station key: strip accents and punctuation, combine with registration ID
  station_id = as.factor(x = paste(iconv(x = gsub(pattern = "[ /.]", 
                                                  replacement = "", 
                                                  x = location), 
                                         to = "ASCII//TRANSLIT"),
                                   reg_id, 
                                   sep = "_"))
)]

# Drop the raw character column to reclaim RAM:
dt[, date_raw := NULL]

# TEACHING WARNING: Copying data.tables
# Writing `dt_backup <- dt` creates a reference pointer, NOT a separate copy!
# Any mutation on `dt_backup` will immediately alter `dt`.
# To create a true independent clone:
# dt_safe_copy <- copy(dt)


# ==============================================================================
# 3. ROW FILTERING (THE 'i' CLAUSE)
# ==============================================================================

# TASK: Extract all sulfate (SO4(2-)) measurements in 'Jihočeský' during 1976-1980

# data.table provides optimized operators:
# - %chin%: Fast character-matching replacement for %in% (C-level hash lookup).
# - %between%: Shorthand for (x >= lower & x <= upper), evaluated in C.

filtered_records <- dt[variable %chin% c("SO4(2-)", "SO4") & 
                         region == "Jihočeský" & 
                         year %between% c(1976, 1980)]

head(x = filtered_records)

# Boolean filter: Select precipitation readings where rainfall exceeded 100 mm
heavy_monthly_rain <- dt[variable == "rain" & val > 100]
head(x = heavy_monthly_rain)


# ==============================================================================
# 4. COLUMN COMPUTATION & AGGREGATION (THE 'j' CLAUSE)
# ==============================================================================

# In 'j', `.()` is a syntactic shortcut for `list()`.
# Wrapping expressions in `.()` guarantees the output is returned as a data.table.

# SPECIAL SYMBOLS IN 'j':
# - .N        : Integer count of rows in the current slice/table.
# - uniqueN() : Highly optimized version of length(unique(x)).

overall_stats <- dt[, .(total_observations = .N,
                        monitored_stations = uniqueN(x = station_id),
                        variables_tracked = uniqueN(x = variable),
                        earliest_record = min(date, na.rm = TRUE),
                        latest_record = max(date, na.rm = TRUE))]

overall_stats

# Variable-specific summary table:
analyte_summary <- dt[, .(samples = .N,
                          stations = uniqueN(x = station_id),
                          mean_val = round(x = mean(val, 
                                                    na.rm = TRUE), 
                                           digits = 2),
                          median_val = round(x = median(val, 
                                                        na.rm = TRUE), 
                                             digits = 2),
                          max_val = max(val, na.rm = TRUE),
                          unit = unique(x = unit)[1]),
                      by = variable][order(-samples)]

analyte_summary


# ==============================================================================
# 5. GROUPED COMPUTATIONS: 'by' VS. 'keyby'
# ==============================================================================

# TASK: Calculate decadal precipitation trends per region

# - 'by'    : Preserves the original encounter order of groups (faster).
# - 'keyby' : Automatically sorts the grouped output AND sets the grouping 
#             columns as sorted physical keys via setkey().
dt[, elevation := as.numeric(elevation)]

decadal_precip <- dt[variable == "rain", .(station_count  = uniqueN(x = station_id),
                                           mean_annual_mm = round(x = mean(val, 
                                                                           na.rm = TRUE), 
                                                                  digits = 1),
                                           max_month_mm   = max(val, na.rm = TRUE),
                                           total_samples  = .N), 
                     keyby = .(region, decade = (year %/% 10) * 10)]

# Notice the output is already sorted ascending by region, then decade:
head(x = decadal_precip, n = 12)


# ==============================================================================
# 6. ADVANCED GROUPING WITH .SD & .SDcols
# ==============================================================================

## .SD = "Subset of Data"
# An internal data.table holding all columns for the current group EXCLUDING
# the grouping columns specified in 'by'.
# .SDcols restricts which columns are loaded into .SD to conserve RAM.

# TASK A: Apply identical functions (median and IQR) across multiple numeric variables:
numeric_cols <- c("val", "elevation")

grouped_sd <- dt[variable == "rain", 
                 c(lapply(X = .SD, 
                          FUN = median, 
                          na.rm = TRUE),
                   lapply(X = .SD,
                          FUN = IQR, 
                          na.rm = TRUE)), 
                 by = region, 
                 .SDcols = numeric_cols]

grouped_sd

# TASK B: Top-N record extraction per group
# Find the single highest rainfall record ever observed for each region:
highest_regional_rain <- dt[variable == "rain", 
                            .SD[which.max(val)], 
                            by = region, 
                            .SDcols = c("location", "date", "val", "elevation")]

highest_regional_rain[order(-val)]


# ==============================================================================
# 7. FAST BINARY SEARCH INDEXING (setkey)
# ==============================================================================

## VECTOR SCANS VS. BINARY SEARCH:
# Standard filtering `dt[station_id == "Modletin_21778"]` performs a linear vector
# scan through all 800,000+ rows (Time complexity: O(N)).
# Setting a key sorts the data in-place and builds an internal index tree, allowing
# binary search lookups (Time complexity: O(log N) — practically instantaneous).

# Set compound primary key:
setkey(dt, variable, station_id, date)

# Confirm key status:
key(x = dt)

# Binary search syntax: DT[.(val1, val2)]
# Extract all historical rain records for station 'Modletin_21778':
modletin_ph <- dt[.("rain", "Modletin_21778"), nomatch = NULL]
head(x = modletin_ph)

# Extract multiple variables simultaneously (Drops non-matching rows instead of returning NA rows):
modletin_ions <- dt[.(c("pH", "SO4(2-)", "NO3(-)"), "Modletin_21778"), nomatch = NULL]
head(x = modletin_ions)


# ==============================================================================
# 8. RESHAPING DATA: DCAST (LONG TO WIDE) & MELT (WIDE TO LONG)
# ==============================================================================

# --- A. dcast(): PIVOTING FROM LONG TO WIDE ---
# Format: dcast(data, LHS (row identifiers) ~ RHS (new column names), value.var = "...")

chem_subset <- c("rain", "pH", "SO4(2-)", "NO3(-)", "Ca")

# Build a multivariate matrix: one row per station per date
chem_wide <- dcast(data = dt[variable %chin% chem_subset],
                   formula = station_id + region + date + year + month ~ variable,
                   value.var = "val",
                   fun.aggregate = mean,
                   fill = NA)

head(x = chem_wide)

# Multiple value variables: Cast both value AND laboratory method
chem_wide_multi <- dcast(data = dt[variable %chin% c("pH", "SO4(2-)")],
                         formula = station_id + date ~ variable,
                         value.var = c("val", "method"),
                         fun.aggregate = function(x) x[1])

head(x = chem_wide_multi)

# --- B. melt(): UNPIVOTING FROM WIDE TO LONG ---
# Format: melt(data, id.vars = c(...), measure.vars = c(...))

chem_long <- melt(data = chem_wide,
                  id.vars = c("station_id", "region", "date", "year", "month"),
                  measure.vars = chem_subset,
                  variable.name = "analyte",
                  value.name = "concentration",
                  na.rm = TRUE)

head(x = chem_long)

# Advanced: Unpivot paired metrics simultaneously using patterns()
multi_long <- melt(data = chem_wide_multi,
                   id.vars = c("station_id", "date"),
                   measure.vars = patterns(val = "^val_",
                                           method = "^method_"),
                   variable.name = "analyte_code")

# Replace numeric index with original parameter labels:
multi_long[, analyte := c("pH", "SO4(2-)")[as.integer(analyte_code)]]
multi_long[, analyte_code := NULL]

head(x = multi_long)


# ==============================================================================
# 9. TIME SERIES WINDOWS & MISSING INTERVAL DETECTION
# ==============================================================================

# TASK: Identify consecutive measurement gaps per station using shift()
# shift(x, n = 1, type = "lag") accesses the preceding element in a group.

# Sort table chronologically per station:
setorder(dt, station_id, variable, date)

# Compute lag interval on monthly rainfall:
dt[variable == "rain", let(prev_date  = shift(x = date, 
                                              n = 1, 
                                              type = "lag"),
                           days_gap   = as.numeric(date - shift(x = date, 
                                                                n = 1, 
                                                                type = "lag"))),
   by = station_id]

# Identify missing monthly data gaps (> 40 days between samples):
gaps <- dt[variable == "rain" & days_gap > 40, .(station_id,
                                                 gap_start   = prev_date,
                                                 gap_end     = date,
                                                 days_missed = days_gap)][order(-days_missed)]

head(x = gaps, n = 10)

# Fast rolling aggregates: 12-month rolling mean of rainfall per station
# frollmean() is an optimized C routine operating directly on vectors:
dt[variable == "rain", 
   rolling_12m_rain := frollmean(x = val, 
                                 n = 12, 
                                 align = "right", 
                                 fill = NA), 
   by = station_id]

head(x = dt[variable == "rain" & !is.na(rolling_12m_rain), 
            .(station_id, date, val, rolling_12m_rain)])


# ==============================================================================
# 10. FAST JOINS & LOOKUPS
# ==============================================================================

# Create a small station metadata reference table:
station_meta <- unique(x = dt[, .(station_id, location, region, elevation, lon, lat)])

# Create an aggregated station summary table:
station_rain_totals <- dt[variable == "rain", .(total_rain_mm = round(x = sum(val, 
                                                                              na.rm = TRUE), 
                                                                      digits = 1),
                                                sampling_years = uniqueN(x = year)), 
                          by = station_id]

# --- SYNTAX: DT[Target, on = ...] ---
# Performs an immediate join without calling merge().
# Reads as: "Take 'station_rain_totals', and match it ONTO 'station_metadata'".
joined_summary <- station_rain_totals[station_metadata, on = .(station_id)]

head(x = joined_summary)
