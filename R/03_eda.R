# ==============================================================================
# 3. DATA CLEANING AND TIME SERIES PREPARATION
# ==============================================================================
# Objective:
#   1. Clean, subset, and standardize raw meteorological data from CHMI.
#   2. Parse ISO 8601 UTC timestamps into native POSIXct datetime objects.
#   3. Clean text strings (stripping Czech diacritics via transliteration).
#   4. Optimize memory footprint using categorical factors.
#   5. Identify station observation frequencies and filter daily series.
#   6. EXPANSION: Detect multi-station boundary bugs, aggregate daily/monthly 
#      precipitation totals, and compute wet/dry day metrics.
# ==============================================================================


# ==============================================================================
# 1. INITIAL INSPECTION & COLUMN SUBSETTING
# ==============================================================================

# 'dta_prec' was constructed in the previous ingestion script by binding
# measurements and station registry metadata.
dta_prec

# Inspect the high-level class (typically 'data.frame')
class(x = dta_prec)

# Check internal schema, variable types, and row/column counts
str(object = dta_prec)

# Preview unique timestamp representations to determine the parsing format
unique(x = dta_prec$START_TIME)

# --- SAFE COLUMN SELECTION & RENAMING ---
# PEDAGOGICAL TRAP: Using `names(dta_prec) %in% c(...)` subsets columns in the 
# order they currently exist in 'dta_prec', NOT the order written in the vector.
# If the source file reorders columns, assigning positional names (`names(dta) <- ...`)
# will silently corrupt column mappings. 
# Explicitly specifying the column order by name avoids this bug:

target_cols <- c("START_TIME", "VALUE", "ID_REGISTRACE", "LOKALITA_NAZEV", 
                 "VELICINA_ZKRATKA", "ZEMEPISNA_DELKA", "ZEMEPISNA_SIRKA", 
                 "NADMORSKA_VYSKA")

# Subset using the explicit column order:
dta <- dta_prec[, target_cols]

# Assign clean, snake_case English variable names:
names(x = dta) <- c("date", "value", "reg_id", 
                    "location", "variable", 
                    "lon", "lat", 
                    "elevation")

# Verify schema after renaming
str(object = dta)


# ==============================================================================
# 2. TIMESTAMP PARSING & TIME ZONE HANDLING
# ==============================================================================

# Extract the first timestamp string to test conversion parameters interactively
a <- dta$date[1]

# Inspect string type and character length
class(x = a)
nchar(x = a)

# ISO 8601 Format Breakdown: "2023-01-01T06:00:00Z"
#   %Y : 4-digit year
#   %m : 2-digit month (01-12)
#   %d : 2-digit day of the month (01-31)
#   T  : Literal separator character
#   %H : Hour in 24-hour format (00-23)
#   %M : Minute (00-59)
#   %OS: Seconds including optional decimal sub-seconds
#   Z  : Zulu time (UTC) indicator
# TEACHING NOTE: Always pass 'tz = "UTC"' when parsing UTC ISO strings; otherwise,
# R shifts the hour to your machine's local time zone automatically.
as.POSIXct(x = a,
           format = "%Y-%m-%dT%H:%M:%OSZ",
           tz = "UTC")

# Measure RAM usage before type transformations
object.size(x = dta)

# Vectorized conversion across all rows:
dta$date <- as.POSIXct(x = dta$date,
                       format = "%Y-%m-%dT%H:%M:%OSZ",
                       tz = "UTC")

str(object = dta)


# ==============================================================================
# 3. TEXT SANITIZATION, TRANSLITERATION & COMPOSITE KEYS
# ==============================================================================

# Convert low-cardinality metadata columns into factors to save RAM:
dta$location <- as.factor(x = dta$location)
dta$variable <- as.factor(x = dta$variable)

# Construct a unique, filesystem-safe Station ID ('id'):
# 1. gsub(): Removes spaces, dots, and slashes using regular expression character alternation.
# 2. iconv(..., to = "ASCII//TRANSLIT"): Transliterates regional accented characters 
#    into standard Latin equivalents (e.g., "České Budějovice" -> "Ceske Budejovice").
# 3. paste(..., sep = "_"): Combines sanitized location name with registration ID.
# 4. as.factor(): Compresses the resulting keys into integer factor levels.
dta$id <- as.factor(x = paste(iconv(x = gsub(pattern = "\\ |\\.|\\/",
                                             replacement = "",
                                             x = dta$location), 
                                    to = "ASCII//TRANSLIT"),
                              dta$reg_id,
                              sep = "_"))

# Compare memory reduction achieved by factorizing repeated strings
object.size(x = dta)
str(object = dta)


# ==============================================================================
# 4. FILTERING BY VARIABLE & EXPLORING TIME STEP FREQUENCY
# ==============================================================================

# Subset precipitation observations.
# NOTE: In CHMI datasets, precipitation is often coded as 'SRAZKA', 'PR', or 'rain'.
# Adjust the filter condition to match the specific abbreviation in your source data:
prec <- dta[dta$variable %in% c("rain", "SRAZKA", "PR"), c("date", "value", "id", "location")]

# PEDAGOGICAL GOTCHA: In the original draft, 'location' was omitted from the 
# column vector in the subset above, which caused `unique(prec$location)` to return NULL.
# We included 'location' above so this inspection works:
unique(x = prec$location)

# --- MULTI-STATION CONTINUITY TRAP ---
# Running `diff(as.Date(prec$date))` directly over the whole data frame has two bugs:
#   1. diff(vec) returns length (N - 1), creating an off-by-one index misalignment.
#   2. At station boundaries (where Station A ends and Station B begins), diff() computes 
#      the time difference between two completely unrelated stations!
#
# Below is the original draft line:
# diff(x = as.Date(x = prec$date))

# Original logic attempt:
# id <- unique(x = prec[which(diff(x = as.Date(x = prec$date)) == 1), "id"])
# daily <- prec[prec$id %in% id, ]

# ROBUST REFACTOR: Calculate consecutive time differences PER STATION using ave():
prec <- prec[order(prec$id, prec$date), ] # Ensure sorted by station and timestamp

prec$time_step_days <- ave(as.numeric(as.Date(prec$date)), 
                           prec$id, 
                           FUN = function(d) c(NA, diff(d)))

# Identify stations where the median sampling cadence is exactly 1 day (daily stations):
station_cadence <- tapply(prec$time_step_days, prec$id, median, na.rm = TRUE)
daily_station_ids <- names(station_cadence)[station_cadence == 1]

# Filter for confirmed daily stations
daily <- prec[prec$id %in% daily_station_ids, ]
summary(object = daily)


# ==============================================================================
# 5. SINGLE-STATION TIME SERIES DIAGNOSTICS & PLOTTING
# ==============================================================================

# Pick a single station from the daily subset (e.g., the 8th station)
selected_id <- unique(daily$id)[8]
test <- daily[daily$id == selected_id, ]

# Plot a hyetograph (vertical impulse bars standard for precipitation records)
plot(x = test$date,
     y = test$value, 
     type = "h",
     col = "steelblue",
     lwd = 2,
     main = paste("Precipitation Hyetograph:", selected_id),
     xlab = "Date",
     ylab = "Precipitation [mm]")
grid(col = "gray85")

# Verify sampling interval regularity for this specific station:
# Should consist almost entirely of 1s (or 86400 seconds)
table(diff(x = test$date))


# ==============================================================================
# 6. EXPANSION: ADVANCED METEOROLOGICAL AGGREGATIONS & EDA
# ==============================================================================

# Filter for wet days (measurable precipitation events > 0 mm)
nz <- daily[daily$value > 0, ]

# --- METRIC 1: Wet Day Frequency vs. Dry Days ---
# Calculate the percentage of rainy days per station:
wet_day_summary <- aggregate(value ~ id, data = daily, FUN = function(v) {
  total_days <- length(v)
  rain_days  <- sum(v > 0, na.rm = TRUE)
  round((rain_days / total_days) * 100, 1)
})
names(wet_day_summary)[2] <- "wet_day_pct"
head(wet_day_summary)

# --- METRIC 2: Extreme Rainfall Events (Daily Totals > 20 mm) ---
heavy_rain <- daily[daily$value >= 20, c("id", "location", "date", "value")]
heavy_rain <- heavy_rain[order(-heavy_rain$value), ]
head(heavy_rain, n = 10)

# --- METRIC 3: Monthly Aggregation by Station ---
# Create a Year-Month factor grouping key
daily$year_month <- format(daily$date, "%Y-%m")

# Sum monthly precipitation by station and month
monthly_totals <- aggregate(value ~ id + year_month, 
                            data = daily, 
                            FUN = sum, 
                            na.rm = TRUE)
names(monthly_totals)[3] <- "total_precip_mm"

# Display sample of aggregated monthly totals:
head(monthly_totals,
     n = 12)
