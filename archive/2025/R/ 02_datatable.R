# This script demonstrates package management, data manipulation, and benchmarking
# techniques in R using data.table and fst packages.

### Step 1: Install and load required packages
# Define the list of required packages
lop <- c("data.table", "fst")

# Identify packages not installed
to_instal <- lop[which(x = !(lop %in% installed.packages()[,"Package"]))]

# Install missing packages
if(length(to_instal) != 0) {
  install.packages(to_instal)
}

# Load the installed packages
temp <- lapply(X = lop, 
               FUN = library, 
               character.only = TRUE)

# Clean up the environment
rm(temp, lop, to_instal)

# **EXERCISE 1:** Add another package to `lop` (e.g., "ggplot2") and repeat this process.
# What happens if the package is already installed?

### Step 2: Load a pre-processed dataset
dta <- readRDS(file = "./data/camel_br.rds") # Load RDS file containing the dataset

# Check the class of the dataset
class(x = dta)

# View the `date` column in two ways
dta$date  # Access as a vector
dta[, "date"]  # Access as a subset of the data.table

# Extract rows 5 to 15 and the second column
dta[c(5:15), 2]

# **EXERCISE 2:** Use indexing to extract:
# - The first 10 rows of the "id" column
# - All rows where "streamflow_mm" > 10

### Step 3: Convert the dataset to a data.table
# Convert the dataset to a data.table for faster operations
dta <- as.data.table(x = dta)
dta

### Step 4: Convert the dataset to wide format
# Create a wide-format data.table where each ID becomes a column
dta_wide <- dcast(
  data = dta,
  formula = date ~ id,
  value.var = "streamflow_mm"
)
dta_wide

# **EXERCISE 3:** Write a similar dcast operation to group data by `id` and summarize the mean streamflow.

### Step 5: Benchmarking long vs wide data
# Measure the time to save the long-format data
system.time({
  write.fst(x = dta, 
            path = "./data/dta_long.fst")
})

# Measure the time to save the wide-format data
system.time({
  write.fst(x = dta_wide, 
            path = "./data/dta_wide.fst")
})

# Compare file sizes
file.size("./data/dta_long.fst")
file.size("./data/dta_wide.fst")

# **EXERCISE 4:** Which format (long or wide) is smaller in file size? Why might that be?

### Step 6: Reading and subsetting data with fst
# Read the long-format data
f <- fst(path = "./data/dta_long.fst")

# Check the size of the imported data
object.size(x = f)

# Import a subset of data (rows 1-100,000 and specific columns)
dta_import <- f[1:100000, c("date", "streamflow_mm")]

# Read the fst file as a data.table
dta <- read.fst(path = "./data/dta_long.fst", 
                as.data.table = TRUE)

# **EXERCISE 5:** Experiment with reading only specific rows or columns.

### Step 7: Aggregations and filtering with data.table
# Calculate mean and standard deviation of streamflow_mm for years 1988-2000
dta[between(x = as.numeric(x = year(x = date)), 
            lower = 1988, 
            upper = 2000), 
    .(mean = mean(streamflow_mm), 
      sd = sd(streamflow_mm)),
    by = id]

# **EXERCISE 6:** Modify the year range to 2001-2010 and see how the results change.

### Step 8: Basic data.table operations
# Examples of accessing rows, columns, and creating new variables
dta[1:3]  # First 3 rows
dta[, 2]  # Second column
dta$streamflow_mm  # Streamflow column as a vector
dta[, .(date, id)]  # Subset specific columns

# Add a new column with random values
dta[, val := rnorm(n = .N)]

# Flag rows where the new column value is greater than 1
dta[val > 1, flag := TRUE]

# Filter rows with the flag
dta[flag == TRUE,]

# Remove the flag column
dta[, flag := NULL]

# **EXERCISE 7:** Create a column that calculates the log of `streamflow_mm`. Filter rows where log > 1.

### Step 9: Joining data.tables
# Create a new data.table for merging
aux <- data.table(
  catchment = unique(dta$id),
  something = sample(x = letters, 
                     replace = TRUE, 
                     size = length(x = unique(x = dta$id)))
)

# Merge data.tables
dta_merge <- merge(
  x = dta,
  y = aux,
  by.x = "id",
  by.y = "catchment"
)

# Rename columns for consistency
setnames(x = aux,
         old = "catchment",
         new = "id")

# Merge again using the renamed column
dta_merge <- merge(
  x = dta,
  y = aux,
  by = "id"
)

# Group and summarize after merge
dta_merge[, .(mean_val = mean(val)), 
          by = .(something, id)]

# **EXERCISE 8:** Add another column to `aux` and perform a similar merge and summarization.

### Step 10: Writing and reading CSV files
# Write a data.table to a CSV file
fwrite(x = dta, 
       file = "./data/dta_long.csv")

# Read the CSV file
dta_csv <- fread(input = "./data/dta_long.csv")

# **EXERCISE 9:** Export `dta_wide` to a CSV and compare its size to `dta_long.csv`.

