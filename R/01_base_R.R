# ==============================================================================
# 1. BASIC ASSIGNMENT, DATA TYPES, AND S3 CLASSES
# ==============================================================================

# Assign the number 12 to the variable 'a' using the assignment operator (<-).
# By default, R stores plain numbers as double-precision floating-point ("numeric").
a <- 12

# Check the high-level class of object 'a'. 
# Returns: "numeric"
class(x = a)

# Wrapping a value in quotes makes it a text string.
# Returns: "character"
class(x = "a")

# Sys.Date() returns the current system date.
# Although stored internally as the number of days since January 1, 1970,
# its class attribute is set to "Date", which changes how R prints and plots it.
class(x = Sys.Date())

# Print today's date in ISO 8601 format ("YYYY-MM-DD").
Sys.Date()


# ==============================================================================
# 2. GETTING HELP & S3 GENERIC METHODS
# ==============================================================================

## ?fun, or press F1
# TIP: Type ?class or ?Sys.Date in the console (or put your cursor on a function 
# in RStudio and press F1) to open its official documentation page.

# R uses an object-oriented system called S3. Generic functions (like plot, print, 
# or summary) behave differently depending on the class of the input object.

# List all generic functions that have a specific method written for the "Date" class:
methods(class = "Date")

# List all object classes that the generic 'plot()' function knows how to handle:
methods(generic.function = "plot")


# ==============================================================================
# 3. ATOMIC VECTORS, INDEXING, AND TYPE COERCION
# ==============================================================================

# Create a numeric vector using c() ("combine" or "concatenate").
# 'pi' is a built-in constant in R (~3.141593).
b <- c(1, 2, 5, 12, pi)

# Subset the 3rd element of vector 'b' using square brackets [].
# NOTE: R uses 1-based indexing (the first element is at index 1, not 0).
# Returns: 5
b[3]

# Confirm the class of 'b'. Returns: "numeric"
class(x = b)

# Adding the 'L' suffix forces R to store numbers strictly as 32-bit integers 
# rather than double-precision floats, which uses half the memory.
i <- c(1L, 5L, 88L)

# Returns: "integer"
class(x = i)

# --- IMPLICIT TYPE COERCION ---
# Atomic vectors in R can only hold ONE data type. When you combine different types,
# R automatically coerces all elements to the most flexible type in this hierarchy:
# logical -> integer -> numeric (double) -> character

# Combining an integer vector ('i') with a numeric element ('b[1]') upgrades 
# all integers to "numeric" (doubles) so no decimal precision can be lost.
x <- c(i, b[1])
class(x = x) # Returns: "numeric"

# Adding a single character string ("pi") forces every number in the vector 
# to be converted into a text string (e.g., 1 becomes "1").
y <- c(i, b[1], "pi")
class(x = y) # Returns: "character"


# ==============================================================================
# 4. FACTORS AND MEMORY EFFICIENCY
# ==============================================================================

## super awesome class: factor
# Factors are used to store categorical data with a fixed set of unique values ("levels").
factor()

# Generate a huge character vector of 100 million (10e7 = 1 * 10^8) random 
# lowercase letters sampled from the built-in 'letters' constant ("a" to "z").
# 'replace = TRUE' allows each letter to be picked multiple times.
a <- sample(x = letters, 
            size = 10e7, 
            replace = TRUE)

# Check how much RAM 'a' consumes (~800 MB, since each entry stores an 8-byte string pointer).
object.size(x = a)

# Convert the character vector into a factor.
# Under the hood, R replaces the 100 million strings with 32-bit integers (1 to 26)
# and keeps a tiny lookup table of the 26 unique levels ("a" through "z").
b <- as.factor(x = a)

# Compare memory usage: 'b' takes roughly ~400 MB—about half the size of 'a'!
# (Note: Modern R uses a global string pool so character vectors are already 
# somewhat optimized, but factors still halve the pointer overhead on 64-bit systems.)
object.size(x = b)

# Inspect the first 6 elements of each object.
# Notice how 'a' prints with quotes, while 'b' prints without quotes and lists its "Levels" below.
head(x = a)
head(x = b)


# ==============================================================================
# 5. MATRICES AND ARRAYS (MULTI-DIMENSIONAL HOMOGENEOUS DATA)
# ==============================================================================

# A matrix is a 2D atomic vector (all elements must be the same data type).
# Here we generate 25 random numbers from a standard normal distribution (mean = 0, sd = 1)
# and arrange them into 5 columns (which automatically creates 5 rows).
# Note: By default, R fills matrices column-by-column (byrow = FALSE).
m <- matrix(data = rnorm(n = 25), 
            ncol = 5)

# Subset the matrix using [row_indices, column_indices].
# This extracts the values in rows 1 and 3 from column 2.
m[c(1, 3), 2]

# array -> [, , 3]
# Concept note: While a matrix has 2 dimensions [rows, cols], an 'array' can have 
# 3 or more dimensions (e.g., [rows, cols, slices]), subsetted as my_array[, , 3].


# ==============================================================================
# 6. LISTS AND DATA FRAMES (HETEROGENEOUS DATA STRUCTURES)
# ==============================================================================

## data.frame | list

# Unlike atomic vectors, a 'list' can store elements of different types and lengths 
# (numbers, strings, vectors, or even other lists and functions).
x <- list()

# A 'data.frame' is a special kind of list where every element is a vector (column) 
# of the EXACT SAME length, forming a rectangular table.
# Column 1 ('date'): 50 consecutive days starting from today.
# Column 2 ('val'):  50 random numbers drawn from a standard normal distribution.
df <- data.frame(date = seq.Date(from = Sys.Date(),
                                 by = "day", 
                                 length.out = 50),
                 val = rnorm(n = 50))

# Print the entire data frame to the console.
df

# Check the class: returns "data.frame"
class(x = df)

# str() ("structure") is one of the most useful inspection functions in R.
# It shows the dimensions (50 obs. of 2 variables) and the class/preview of each column.
str(object = df)

# Because 'df' has a Date column first and a numeric column second, plot() 
# automatically creates a time-series scatter plot. 
# 'type = "l"' connects the points as a continuous line chart.
plot(x = df, 
     type = "l")


# ==============================================================================
# 7. FORMATTING AND EXTRACTING DATE COMPONENTS
# ==============================================================================

# Extract the Date in row 5, column 1 (df[5, 1]) and format it as a character string
# using POSIX strftime codes:

# "%Y" -> 4-digit year (e.g., "2026")
format(x = df[5, 1], 
       format = "%Y")

# "%a" -> Abbreviated weekday name in your system's locale (e.g., "Wed")
format(x = df[5, 1], 
       format = "%a")

# "%A" -> Full weekday name in your system's locale (e.g., "Wednesday")
format(x = df[5, 1], 
       format = "%A")

# "%B" -> Full month name (e.g., "November")
format(x = df[5, 1], 
       format = "%B")

# "%b" -> Abbreviated month name (e.g., "Nov")
format(x = df[5, 1], 
       format = "%b")


# ==============================================================================
# 8. WRITING CUSTOM FUNCTIONS & SPLIT-APPLY-COMBINE
# ==============================================================================

## Goal: Create a function that selects specific weekdays from a data frame
##       and calculates descriptive statistics for the selected values.

# Define 'fun' with two parameters:
# - 'x': the input data frame (no default value; required).
# - 'day': the abbreviated weekday(s) to filter by, defaulting to "Wed" if omitted.
fun <- function(x, day = "Wed") {
  
  # PRO TIP FOR DEBUGGING: Temporarily assign 'x <- df' in your global environment 
  # so you can test the body of the function line-by-line before wrapping it.
  # x <- df
  
  ## error handling with try|catch|stop
  # NOTE: In production code, you would validate inputs here using stop() or tryCatch(),
  # e.g., if (!is.data.frame(x)) stop("'x' must be a data.frame")
  
  # Branch 1: Calculate statistics for EVERY weekday separately
  if (day == "All") {
    
    # 1. SPLIT: Divide the numeric column into a named list of vectors, 
    # grouped by the abbreviated weekday name ("Mon", "Tue", etc.).
    # (Teaching note: Notice 'df$val' below—this works if 'df' exists in your global 
    # workspace, but best practice is to write 'x$val' so the function doesn't rely 
    # on external variables!)
    aux <- split(x = df$val,
                 f = format(x = x$date,
                            format = "%a"))
    
    # 2. APPLY: Loop over each weekday's vector in the list 'aux' using lapply() 
    # ("list apply") and an anonymous helper function to compute 4 summary metrics.
    # 'na.rm = TRUE' ensures missing values (NAs) don't break the math.
    out <- lapply(X = aux, 
                  FUN = function(val) {
                    
                    return(c("number of values" = length(x = val),
                             "mean" = mean(x = val, 
                                           na.rm = TRUE),
                             "sd" = sd(x = val, 
                                       na.rm = TRUE),
                             "iqr" = IQR(x = val, 
                                         na.rm = TRUE)))
                  })
    
    # 3. COMBINE: lapply() returns a list of 7 named vectors. 
    # do.call(rbind, out) stacks all 7 vectors row-by-row into a clean summary matrix.
    out <- do.call(what = rbind, 
                   args = out)
    
  } else {
    
    # Branch 2: Filter rows where the formatted weekday matches 'day'.
    # Using '%in%' instead of '==' allows passing multiple days, like day = c("Mon", "Fri").
    val <- x[format(x = x$date,
                    format = "%a") %in% day, "val"]
    
    # Compute the same 4 descriptive statistics on the filtered vector:
    # count, arithmetic mean, standard deviation (sd), and interquartile range (IQR).
    out <- c("number of values" = length(x = val),
             "mean" = mean(x = val, 
                           na.rm = TRUE),
             "sd" = sd(x = val, 
                       na.rm = TRUE),
             "iqr" = IQR(x = val, 
                         na.rm = TRUE))
  }
  
  # Return the final result (in R, the last evaluated expression is returned automatically).
  out
}

# --- TESTING THE FUNCTION ---

# Call 1: Uses the default argument (day = "Wed"). Returns stats for Wednesdays only.
fun(x = df)

# Call 2: Override the default argument to calculate stats for Mondays.
fun(x = df,
    day = "Mon")

# Call 3: Trigger the "All" branch to return a 7-row summary matrix for every day of the week.
fun(x = df,
    day = "All")