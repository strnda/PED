# ==============================================================================
# R DATA ACQUISITION & ETL: WEB SCRAPING, ARCHIVE HANDLING & BATCH PROCESSING
# ==============================================================================
# Objective:
#   1. Inspect an open data HTTP directory index (CHMI precipitation data).
#   2. Parse the remote HTML to dynamically extract a target file URL.
#   3. Download, verify, and unpack nested ZIP archives.
#   4. Read and merge multiple CSV pairs using defensive error handling.
#   5. Consolidate everything into a single, analysis-ready data frame.
# ==============================================================================


# ==============================================================================
# 1. DOCUMENTATION & ENVIRONMENT SETUP
# ==============================================================================

# Open interactive help documentation for the key functions used in this module:
?scan            # Low-level data/token reading from console, files, or URLs
?readLines       # Line-by-line text reading (standard for plain text and HTML)
?download.file   # Base utility for downloading remote resources over HTTP/FTP

# Define the remote directory URL (Czech Hydrometeorological Institute - CHMI)
url <- "https://opendata.chmi.cz/air_quality/historical/precipitation/"

# Define the local target directory:
# TIP: Avoid hardcoding absolute OS paths like "C:/Users/...". Using "~/Desktop/"
# or file.path(path.expand("~"), "Desktop") ensures cross-platform portability.
# pth <- "C:/Users/strnadf/Downloads/"
pth <- "~/Desktop/"


# ==============================================================================
# 2. READING REMOTE HTML: 'scan()' VS. 'readLines()'
# ==============================================================================

# ATTEMPT 1: Default scan()
# By default, scan() expects whitespace-delimited floating-point numbers (what = double()).
# Running this against an HTML webpage will throw an error or warning because HTML contains text.
test_1 <- scan(file = url)

# ATTEMPT 2: scan() with what = "list"
# Tokenizes the raw HTML stream into a list/vector of words split by whitespace.
# It works, but it breaks single HTML tags across multiple vector elements.
test_2 <- scan(file = url,
               what = "list")

# ATTEMPT 3: readLines()
# The standard base-R method for reading web pages and text documents.
# It reads the remote document line-by-line, preserving tag boundaries per row.
test_3 <- readLines(con = url)

# Inspect the outputs to compare structures:
test_2   # A character vector of individual space-separated tokens
test_3   # A character vector where each element is an entire line of HTML source


# ==============================================================================
# 3. REGEX PATTERN MATCHING & STRING PARSING
# ==============================================================================

# grep() returns the integer vector of indices matching the pattern "data" (case-insensitive).
grep(pattern = "data",
     x = test_2, 
     ignore.case = TRUE)

# grepl() ("grep logical") returns a boolean vector of TRUE/FALSE for every element.
grepl(pattern = "data",
      x = test_2, 
      ignore.case = TRUE)

# which(grepl(...)) converts the boolean TRUEs into numeric indices.
# Pedagogical Note: which(grepl(...)) is functionally identical to grep(...),
# but grep() is faster and more idiomatic because it skips allocating the logical vector.
which(x = grepl(pattern = "data",
                x = test_2, 
                ignore.case = TRUE))

# --- Extract the filename from the HTML string ---
# 1. test_2[grep(...)]: Retrieves the specific token containing "data" (e.g., href="data_precip.zip").
# 2. strsplit(..., split = '\"'): Splits the token at double quotes (").
# 3. [[1]][2]: Extracts the 2nd piece of the split string, which is the clean file name.
fl_nm <- strsplit(x = test_2[grep(pattern = "data",
                                  x = test_2, 
                                  ignore.case = TRUE)],
                  split = '\"')[[1]][2]


# ==============================================================================
# 4. DOWNLOADING AND FILE INTEGRITY INSPECTION
# ==============================================================================

# Download the file by concatenating the base URL with the extracted filename.
# TEACHING GOTCHA: On Windows systems, always pass 'mode = "wb"' (write binary) 
# when downloading ZIP files, PDFs, or images to prevent character-stream corruption.
download.file(url = paste0(url, fl_nm),
              destfile = paste0(pth, fl_nm))

# Verify that the file successfully landed in the target folder (returns TRUE/FALSE)
file.exists(paste0(pth, fl_nm))

# Check file size in bytes to ensure it isn't an empty 0-byte error response
file.size(paste0(pth, fl_nm))

# Retrieve full filesystem metadata: size, permissions, creation/modification timestamps
file.info(paste0(pth, fl_nm))


# ==============================================================================
# 5. ARCHIVE EXTRACTION & RECURSIVE DISCOVERY
# ==============================================================================

# Create a local subfolder to keep extracted contents organized
dir.create(path = paste0(pth, "data"))

# Unpack the main parent ZIP archive into the newly created folder
unzip(zipfile = paste0(pth, fl_nm),
      exdir = paste0(pth, "data"))

# Recursively locate all files unpacked inside subdirectories.
# - 'recursive = TRUE': Walks down every nested folder.
# - 'full.names = TRUE': Prepends directory paths so the files can be opened directly.
fls <- list.files(path = paste0(pth, "data"), 
                  recursive = TRUE,
                  full.names = TRUE)

# Filter the file list to keep only nested ZIP archives (case-insensitive)
fls_zip <- fls[grep(pattern = ".zip",
                    x = fls,
                    ignore.case = TRUE)]


# ==============================================================================
# 6. BATCH EXTRACTION, DEFENSIVE INGESTION, AND MERGING
# ==============================================================================

## TASK: Extract time series from each nested zip and import into one consolidated data frame.

?read.table  # General tabular file reader (read.csv is a wrapper for it)
?lapply      # Functional loop: applies a function over a list or vector
?Control     # Documentation for R control flow constructs (if, else, for, etc.)

# Iterate through every nested ZIP archive using lapply()
dta_prec <- lapply(
  X = fls_zip, 
  FUN = function(x) {
    
    # PRO-TIP FOR WRITING/DEBUGGING FUNCTION BODIES:
    # Temporarily hardcode 'x' to test individual components inside the loop:
    # x <- fls_zip[20]
    
    # try() allows execution to continue even if an individual archive is corrupted.
    # 'silent = TRUE' suppresses printing the red error message to the console.
    e <- try(expr = {
      
      # Use an isolated temporary directory to avoid cluttering local disk storage
      tmp <- tempdir()
      
      # Extract the nested archive into the temp directory
      unzip(zipfile = x,
            exdir = tmp)
      
      # Identify all unpacked files in the temporary directory
      aux_fls <- list.files(path = tmp, 
                            full.names = TRUE)
      
      # Read the measurements CSV (file name containing "data")
      dta <- read.csv(file = aux_fls[grep(pattern = "data",
                                          x = aux_fls)])
      
      # Read the station registry/metadata CSV (file name containing "registr")
      nfo <- read.csv(file = aux_fls[grep(pattern = "registr",
                                          x = aux_fls)])
      
      # Column-bind measurements and station metadata.
      # Because 'nfo' typically contains a single row of station metadata,
      # R automatically recycles 'nfo' across all rows of 'dta'.
      out <- cbind(dta, nfo)
    }, 
    silent = TRUE)
    
    # If the try block fails, 'e' will have the class "try-error".
    # In that case, assign NA so we can filter out failed imports later.
    if (inherits(x = e,
                 what = "try-error")) {
      out <- NA
    } 
    
    out
  }
)

# --- COMBINE INTO A SINGLE DATA FRAME ---
# 1. Filter out failed/NA list entries.
#    (Note: is.na() evaluates element-wise, but using it with !is.na(...) strips failed entries).
# 2. do.call(rbind, ...) passes every remaining data.frame in the list as separate 
#    arguments to rbind(), stacking them vertically into one master data frame.
dta_prec <- do.call(what = rbind, 
                    args = dta_prec[!is.na(x = dta_prec)])
