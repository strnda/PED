# The script downloads, extracts, processes, and saves streamflow data from the CAMELS dataset.

# Step 1: Download the ZIP file from a specified URL and save it locally
download.file(
  url = "https://zenodo.org/records/3964745/files/03_CAMELS_BR_streamflow_mm_selected_catchments.zip?download=1",
  destfile = "./data/03_CAMELS_BR_streamflow_mm_selected_catchments.zip" # Local path for the downloaded file
)
# Check if the file was downloaded successfully
if (file.exists("./data/03_CAMELS_BR_streamflow_mm_selected_catchments.zip")) {
  print("File downloaded successfully!")
} else {
  stop("File download failed!")
}

# Step 2: Create a directory to extract the data
if (!dir.exists("./data/camel_data")) { # Check if directory exists to avoid overwriting
  dir.create(path = "./data/camel_data") # Create the directory
  print("Directory created for extracted data.")
} else {
  print("Directory already exists.")
}

# Step 3: Extract the ZIP file into the created directory
unzip(
  zipfile = "./data/03_CAMELS_BR_streamflow_mm_selected_catchments.zip", # Path to ZIP file
  exdir = "./data/camel_data" # Extraction path
)
print("ZIP file extracted.")

# Step 4: List all `.txt` files in the extracted folder
fls <- list.files(
  path = "./data/camel_data/03_CAMELS_BR_streamflow_mm_selected_catchments/", # Folder with data
  full.names = TRUE, # Get full file paths
  recursive = TRUE, # Include files in subdirectories
  pattern = ".txt" # Only select files with `.txt` extension
)
print(paste("Found", length(fls), "files.")) # Feedback for the user

# Step 5: Process each `.txt` file and combine the data
dta_all <- lapply(
  X = fls, 
  FUN = function(i) {
    # Read data from the file
    dta <- read.table(
      file = i, 
      header = TRUE, # Use first row as column names
      na.strings = "nan" # Treat "nan" values as missing
    )
    
    # Create a `date` column by combining year, month, and day
    dta$date <- as.Date(
      x = paste(dta$year, dta$month, dta$day,
                sep = "-"), # Combine columns into "YYYY-MM-DD"
      format = "%Y-%m-%d" # Specify the date format
    )
    
    # Extract the ID from the file name
    aux <- strsplit(x = i, split = "/")[[1]] # Split the file path by "/"
    dta$id <- strsplit(x = aux[length(aux)], split = "_")[[1]][1] # Extract ID from the file name
    
    # Select relevant columns for the output
    dta_out <- dta[, c("date", "streamflow_mm", "id")]
    
    return(dta_out) # Return processed data
  }
)

# Combine the list of data frames into one large data frame
dta_all <- do.call(what = rbind, 
                   args = dta_all)

# Convert the ID column to a factor for easier handling
dta_all$id <- as.factor(x = dta_all$id)

# Remove rows with missing values
dta_all <- na.omit(object = dta_all)
print(paste("Final dataset has", nrow(dta_all), "rows after removing missing values."))

# Step 6: Save the cleaned and combined dataset to an RDS file
saveRDS(
  object = dta_all,
  file = "./data/camel_br.rds" # Save path
)
print("Processed data saved as RDS file.")

# Step 7: Test loading the saved RDS file
test <- readRDS(file = "./data/camel_br.rds") # Load the RDS file
print("Loaded data preview:")
print(head(test)) # Display the first few rows
