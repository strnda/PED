# root folder
pth <- "~/Desktop/PED/"
# target name of the ZIP file (downloaded data)
file_name <- "04_CAMELS_BR_streamflow_simulated.zip"
# creates a folder joining the root path with the subfolder 'data/', checks if all directories exists before creating
dir.create(path = paste0(pth, "data/"), 
           recursive = TRUE)
# download file from Zenodo into 'data/', returns : "Content type 'application/octet-stream' length 43100581 bytes (41.1 MB), Downloaded 41.1MB"
download.file(url = paste0("https://zenodo.org/records/3964745/files/", 
                           file_name,"?download=1"),
              destfile = paste0(pth, "data/", file_name))
# checks if file arrived in directory, returns message depending on condition 
if (file.exists(paste0(pth, "data/", file_name))) {
  
  cat("File downloaded successfully!")
} else {
  
  cat("File download failed!")
}

# creates a folder to hold unzipped files
dir.create(path = paste0(pth, "data/camel_data/"))

#unzips the previously downloaded data into "camel_data/"
unzip(zipfile =  paste0(pth, "data/", file_name), 
      exdir = paste0(pth, "data/camel_data"))

# list all .txt files | if downloaded and extracted manually, paste the extracted files' path
fls <- list.files(path = paste0(pth, "data/camel_data/"),
                  full.names = TRUE,
                  recursive = TRUE,
                  pattern = ".txt")
# read and process the files in the fls list
dta_all <- lapply(
  X = fls, 
  FUN = function(i) { 
    # i <- fls[1]
    # read the file into  data table, changes "nan" into NA (missing value)
    dta <- read.table(file = i, 
                      header = TRUE,
                      na.strings = "nan")
    # creates a new column with date (Y,M,D)
    dta$date <- as.Date(x = paste(dta$year, dta$month, dta$day,
                                  sep = "-"), 
                        format = "%Y-%m-%d")
    # get the file name from the full path
    aux <- strsplit(x = i, split = "/")[[1]]
# extract the basin ID from the file name (what's before the underscore)
    dta$id <- as.factor(x = strsplit(x = aux[length(aux)], split = "_")[[1]][1])
# removes year, month, day columns (we have 'date' now)
    dta_out <- dta[, which(x = !(names(x = dta) %in% c("year", "month", "day")))]
# returns the claned data
    return(dta_out)
  }
)
# picks a basin from the list
test <- dta_all[[sample(x = seq_along(along.with = dta_all),
                        size = 1)]]
# shows information for the selected basin. Stats shown : Min, 1st Qu., Mean, Median, 3rd Qu., Max (for simulated streamflow, date and shows ID number)
summary(object = test)

# plots streamflow over time for one basin : x-axis = date, y-axis = simulated streamflow. Sets ID as title, the plot is a line plot 
plot(x = test$date,
     y = test$simulated_streamflow_m3s,
     main = unique(test$id), 
     type = "l")
# shows autocorrelation plot series of the streamflow : x-axis = lag, y-axis = autocorrelation
acf(x = test$simulated_streamflow_m3s)

# creates boxplot of streamflow by month : x-axsi = month, y-axis = simulated streamflow
boxplot(simulated_streamflow_m3s ~ format(x = date, 
                                          format = "%m"), 
        data = test)
# creates ECDF (empirical cumulative distribution function) from simulated streamflow value
e <- ecdf(x = test$simulated_streamflow_m3s)
# plots the ECDF curve : x-axis = streamflow, y-axis = cumulative probability
plot(x = e)
# tests ECDF at specific streamflow values
e(v = 2300)
e(v = 5250)

# merges all basin dataframes into one large dataframe
dta_all <- do.call(what = rbind, 
                   args = dta_all)
# shows basic stats for the combined basins 
summary(object = dta_all)

# saves the full datased as an RDS file
saveRDS(object = dta_all,
        file = paste0(pth, "data/camel_data.rds"))
