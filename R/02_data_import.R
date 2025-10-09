pth <- "~/Desktop/PED/"

file_name <- "04_CAMELS_BR_streamflow_simulated.zip"

dir.create(path = paste0(pth, "data/"), 
           recursive = TRUE)

download.file(url = paste0("https://zenodo.org/records/3964745/files/", 
                           file_name,"?download=1"),
              destfile = paste0(pth, "data/", file_name))

if (file.exists(paste0(pth, "data/", file_name))) {
  
  cat("File downloaded successfully!")
} else {
  
  cat("File download failed!")
}


dir.create(path = paste0(pth, "data/camel_data/"))

unzip(zipfile =  paste0(pth, "data/", file_name), 
      exdir = paste0(pth, "data/camel_data"))

fls <- list.files(path = paste0(pth, "data/camel_data/"),
                  full.names = TRUE,
                  recursive = TRUE,
                  pattern = ".txt")

dta_all <- lapply(
  X = fls, 
  FUN = function(i) {
    
    # i <- fls[1]
    
    dta <- read.table(file = i, 
                      header = TRUE,
                      na.strings = "nan")
    
    dta$date <- as.Date(x = paste(dta$year, dta$month, dta$day,
                                  sep = "-"), 
                        format = "%Y-%m-%d")
    
    aux <- strsplit(x = i, split = "/")[[1]]
    dta$id <- as.factor(x = strsplit(x = aux[length(aux)], split = "_")[[1]][1])
    
    dta_out <- dta[, which(x = !(names(x = dta) %in% c("year", "month", "day")))]
    
    return(dta_out)
  }
)

test <- dta_all[[sample(x = seq_along(along.with = dta_all),
                        size = 1)]]

summary(object = test)

plot(x = test$date,
     y = test$simulated_streamflow_m3s,
     main = unique(test$id), 
     type = "l")

acf(x = test$simulated_streamflow_m3s)

boxplot(simulated_streamflow_m3s ~ format(x = date, 
                                          format = "%m"), 
        data = test)

e <- ecdf(x = test$simulated_streamflow_m3s)

plot(x = e)

e(v = 2300)
e(v = 5250)

dta_all <- do.call(what = rbind, 
                   args = dta_all)

summary(object = dta_all)


saveRDS(object = dta_all,
        file = paste0(pth, "data/camel_data.rds"))
