download.file(url = "https://zenodo.org/records/3964745/files/03_CAMELS_BR_streamflow_mm_selected_catchments.zip?download=1",
              destfile = "./data/03_CAMELS_BR_streamflow_mm_selected_catchments.zip")

file.exists("./data/03_CAMELS_BR_streamflow_mm_selected_catchments.zip")

dir.create(path = "./data/camel_data")
unzip(zipfile = "./data/03_CAMELS_BR_streamflow_mm_selected_catchments.zip",
      exdir = "./data/camel_data")

fls <- list.files(path = "./data/camel_data/03_CAMELS_BR_streamflow_mm_selected_catchments/", 
                  full.names = TRUE,
                  recursive = TRUE,
                  pattern = ".txt")

dta_all <- lapply(
  X = fls, 
  FUN = function(i) {
    
    dta <- read.table(file = i, 
                      header = TRUE,
                      na.strings = "nan")
    
    dta$date <- as.Date(x = paste(dta$year,
                                  dta$month,
                                  dta$day,
                                  sep = "-"),
                        format = "%Y-%m-%d")
    aux <- strsplit(x = i, 
                    split = "/")[[1]]
    dta$id <- strsplit(x = aux[length(aux)], 
                       split = "_")[[1]][1]
    dta_out <- dta[, c("date", "streamflow_mm", "id")]
    
    dta_out
  }
)

dta_all <- do.call(what = rbind,
                   args = dta_all)
dta_all$id <- as.factor(x = dta_all$id)

dta_all <- na.omit(object = dta_all)

saveRDS(object = dta_all,
        file = "./data/camel_br.rds")

test <- readRDS(file = "./data/camel_br.rds")
