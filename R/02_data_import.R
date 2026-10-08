## file link -> https://opendata.chmi.cz/air_quality/historical/precipitation/

## find the file url & dl the file

?scan
?readLines
?download.file

url <- "https://opendata.chmi.cz/air_quality/historical/precipitation/"
pth <- "C:/Users/strnadf/Downloads/"

test_1 <- scan(file = url)

test_2 <- scan(file = url,
               what = "list")

test_3 <- readLines(con = url)

test_2
test_3

grep(pattern = "data",
     x = test_2, 
     ignore.case = TRUE)
grepl(pattern = "data",
      x = test_2, 
      ignore.case = TRUE)
which(x = grepl(pattern = "data",
                x = test_2, 
                ignore.case = TRUE))

fl_nm <- strsplit(x = test_2[grep(pattern = "data",
                                  x = test_2, 
                                  ignore.case = TRUE)],
                  split = '\"')[[1]][2]

download.file(url = paste0(url, fl_nm),
              destfile = paste0(pth,
                                fl_nm))

file.exists(paste0(pth,
                   fl_nm))
file.size(paste0(pth,
                 fl_nm))
file.info(paste0(pth,
                 fl_nm))

dir.create(path = paste0(pth,
                         "data"))

unzip(zipfile = paste0(pth,
                       fl_nm),
      exdir = paste0(pth,
                     "data"))
fls <- list.files(path = paste0(pth,
                                "data"), 
                  recursive = TRUE,
                  full.names = TRUE)

fls_zip <- fls[grep(pattern = ".zip",
                    x = fls,
                    ignore.case = TRUE)]

## task <- extract the TS and import the data in form of a one big dataframe.

?read.table
?lapply
?Control 

dta_prec <- lapply(
  X = fls_zip, 
  FUN = function(x) {
    
    # x <- fls_zip[20]
    
    e <- try(expr = {
      
      tmp <- tempdir()
      unzip(zipfile = x,
            exdir = tmp)
      
      aux_fls <- list.files(path = tmp, 
                            full.names = TRUE)
      
      dta <- read.csv(file = aux_fls[grep(pattern = "data",
                                          x = aux_fls)])
      nfo <- read.csv(file = aux_fls[grep(pattern = "registr",
                                          x = aux_fls)])
      
      out <- cbind(dta, nfo)
    }, 
    silent = TRUE)
    
    if (inherits(x = e,
                 what = "try-error")) {
      
      out <- NA
    } 
    
    out
  }
)

dta_prec <- do.call(what = rbind,
                    args = dta_prec[!is.na(x = dta_prec)])


