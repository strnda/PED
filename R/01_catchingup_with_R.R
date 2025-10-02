TRUE
FALSE

a <- 1 

object.size(x = a)

class(x = a)

a + "b"

d <- Sys.Date()

class(x = d)

methods(generic.function = plot)
plot()

methods(class = "Date")
plot()

d + 20

d
format(x = d,
       format = "%A")

x <- sample(x = letters,
            size = 10E7, 
            replace = TRUE)

object.size(x = x)

y <- as.factor(x = x)

object.size(x = y)

head(x = x)
head(x = y)

m <- matrix(data = rnorm(n = 12),
            nrow = 4)

m[2,]
m[2, 3]

m[2, c(2, 3)]
m[2, -1, ]

class(x = m)
mode(x = m)

rm(x)
rm(y)

gc()

ls()

rm(list = ls())

whatIwant <- "something"

rm(list = ls()[which(x = ls() != "whatIwant")])


getwd()
setwd(dir = "")

pth <- "C:/Users/strnadf/Desktop/"

write.csv(x = m,
          file = paste0(pth, "matrix.csv"))

file.exists(paste0(pth, "matrix.csv"))
file.size(paste0(pth, "matrix.csv"))

data("Nile")

dta <- read.csv(file = paste0(pth, "matrix.csv"))

### create dataframes of values (at least 1000)
### drawn from 4 distributions, save the files  
### import the files into one big dataframe

### see lapply, for loops

### system.time for benchmark

?distribution

pth <- "C:/Users/strnadf/Desktop/"
dst <- c("norm", "lnorm", "cauchy", "exp")
n <- 1000

dir.create(path = paste0(pth, "dist_data"))

aux <- lapply(
  X = dst, 
  FUN = function(x) {
    
    dta <- data.frame(val = do.call(what = paste0("r", x),
                                    args = list(n = n)),
                      dist = x)
    
    write.csv(x = dta,
              file = paste0(pth, "dist_data/",
                            x, ".csv"),
              row.names = FALSE)
  }
)

fls <- list.files(path = paste0(pth, "dist_data/"),
                  pattern = ".csv")

dta_all <- lapply(X = paste0(pth, "dist_data/", fls), 
                  FUN = read.csv)

dta_all <- do.call(what = rbind,
                   args = dta_all)
