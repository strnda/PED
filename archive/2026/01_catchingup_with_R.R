TRUE
FALSE

# Creates numeric object of 1
a <- 1 

# Shows memory used by 'a'
object.size(x = a)

# Shows class of 'a'
class(x = a)

# error : non numerical argument for binary operator
a + "b"

# gets today's date as 'd'
d <- Sys.Date()

# shows class of 'd'
class(x = d)

# lists methods for generic plot
methods(generic.function = plot)
# plot with no data (warning)
plot()

# lists methods for objects of class "Date"
methods(class = "Date")
# plot with no data (no effect)
plot()

# adds 20 days to date
d + 20

# prints the new date
d
# formats date as weekday name
format(x = d,
       format = "%A")

# creates a vector (100 000 000 letter characters)
x <- sample(x = letters,
            size = 10E7, 
            replace = TRUE)
# shows memory of 'x' (800001504 bytes)
object.size(x = x)
# converts to factor (smaller memory required)
y <- as.factor(x = x)
# shows memory of 'y' (400002096 bytes)
object.size(x = y)
# preview of the first elements
head(x = x)
head(x = y)
# builds 4*3 matrix of N(0,1)
m <- matrix(data = rnorm(n = 12),
            nrow = 4)
# selects 2nd row
m[2,]
# selects element (row2, col3)
m[2, 3]
# selects row 2, cols 2 and 3
m[2, c(2, 3)]
# selects row 2, all but col 1
m[2, -1, ]
# shows class and storage mode
class(x = m)
mode(x = m)
# removes big objects
rm(x)
rm(y)
#asks garbage collector for free memory
gc()
# lists objects in workspace
ls()
# removes everything from the lists
rm(list = ls())

# keeps only this object, removes others
whatIwant <- "something"

rm(list = ls()[which(x = ls() != "whatIwant")])

#shows working directory
getwd()
setwd(dir = "")

# sets path for files (windows example)
pth <- "C:/Users/strnadf/Desktop/"

#writes matrix to csv
write.csv(x = m,
          file = paste0(pth, "matrix.csv"))
#checks if file exists and its size
file.exists(paste0(pth, "matrix.csv"))
file.size(paste0(pth, "matrix.csv"))

# loads example dataset "Nile"
data("Nile")

read csv back into dataframe
dta <- read.csv(file = paste0(pth, "matrix.csv"))

### create dataframes of values (at least 1000)
### drawn from 4 distributions, save the files  
### import the files into one big dataframe

### see lapply, for loops

### system.time for benchmark

# Shows page pointing to distributions (rnorm, dnorm, ...)
?distribution

# sets path to create the directory
pth <- "C:/Users/strnadf/Desktop/"
# Names used to build the function names
dst <- c("norm", "lnorm", "cauchy", "exp")
# Number of draws per distribution
n <- 1000 

# Generetas a folder for the distribution data
dir.create(path = paste0(pth, "dist_data"))

# Creates, add labels and saves one CSV file per distribtion
aux <- lapply(
  X = dst, 
  FUN = function(x) {
    # Makes data for one distribution
    dta <- data.frame(val = do.call(what = paste0("r", x),
                                    args = list(n = n)),
                      dist = x)
    # writes csv without row names
    write.csv(x = dta,
              file = paste0(pth, "dist_data/",
                            x, ".csv"),
              row.names = FALSE)
  }
)
# list the created csv files
fls <- list.files(path = paste0(pth, "dist_data/"),
                  pattern = ".csv")
# converts csvs into dataframes
dta_all <- lapply(X = paste0(pth, "dist_data/", fls), 
                  FUN = read.csv)
# combines all dataframes into one
dta_all <- do.call(what = rbind,
                   args = dta_all)
