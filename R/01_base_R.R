a <- 12

class(x = a)
class(x = "a")

class(x = Sys.Date())
Sys.Date()

## ?fun, or press f1

methods(class = "Date")
methods(generic.function = "plot")

b <- c(1, 2, 5, 12, pi)
b[3]

class(x = b)

i <- c(1L,5L, 88L)

class(x = i)

x <- c(i, b[1])

class(x = x)

y <- c(i, b[1], "pi")

class(x = y)

## super awesome class
factor()

a <- sample(x = letters, 
            size = 10e7, 
            replace = TRUE)
object.size(x = a)

b <- as.factor(x = a)

object.size(x = b)

head(x = a)
head(x = b)

m <- matrix(data = rnorm(n = 25), 
            ncol = 5)

m[c(1, 3), 2]

# array -> [, , 3]

## data.frame | list

x <- list()

df <- data.frame(date = seq.Date(from = Sys.Date(),
                                 by = "day", 
                                 length.out = 50),
                 val = rnorm(n = 50))

df

class(x = df)
str(object = df)

plot(x = df, 
     type = "l")

format(x = df[5, 1], 
       format = "%Y")

format(x = df[5, 1], 
       format = "%a")

format(x = df[5, 1], 
       format = "%A")

format(x = df[5, 1], 
       format = "%B")

format(x = df[5, 1], 
       format = "%b")

## create a fun: select wednesdays from df,
##               calc desc stat for the selected values

fun <- function(x, day = "Wed") {
  
  # x <- df
  
  ## error handling with try|catch|stop
  
  if (day == "All") {

    aux <- split(x = df$val,
                 f = format(x = x$date,
                            format = "%a"))
    
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
    
    out <- do.call(what = rbind, 
                   args = out)
  } else {
    
    val <- x[format(x = x$date,
                    format = "%a") %in% day, "val"]
    out <- c("number of values" = length(x = val),
             "mean" = mean(x = val, 
                           na.rm = TRUE),
             "sd" = sd(x = val, 
                       na.rm = TRUE),
             "iqr" = IQR(x = val, 
                         na.rm = TRUE))
  }
  
  out
}

fun(x = df)
fun(x = df,
    day = "Mon")
fun(x = df,
    day = "All")
