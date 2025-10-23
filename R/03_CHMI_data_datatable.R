url <- "https://opendata.chmi.cz/meteorology/climate/historical_csv/data/monthly/temperature/"

x <- read.csv(file = url)

x <- x[apply(X = x, 
             MARGIN = 1,
             grepl,
             pattern = "href=mly"),]

fls <- sapply(
  X = x, 
  FUN = function(i) {
    
    # i <- x[12]
    
    gsub(pattern = "<a href=",
         replacement = "",
         x = strsplit(x = i,
                      split = ".csv")[[1]][1])
  }
)

fls <- as.vector(x = fls)

dta_all <- lapply(X = paste0(url,
                             fls[1:10],
                             ".csv"), 
                  FUN = read.csv)

dta_all <- do.call(what = rbind,
                   args = dta_all)

unique(x = dta_all$ELEMENT)

## select "TMI" from all the stations and calculate mean, sd, iqr, min, max, 
## q - 5%, 10%, 15%, 25%, median, 75%, 85%, 90%, 95%

dta_sub <- dta_all[dta_all$ELEMENT == "TMI", c("STATION", "YEAR", "MONTH", "VALUE")]
dta_sub <- na.omit(object = dta_sub)

desc_stat <- function(x, na.rm = TRUE) {
  c(mean = mean(x = x,
                na.rm = na.rm),
    sd = sd(x = x,
            na.rm = na.rm),
    iqr = IQR(x = x,
              na.rm = na.rm),
    min = min(x = x,
              na.rm = na.rm),
    max = max(x = x,
              na.rm = na.rm),
    quantile(x = x,
             na.rm = na.rm,
             probs = c(.05, .1, .15, .25, .5, .75, .85, .90, .95)))
}

desc_stat(x = x)

dta_l <- split(x = dta_sub,
               f = dta_sub$STATION)

stat <- sapply(
  X = dta_l, 
  FUN = function(i) {
    desc_stat(x = i$VALUE)
  }
)

View(stat)

install.packages("data.table")

library(data.table)

class(dta_all)
str(object = dta_all)

dta <- as.data.table(x = dta_all)

class(dta)
str(object = dta)

dta_all
dta

dta[ELEMENT == "TMI", desc_stat(x = VALUE), by = STATION]

names(x = dta)

dta[, RN := rnorm(n = .N)]

dta
min_val <- dta[, .(min = min(x = VALUE, 
                             na.rm = TRUE),
                   max = max(x = VALUE, 
                             na.rm = TRUE)),
               by = .(STATION, ELEMENT, YEAR)]

dta_long <- na.omit(object = dta[ELEMENT == "TMI" & MDFUNCTION == "MIN", .(STATION, YEAR, MONTH, VALUE)])
dta_wide <- dcast(data = dta_long,
                  formula = YEAR + MONTH ~ STATION,
                  value.var = "VALUE")

object.size(x = dta_long)
object.size(x = dta_wide)

dta_long[, STATION := as.factor(x = STATION)]

object.size(x = dta_long)
object.size(x = dta_wide)

dta_long_again <- melt(data = dta_wide,
                       id.vars = c("YEAR", "MONTH"),
                       variable.name = "STATION",
                       value.name = "VALUE")

# dta[i(rows), j(cols), by(groups)]
