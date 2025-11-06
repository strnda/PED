if(!require(ggplot)) {
  
  install.packages("ggplot2")
  library(ggplot2)
}

library(data.table)

dta <- fread(input = "./data/data_chmi.csv")

class(x = dta)

dta

x <- dta[STATION == "0-20000-0-11406",]

p <- plot(x = x$VALUE,
          type = "h",
          col = "royalblue4",
          main = "plot title")
p

p <- ggplot(data = x[MDFUNCTION %in% c("AVG", "MIN", "MAX") & YEAR >= 1988, ]) +
  geom_point(mapping = aes(x = as.Date(paste(YEAR, MONTH, 1, sep = "-"),
                                       format = "%Y-%m-%d"),
                           y = VALUE,
                           colour = TIMEFUNCTION),
             alpha = .1) +
  facet_wrap(facets = ~MDFUNCTION,
             ncol = 1)
p
p + theme_bw()

p + scale_color_manual(values = c("gray1", "gray99", "red4", "steelblue4"),
                       labels = c("1", "A", "something", "something\nelse"),
                       name = "") +
  scale_y_log10() +
  coord_radial()

p + scale_color_manual(values = c("gray1", "gray99", "red4", "steelblue4"),
                       labels = c("1", "A", "something", "something\nelse"),
                       name = "") +
  geom_smooth(mapping = aes(x = as.Date(paste(YEAR, MONTH, 1, sep = "-"),
                                        format = "%Y-%m-%d"),
                            y = VALUE,
                            colour = TIMEFUNCTION),
              method = "lm",
              formula = y ~ poly(x = x,
                                 degree = 5),
              se = FALSE) + 
  geom_line(mapping = aes(x = as.Date(paste(YEAR, MONTH, 1, sep = "-"),
                                      format = "%Y-%m-%d"),
                          y = VALUE,
                          colour = TIMEFUNCTION))

## choose 5 stations with just temperatues (one variable for each station)
## box plots - mthly for each station,
## densities & histograms in one panel for aeach station
## ACF - autocorrelation function

## if you feel cool draw a gumbel plot...
## gumbel x-axis -log(-log(p)); y-axis - corresponding quantile
## find "p" using a plotting position method (idealy Chegodayev)


