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

stations <- unique(x = dta[, .(STATION, MDFUNCTION)])

n <- stations[, .(.N),
              by = STATION]

dta_sub <- dta[STATION %in% sample(size = 5,
                                   x = STATION) &
                 TIMEFUNCTION == "AVG" &
                 MDFUNCTION == "AVG" &
                 YEAR >= 1988]
dta_sub[, DATE := as.IDate(paste(YEAR, MONTH, 1, sep = "-"),
                           format = "%Y-%m-%d")]

ggplot(data = dta_sub,
       mapping = aes(x = DATE,
                     y = VALUE,
                     colour = STATION)) +
  geom_point(size = .5) +
  geom_line() +
  facet_wrap(facets = ~STATION,
             ncol = 1) +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(data = dta_sub,
       mapping = aes(x = MONTH,
                     y = VALUE,
                     group = MONTH,
                     fill = STATION)) +
  geom_boxplot() +
  facet_wrap(facets = ~STATION,
             ncol = 5) +
  theme_bw()

ggplot(data = dta_sub,
       mapping = aes(x = VALUE,
                     y = after_stat(x = density))) +
  geom_histogram(binwidth = 1,
                 colour = "grey50",
                 fill = NA) +
  stat_density(trim = TRUE,
               geom = "line",
               colour = "red4",
               linewidth = 1) +
  theme_bw() +
  facet_wrap(facets = ~STATION,
             ncol = 5)

dta_l <- split(x = dta_sub,
               f = dta_sub$STATION)

acf_l  <- lapply(
  X = dta_l, 
  FUN = function(x){
    
    acf_e <- acf(x = x$VALUE,
                 plot = FALSE,
                 lag.max = 10)
    out <- with(data = acf_e, 
                expr = data.frame(lag, acf))
    
    out
  }
)

dta_acf <- rbindlist(l = acf_l,
                     idcol = "STATION")

ggplot(data = dta_acf, 
       mapping = aes(x = lag, 
                     y = acf)) +
  geom_hline(yintercept = 0, 
             colour = "grey15") +
  geom_hline(yintercept = c(-.1, .1), 
             colour = "red4",
             linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, 
                             yend = 0), 
               colour = "royalblue4") +
  labs(x = "Lag",
       y = "acf",
       title = "ACF for 5 staions") +
  scale_x_continuous(breaks = 0:10,
                     labels = 0:10,
                     minor_breaks = NULL) +
  facet_wrap(facets = ~STATION,
             ncol = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5, 
                                   hjust = 1))

mx <- dta_sub[, .(mx = max(x = VALUE,
                           na.rm = TRUE)),
              by = .(STATION, YEAR)]

mx[, p := (rank(mx) - .3)/(length(mx) + .4),
   by = .(STATION)]

ggplot(data = mx,
       mapping = aes(x = -log(x = -log(x = p)),
                     y = mx)) +
  geom_point() +
  facet_wrap(facets = ~STATION, 
             nrow = 1) +
  labs(x = expression(-log(-log(p))),
       y = "Quantile",
       title = "Gumbelplot",
       subtitle = "Annual temperature maxima") +
  theme_bw() +
  coord_fixed()


