########
lop <- c("data.table", "fst", "ggplot2")

to_instal <- lop[which(x = !(lop %in% installed.packages()[,"Package"]))]

if(length(to_instal) != 0) {
  
  install.packages(to_instal)
}

temp <- lapply(X = lop, 
               FUN = library, 
               character.only = T)
rm(temp, lop, to_instal)

########

x <- rnorm(n = 1000)

p <- plot(x = x,
          type = "b",
          col = "red4")

p

p <- ggplot() +
  geom_line(mapping = aes(x = 1:1000,
                          y = x),
            colour = "red4") +
  theme_bw()
p

##

dta <- fst(path = "../Downloads/dta_wide.fst")
dta_sub <- dta[, c(1, sample(x = 2:ncol(x = dta), 
                             size = 6))]

dta_sub

p <- ggplot(data = dta_sub[, c(1, 2)],
            mapping = aes(x = date,
                          y = `76310000`)) +
  geom_line(colour = "red1") +
  geom_point(colour = "darkgreen",
             na.rm = TRUE) +
  labs(x = "Time",
       y = "Discharge",
       title = "my super duper ggplot",
       subtitle = "it really is awesome") +
  theme_bw()

p + theme_dark()

dta_m <- melt(data = as.data.table(x = dta_sub),
              id.vars = "date")

dta_m

p <- ggplot() +
  geom_line(data = dta_m,
            mapping = aes(x = date,
                          y = value,
                          colour = variable),
            na.rm = TRUE) +
  facet_wrap(facets = ~variable, 
             scales = "free") 
p
saveRDS(object = p,
        file = file.path(getwd(), "grob.rds"))

grob <- readRDS(file = file.path(getwd(), "grob.rds"))

grob + 
  scale_color_manual(name = "",
                     values = c("steelblue4",
                                "purple",
                                "#1faa0c",
                                "gold3",
                                "darkorange",
                                "red3")) +
  theme(legend.position = "none") +
  labs(x = "Time",
       y = "Discharge")

dta_m

ggplot() +
  geom_boxplot(data = dta_m,
               mapping = aes(x = variable,
                             y = value,
                             fill = variable),
               color = "purple",
               outliers = FALSE,
               na.rm = TRUE)

x <- rnorm(n = 1000)
y <- rpois(n = 1000, 
           lambda = 3)
layout(mat = matrix(data = 1:2,
                    ncol = 2))
plot(x = ecdf(x = x))
plot(x = ecdf(x = y))

ggplot() +
  geom_density(data = dta_m,
               mapping = aes(x = value)) +
  facet_wrap(facets = ~variable)

ggplot() +
  stat_ecdf(data = dta_m,
            mapping = aes(x = value)) +
  facet_wrap(facets = ~variable)

ggplot() +
  geom_histogram(data = dta_m,
                 mapping = aes(x = value)) +
  facet_wrap(facets = ~variable)

grob + coord_polar()
grob + coord_radial()

grob + lims(y = c(0, 5))

ggplot(data = dta_sub[, c(1, 2)],
       mapping = aes(x = date,
                     y = `76310000`)) +
  geom_point(colour = "darkgreen",
             na.rm = TRUE) +
  stat_smooth(method = "lm") +
  theme_bw()

dta_m[, q := quantile(x = value,
                      probs = .95, 
                      na.rm = TRUE),
      by = variable]

ggplot(data = dta_m) +
  geom_line(mapping = aes(x = date,
                          y = value,
                          colour = variable),
            na.rm = TRUE) +
  geom_hline(mapping = aes(yintercept = q), 
             linetype = 2, 
             colour = "red3") +
  facet_wrap(facets = ~variable) 
