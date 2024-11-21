## Grammar of Graphics with ggplot2 ##

# Step 1: Specify required packages
lop <- c("data.table", "fst", "ggplot2")

# Step 2: Identify and install any missing packages
to_instal <- lop[which(x = !(lop %in% installed.packages()[, "Package"]))]
if (length(to_instal) != 0) {
  install.packages(to_instal)
}

# Step 3: Load the packages
temp <- lapply(X = lop, 
               FUN = library, 
               character.only = TRUE)

# Clean up environment
rm(temp, lop, to_instal)

# **EXERCISE 1:** Add the package "dplyr" to the list and check if it installs correctly.

## RANDOM DATA GENERATION AND PLOTTING ##

# Step 4: Generate random data
x <- rnorm(n = 1000)  # Generate 1000 random numbers from a normal distribution

# Step 5: Create a base R plot
p <- plot(x = x, 
          type = "b",  # Line and points
          col = "red4")  # Line color
p

# Step 6: Create a ggplot version of the plot
p <- ggplot() +
  geom_line(mapping = aes(x = 1:1000, y = x), colour = "red4") +
  theme_bw()
p

# **EXERCISE 2:** Experiment with `geom_point` to overlay points on the line plot.

## WORKING WITH DATA FILES ##

# Step 7: Load a wide-format dataset (example uses .fst format)
dta <- fst(path = "./data/dta_wide.fst")

# Step 8: Subset the dataset to include the first column and 6 random columns
dta_sub <- dta[, c(1, sample(x = 2:ncol(x = dta), 
                             size = 6))]

## PLOTTING TIME SERIES ##

# Step 9: Create a time-series plot for one variable
p <- ggplot(data = dta_sub[, c(1, 2)],
            mapping = aes(x = date, 
                          y = .data[[names(x = dta_sub)[2]]])) +
  geom_line(colour = "red1",
            na.rm = TRUE) +
  geom_point(colour = "darkgreen",
             na.rm = TRUE) +
  labs(x = "Time", 
       y = "Discharge", 
       title = "My Super Duper ggplot", 
       subtitle = "It really is awesome") +
  theme_bw()
p + theme_dark()

# **EXERCISE 3:** Modify the title and subtitle to describe the dataset more specifically.

## MELTING AND FACET PLOTS ##

# Step 10: Convert the data to long format for visualization
dta_m <- melt(data = as.data.table(x = dta_sub), 
              id.vars = "date")
dta_m

# Step 11: Plot multiple time series in a faceted plot
p <- ggplot() +
  geom_line(data = dta_m, 
            mapping = aes(x = date, 
                          y = value, 
                          colour = variable), 
            na.rm = TRUE) +
  facet_wrap(facets = ~variable, 
             scales = "free")
p

# Save and reload the plot object
saveRDS(object = p, 
        file = file.path(getwd(), "grob.rds"))
grob <- readRDS(file = file.path(getwd(), "grob.rds"))

# Step 12: Customize colors and remove the legend
grob + 
  scale_color_manual(name = "", 
                     values = c("steelblue4", "purple", "#1faa0c", "gold3", "darkorange", "red3")) +
  theme(legend.position = "none") +
  labs(x = "Time", y = "Discharge")

# **EXERCISE 4:** Add a new variable to the dataset and include it in the melted data for plotting.

## BOXPLOTS ##

# Step 13: Create a boxplot for all variables
ggplot() +
  geom_boxplot(data = dta_m, 
               mapping = aes(x = variable, 
                             y = value, 
                             fill = variable), 
               color = "purple", 
               na.rm = TRUE)

# **EXERCISE 5:** Adjust the boxplot to show only variables with mean discharge > 10.

## EMPIRICAL CUMULATIVE DISTRIBUTION FUNCTIONS ##

# Step 14: Compare distributions of random variables
x <- rnorm(n = 1000)
y <- rpois(n = 1000, 
           lambda = 3)

layout(mat = matrix(data = 1:2, ncol = 2))
plot(x = ecdf(x = x))
plot(x = ecdf(x = y))

# ggplot version for density plots
ggplot() +
  geom_density(data = dta_m, mapping = aes(x = value)) +
  facet_wrap(facets = ~variable)

# **EXERCISE 6:** Use `stat_ecdf` to create ECDF plots for all variables in the dataset.

## HISTOGRAMS ##

# Step 15: Create histograms for each variable
ggplot() +
  geom_histogram(data = dta_m, mapping = aes(x = value)) +
  facet_wrap(facets = ~variable)

## COORDINATE SYSTEMS ##

# Step 16: Explore alternative coordinate systems
grob + coord_polar()
grob + coord_radial()

# Set y-axis limits
grob + lims(y = c(0, 5))

## REGRESSION LINE ##

# Step 17: Fit and plot a regression line
ggplot(data = dta_sub[, c(1, 2)], 
       mapping = aes(x = date, 
                     y = .data[[names(x = dta_sub)[2]]])) +
  geom_point(colour = "darkgreen", 
             na.rm = TRUE) +
  stat_smooth(method = "lm") +  # Linear model fit
  theme_bw()

# **EXERCISE 7:** Use `method = "loess"` for a smooth curve fit.

## CALCULATE QUANTILES AND PLOT ##

# Step 18: Add quantiles and horizontal lines
dta_m[, q := quantile(x = value, probs = .95, na.rm = TRUE), by = variable]

ggplot(data = dta_m) +
  geom_line(mapping = aes(x = date, 
                          y = value,
                          colour = variable), 
            na.rm = TRUE) +
  geom_hline(mapping = aes(yintercept = q), 
             linetype = 2,
             colour = "red3") +
  facet_wrap(facets = ~variable)

# **EXERCISE 8:** Calculate and plot the 25th percentile (Q1) alongside the 95th percentile.

