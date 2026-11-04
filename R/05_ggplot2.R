# ==============================================================================
# 05 GRAMMER OF GRAPHICSWITH GGPLOT2
# ==============================================================================
# OUTLINE:
#   1. The Grammar of Graphics Philosophy: Data + Aesthetics + Geoms
#   2. Univariate Distributions: Histograms, Densities, Boxplots & Violins
#   3. Time Series & Hyetographs: geom_line(), geom_col(), and Date Scaling
#   4. Scatter Plots & Trendlines: geom_point(), geom_smooth(), Overplotting
#   5. Small Multiples: Faceting across Analytes and Regions (facet_wrap / grid)
#   6. Scales & Color Palettes: Continuous, Discrete, and Log Transformations
#   7. Publication Polish: Custom Themes, Typography, and ggsave()
# ==============================================================================

# Install packages if not already available:
# Define required packages
lop <- c("data.table", "ggplot2", "scales", "viridis")

# Identify and install missing packages
to_install <- setdiff(lop, .packages(all.available = TRUE))
if (length(to_install)) install.packages(to_install)

# Load packages without saving return values
invisible(lapply(lop, library, character.only = TRUE))

# Clean up
rm(lop, to_install)


# ==============================================================================
# 0. RECAP: PREPARING THE PLOTTING OBJECTS
# ==============================================================================

# Ensure 'dt' has consistent station IDs and date classes:
# (From previous lectures: station_id is based on location to link sensors)
dt[, `:=`(station_id = as.factor(iconv(x = gsub(pattern = "[ /.]", 
                                                replacement = "", 
                                                x = location), 
                                       to = "ASCII//TRANSLIT")),
          date = as.Date(date))] # ggplot2 seamlessly handles base Date and IDate

# Prepare wide-format chemistry data for bivariate scatter plots:
chem_wide <- dcast(data = dt[variable %chin% c("rain", "pH", "SO4(2-)", "NO3(-)", "Ca")],
                   formula = station_id + region + location + date + year ~ variable,
                   value.var = "val",
                   fun.aggregate = mean,
                   fill = NA)

# Prepare long-format data for automated faceted comparisons:
chem_long <- melt(data = chem_wide,
                  id.vars = c("station_id", "region", "location", "date", "year"),
                  measure.vars = c("rain", "pH", "SO4(2-)", "NO3(-)", "Ca"),
                  variable.name = "analyte",
                  value.name = "concentration",
                  na.rm = TRUE)


# ==============================================================================
# 1. THE GRAMMAR OF GRAPHICS MENTAL MODEL
# ==============================================================================

## THE CORE GGPLOT2 ARCHITECTURE:
#
#   ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) +
#     <GEOM_FUNCTION>(mapping = aes(...), stat = ..., position = ...) +
#     <COORDINATE_FUNCTION>() +
#     <FACET_FUNCTION>() +
#     <SCALE_FUNCTION>() +
#     <THEME_FUNCTION>()
#
# - DATA        : Must be a data.frame or data.table (tidy long format preferred).
# - AESTHETICS  : aes(x, y, color, fill, shape, size, alpha) -> maps DATA to visual channels.
# - GEOMS       : Geometric marks representing rows (points, lines, bars, ribbons).

# TEACHING GOTCHA: Mapping vs. Setting
# - Mapping: aes(color = region) -> Color depends on data values (inside aes).
# - Setting: geom_point(color = "steelblue") -> Fixed constant color (OUTSIDE aes).


# ==============================================================================
# 2. UNIVARIATE DISTRIBUTIONS (HISTOGRAMS, DENSITY & BOXPLOTS)
# ==============================================================================

# --- A. Histograms & Density Curves ---
# Environmental concentrations often exhibit strong positive skew (log-normal).
# Inspect the distribution of sulfate concentrations (SO4(2-)):

p_dist <- ggplot(data = dt[variable == "SO4(2-)" & val > 0], 
                 mapping = aes(x = val)) +
  # Raw histogram showing frequency counts
  geom_histogram(binwidth = 250, 
                 fill = "#2c3e50", 
                 color = "grey50", 
                 alpha = 0.8) +
  labs(title = "Distribution of Sulfate Concentrations in Precipitation",
       subtitle = "CHMI Historical Wet Deposition Network",
       x = "SO4(2-) Concentration [ug/l]",
       y = "Observation Count") +
  theme_minimal()

p_dist

# --- B. Multi-Group Boxplots & Violins Across Decades ---
# Track the historical recovery of acid rain (pH values grouped by decade):
dt[, decade := as.factor((year %/% 10) * 10)]

p_ph_decades <- ggplot(data = dt[variable == "pH" & val >= 3 & val <= 9 & !is.na(decade)], # This discards error flags (e.g., 9999)
                       mapping = aes(x = decade, 
                                     y = val, 
                                     fill = decade)) +
  # Violin shows probability density width; boxplot gives IQR and median
  geom_violin(trim = FALSE, 
              alpha = 0.5, 
              show.legend = FALSE) +
  geom_boxplot(width = 0.2, 
               color = "black", 
               outlier.alpha = 0.2, 
               show.legend = FALSE) +
  # Threshold line: Rain with pH < 5.6 is technically acidic; pH < 4.0 is severe acid rain
  geom_hline(yintercept = 5.6, 
             linetype = "dashed", 
             color = "darkred", 
             linewidth = 0.8) +
  annotate("text",
           x = 1.2, 
           y = 5.8, 
           label = "Clean Rain Threshold (pH 5.6)", 
           color = "darkred", 
           size = 3.5, 
           fontface = "bold") +
  scale_fill_viridis_d(option = "mako") +
  labs(title = "Recovery from Acid Rain in the Czech Republic (1970s - Present)",
       subtitle = "Precipitation pH distributions grouped by decade",
       x = "Decade",
       y = "pH Value") +
  theme_minimal()

p_ph_decades


# ==============================================================================
# 3. TIME SERIES, TRENDLINES & HYETOGRAPHS
# ==============================================================================

# --- A. Precipitation Hyetograph (Impulse / Column Bars) ---
# Filter one specific station with long-term monitoring:
modletin_rain <- dt[location == "Modletín" & variable == "rain"]

p_hyeto <- ggplot(data = modletin_rain, 
                  mapping = aes(x = date, y = val)) +
  # geom_col() represents vertical columns starting at zero (ideal for precipitation)
  geom_col(fill = "#3498db", 
           width = 25) +
  scale_x_date(date_breaks = "5 years", 
               date_labels = "%Y", 
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = label_comma(suffix = " mm")) +
  labs(title = "Monthly Precipitation Hyetograph: Modletín",
       x = "Observation Date",
       y = "Monthly Total") +
  theme_minimal()

p_hyeto

# --- B. Continuous Trend with Generalized Additive Model (GAM) Smoothing ---
# Track the long-term decline of atmospheric sulfate across all Czech stations:
p_so4_trend <- ggplot(data = dt[variable == "SO4(2-)" & val < 10000], 
                      mapping = aes(x = date, 
                                    y = val)) +
  # Semi-transparent scatter points to prevent overplotting
  geom_point(alpha = 0.08, 
             size = 0.8, 
             color = "gray30") +
  # Non-parametric trendline using GAM (mgcv) smoothing:
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs"), 
              color = "#e74c3c",
              linewidth = 1.2,
              fill = "#f1948a") +
  scale_x_date(date_breaks = "5 years", 
               date_labels = "%Y") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Atmospheric Desulfurization: 50-Year Sulfate Decline",
       subtitle = "Points represent monthly wet deposition samples across all stations",
       x = "Year",
       y = "SO4(2-) Concentration [ug/l]") +
  theme_minimal()

p_so4_trend


# ==============================================================================
# 4. BIVARIATE SCATTER PLOTS & OVERPLOTTING MITIGATION
# ==============================================================================

# TASK: Examine the stoichiometric relationship between Sulfate and Nitrate in acid rain.
# Using 'chem_wide' where both ions are aligned on the same row:
chem_complete <- chem_wide[!is.na(`SO4(2-)`) & !is.na(`NO3(-)`)]

# --- TECHNIQUE: 2D Hexagonal Binning for High-Density Datasets ---
# With tens of thousands of points, standard points form an indistinguishable blob.
# geom_hex() divides the Cartesian plane into regular hexagons and counts points per bin:
p_hex <- ggplot(data = chem_complete, 
                mapping = aes(x = `NO3(-)`, y = `SO4(2-)`)) +
  geom_hex(bins = 45) +
  scale_fill_viridis_c(trans = "log10", 
                       option = "inferno", 
                       labels = label_comma(), 
                       name = "Sample\nCount") +
  # Add log-log regression line
  geom_smooth(method = "lm", color = "cyan4", 
              linetype = "dashed", se = FALSE) +
  scale_x_log10(labels = label_comma()) +
  scale_y_log10(labels = label_comma()) +
  labs(title = "Bivariate Density: Sulfate vs. Nitrate Deposition",
       subtitle = "Plotted on log10-log10 axes to linearize power relationships",
       x = "NO3(-) Concentration [ug/l] (log scale)",
       y = "SO4(2-) Concentration [ug/l] (log scale)") +
  theme_minimal() +
  theme(legend.position = "right")

p_hex


# ==============================================================================
# 5. SMALL MULTIPLES / FACETING
# ==============================================================================

# FACETING PRINCIPLES:
# - facet_wrap(~ group)        : Wraps a 1D sequence of panels into a 2D ribbon/grid.
# - facet_grid(row_var ~ col_var): Creates an explicit 2D matrix of subsets.
# - scales = "free_y"          : Lets each panel adapt its own y-axis limits (essential 
#                                when variables have differing units/magnitudes).

# TASK: Plot multi-analyte historical trends simultaneously using 'chem_long'
selected_analytes <- c("rain", "SO4(2-)", "NO3(-)")

p_faceted <- ggplot(data = chem_long[analyte %chin% selected_analytes], 
                    mapping = aes(x = date, 
                                  y = concentration, 
                                  color = analyte)) +
  geom_point(alpha = 0.05, 
             size = 0.5) +
  geom_smooth(method = "loess",
              span = 0.2,
              se = FALSE, 
              color = "black", 
              linewidth = 0.9) +
  facet_wrap(~ analyte,
             scales = "free_y", ###
             nrow = 1) +
  scale_x_date(date_breaks = "10 years", 
               date_labels = "%Y") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Multi-Parameter Environmental Profiles Over Time",
       subtitle = "Loess smoothed trends for key wet deposition variables",
       x = "Year",
       y = "Measured Value (native units)") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#ecf0f1", 
                                        color = "gray80"),
        strip.text = element_text(face = "bold", 
                                  size = 11))

p_faceted


# ==============================================================================
# 6. SCALES, PALETTES & THEME CUSTOMIZATION (PUBLICATION STANDARDS)
# ==============================================================================

# Build a customized publication-ready plot:
# Regional annual rainfall comparison with custom typography, limits, and palette:

regional_annual_rain <- dt[variable == "rain", .(mean_rain = mean(val, 
                                                                  na.rm = TRUE)), 
                           by = .(region, year)][!is.na(region) & year >= 1980]

p_pub <- ggplot(data = regional_annual_rain[year > 2000], 
                mapping = aes(x = year, 
                              y = mean_rain, 
                              color = region)) +
  geom_line(linewidth = 0.9, 
            alpha = 0.85) +
  geom_point(size = 1.4) +
  # Scientific color palette:
  scale_color_viridis_d(option = "turbo", 
                        name = "Administrative\nRegion") +
  scale_x_continuous(breaks = seq(1980, 2025, 
                                  by = 5)) +
  scale_y_continuous(labels = label_comma(suffix = " mm"), 
                     limits = c(0, NA)) +
  labs(title = "Regional Annual Mean Monthly Precipitation",
       subtitle = "CHMI Meteorological Stations (1980 - 2025)",
       caption = "Data source: Czech Hydrometeorological Institute (CHMI open data)",
       x = "Calendar Year",
       y = "Mean Monthly Precipitation") +
  # Custom theme modifications:
  theme_minimal(base_size = 11, 
                base_family = "sans") +
  theme(plot.title = element_text(face = "bold", 
                                  size = 14,
                                  color = "gray15"),
        plot.subtitle = element_text(margin = margin(b = 10), 
                                     color = "gray40"),
        plot.caption = element_text(face = "italic", 
                                    color = "gray60",
                                    margin = margin(t = 10)),
        axis.title.x = element_text(face = "bold", 
                                    margin = margin(t = 8)),
        axis.title.y = element_text(face = "bold",
                                    margin = margin(r = 8)),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted",
                                          color = "gray85"),
        panel.grid.major.y = element_line(color = "gray90"),
        legend.position = "right",
        legend.title = element_text(face = "bold", 
                                    size = 9),
        legend.background = element_rect(fill = "gray98",
                                         color = "transparent"))

p_pub


# ==============================================================================
# 7. EXPORTING HIGH-RESOLUTION GRAPHICS (ggsave)
# ==============================================================================

# ggsave() automatically inspects the file extension to pick the right device driver
# (e.g., .png, .pdf, .svg, .tiff).

# Save as publication-grade raster image (300 DPI print standard):
ggsave(filename = "chmi_regional_precipitation.png",
       plot = p_pub,
       path = tempdir(), # Replace with your target project directory
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

# Save as lossless vector PDF (ideal for LaTeX / publication manuscripts):
ggsave(filename = "chmi_regional_precipitation.pdf",
       plot = p_pub,
       path = tempdir(),
       width = 10,
       height = 6,
       units = "in",
       device = cairo_pdf) # High-fidelity font and transparency rendering