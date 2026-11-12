# ==============================================================================
# 4. ADVANCED SPATIAL ANALYSIS WORKFLOW WITH sf
# ==============================================================================
# Objective:
#   1. Construct and inspect spatial polygons (simulating boundary shapefiles).
#   2. Perform Spatial Joins (Point-in-Polygon analysis).
#   3. Extract geometric properties (Areas and Centroids) handling "units".
#   4. Compute distance matrices (Proximity analysis).
#   5. Perform spatial aggregation (Dissolving boundaries via unions).
# ==============================================================================


# ==============================================================================
# 1. DOCUMENTATION & ENVIRONMENT SETUP
# ==============================================================================

library(data.table)  
library(sf)          
library(ggplot2)     
library(units)       # CRITICAL for handling measurements (meters, km2) returned by sf
library(RCzechia)    # Provides official spatial objects (boundaries) for the Czech Republic

?st_read       # Read vector data (shapefiles, GeoJSON, KML) from disk
?st_join       # Spatial joins (e.g., merging attributes based on intersection)
?st_area       # Calculate surface area of polygons
?st_centroid   # Find the geometric center of a polygon
?st_distance   # Calculate distance matrices between geometries
?st_union      # Combine multiple geometries into one (dissolve)

# ------------------------------------------------------------------------------
# STEP A: THE POINTS (Stations)
# ------------------------------------------------------------------------------
# We create our stations from the previous module and project them to 
# S-JTSK (EPSG:5514) for accurate metric calculations.

# Convert the raw data frame into a high-performance data.table
dta_prec <- as.data.table(x = dta_prec)

# Define a vector of columns we actually need to save memory
cols_keep <- c("START_TIME", "VALUE", "ID_REGISTRACE", "KRAJ_NAZEV", 
               "LOKALITA_NAZEV", "VELICINA_ZKRATKA", "METODA_ZKRATKA", 
               "ZEMEPISNA_DELKA", "ZEMEPISNA_SIRKA", "NADMORSKA_VYSKA", 
               "JEDNOTKA_ZKRATKA", "DATA_INTERVAL")

# PRO-TIP: The '..' prefix tells data.table to evaluate 'cols_keep' in the 
# calling environment (the global workspace) rather than looking for a column 
# named 'cols_keep' inside the data itself.
dt <- dta_prec[, ..cols_keep]

# Rename columns by reference (zero memory copy). This is much faster and 
# more memory-efficient than `names(dt) <- c(...)` for large datasets.
setnames(x = dt, 
         old = cols_keep, 
         new = c("date_raw", "val", "reg_id", "region", 
                 "location", "variable", "method", 
                 "lon", "lat", "elevation", 
                 "unit", "interval"))

# Data Cleaning & Type Conversion
# let() is the functional equivalent of data.table's `:=` operator.
# It allows us to add or modify multiple columns in-place simultaneously.
dt[, let(
  # Extract the date part of the timestamp and convert to efficient IDate format
  date = as.IDate(x = substr(x = date_raw, 
                             start = 1, 
                             stop = 10)),
  region = as.factor(x = region),
  variable = as.factor(x = variable),
  unit = as.factor(x = unit),
  
  # TEACHING GOTCHA: Dealing with local diacritics in location names.
  # 1. gsub() removes spaces, slashes, and periods.
  # 2. iconv(..., to = "ASCII//TRANSLIT") converts characters like 'č' to 'c'
  # 3. paste() combines it with the registration ID to ensure a unique station identifier.
  station_id = as.factor(x = paste(iconv(x = gsub(pattern = "[ /.]", 
                                                  replacement = "", 
                                                  x = location), 
                                         to = "ASCII//TRANSLIT"),
                                   reg_id, 
                                   sep = "_"))
)]

# Filter for valid spatial coordinates, positive elevation, and rain measurements.
# Then select only the columns relevant for spatial analysis.
dta_spat <- dt[lon > 0 & lat > 0 & elevation > 0 & variable == "rain", 
               .(station_id, val, lon, lat, elevation)]

# Aggregate: Calculate the maximum precipitation (mx) for each station, 
# while keeping the spatial coordinates intact.
dta_spat <- dta_spat[, .(mx = max(val),
                         lat = lat,
                         lon = lon,
                         elevation = elevation), 
                     by = station_id]

# Keep only one row per station for our spatial points dataset
sf_stations <- unique(x = dta_spat)

# EPSG:4326 is the standard WGS84 coordinate system (used by GPS)
crs_wgs <- 4326

# Convert the flat data.table into a spatial 'sf' object
sf_stations <- st_as_sf(x = sf_stations, 
                        coords = c("lon", "lat"), 
                        crs = crs_wgs)

# Transform (reproject) the data into the Czech national grid S-JTSK (EPSG:5514)
# This converts the coordinates from Degrees into Meters!
sf_stations <- st_transform(x = sf_stations, 
                            crs = 5514)

# ------------------------------------------------------------------------------
# STEP B: THE POLYGONS (Regions)
# ------------------------------------------------------------------------------
# In a real workflow, you would read a regional boundary file like this:
# sf_regions <- st_read(dsn = "path/to/data", layer = "czech_regions")

# For this script, we will synthetically create two bounding boxes (Bohemia and Moravia)
# to serve as our regional polygons. (PRO-TIP: you can also use the locator() 
# function in interactive mode to click and draw polygons directly on a plot!)
poly_bohemia <- st_polygon(x = list(matrix(c(12, 48, 16, 48, 16, 51.2, 12, 51.2, 12,48), 
                                           ncol = 2, 
                                           byrow = TRUE)))
poly_moravia <- st_polygon(x = list(matrix(c(16, 48, 19, 48, 19, 51.2, 16, 51.2, 16, 48), 
                                           ncol = 2, 
                                           byrow = TRUE)))

# Combine them into an 'sf' data.frame
sf_regions <- st_sf(REGION_NAME = c("Bohemia", "Moravia-Silesia"),
                    geometry = st_sfc(poly_bohemia,poly_moravia, 
                                      crs = 4326))
# Project the synthetic polygons to S-JTSK (Meters) so they align with our points
sf_regions <- st_transform(x = sf_regions, 
                           crs = 5514)


# ==============================================================================
# 2. SPATIAL JOINS (POINT-IN-POLYGON)
# ==============================================================================
# Goal: Automatically determine which region each weather station falls into.

# st_join() works like a left join in SQL, but uses spatial relationships 
# instead of matching keys.
# PRO-TIP: By default, st_join uses the topological operation `st_intersects`.
# You can change this using the `join` argument (e.g., join = st_within).

sf_joined <- st_join(x = sf_stations, 
                     y = sf_regions, 
                     join = st_intersects)

# Inspect the result: The REGION_NAME from sf_regions has been appended 
# to the stations based purely on their geographic overlap!
print(sf_joined)


# ==============================================================================
# 3. SPATIAL METRICS: AREAS AND CENTROIDS
# ==============================================================================

# Calculate the area of our regions
region_areas <- st_area(x = sf_regions)

# TEACHING GOTCHA: The 'units' package
# Notice the output isn't just a number; it has "[m^2]" attached to it.
# This prevents you from accidentally mixing up meters, feet, or degrees.
print(region_areas)

# To convert meters squared to square kilometers safely:
region_areas_km2 <- set_units(x = region_areas, 
                              value = km^2)
sf_regions$AREA_KM2 <- drop_units(x = region_areas_km2) # Drop units to store as raw numeric

# Extract the Centroids (geometric center) of the regions
sf_centroids <- st_centroid(x = sf_regions)


# ==============================================================================
# 4. DISTANCE MATRICES (PROXIMITY)
# ==============================================================================
# Goal: How far is each weather station from the center of its region?

# st_distance returns a dense matrix of distances between all features in X and Y.
# Rows = sf_stations, Columns = sf_centroids
dist_matrix <- st_distance(x = sf_stations, 
                           y = sf_centroids)

# Let's look at the raw distance matrix (in meters)
print(dist_matrix)

# PRO-TIP: Extracting specific distances.
# Since we know Station 1 and 2 are in Bohemia (Centroid 1), and Station 3 and 4 
# are in Moravia (Centroid 2), we can pull the diagonal-ish values programmatically:
# (In a real dataset, you would merge these IDs first)
sf_stations$DIST_TO_BOHEMIA_CENTER_KM <- drop_units(x = set_units(dist_matrix[, 1], 
                                                                  value = km))


# ==============================================================================
# 5. SPATIAL AGGREGATION (DISSOLVING)
# ==============================================================================
# Goal: We have regional boundaries, but we want a single boundary for the 
# entire country (dissolving the internal borders).

# st_union merges overlapping or touching geometries into a single geometry.
sf_country <- st_union(x = sf_regions)

# Notice it is now an 'sfc' (Simple Feature Geometry Column) with ONE feature,
# rather than two separate polygons.
class(x = sf_country)


# ==============================================================================
# 6. BRINGING IT ALL TOGETHER WITH GGPLOT2
# ==============================================================================

p3 <- ggplot() +
  # 1. Base layer: The dissolved country boundary (thick black outline)
  geom_sf(data = sf_country, 
          fill = "white", 
          color = "black", 
          linewidth = 1.2) +
  # 2. Regional layer: The synthetic regions (colored, slightly transparent)
  geom_sf(data = sf_regions, 
          mapping = aes(fill = REGION_NAME), 
          alpha = 0.3, 
          color = "grey50") +
  # 3. Centroids layer: (Commented out to keep the map clean)
  # geom_sf(data = sf_centroids, 
  #         color = "red", 
  #         shape = 4, 
  #         size = 5, 
  #         stroke = 2) +
  # 4. Points layer: The stations, mapping aesthetics to variables
  #    Size represents Elevation, Color represents Maximum Precipitation
  geom_sf(data = sf_joined, 
          mapping = aes(size = elevation,
                        colour = mx),
          alpha = 0.8) +
  # 5. RCzechia integration: Overlay the actual official Czech regional borders!
  #    'resolution = "low"' downloads a lightweight version of the geometry.
  #    fill = NA ensures we only see the borders, not the fill.
  geom_sf(data = kraje(resolution = "low"),
          fill = NA) +
  # 6. Thematic Styling
  scale_fill_manual(values = c("Bohemia" = "steelblue4", 
                               "Moravia-Silesia" = "pink3")) +
  labs(title = "Methodological Spatial Analysis",
       subtitle = "Spatial Joins, Centroids, and Data Aggregation",
       fill = "Region",
       size = "Elevation",
       colour = "Max. Precipitation") +
  theme_minimal()

# Render plot
print(p3)
