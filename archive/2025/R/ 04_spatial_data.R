# Install and load required packages
# install.packages(c("sf", "terra"))
library(sf)  # For working with vector data
library(terra)  # For working with raster data

# Step 1: Load Czech Republic Shapefile
# Download link for the shapefile:
# https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/czech-republic-shapefile/at_download/file
dta <- read_sf("./data/shp/cz_10km.shp")
dta  # Inspect the shapefile

# Plot the shapefile and color regions by the "EOFORIGIN" attribute
plot(x = dta["EOFORIGIN"])

# **EXERCISE 1:** Try plotting a different column in the shapefile.

# Step 2: Interactive Point Selection
# Use the locator to select 5 points interactively
loc <- locator(n = 5)  # Select 5 points on the plot
loc  # Inspect the selected points

# Convert the points into a coordinate matrix
coor <- do.call(what = cbind, args = loc)

# Add the selected points to the plot
points(x = coor, col = "red")  # Plot points in red

# Step 3: Create Spatial Points and Buffers
# Convert points to spatial formats
st_point(x = coor)  # Create a single point
st_multipoint(x = coor)  # Create a multipoint geometry

# Create a DataFrame for the points
df <- data.frame(ID = 1:nrow(x = coor))
df$geometry <- st_sfc(apply(X = coor, 
                            MARGIN = 1, 
                            FUN = st_point,
                            simplify = FALSE), 
                      crs = crs(x = dta))  # Assign the coordinate reference system (CRS)

df  # Inspect the data frame
dta  # Inspect the original shapefile

# Convert the DataFrame to an sf object
df_shp <- st_sf(df)
df_shp

# Create buffers around each point (50,000 meters radius)
buff <- st_buffer(x = df_shp, dist = 50000)

# Step 4: Visualize Spatial Data with ggplot2
library(ggplot2)

ggplot() +
  geom_sf(data = dta, fill = NA) +  # Plot the shapefile
  geom_sf(data = df_shp) +  # Plot the points
  geom_sf(data = buff, alpha = .8) +  # Plot the buffers with transparency
  coord_sf(crs = 5514)  # Set CRS for the plot

# **EXERCISE 2:** Modify the buffer distance to 100,000 meters and observe the changes.

# Step 5: Interactive Visualization with leaflet
library(leaflet)

leaflet() %>%
  addTiles() %>%  # Add a base map
  addPolygons(data = st_transform(dta, crs = 4326))  # Transform CRS and plot the shapefile

# Save buffers as a shapefile
st_write(obj = buff, dsn = "./data/shp/buffer.shp")

# Read the saved shapefile
test <- st_read(dsn = "./data/shp/buffer.shp")
test  # Inspect the loaded shapefile

# **EXERCISE 3:** Add the buffer polygons to a leaflet map and color them by their IDs.

######## WORKING WITH RASTER DATA USING TERRA ########

# Step 6: Load a raster file (example file from the terra package)
f <- system.file("ex/elev.tif", package = "terra")
r <- rast(f)  # Load the raster
plot(x = r)  # Plot the raster

# Step 7: Select Points and Create a Polygon
loc <- locator(5)  # Select 5 points interactively
coor <- do.call(what = cbind, args = loc)  # Convert to a coordinate matrix

# Create a polygon from the selected points
pol <- data.frame(ID = "a")
pol$geometry <- st_sfc(st_polygon(x = list(rbind(coor, coor[1,]))), 
                       crs = crs(x = r))
pol <- st_sf(pol)

# Plot the raster with the polygon and points
plot(x = r)  # Plot the raster
lines(x = pol)  # Overlay the polygon
points(x = coor)  # Overlay the points

# Step 8: Crop the Raster Using the Polygon
cr <- crop(x = r, y = pol)  # Crop the raster to the polygon
plot(x = cr)  # Plot the cropped raster

# Step 9: Extract Raster Values
# Convert the raster to a matrix
as.matrix(x = cr)

# Extract raster values
values(x = cr)

# Calculate the mean value of the cropped raster (excluding NA values)
mean(x = values(x = cr), na.rm = TRUE)

# **EXERCISE 4:** Experiment with cropping a different part of the raster. How do the extracted values change?

