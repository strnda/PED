install.packages(c("sf", "terra"))

library(sf); library(terra)

## https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/czech-republic-shapefile/at_download/file

dta <- read_sf("../Downloads/Czech_Republic_shapefile/cz_10km.shp")
dta

plot(x = dta["EOFORIGIN"])

loc <- locator(n = 5)
loc

coor <- do.call(what = cbind,
                args = loc)

points(x = coor,
       col = "red")

##
st_point(x = coor)
st_multipoint(x = coor)
##

df <- data.frame(ID = 1:nrow(x = coor))
df$geometry <- st_sfc(apply(X = coor, 
                            MARGIN = 1, 
                            FUN = st_point,
                            simplify = FALSE),
                      crs = crs(x = dta))

df
dta

df_shp <- st_sf(df)
df_shp

buff <- st_buffer(x = df_shp,
                  dist = 50000)

library(ggplot2)

ggplot() +
  geom_sf(data = dta,
          fill = NA) +
  geom_sf(data = df_shp) +
  geom_sf(data = df_buff, 
          alpha = .8) +
  coord_sf(crs = 5514)

library(leaflet)

leaflet() %>%
  addTiles() %>%
  addPolygons(data = st_transform(dta,
                                  crs = 4326))

st_write(obj = df_buff,
         dsn = "../Downloads/buffer.shp")
test <- st_read(dsn = "../Downloads/buffer.shp")
test

## terra

f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)

plot(x = r)

loc <- locator(5)
coor <- do.call(what = cbind,
                args = loc)
pol <- data.frame(ID = "a")
pol$geometry <- st_sfc(st_polygon(x = list(rbind(coor, coor[1,]))),
                       crs = crs(x = r))
pol <- st_sf(pol)

plot(x = r)
lines(x = pol)
points(x = coor)


cr <- crop(x = r,
           y = pol)


plot(x = cr)

as.matrix(x = cr)

values(x = cr)

mean(x = values(x = cr), 
     na.rm = TRUE)
