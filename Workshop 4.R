#5.4 Installing the spatial R packages
#In this section we are going to learn how R can be used to wrangle and analyse spatial data by working through a case study involving copepod data

# install and load your packages
install.packages("sf") 
install.packages("terra")
install.packages("tmap")


#load into R library
library(tidyverse)
library(sf) # simple features
library (terra) # for raster
library(tmap) # Thematic maps are geographical maps in which spatial data distributions are visualized

#Let’s get started with that copepod richness data. In this part of the course we are going to clean it up and run some basic analyses.
#We will load in the data using a package from the tidyverse called readr. readr is handy because it does extra checks on data consistency over and above what the base R functions do. 

library(readr)
dat <- read_csv("data-for-course/copepods_raw.csv")
dat

#5.7.1 Check Coordinates
#The first step to making our first map using ggplot2  is to plot the coordinates for the samples (segments of the CPR silks)

library(ggplot2)
ggplot(dat) + 
  aes(x = longitude, y = latitude, color = richness_raw) +
  geom_point()

#This looks good. But this is not a map. It doesn’t have those critical things a real map needs, such as a projection (to bend or warp your data over a spherical globe, the earth) so the real distances between these points when measured with a ruler are probably wrong. It’s simply a scatter plot, but is a nice and easy way to look at your spatial data. 
#So, now let’s look at the richness data (our main variable for analysis). This time we are going to visualize richness in a non-spatial way with latitude on the x-axis and richness on the y-axis. 

ggplot(dat, aes(x = latitude, y = richness_raw)) + 
  stat_smooth() + 
  geom_point()

#5.8 Getting Going with Maps
#We will now repeat the above map of richness, but this time using some of R’s specialist packages for GIS and mapping. Now we introduce those important components of a GIS, the ability to reference data to real locations on the planet, and bend it around a mostly spherical ball that is the earth. 
#Lucky for us, R has some special packages developed specifically to do this.
#First, we will turn our point data into a spatially referenced data frame using the sf package (sf stands for ‘simple features’) which is an open standard for geospatial databases. For those that think in GIS, you can think of this format as a shapefile or feature collection.

sdat <- st_as_sf(dat, coords = c("longitude", "latitude"), 
                 crs = 4326)

#st_as_sf converts different data types to simple features. 
#dat is our original data. 
#coords gives the names of the columns that relate to the spatial coordinates (in order of X coordinate followed by Y coordinate).
#crs stands for coordinate reference system which we will discuss next.

#5.9 Coordinate Reference Systems
#In mapping, we refer to the reference point as datum and the lumpy spherical earth model as an ellipsoid. Together, these make a geographic coordinate reference system (GCS), which tells us where the coordinates of our copepod data are located on the earth.
#GCS’s are represented by angular units (i.e. longitude and latitude), usually in decimal degrees. Our copepod coordinates are long-lat, so we chose a common ‘one-size-fits-all’ GCS called WGS84 to define the crs using the EPSG code 4326. What is an EPSG code? It’s a unique, short-hand code for a specific coordinate reference system (CRS).
#In R, best practice is to either use an EPSG code or Well-known text (WKT) to define a CRS. A WKT string contains all of the detailed information we need to define a crs, but is cumbersome if you don’t need all of the detail. Read this for a more complete overview.
#It’s easy to find out all of the above for a chosen crs in R. For example, for the EPSG code 4326 we can find out: 1) what the name of this crs is, 2) the corresponding proj4string, and 3) the WKT

crs4326 <- st_crs(4326)
crs4326 # look at the whole CRS
crs4326$Name # pull out just the name of the crs
[1] "WGS 84"

#Now check out what the WKT looks like

crs4326$wkt # crs in well-known text format

#5.10 Feature Collection (Points)
#Let’s now look at what we created with sdat.

sdat

#A simple feature is like a shapefile, in that it holds a lot of data in columns and rows but is spatially aware. Essentially, that includes extra columns regarding each row's position (in coordinates) and metadata about the coordinate reference system, the type of geometry (Point) and so on.

#5.11 Cartography
#.sf has some simple plotting features:

plot(sdat["richness_raw"])
plot(sdat)

#5.12 Thematic Maps for Communication
#In this module we will use tmap. tmap works similarly to ggplot2 in that we build and add on layers. Here we only have one layer from sdat. We declare the layer with tm_shape() (in this case sdat), then the plot type with the following command.

tm_shape(sdat) + 
  tm_dots(col = "richness_raw")

#tm_dots to plot dots of the coordinates. Other options are tm_polygons, tm_symbols and many others we’ll see later.
#We’ve chosen "richness_raw" as the color scale
#Use tmap_save to save the map to your working directory. Remember to change the output path if you need to save it to a different folder.

tmap_save(tm1, filename = "Richness-map.png", 
          width = 600, height = 600)

#Mapping Polygons as Spatial Layers
#We can read shapefiles directly into R with the st_read command (which is like read_csv, but for spatial files):

aus <- st_read("data-for-course/spatial-data/Aussie/Aussie.shp")
shelf <- st_read("data-for-course/spatial-data/aus_shelf/aus_shelf.shp")

#As always check out the data by typing the object names and reviewing the output in the console. Note here that the CRS is provided in the shapefile, it’s already spatially aware.

aus
shelf

#5.13.2 mapping Polygons
#Again, tmap makes it very straightforward to make a map of polygons

tm_shape(shelf) + 
  tm_polygons()

#Remember we can make a thematic map by layering it up just as we do for plots in ggplot2. Here we have indicated the shape of our map (shelf) and we have added a command bbox = sdat to expand the extent of the map so it depicts all of our copepod data points. We then add the shape of Australia (aus) on top of the shelf, and finally our copepod data (sdat) in the form of points using tm_dots().

tm_shape(shelf, bbox = sdat) + 
  tm_polygons() +
  tm_shape(aus) + 
  tm_polygons() + 
  tm_shape(sdat) + 
  tm_dots()

#5.14 Exploring t_map
install.packages("tmap")
library(tmap)
vignette('tmap-getstarted')

data("World")

tm_shape(World) +
  tm_polygons("HPI")

#Each map can be plotted as a static image or viewed interactively using "plot" and "view" modes, respectively. The mode can be set with the function tmap_mode, and toggling between the modes can be done with the ‘switch’ ttm() (which stands for toggle thematic map.

tmap_mode("view")

tm_shape(World) +
  tm_polygons("HPI")

#A shape is a spatial object (with a class from sf, sp, stars, or raster). Multiple shapes and also multiple layers per shape can be plotted:

data(World, metro, rivers, land)

tmap_mode("plot")
tm_shape(land) +
  tm_raster("elevation", palette = terrain.colors(10)) +
  tm_shape(World) +
  tm_borders("white", lwd = .5) +
  tm_text("iso_a3", size = "AREA") +
  tm_shape(metro) +
  tm_symbols(col = "red", size = "pop2020", scale = .5) +
  tm_legend(show = FALSE)

#Facets can be created in three ways:
#By assigning multiple variable names to one aesthetic (in this example the first argument of tm_polygons:
    
tmap_mode("view")
tm_shape(World) +
  tm_polygons(c("HPI", "economy")) +
  tm_facets(sync = TRUE, ncol = 2)

#By splitting the spatial data with the by argument of tm_facets:

tmap_mode("plot")
data(NLD_muni)
NLD_muni$perc_men <- NLD_muni$pop_men / NLD_muni$population * 100
tm_shape(NLD_muni) +
  tm_polygons("perc_men", palette = "RdYlBu") +
  tm_facets(by = "province")

#By using the tmap_arrange function:

tmap_mode("plot")
data(NLD_muni)
tm1 <- tm_shape(NLD_muni) + tm_polygons("population", convert2density = TRUE)
tm2 <- tm_shape(NLD_muni) + tm_bubbles(size = "population")
tmap_arrange(tm1, tm2)

#Tiled basemaps can be added with the layer function tm_basemap. Semi-transparent overlay maps (for example annotation labels) can be added with tm_tiles.

tmap_mode("view")
tm_basemap("Stamen.Watercolor") +
  tm_shape(metro) + tm_bubbles(size = "pop2020", col = "red") +
  tm_tiles("Stamen.TonerLabels")

#The functions tm_layout and tm_view are used to specify the map layout and the interactive aspects respectively. These functions can be used in the same way as the layer functions, e.g.

tmap_mode("plot")
tm_shape(World) +
  tm_polygons("HPI") +
  tm_layout(bg.color = "skyblue", inner.margins = c(0, .02, .02, .02))

#These options, as well as a couple of others, can also be set within with tmap_options, which works in the same way as the base R function options. The main advantage is that these options are set globally, so they do not have to be specified in each map, for the duration of the session.

tmap_options(bg.color = "black", legend.text.color = "white")
tm_shape(World) +
  tm_polygons("HPI", legend.title = "Happy Planet Index")

tmap_style("albatross")
tm_shape(World) +
  tm_polygons("HPI", legend.title = "Happy Planet Index")

#Exporting Maps

tm <- tm_shape(World) +
  tm_polygons("HPI", legend.title = "Happy Planet Index")

# save an image ("plot" mode)

tmap_save(tm, filename = "world_map.png")

# save as stand-alone HTML file ("view" mode)

tmap_save(tm, filename = "world_map.html")

