library(qlcVisualize)
library(qlcData)
library(rworldmap)
library(spatstat)
library(maptools)
library(rgdal)
library(rmapshaper)

# glottolog

data(glottolog)
g <- glottolog
langs <- g[ !is.na(g$longitude) ,]
coords <- langs[, c("longitude","latitude")]
rownames(coords) <- rownames(langs)

# proj4
# scan("http://spatialreference.org/ref/sr-org/gall-peters-orthographic-projection/proj4/", what="character",sep="\n")

pacific <- "+proj=longlat +lon_wrap=155 +lon_0=155 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs"
atlantic <- "+proj=longlat +lon_wrap=8 +lon_0=8 +ellps=WGS84 +datum=WGS84 +no_defs"
gall <- "+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs "
mollweide <- "+proj=moll +lon_wrap=155 +lon_0=155 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
mollweide_atlantic <- "+proj=moll +lon_wrap=9 +lon_0=9 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# ===== simple worldmap

# w <- getMap("less islands")
# w <- w[-which(w$continent ==  "Antarctica"), ]
# w <- unionSpatialPolygons(w,rep(1,length(w)))

# some languages are missing from the polygon (8.7 %)
# test <- inside.owin(coords[,1],coords[,2],w = as.owin(w))
# sum(!test)/length(test)

makePoly <- function(point, size = .1) {

  p <- cbind(c(point[1]+size, point[1]+size, point[1]-size, point[1]-size)
             , c(point[2]-size, point[2]+size, point[2]+size, point[2]-size))
  return(p)
}

# add <- sapply(which(!test),function(x){makePoly(as.numeric(coords[x,]))},simplify=F)
# add <- Polygons(sapply(add,Polygon), "add")
# add <- SpatialPolygons(list(add))
# poly <- SpatialPolygons(c(slot(w,"polygons"),slot(add,"polygons")),proj4string = CRS(wgs))

# poly <- spTransform(poly, CRS(pacific))

# this map is manually edited for better visibility

# IDs <- sapply(slot(poly, "polygons"), function(x) slot(x, "ID"))
# df <- data.frame(rep(0, length(IDs)), row.names=IDs)
# poly <- SpatialPolygonsDataFrame(poly, df)
# writeOGR(poly,"../gis", "world2","ESRI Shapefile")

# ========== Edited map

w <- readOGR("gis2/world.shp")
# w <- spTransform(w,CRS(atlantic))

# shifted coordinates!
# coords$longitude[coords$longitude < -172] <- coords$longitude[coords$longitude < -172] + 360

coords$longitude[coords$longitude < -35] <- coords$longitude[coords$longitude < -35] + 360


# test again: less languages are missing from the polygon (1.9 %)
test <- inside.owin(coords[,1],coords[,2],w = as.owin(w))
sum(!test)/length(test)

# add the missing ones
add <- sapply(which(!test),function(x){makePoly(as.numeric(coords[x,]), size = .4)},simplify=F)
add <- Polygons(sapply(add,Polygon, hole=FALSE), "add")
add <- SpatialPolygons(list(add),proj4string = CRS(wgs))
w <- SpatialPolygons(c(w@polygons,add@polygons),proj4string = CRS(wgs))
w <- unionSpatialPolygons(w,rep(1,length(w)))

# voronoi
v <- voronoi(coords,as.owin(w))

# conversion
poly <- as(v, "SpatialPolygons")
proj4string(poly) <-  CRS(wgs)
poly <- spTransform(poly, CRS(mollweide))

# cartogram
poly <- SpatialPolygonsDataFrame(poly, data.frame(weight = rep(1, length(poly))))

# this works very slowly and the result has overlapping polygons. not nice
# library(cartogram)
# cart <- cartogram(poly,"weight", threshold = .05, itermax = 60)

# alternative. Parameter "res" influences the strength of the deformation
library(getcartr)
cart <- quick.carto(poly, poly$weight, res = 1000, gapdens = 0.5)

# back conversion
regions <- slot(cart, "polygons")
regions <- lapply(regions, function(x) { SpatialPolygons(list(x)) })
windows <- lapply(regions, as.owin)
map <- tess(tiles = windows)

save(cart,v,langs,map,file="~/Desktop/cart5.Rdata")

# cols <- rep("grey",nrow(langs))
# cols[langs$stock == "Nuclear Trans New Guinea"] <- "blue"
# cols[langs$stock == "Turkic"] <- "red"
# cols[langs$stock == "Arawakan"] <- "green"
# cols[langs$stock == "Atlantic-Congo"] <- "orange"
# cols[langs$stock == "Afro-Asiatic"] <- "black"
# cols[langs$stock == "Mayan"] <- "purple"
# vmap(map,col=cols, border = NA, outer.border = NA)


