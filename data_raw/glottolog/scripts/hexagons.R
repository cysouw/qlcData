# check this on hexagon plotting!
library(rmapshaper)
# https://rpubs.com/RobWHickman/testhex


hex_number <- length(cart)
size = 5.25
guessed_hex_centre <- spsample(cart, type = "hexagonal", cellsize = size)
guessed_hex <- HexPoints2SpatialPolygons(guessed_hex_centre, dx = size)

while(hex_number != length(guessed_hex)){
  ifelse(hex_number > length(guessed_hex), size <- size * 0.99999, size <- size * 1.00001)
  guessed_hex_centre <- spsample(cart, type = "hexagonal", cellsize = size)
  guessed_hex <- HexPoints2SpatialPolygons(guessed_hex_centre, dx = size)
  print(length(guessed_hex));print(size)
}

whole_shape <- ms_dissolve(guessed_hex)
plot(whole_shape)


proj4string(guessed_hex) <- CRS(mollweide)
hex <- spTransform(guessed_hex, CRS(mollweide))
ids <- data.frame("ID" = 1:length(hex@polygons))
hex <- SpatialPolygonsDataFrame(hex, ids, match.ID = FALSE)

cart@data$name <- langs$glottocode

library(rgeos)
data <- over(hex, cart, returnList = T)




