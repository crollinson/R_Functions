dat.out <- "~/Desktop/ITRDB_test"

us <- getData("GADM", country="USA", level=1)
us$NAME_1 <- as.factor(us$NAME_1)
states <- c("Pennsylvania")
# borders <- us[us$NAME_1 %in% c("New York"),]
borders <- us[us$NAME_1 %in% states,]

xmin = xmin(borders)
xmax = xmax(borders)
ymin = ymin(borders)
ymax = ymax(borders)

test <- data.frame(x=c(xmin, xmax, xmax, xmin, xmin),
                   y=c(ymin, ymin, ymax, ymax, ymin))

test <- Polygon(test)
test <- SpatialPolygons(list(Polygons(list(test), ID="a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

itrdb.out <- extract.itrdb(area.extract=borders, download.types=c("Chronology", "Raw Measurements"), dir.out="~/Desktop/ITRDB_europe")
