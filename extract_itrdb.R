library(curl)
library(XML)
library(stringr)
library(raster)

dat.out <- "~/Desktop/ITRDB_test"
if(!dir.exists(dat.out)) dir.create(dat.out, recursive=T)


us <- getData("GADM", country="USA", level=1)
us$NAME_1 <- as.factor(us$NAME_1)
states <- c("Maine", "Vermont", "Massachusetts", "New Hampshire", "Vermont", "Connecticut", "Rhode Island", "New York", "Pennsylvania", "New Jersey")
borders <- us[us$NAME_1 %in% c("New York"),]

xmin = xmin(borders)
xmax = xmax(borders)
ymin = ymin(borders)
ymax = ymax(borders)

itrdb.base <- paste0("https://www.ncdc.noaa.gov/paleo-search/study/search.json?headersOnly=true&dataPublisher=NOAA&dataTypeId=18&minLat=",ymin, "&maxLat=", ymax,
                     "&minLon=", xmin, "&maxLon=", xmax)

noaa.meta <- readLines(itrdb.base)

meta.parse <- str_split(noaa.meta, ",")[[1]]
# meta.parse[1:30]
noaa.id <- grep("NOAAStudyId", meta.parse) # get our indices for our URLS we'll query

itrdb.metadata <- data.frame(ID=NA, author=NA, site=NA, species=NA, lat=NA, lon=NA, elev=NA, ITRDB.id=NA, yr.min=NA, yr.max=NA)

# Working with one site for now; 
# # will need to add loop here in future
pb <- txtProgressBar(min=0, max=length(noaa.id), style=3)
for(i in 1:length(noaa.id)){
  site.id <- str_split(meta.parse[noaa.id[i]], '":\"')[[1]][2]
  site.id <- substr(site.id, 1, nchar(site.id)-1)

  # Store the Site ID
  itrdb.metadata[i,"ID"     ] <- site.id
  
  # Download and save the XML document with all of the metadata for the site
  # We can extract info from this site for our use later
  site.xml <- xmlParse(readLines(paste0("https://www1.ncdc.noaa.gov/pub/data/metadata/published/paleo/dif/xml/noaa-tree-",site.id,".xml")))
  
  # Parsing the XML file into something easier to use
  site.xml2 <- xmlToList(site.xml)
  summary(site.xml2)

  title <- str_split(site.xml2$Entry_Title, "- ")[[1]]
  itrdb.metadata[i,"author" ]  <- title[1]
  itrdb.metadata[i,"site"   ]  <- title[2]
  itrdb.metadata[i,"spieces"]  <- title[3]

  itrdb.id <- substr(title[4], nchar(title[4])-4, nchar(title[4]))
  itrdb.metadata[i,"ITRDB.id"] <- itrdb.id
  saveXML(site.xml, file=file.path(dat.out, paste0("Metadata_", itrdb.id, ".xml")))
  
  # Extract & Store spatial info
  lat1 <- as.numeric(site.xml2$Spatial_Coverage$Southernmost_Latitude)
  lat2 <- as.numeric(site.xml2$Spatial_Coverage$Northernmost_Latitude)
  lon1 <- as.numeric(site.xml2$Spatial_Coverage$Westernmost_Longitude)
  lon2 <- as.numeric(site.xml2$Spatial_Coverage$Easternmost_Longitude)
  elv1 <- as.numeric(site.xml2$Spatial_Coverage$Minimum_Altitude)
  elv2 <- as.numeric(site.xml2$Spatial_Coverage$Maximum_Altitude)
  itrdb.metadata[i,"lat"    ] <- mean(lat1, lat2)
  itrdb.metadata[i,"lon"    ] <- mean(lon1, lon2)
  itrdb.metadata[i,"elev"   ] <- ifelse(length(elv1)>0, mean(elv1, elv2), NA)
  
  # Extract for the start/end dates
  itrdb.metadata[i,"yr.min"] <- site.xml2$Paleo_Temporal_Coverage$Paleo_Start_Date
  itrdb.metadata[i,"yr.max"] <- site.xml2$Paleo_Temporal_Coverage$Paleo_Stop_Date
  
  # Going through the URLs to find a .rwl file
  data.urls <- which(names(site.xml2)=="Related_URL")
  
  # url.rwl <- list()
  for(j in 1:length(data.urls)){
    url.temp <- site.xml2[[data.urls[j]]]
    url.ind <- which(names(url.temp)=="URL")
    for(z in 1:length(url.ind)){
      fname <- str_split(url.temp[url.ind[z]], "/")[[1]]
      fname <- fname[length(fname)]
      
      url.ext <- substr(fname, nchar(fname)-3, nchar(fname))
      if(url.ext %in% c(".rwl", ".crn")){ 
        download.file(url.temp[url.ind[z]]$URL, file.path(dat.out, fname), quiet=T)
      } # End saving file
    } # End looping through URLs
  } # End looping thorugh URL types
  
  setTxtProgressBar(pb, i)
}

itrdb.metadata$ITRDB.id <- as.factor(itrdb.metadata$ITRDB.id)
itrdb.metadata$species <- as.factor(itrdb.metadata$species)
# itrdb.metadata$species <- as.factor(itrdb.metadata$speices)
summary(itrdb.metadata)
