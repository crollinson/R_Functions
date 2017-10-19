library(curl)
library(XML)
library(stringr)
library(raster)
library(RCurl)
library(jsonlite)

dat.out <- "~/Desktop/ITRDB_test"
if(!dir.exists(dat.out)) dir.create(dat.out, recursive=T)

# download.all <- c("Chronology", "Raw Measurements", "Correlation Stats")
download.types <- c("Chronology", "Raw Measurements")

us <- getData("GADM", country="USA", level=1)
us$NAME_1 <- as.factor(us$NAME_1)
states <- c("Maine", "Vermont", "Massachusetts", "New Hampshire", "Vermont", "Connecticut", "Rhode Island", "New York", "Pennsylvania", "New Jersey")
# borders <- us[us$NAME_1 %in% c("New York"),]
borders <- us[us$NAME_1 %in% states,]

xmin = xmin(borders)
xmax = xmax(borders)
ymin = ymin(borders)
ymax = ymax(borders)

itrdb.base <- paste0("https://www.ncdc.noaa.gov/paleo-search/study/search.json?headersOnly=true&dataPublisher=NOAA&dataTypeId=18&minLat=",ymin, "&maxLat=", ymax,
                     "&minLon=", xmin, "&maxLon=", xmax)

noaa.meta <- RCurl::getURL(itrdb.base)
noaa.meta <- jsonlite::fromJSON(noaa.meta)$study

for(i in 1:ncol(noaa.meta)){
  noaa.meta[,i] <- as.factor(noaa.meta[,i])
}
summary(noaa.meta)

# Set up where we're going to store everything
# itrdb.metadata <- data.frame(ID.study=NA, investigators=NA, studyCode=NA, species=NA, lat=NA, lon=NA, elev=NA, ITRDB.id=NA, yr.min=NA, yr.max=NA)

# Working with one site for now; 
# # will need to add loop here in future
pb <- txtProgressBar(min=0, max=nrow(noaa.meta), style=3)
for(i in 1:nrow(noaa.meta)){
  id.xml <- noaa.meta[i, "xmlId"]
  
  # Download and save the XML document with all of the metadata for the site
  # We can extract info from this site for our use later
  site.dat <- jsonlite::fromJSON(paste0("https://www.ncdc.noaa.gov/paleo-search/study/search.json?xmlId=", id.xml))$study
  summary(site.dat)
  
  # itrdb.metadata[i,"investigators" ]  <- site.dat$investigators
  noaa.meta[i ,"studyCode"] <- site.dat$studyCode
  noaa.meta[i, "siteName" ] <- site.dat$site[[1]]$siteName
  noaa.meta[i, "Latitude" ] <- as.numeric(site.dat$site[[1]]$geo$geometry$coordinates[[1]][1])
  noaa.meta[i, "Longitude"] <- as.numeric(site.dat$site[[1]]$geo$geometry$coordinates[[1]][2])
  noaa.meta[i, "Elevation"] <- mean(as.numeric(site.dat$site[[1]]$geo$properties[,c("minElevationMeters", "maxElevationMeters")]))
  
  # Extract the species info where available
  if(length(site.dat$site[[1]]$paleoData[[1]]$species[[1]])>0){
    noaa.meta[i,"species.code"   ]  <- site.dat$site[[1]]$paleoData[[1]]$species[[1]]$speciesCode
    noaa.meta[i,"species.name"   ]  <- site.dat$site[[1]]$paleoData[[1]]$species[[1]]$scientificName
  }
  
  

  # Extract for the start/end dates
  noaa.meta[i,"yr.min"] <- as.numeric(site.dat$earliestYearCE)
  noaa.meta[i,"yr.max"] <- as.numeric(site.dat$mostRecentYearCE)
  
  # Going through the URLs to find a .rwl file
  site.dat <- site.dat$site[[1]]$paleoData[[1]]$dataFile[[1]]
  
  data.urls <- site.dat[site.dat$urlDescription %in% download.types,]
  
  # Make a note of how many files were downloaded
  noaa.meta[i,"files.download"] <- nrow(data.urls)
  
  if(nrow(data.urls)==0) next 
  for(j in 1:nrow(data.urls)){
    download.file(data.urls$fileUrl[j], file.path(dat.out, data.urls$linkText[j]), quiet=T)
  } # End looping thorugh URL types
  
  setTxtProgressBar(pb, i)
}
noaa.meta$species.code <- as.factor(noaa.meta$species.code)
noaa.meta$species.name <- as.factor(noaa.meta$species.code)
summary(noaa.meta)

