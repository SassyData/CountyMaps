

state2abbr <- function(state = "Texas") {
  
  if(sum(state %in% state.name) > 0) {
     
    sapply(state, function(i) {
      
                          if(!is.na(i) & length(state.abb[state.name == i]) > 0) {
                            
                            state.abb[state.name == i]
                            
                          } else NA
      
      }) 
    
  } else return(rep(NA, length(state)))
  
  
}

simple_cap <- function(x) {
  
  #x = dat$COUNTY_NAME
  
  tmp = sapply(x, function(j) strsplit(j, " "))
  
  lapply(tmp, function(i) paste(toupper(substring(i, 1, 1)), substring(i, 2), sep = "", collapse = " "))
  
  #paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  
}



length(state.abb[state.name == NA])

state2abbr(state = c("Texas", NA))

state = c("Texas", "North Carolina", "South Carolina", "Lousisiana", NA)

tmp <- sapply(state, function(i) {
  
  if(!is.na(i)) {
    
    state.abb[state.name == i]
    
  } else NA_character_
  
})




library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(dplyr)
library(leaflet)
library(scales)
### Begin data prep
# Grab air/water quality data from the EPA
url = "https://data.cdc.gov/api/views/cjae-szjv/rows.csv?accessType=DOWNLOAD"
dat <- read.csv(url, stringsAsFactors = FALSE)
# Colnames tolower
names(dat) <- tolower(names(dat))
dat$countyname <- tolower(dat$countyname)
# Wide data set, subset only what we need.
county_dat <- subset(dat, measureid == "296", 
                     select = c("reportyear","countyfips","statename", "countyname", "value", "unitname")) %>%
  subset(reportyear==2011, select = c("countyfips", "value"))
# Rename columns to make for a clean df merge later.
colnames(county_dat) <- c("GEOID", "airqlty")
# Have to add leading zeos to any FIPS code that's less than 5 digits long to get a good match.
# I'm cheating by using C code. sprintf will work as well.
county_dat$GEOID <- formatC(county_dat$GEOID, width = 5, format = "d", flag = "0")
### End data prep

# Download county shape file from Tiger.
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
#us.map <- readOGR(dsn = ".", layer = "cb_2013_us_county_20m", stringsAsFactors = FALSE)

us.map <- readOGR("W:/Shared/Central Analysis Group/2. Team Folders/TJ/cb_2017_us_county_20m/cb_2017_us_county_20m.shp",
                    layer = "cb_2017_us_county_20m", stringsAsFactors = FALSE)

# states <- readOGR("W:/Shared/Central Analysis Group/2. Team Folders/TJ/cb_2017_us_state_20m/cb_2017_us_state_20m.shp",
#                   layer = "cb_2017_us_state_20m", 
#                   GDAL1_integer64_policy = TRUE)

# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]
# Merge spatial df with downloade ddata.
leafmap <- merge(us.map, county_dat, by=c("GEOID"))

# Format popup data for leaflet map.
popup_dat <- paste0("<strong>County: </strong>", 
                    leafmap$NAME, 
                    "<br><strong>Value: </strong>", 
                    leafmap$airqlty)

pal <- colorQuantile("YlOrRd", NULL, n = 20)
# Render final map in leaflet.
leaflet(data = leafmap) %>% 
  addTiles() %>%
  addPolygons(fillColor = ~pal(airqlty), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_dat)


