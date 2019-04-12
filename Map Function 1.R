library(formattable)
library(tools)
library(leaflet)
library(rgdal)
library(viridis)
library(sp)


# assume data is loaded
address = "W:/Shared/Central Analysis Group/2. Team Folders/TJ/county_table_output.csv"
dat <- read.csv(address, stringsAsFactors = FALSE)


# Data files --------------------------------------------------------------


# your 3 data files
counties_dat <- readOGR("W:/Shared/Central Analysis Group/2. Team Folders/TJ/cb_2017_us_county_20m/cb_2017_us_county_20m.shp",
                        layer = "cb_2017_us_county_20m", 
                        GDAL1_integer64_policy = TRUE)

states_dat <- readOGR("W:/Shared/Central Analysis Group/2. Team Folders/TJ/cb_2017_us_state_20m/cb_2017_us_state_20m.shp",
                      layer = "cb_2017_us_state_20m", 
                      GDAL1_integer64_policy = TRUE)

fips <- read.csv("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt",
                 header = FALSE, sep = ",",
                 col.names = c("stateabb", "statefp", "countyfp", "COUNTY_NAME", "classfp"))


#  Contributing Functions --------------------------------------------------------------


# Any other fucntions that need defining 

# Give back state abbreviation from name
state2abbr <- function(state = "Texas") {
  
  if(sum(state %in% state.name) > 0) {
    sapply(state, function(i) {
      
      if(!is.na(i) & length(state.abb[state.name == i]) > 0) {
        state.abb[state.name == i]
        
      } else NA
    }) 
    
  } else return(rep(NA, length(state)))
}

# Put County / State names into proper format (capital 1st letter)
Capital1st <- function(x) {
  tmp = sapply(x, function(j) strsplit(j, " "))
  lapply(tmp, function(i) paste(toupper(substring(i, 1, 1)), substring(i, 2), sep = "", collapse = " "))
}

# Return string of specified length (adding zeros at start)
makeXchars <- function(i, x = 3) {
  
  makeXchars0 <- function(i, x = 3) {
    if(nchar(i) == x) return(i)
    if(nchar(i) > x) stop("WARNING: input greater than desired length !")
    if(nchar(i) < x) {
      tmp = paste0(rep(0, x - nchar(i)), collapse = "")
      return(paste0(tmp, i, collapse = ""))
    }
  }
  
  sapply(i, function(z) makeXchars0(z, x))
}


# Leaflet Map Function ----------------------------------------------------------------

# New function
county_map_produce <- function(data) {
  
  
  tryCatch({
    
    dat          <- dat[dat$STATE_NAME %in% c(state.name), ]
    dat$stateabb <- state2abbr(dat$STATE_NAME)
    
    fips$COUNTY_NAME <- tolower(fips$COUNTY_NAME)
    dat$COUNTY_NAME  <- tolower(dat$COUNTY_NAME)
    #dat$cat_ratio[is.na(dat$cat_ratio)] <- 0
    dat$countyfp <- NA
    dat$statefp  <- NA
    
    for(i in 1:nrow(dat)) {
      
      dat$countyfp[i] <- fips$countyfp[fips$COUNTY_NAME == dat$COUNTY_NAME[i] & 
                                         fips$stateabb == dat$stateabb[i]][1]
      
      dat$statefp[i]  <- fips$statefp[fips$stateabb == dat$stateabb[i]][1]
      
    }
    
    dat <- dat[!is.na(dat$countyfp), ]
    
    dat$countyfp <- makeXchars(dat$countyfp, 3)
    dat$statefp  <- makeXchars(dat$statefp, 2)
    
    dat$COUNTY_NAME <- Capital1st(dat$COUNTY_NAME)
    
    dat$popup_dat <-
      paste0('<b>', "County: ", "</b>",     dat$COUNTY_NAME,  '<br/>',
             '<b>', "Cat Ratio: ", "</b>",    dat$cat_ratio, '<br/>',
             '<b>', "Loss Ratio: ", "</b>",      percent(dat$loss_ratio, 1), '<br/>',
             '<b>', "-----------------------------------", "</b>",'<br/>',
             '<b>', "Cat Prem: ",  "</b>",       currency(symbol = "$", digits = 0, x = dat$cat_prem), '<br/>',
             '<b>', "Non Cat Prem: ",  "</b>",       currency(symbol = "$", digits = 0, x = dat$non_cat_prem), '<br/>',
             '<b>', "Total Premiumt: ",  "</b>", currency(symbol = "$", digits = 0, x = dat$total_prem), '<br/>') %>% 
      lapply(htmltools::HTML)
    
    dat <- dat[!is.na(dat$loss_ratio), ]
    
    map_dat <- merge(counties_dat, dat[ , c("statefp", 
                                            "countyfp", 
                                            "COUNTY_NAME", 
                                            "STATE_NAME", 
                                            "popup_dat", 
                                            "loss_ratio", 
                                            "cat_ratio", 
                                            "cat_prem",
                                            "non_cat_prem", 
                                            "total_prem", 
                                            "locations")], 
                     by.x = c("STATEFP", "COUNTYFP"), 
                     by.y = c("statefp", "countyfp"))
    
    map_dat$loss_ratio <- ifelse(map_dat$loss_ratio == Inf, NA, map_dat$loss_ratio)
    
    lr_pal <- colorQuantile("BuPu", map_dat$loss_ratio, reverse = TRUE, n = 8)
    cr_pal <- colorQuantile("BuPu", map_dat$cat_ratio, n = 8)
    cp_pal <- colorQuantile("BuPu", map_dat$cat_prem, n = 8)
    tp_pal <- colorQuantile("BuPu", map_dat$total_prem, n = 8)
    loc_pal <- colorQuantile("BuPu", map_dat$locations, n = 8)
    ncp_pal <- colorQuantile("BuPu", map_dat$non_cat_prem, n = 8)
    

  county_map <- leaflet(map_dat) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("OpenStreetMap") %>% 
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = lr_pal(map_dat$loss_ratio),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "Loss Ratio") %>%
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = cr_pal(map_dat$cat_ratio),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "Cat Ratio") %>% 
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = cp_pal(map_dat$cat_prem),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "Cat Premium") %>% 
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = tp_pal(map_dat$total_prem),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "Non Cat Premuim") %>% 
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = loc_pal(map_dat$locations),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "Total Premuim") %>% 
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = ncp_pal(map_dat$non_cat_prem),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "No. Locations") %>% 
addLayersControl(baseGroups = c("Loss Ratio", "Cat Ratio", 
                                "Cat Premium", "Non Cat Premium", "Total Premuim", 
                                "No. Locations"),
                 options = layersControlOptions(collapsed = FALSE))

    
    
    
    
    
    return(county_map)
  
        
    
    
    
  }, 
           
           error = function(err){
             warning(err)
             return(NULL)
             })
  
}



# Test --------------------------------------------------------------------

county_map_produce(dat)


# Tabithas experiment -----------------------------------------------------


# map_dat <- st_as_sf(map_dat)
map_dat <- map_dat %>% subset(STATE_NAME == state)

county_map <- leaflet(map_dat) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("OpenStreetMap") %>% 
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = pal(map_dat$loss_ratio),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "Loss Ratio") %>%
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = pal(map_dat$loss_ratio),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "Cat Ratio") %>% 
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = pal(map_dat$loss_ratio),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "Cat Premium") %>% 
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = pal(map_dat$loss_ratio),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "Non Cat Premuim") %>% 
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = pal(map_dat$loss_ratio),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "Total Premuim") %>% 
  addPolygons(color = "White",
              popup = map_dat$popup_dat,
              label = paste(map_dat$STATE_NAME, "," , map_dat$COUNTY_NAME),
              weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.6,
              fillColor = pal(map_dat$loss_ratio),
              highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE),
              group = "No. Locations") %>% 
  addLayersControl(baseGroups = c("Loss Ratio", "Cat Ratio", 
                                  "Cat Premium", "Non Cat Premium", "Total Premuim", 
                                  "No. Locations"),
                   options = layersControlOptions(collapsed = FALSE))






return(county_map)
