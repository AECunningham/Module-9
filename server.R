library(shiny)
library(dplyr)
library(plyr)
library(leaflet)
library(rgdal)
library(sp)

localities <- readOGR(dsn=".","Localities_region")
localities.wgs84 <- spTransform(localities,CRS("+init=epsg:4326"))

# Read the bwd files from directory
bwdacc <- read.csv("bwdacc.csv",header=TRUE,row.names=NULL,stringsAsFactors=FALSE)
bwdcas <- read.csv("bwdcas.csv",header=TRUE,row.names=NULL,stringsAsFactors=FALSE)
bwdveh <- read.csv("bwdveh.csv",header=TRUE,row.names=NULL,stringsAsFactors=FALSE)

# Restrict to KSIs (Killed and Seriously Injured)
ksi <- bwdcas[which(bwdcas$Casualty_Severity ==1 | bwdcas$Casualty_Severity ==2),]

# Attach column for year (first four characters of Accident_Index)
ksi$Year <- as.numeric(substr(ksi$Accident_Index,1,4))
ksi$ColourVar  <- as.character(ksi$Year)

# Attach column for simplified road user type
ksi$RoadUser[ksi$Casualty_Class == -1] <- NA
ksi$RoadUser[ksi$Casualty_Class == 2] <- "Passenger"
ksi$RoadUser[ksi$Casualty_Class == 3] <- "Pedestrian"
# if Casualty_Class is not 2 or 3 it must be 1, i.e. "Driver or rider"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type == -1] <- "Other or unspecified driver or rider"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type == 1] <- "Cyclist"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type >= 2 & ksi$Casualty_Type <= 5] <- "Motorcyclist"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type == 23] <- "Motorcyclist"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type == 97] <- "Motorcyclist"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type >= 8 & ksi$Casualty_Type <= 11] <- "Driver"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type >= 17 & ksi$Casualty_Type <= 21] <- "Driver"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type == 90] <- "Driver"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type == 98] <- "Driver"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type == 16] <- "Other or unspecified driver or rider"
ksi$RoadUser[ksi$Casualty_Class == 1 & ksi$Casualty_Type == 22] <- "Other or unspecified driver or rider"

# Attach column for detailed age-group
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == -1] <- "unknown age"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 1] <- "0-5"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 2] <- "6-10"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 3] <- "11-15"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 4] <- "16-20"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 5] <- "21-25"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 6] <- "26-35"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 7] <- "36-45"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 8] <- "46-55"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 9] <- "56-65"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 10] <- "66-75"
ksi$detailedAgeBand[ksi$Age_Band_of_Casualty == 11] <- "75 plus"

# Attach columns for longitude and latitude
AccLongLat <- subset(bwdacc,select=c(Accident_Index,Longitude,Latitude))
Ksi <- merge(ksi,AccLongLat,by="Accident_Index")
rm(ksi)

# Attach column for marker popup
Ksi$markerPopup <- paste0("<strong>",ifelse(Ksi$Casualty_Severity == 1,"Fatality","Serious Injury"), "</strong> occurred in <strong>",
                       as.character(Ksi$Year),"</strong><br>
                       Casualty was a <strong>",Ksi$RoadUser,"</strong>", ifelse(Ksi$Age_Band_of_Casualty >= 1, " aged ", " of "),"<strong>",
                       Ksi$detailedAgeBand,"</strong>")

pal <- colorFactor(
      palette = c(rainbow(9),"black","blueviolet","cornflowerblue","darkblue",rainbow(6)),
      levels = c("2005","2006","2007","2008","2009","2010","2011","2012","2013","Fatality","Seriously Injured","Child","Adult",
               "Pedestrian","Driver","Passenger","Cyclist","Motorcyclist","Other or unspecified driver or rider"))

shinyServer(function(input, output) {

     dataInput <- reactive({

   # Select desired years 
     PlotKsi <- subset(Ksi,Year >= input$yearSlider[1] & Year <= input$yearSlider[2])

   # Select desired age-group
     if (input$radioAge == 2) PlotKsi <- PlotKsi[which(PlotKsi$Age_Band_of_Casualty >= 1 & PlotKsi$Age_Band_of_Casualty <= 3),]
     if (input$radioAge == 3) PlotKsi <- PlotKsi[which(PlotKsi$Age_Band_of_Casualty >= 4),]   

   # Select desired severity
     if (input$radioSeverity == 2) PlotKsi <- PlotKsi[which(PlotKsi$Casualty_Severity == 1),]
     if (input$radioSeverity == 3) PlotKsi <- PlotKsi[which(PlotKsi$Casualty_Severity == 2),]

   # Select desired road user type
     if (input$radioRoadUser == 2) PlotKsi <- PlotKsi[which(PlotKsi$RoadUser == "Pedestrian"),]
     if (input$radioRoadUser == 3) PlotKsi <- PlotKsi[which(PlotKsi$RoadUser == "Driver"),]
     if (input$radioRoadUser == 4) PlotKsi <- PlotKsi[which(PlotKsi$RoadUser == "Passenger"),]
     if (input$radioRoadUser == 5) PlotKsi <- PlotKsi[which(PlotKsi$RoadUser == "Cyclist"),]
     if (input$radioRoadUser == 6) PlotKsi <- PlotKsi[which(PlotKsi$RoadUser == "Motorcyclist"),]

   # Populate column to be used for colouring:

   # If user chose to colour by Year
     if (input$radioColouring == "Severity") PlotKsi$ColourVar <- as.character(PlotKsi$Year)

   # If user chose to colour by Severity
     if (input$radioColouring == "Severity")
     {PlotKsi$ColourVar[PlotKsi$Casualty_Severity == -1] <- NA
     PlotKsi$ColourVar[PlotKsi$Casualty_Severity == 1] <- "Fatality"
     PlotKsi$ColourVar[PlotKsi$Casualty_Severity == 2] <- "Seriously Injured"}

   # If user chose to colour by Ageband
     if (input$radioColouring == "AdultChild")
     {PlotKsi$ColourVar[PlotKsi$Age_Band_of_Casualty == -1] <- NA
     PlotKsi$ColourVar[PlotKsi$Age_Band_of_Casualty >= 1 & PlotKsi$Age_Band_of_Casualty <= 3] <- "Child"
     PlotKsi$ColourVar[PlotKsi$Age_Band_of_Casualty >3] <- "Adult"}

   # If user chose to colour by Road User Type
     if (input$radioColouring == "RoadUser") PlotKsi$ColourVar <- PlotKsi$RoadUser

   # Work out heading for legend
     if(input$radioColouring == "Year") legendTitle <- "Year of accident"
     if(input$radioColouring == "AdultChild") legendTitle <- "Adult or Child"
     if(input$radioColouring == "Severity") legendTitle <- "Severity"
     if(input$radioColouring == "RoadUser") legendTitle <- "Road User Type"

   # Calculate number of casualties in each locality
     if(nrow(PlotKsi) > 0) {                     # Do not run if there are no points to plot 
                                                 # (e.g. child fatalities in 2010), as it throws an error
        xy = na.omit(cbind(PlotKsi$Longitude,PlotKsi$Latitude))
        pts = SpatialPoints(xy)
        proj4string(pts) <- CRS("+init=epsg:4326")
        res <- over(pts,localities.wgs84)
        locpoints <- table(res$NAME)
        }
     else {
        locpoints <- table(localities.wgs84@data$NAME)
        locpoints[[1]] <- 0
        locpoints[[2]] <- 0
        locpoints[[3]] <- 0
        locpoints[[4]] <- 0
        }      
    
     output <- list(PlotKsi,locpoints, legendTitle)
     return(output)

    })
     
       output$myMap = renderLeaflet({

       if(nrow(dataInput()[[1]]) > 0)
                         {leaflet(dataInput()) %>% mapOptions(zoomToLimits = "first") %>%
                                 addTiles(options = tileOptions(opacity=0.6)) %>% 
                                 addPolygons(data = localities.wgs84,weight=2, fillOpacity = 0, opacity = 1, 
                                             popup = paste0("<strong>", localities.wgs84$NAME, "</strong> locality had <strong>",
                                                            dataInput()[[2]][localities.wgs84$NAME],"</strong> casualties meeting the selection criteria")) %>%
                                 addCircleMarkers(data = dataInput()[[1]], color = ~pal(ColourVar),stroke = FALSE, fillOpacity = 1,
                                       #      popup = ~as.character(Year)) %>% 
                                              popup = ~markerPopup) %>%                                
                                 addLegend(pal=pal,values = ~dataInput()[[1]]$ColourVar,na.label = "Unrecorded",title = dataInput()[[3]],opacity=1)

}
        else   # No markers or legend if there are no points to plot (e.g. child fatalities in 2010)

                         {leaflet(dataInput()) %>% mapOptions(zoomToLimits = "first") %>%
                                 addTiles(options = tileOptions(opacity=0.6)) %>% 
                                 addPolygons(data = localities.wgs84,weight=2, fillOpacity = 0, opacity = 1, 
                                             popup = paste0("<strong>", localities.wgs84$NAME, "</strong> locality had <strong>",
                                                            dataInput()[[2]][localities.wgs84$NAME],"</strong> casualties meeting the selection criteria"))}
  })                             
  })
   
  

