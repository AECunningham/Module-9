library(shiny)
library(leaflet)

shinyUI(fluidPage(title = "Killed and Seriously Injured in Blackburn with Darwen",
  tabsetPanel(               

  # Application title
  # titlePanel("Killed and Seriously Injured in Blackburn with Darwen"),
  tabPanel(title = "Map Tool",
  fluidRow(

  # Sidebar 
  column(3,
     h4("Killed and Seriously Injured in Blackburn with Darwen"),     
     
     radioButtons("radioAge", label = h4("Age"),
        choices = list("All" = 1, "Child (0-15)" = 2,
                       "Adult" = 3),selected = 1),
  
     sliderInput("yearSlider", label = h4("Year(s)"),
        min = 2005, max = 2013, ticks = FALSE, sep = "", value = c(2011, 2013)),

     radioButtons("radioSeverity",label = h4("Severity"),
        choices = list("All Killed or Seriously Injured" = 1,
                       "Fatalities only" = 2,
                       "Seriously Injured only" = 3), selected = 1),
  
     radioButtons("radioRoadUser",label = h4("Road User Type"),
        choices = list("All road users" = 1,
                       "Pedestrians" = 2,
                       "Drivers" = 3,
                       "Passengers" = 4,
                       "Cyclists" = 5,
                       "Motorcyclists" = 6), selected = 1)),
   
  column(7,
    leafletOutput("myMap",width="100%",height=600)),

  column(2,
    radioButtons("radioColouring",label = em(h4("Colour according to:")),
       choices = list("Year" = "Year",
                      "Age (child or adult)" = "AdultChild",
                      "Severity" = "Severity",
                      "Road User Type" = "RoadUser")),
       br(),
       br(),
       br(),
       em(p("Click on a ", span("dot",style = "font-weight: bold")," for details of the casualty, 
            or on the ", span("map background",style = "font-weight: bold")," for total casualties in each of the four localities",style = "color:blue")))
   
  )
  ), # end of first tabPanel
  
  tabPanel(title = "Documentation",
           
           includeMarkdown("documentation.Rmd")
           
  ) # end of second tabPanel
  ) # end of tabsetPanel
))