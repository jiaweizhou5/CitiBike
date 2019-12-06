#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

#

library(shiny)
library(leaflet)

citibike <- read.csv(file = "201607-citibike-tripdata.csv", header = TRUE, sep = ",")
citibike$route <- with(citibike,paste(as.character(citibike$start.station.name),as.character(citibike$end.station.name)))
toproutes <- tail(names(sort(table(citibike$route))),10)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Citibike Project",
        tabPanel("Station Statistics",  
        
        titlePanel("Station Statistics"),
        p("Overview of the CitiBike system"),
        sidebarLayout(
            sidebarPanel(
                h2("Top 10 Origins:"),
                tableOutput("topStartCount"),
                h2("Top 10 Destinations:"),
                tableOutput("topEndCount")
            ),
            mainPanel(
                h2(""),
                leafletOutput("topTenStartMap"),
                p("Top 10 Origins"),
                h2(""),
                leafletOutput("topTenEndMap"),
                p("Top 10 Destinations")
            )
        )
    ),
    tabPanel("Popularity by Station",  
        
        titlePanel("Station Popularity"),
        p("Explore the popularity of selected stations"),
        sidebarLayout(
            sidebarPanel(
                
                selectInput("stationSelect", "Choose a station", levels(citibike$start.station.name))
            ),
            mainPanel(
                leafletOutput("popularityMap"),
                textOutput("confirm"),
                h3("Time of Day"),
                plotOutput("day"),
                h3("Day of Week"),
                plotOutput("week")
            )
        )
   ),
   tabPanel("Top Routes",
            
            titlePanel("Top Routes"),
            p("The most popular routes in the city"),
            sidebarLayout(
                sidebarPanel(
                    selectInput("routeSelect", "Choose a route", toproutes)
                ),
                mainPanel(
                    leafletOutput("routeMap"),
                    textOutput("selectedRoute")
                )
            )
   ),
   tabPanel("Station Asymmetry",
        
        titlePanel("Station Asymmetry"),
        p("Understand where the bikes are piling up or customers are going unserved"),
        sidebarLayout(
            sidebarPanel(
                selectInput("timeSelect", "Choose a time of day", c("Morning", "Afternoon", "Night"))
            ),
            mainPanel(
                h2("Most Asymmetric Stations "),
                leafletOutput("asymmetryMap"),
                div(style="display: inline-block;vertical-align:top;",h4("Surplus: "),img(src='pinblue.png', width="24", height="30")),
                div(style="display: inline-block;vertical-align:top;",h4("Deficit: "),img(src='pinred.png', width="24", height="30")),
                h3("By time of Day"),
                h4("Surplus Stations"),
                tableOutput("surplusTable"),
                h4("Deficit Stations"),
                tableOutput("deficitTable")
            )
        )
   ),
   tabPanel("Weather Effects",
        
        titlePanel("Weather Effects"),
        p("Looking at the impact of weather on bike usage"),
        sidebarLayout(
            sidebarPanel(
                p("Examination of inches of precipitation compared to bike usage reinforces the presumption that fewer bikes are checked out on rainy days.")
            ),
            mainPanel(
                plotOutput("weatherPlot")
            )
        )
    ),
   
   tabPanel("Bike Usage",
       
        titlePanel("Most Frequently Used Bikes"),
            
        sidebarLayout(
            sidebarPanel(
                h2("Top 5 Most Used Bikes:"),
                radioButtons("usageFilter", "Select Filter:",
                             c("Frequency" = "frequency",
                               "Duration" = "duration")),
                tableOutput("bikeUsage")
            ),
            mainPanel(
                h3(textOutput("topBikesMin")),
                h3(textOutput("allBikesAvg")),
                h3(textOutput("bikesRatio")),
                h3("Bottom Line:  The top 5 bikes get used more than twice as much as the rest of the bikes.")
            )
        )
    )

))
