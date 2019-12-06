#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(ggplot2)
library(tidyr)
library(dplyr)


blue = makeIcon("pinblue.png", iconWidth = 24, iconHeight =32)
red = makeIcon("pinred.png", iconWidth = 24, iconHeight =32)




#functions
outersect <- function(x, y) {
    sort(c(setdiff(x, y),
           setdiff(y, x)))
}

#input: df 2 objects - station + frequency, binds list - x as a list of stations - not df
getCoords <- function(x) {
    listToMap <- data.frame(x)
    listToMap$lat <- citibike$start.station.latitude[x]
    listToMap$long <- citibike$start.station.longitude[x]
    #remove freq for unique mapping
    # freqTemp <- listToMap
    ##  listToMap$Freq <- NULL
    #listToMap <- unique(listToMap)
    #listToMap$Freq <- freqTemp
    return(listToMap)
}

bikediffs <- function(x, y) {
    print("***********************************************")
    print(y)
    #table turns the station names into df w/statin name as Var1 and Freq as # of rides for that station
    uniqueStartsDF <- as.data.frame(table(x$start.station.name))
    uniqueEndsDF <- as.data.frame(table(x$end.station.name))
    #Organize by most irdes to least rides for each start/stop
    uniqueStartsSorted <- arrange(uniqueStartsDF, desc(Freq))
    uniqueEndsSorted <- arrange(uniqueEndsDF, desc(Freq))
    #find stations that only show up for start/end of trips - very asymmetric likely
    oneOnlyStations <- outersect(uniqueStartsSorted$Var1, uniqueEndsSorted$Var1)
    length(oneOnlyStations)
    singleStationsList <- getCoords(oneOnlyStations)
    
    endsShortAll <- uniqueEndsSorted[1:40, ]
    startsShortAll <- uniqueStartsSorted[1:40, ]
    #table(startsShortAll$Var1 %in% endsShortAll$Var1)
    #table(endsShortAll$Var1 %in% startsShortAll$Var1)
    
    bikeRet <- getCoords(startsShortAll$Var1)
    bikeRet$startList <- getCoords(startsShortAll$Var1)
    bikeRet$endList <- getCoords(endsShortAll$Var1)
    bikeRet$x <- NULL
    bikeRet$lat <- NULL
    bikeRet$long <- NULL
    #map listToMap
    #Print out stations that are very asymmetric, get lat + long
    
    print("***********************************************")
    return(bikeRet)
    ####BUILD GRAPHS, map stations that are false ETC?
}


citibike <- read.csv(file = "201607-citibike-tripdata.csv", header = TRUE, sep = ",")
#citibike <- read.csv(file = "./201607-citibike-tripdata-short.csv", header = TRUE, sep = ",")

# Breaks up the dateTime factors into separate fields
citibike <- separate(citibike, starttime, into=c("startMonth", "startDay", "startYear", "startHour", "startMinute", "startSeconds"))
citibike <- separate(citibike, stoptime, into=c("stopMonth", "stopDay", "stopYear", "stopHour", "stopMinute", "stopSeconds"))
station_names <- levels(citibike$start.station.name)

citibike$timeOfDay <- ifelse(as.numeric(citibike$startHour) >= 5 & as.numeric(citibike$startHour) <=12, "morning",
                             ifelse(as.numeric(citibike$startHour) >= 12 & as.numeric(citibike$startHour) <=20, "afternoon",
                                    "night"))

citibike$dayOfWeekStart <- (as.numeric(citibike$startDay) - 1)%%7
citibike$dayOfWeekStart <- factor(citibike$dayOfWeekStart, labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))
citibike$dayOfWeekEnd <- (as.numeric(citibike$stopDay) - 1)%%7
citibike$dayOfWeekEnd <- factor(citibike$dayOfWeekEnd, labels = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"))


# Data imported from [...]
weather <- read.csv("201607-Weather.csv", sep=";")
colnames(weather)[1] <- "Day"

# Calculates number of trips per day
for (i in 1:length(weather$Day)){
    weather$bikes[i] <- nrow(citibike[citibike$startDay== as.character(i), ])
}

#Calculations for top bikes
topbikes <- tail(names(sort(table(citibike$bikeid))),5)
topbikesmin <- round(sum(citibike$tripduration[(citibike$bikeid %in% topbikes)])/length(topbikes)/60)
allbikesmin <- round(sum(citibike$tripduration[!(citibike$bikeid %in% topbikes)])/length(unique(citibike$bikeid[!(citibike$bikeid %in% topbikes)]))/60)

#Stores origin/destination pairs as routes
citibike$route <- with(citibike,paste(as.character(citibike$start.station.name),as.character(citibike$end.station.name)))

#Station asymmetry calculations

uniqueStarts <- as.data.frame(table(citibike$start.station.name))
uniqueEnds <- as.data.frame(table(citibike$end.station.name))

completeFrame <- data.frame(citibike$start.station.name)
completeFrame <- unique(completeFrame)
completeFrame$numStarts <- uniqueStarts$Freq[match(completeFrame$citibike.start.station.name, uniqueStarts$Var1)]
completeFrame$numEnds <- uniqueEnds$Freq[match(completeFrame$citibike.start.station.name, uniqueEnds$Var1)]
completeFrame$difference <- completeFrame$numEnds - completeFrame$numStarts
completeFrame <- arrange(completeFrame, desc(difference))

completeFrame$lat <- citibike$start.station.latitude[completeFrame$citibike.start.station.name]
completeFrame$long <- citibike$start.station.longitude[completeFrame$citibike.start.station.name]

#break into  time of day
morningSubset <- subset(citibike, citibike$timeOfDay == "morning")
afternoonSubset <- subset(citibike, citibike$timeOfDay == "afternoon")
midnightSubset <- subset(citibike, citibike$timeOfDay == "night")

uniqueStartsMorning <- as.data.frame(table(morningSubset$start.station.name))
uniqueEndsMorning <- as.data.frame(table(morningSubset$end.station.name))

uniqueStartsAfternoon <- as.data.frame(table(afternoonSubset$start.station.name))
uniqueEndsAfternoon <- as.data.frame(table(afternoonSubset$end.station.name))

uniqueStartsMidnight <- as.data.frame(table(midnightSubset$start.station.name))
uniqueEndsMidnight <- as.data.frame(table(midnightSubset$end.station.name))

completeFrame$numStartsMorning <- uniqueStartsMorning$Freq[match(completeFrame$citibike.start.station.name, uniqueStartsMorning$Var1)]
completeFrame$numEndsMorning <- uniqueEndsMorning$Freq[match(completeFrame$citibike.start.station.name, uniqueEndsMorning$Var1)]
completeFrame$differenceMorning <- completeFrame$numEndsMorning - completeFrame$numStartsMorning

morningList <- arrange(completeFrame, desc(differenceMorning))

completeFrame$numStartsAfternoon <- uniqueStartsAfternoon$Freq[match(completeFrame$citibike.start.station.name, uniqueStartsAfternoon$Var1)]
completeFrame$numEndsAfternoon <- uniqueEndsAfternoon$Freq[match(completeFrame$citibike.start.station.name, uniqueEndsAfternoon$Var1)]
completeFrame$differenceAfternoon <- completeFrame$numEndsAfternoon - completeFrame$numStartsAfternoon

afternoonList <- arrange(completeFrame, desc(differenceAfternoon))


completeFrame$numStartsMidnight <- uniqueStartsMidnight$Freq[match(completeFrame$citibike.start.station.name, uniqueStartsMidnight$Var1)]
completeFrame$numEndsMidnight <- uniqueEndsMidnight$Freq[match(completeFrame$citibike.start.station.name, uniqueEndsMidnight$Var1)]
completeFrame$differenceMidnight <- completeFrame$numEndsMidnight - completeFrame$numStartsMidnight

midnightList <- arrange(completeFrame, desc(differenceMidnight))


shinyServer(function(input, output) {
    
    #renderPlots
    output$day <- renderPlot({
        
        stationsubset <- subset(citibike, citibike$start.station.name == input$stationSelect)
        day <- ggplot(stationsubset, aes(x=stationsubset$timeOfDay)) + geom_bar()
        print(day)
    })
    
    output$week <- renderPlot({

        stationsubset <- subset(citibike, citibike$start.station.name == input$stationSelect)
        week =ggplot(stationsubset, aes(stationsubset$dayOfWeekStart)) + geom_bar()
        print(week)
    })
    
    output$weatherPlot <- renderPlot({
        plot(weather$Precipitation, weather$bikes)
        
        lm(formula = weather$bikes ~ weather$Precipitation)
        abline(lm(weather$bikes ~ weather$Precipitation))
    })      
    
    
    
    #renderLeaflets
    output$topTenStartMap <- renderLeaflet({
        topstart <- tail(names(sort(table(citibike$start.station.name))),10)
        topstart_rows <- subset(citibike, citibike$start.station.name == topstart)
        station_latitudes <- unique(topstart_rows$start.station.latitude)
        station_longitudes <- unique(topstart_rows$start.station.longitude)
        station_names <- unique(topstart_rows$start.station.name)
        leaflet() %>%
            addTiles() %>%
            addMarkers(station_longitudes, station_latitudes, popup = station_names)
    })

    output$topTenEndMap <- renderLeaflet({
        topend <- tail(names(sort(table(citibike$end.station.name))),10)
        topend_rows <- subset(citibike, citibike$end.station.name == topend)
        station_latitudes <- unique(topend_rows$end.station.latitude)
        station_longitudes <- unique(topend_rows$end.station.longitude)
        station_names <- unique(topend_rows$end.station.name)
        leaflet() %>%
            addTiles() %>%
            addMarkers(station_longitudes, station_latitudes, popup = station_names)
    })
    
    output$popularityMap <- renderLeaflet({
        stationsubset <- subset(citibike, citibike$start.station.name == input$stationSelect)
        station_longitudes <- unique(stationsubset$start.station.longitude)
        station_latitudes <- unique(stationsubset$start.station.latitude)
        station_names <- unique(stationsubset$start.station.name)
        leaflet() %>%
            addTiles() %>%
            addMarkers(station_longitudes, station_latitudes, popup = station_names)
    })
    
    output$routeMap <- renderLeaflet({
        routesubset <- subset(citibike, citibike$route == input$routeSelect)
        station_longitudes <- c(unique(routesubset$start.station.longitude), unique(routesubset$end.station.longitude))
        station_latitudes <- c(unique(routesubset$start.station.latitude), unique(routesubset$end.station.latitude))
        station_names <- unique(routesubset$start.station.name)
        leaflet() %>%
            addTiles() %>%
            addMarkers(station_longitudes, station_latitudes, popup = station_names)
    })
    
    output$asymmetryMap <- renderLeaflet({
        if (input$timeSelect == "Morning") {
            surplus_station_longitudes <- head(morningList$long,10)
            surplus_station_latitudes <- head(morningList$lat,10)
            surplus_station_names <- head(morningList$citibike.start.station.name,10)
            deficit_station_longitudes <- tail(morningList$long,10)
            deficit_station_latitudes <- tail(morningList$lat,10)
            deficit_station_names <- tail(morningList$citibike.start.station.name,10)
        } else if (input$timeSelect == "Afternoon") {
            surplus_station_longitudes <- head(afternoonList$long,10)
            surplus_station_latitudes <- head(afternoonList$lat,10)
            surplus_station_names <- head(afternoonList$citibike.start.station.name,10)
            deficit_station_longitudes <- tail(afternoonList$long,10)
            deficit_station_latitudes <- tail(afternoonList$lat,10)
            deficit_station_names <- tail(afternoonList$citibike.start.station.name,10)
        } else {
            surplus_station_longitudes <- head(midnightList$long,10)
            surplus_station_latitudes <- head(midnightList$lat,10)
            surplus_station_names <- head(midnightList$citibike.start.station.name,10)
            deficit_station_longitudes <- tail(midnightList$long,10)
            deficit_station_latitudes <- tail(midnightList$lat,10)
            deficit_station_names <- tail(midnightList$citibike.start.station.name,10)
        }
        
        leaflet() %>%
            addTiles() %>%
            addMarkers(deficit_station_longitudes, deficit_station_latitudes, popup = deficit_station_names, icon=red) %>%
                addMarkers(surplus_station_longitudes, surplus_station_latitudes, popup = surplus_station_names, icon=blue)
    })
    
    #renderTables
    output$topStartCount <- renderTable({
        sort(tail(sort(table(citibike$start.station.name)),10), decreasing = TRUE)
    })
    
    output$topEndCount <- renderTable({
        sort(tail(sort(table(citibike$end.station.name)),10), decreasing = TRUE)
    })
    
    output$bikeUsage <- renderTable({
        if (input$usageFilter == "frequency") {
            tail(sort(table(citibike$bikeid)),5)
        } else {
            tripdata <- aggregate(tripduration ~ bikeid, data = citibike, sum)
            tripdata$tripduration <- round(tripdata$tripduration/60,2)
            head(tripdata[order(-tripdata$tripduration),], 5)
        }
    })
    
    output$surplusTable <- renderTable({
        switch(input$timeSelect,
               "Morning" = paste(head(morningList$citibike.start.station.name,10)),
               "Afternoon" = paste(head(afternoonList$citibike.start.station.name,10)),
               "Night" = paste(head(midnightList$citibike.start.station.name,10)))
    })
    
    output$deficitTable <- renderTable({
        switch(input$timeSelect,
               "Morning" = paste(tail(morningList$citibike.start.station.name,10)),
               "Afternoon" = paste(tail(afternoonList$citibike.start.station.name,10)),
               "Night" = paste(tail(midnightList$citibike.start.station.name,10)))
    })
        
    #renderTexts
    output$confirm <- renderText({
        paste(input$stationSelect)
    })
    
    output$selectedRoute <- renderText({
        paste(input$routeSelect)
    })
    
    output$topBikesMin <- renderText({
        paste("Average monthly usage for the top 5 bikes: ", topbikesmin, " minutes")
    })
    
    output$allBikesAvg <- renderText({
        paste("Average monthly usage for all others: ", allbikesmin, " minutes")
    })
    
    output$bikesRatio <- renderText({
        paste("Resulting usage ratio: ", round(topbikesmin/allbikesmin,1))
    })
    

})
