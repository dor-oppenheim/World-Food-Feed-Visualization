library(shiny)
library(leaflet)
library(RColorBrewer)
library(maps)
library(rgdal)
library(dplyr)
require(data.table)


# Prepare Data
FAO_data = read.csv("data/FAO database - Feed&Food.csv",header = T)
years <- paste("Y", 1961:2013, sep="")
myvars = c("X","Area","Item","Element","longitude","latitude",years)
fao = FAO_data[myvars]
years <- paste(1961:2013, sep="")
colnames(fao) <- c("X","Area","Item","Element","longitude","latitude",years)

dt <- data.table(fao, key=c("X","Area","Element"))
dt1 <- dt[, lapply(.SD, sum), by=c("X","Area","Element"), .SDcols=c(7:59)]
dt2 <- dt[, lapply(.SD, head, 1), by=c("X","Area","Element"), .SDcols=c(5,6)]
dt3=dt2[dt1]
dt3$Item <- 'Total production'
dt3 <- data.frame(dt3)
dt3 <- dt3[,c(1,2,59,3,4,5,6,7:58)]
colnames(dt3) <- c("X","Area","Item","Element","longitude","latitude",years)
fao <- rbind(fao,dt3)

Food_Feed_Ratio <- seq(0, 1, length=100)
food_items = unique(fao['Item'])
# Rearrange the drop down food item selections
food_items = data.frame(food_items[c(116,1,83,2:82,84:115),])

                            # Interactive Map #

################################## UI #############################################################################################
# Initialization of UI variables, such as slider and srop down list.
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # Year Slider
                sliderInput("year", "Year", 1961, 2013,
                            value = 2013, step = 1,sep=""
                ),
                # Drop list 
                selectInput("item", "Food Item",
                            food_items
                ),
                # Checkbox for legend
                checkboxInput("legend", "Show legend", TRUE)))

################################## Server ##########################################################################################
# Server side codes and functions
server <- function(input, output, session) {
  
                              # Create the map #
  
  output$map <- renderLeaflet({
    leaflet(fao) %>% 
      addTiles() %>% # in addTiles you can change maps
      setView(lng = 0, lat = 20, zoom = 2)}) # set view to a predefined spot on the map  
  
                             # Get map boundries #
  # Checks what the current boundaries of the map and return countries inside
  countriesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(filteredData()[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(filteredData(),
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])})
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    myvars <- c("X","Area","Item","Element","longitude","latitude",toString(input$year))
    #myvars <- c("X","Area","Item","Element","longitude","latitude","2013")
    fd1 <- fao[myvars]
    # Subsets the chosen food item
    fd2 <- subset(fd1,fd1$Item==input$item) 
    #fd2 <- subset(fd1,fd1$Item=="Millet and products")
    # Replaces columns names
    colnames(fd2) <- c("X","Area","Item","Element","longitude","latitude","year")
    # Creates new dataset according to users choice
    feed <- subset(fd2,Element=='Feed')
    food <- subset(fd2,Element=='Food')
    fd3 <- merge(food,feed, by = c("X","Area","Item","Element","longitude","latitude"), all = TRUE)
    colnames(fd3) <- c("X","Area","Item","Element","longitude","latitude","x","y")
    fd3$x[is.na(fd3$x)] <- 0
    fd3$y[is.na(fd3$y)] <- 0
    fd4 <- aggregate(cbind(fd3$x,fd3$y) ~ fd3$Area + fd3$Item, data = fd3, sum)
    fd5 <- aggregate(cbind(fd3$longitude,fd3$latitude) ~ fd3$Area + fd3$Item, data = fd3, unique)
    food_feed <- merge(fd4,fd5,by=c("fd3$Area","fd3$Item"))
    colnames(food_feed) <- c("Country","Food Item","Food","Feed","longitude","latitude")
    food_feed$ratio <- food_feed$Food/(food_feed$Feed+food_feed$Food)
    food_feed$ratio[is.na(food_feed$ratio)] <- 0
    food_feed$total <- food_feed$Food+food_feed$Feed
    food_feed
      })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
    colorpal <- reactive({
    colorNumeric('RdYlGn',filteredData()$ratio)
    })
  

  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~ total/ sum(total) *5000000, weight = 1, color = "#777777", layerId=~Country,
                 fillColor = ~pal(ratio), fillOpacity = 0.7,stroke=FALSE
      )
  })
  
  # Show a popup at the given location
  showCountryPopup <- function(country, lat, lng) {
    selected_country <- filteredData()[filteredData()$Country == country,]
    ratio_food <- selected_country$ratio
    ratio_feed <- selected_country$ratio
    if ((ratio_feed==0)&&(ratio_food==0)){
      ratio_feed = 0
      ratio_food = 0
    }     else {ratio_feed = 1-ratio_feed}   
    content <- as.character(tagList(
      tags$h4("Total Production:", as.integer(selected_country$total)),
      tags$strong(HTML(sprintf("%s, %s",selected_country$Country, selected_country$`Food Item`))), tags$br(),
      sprintf("Food (1000 tonnes): %s. Ratio from total: %s%%", as.integer(selected_country$Food),as.integer(round(ratio_food, digits = 2)*100)),tags$br(),
      sprintf("Feed (1000 tonnes): %s. Ratio from total: %s%%", as.integer(selected_country$Feed),as.integer((round(ratio_feed, digits = 2))*100)),tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = country)
  }
  # When map is clicked, show a popup with country's info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showCountryPopup(event$id, event$lat, event$lng)
    })
  })

  # Use a separate observer to recreate the legend as needed.

  observe({
    proxy <- leafletProxy("map", data = fao)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Food_Feed_Ratio
      )
    }
  })
  
}

shinyApp(ui, server)

