library(devtools)
library(shiny)
require(shinydashboard)
library(leaflet)
library(ggmap)
library(ggplot2)
library(scales)
library(shinyapps)

aqi <- read.csv("aqidata.csv")
aqi$County <- as.factor(aqi$County)
aqi$AQI.Max <- as.numeric(aqi$AQI.Max)

parks <- read.csv("parks.csv")
parks$latitude <- as.numeric(as.character(parks$latitude))
parks <- parks[complete.cases(parks),]

resp <- read.csv("respiratorysystem.csv")
circ <- read.csv("circulatorysystem.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Air Quality and Health"),
  dashboardSidebar(
    selectInput("choice1", "Select 1st Location:", choices = as.character(levels(aqi$County)), selected = "Washington, DC"),
    selectInput("choice2", "Select 2nd Location:", choices = as.character(levels(aqi$County)), selected = "Los Angeles, CA")               
  ),
  dashboardBody(
    fluidRow(
      box(leafletOutput("plot1", height = 600)),
      box(leafletOutput("plot2", height = 600))
    ),
    fluidRow(
      box(plotOutput("chart1", height = 400)), 
      box(plotOutput("chart2", height = 400))  
    ),
    fluidRow(
      box(plotOutput("rgraph1", height = 400)), 
      box(plotOutput("rgraph2", height = 400))  
    ),
    fluidRow(
      box(plotOutput("cgraph1", height = 400)), 
      box(plotOutput("cgraph2", height = 400))  
    ),
    fluidRow(
      box(imageOutput("image1"), height = 630, width=6),
      box(imageOutput("image2"), height = 630, width=6)  
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderLeaflet({
    location1 <- geocode(input$choice1)
    
    m <- leaflet() %>% addTiles(url = "http://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}")
    m <- m %>% addTiles(url = "http://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}")
    m <- m %>% setView(location1$lon, location1$lat, zoom = 12)
    m
    
    parks1 <- parks[which(parks$County==input$choice1),]
    m %>% addCircles(parks1$longitude, parks1$latitude, color = "green")
  })
  
  output$plot2 <- renderLeaflet({
    location2 <- geocode(input$choice2)
    
    n <- leaflet() %>% addTiles(url = "http://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}")
    n <- n %>% addTiles(url = "http://services.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}")
    n <- n %>% setView(location2$lon, location2$lat, zoom = 12)
    n
    #data.type <- input$choice
    #time <- input$year
    #data2<- data[data$Data.Type == data.type & data$Year == time,]
    
    parks2 <- parks[which(parks$County==input$choice2),]
    n %>% addCircles(parks2$longitude, parks2$latitude, color = "green")
  })
  
  output$chart1 <- renderPlot({
    aqi1 <- aqi[which(aqi$County==input$choice1),]
    aqi2 <- aqi[which(aqi$County==input$choice2),]
    
    p1 <- ggplot() + 
      geom_point(data=aqi1, aes(Year, AQI.Median, color=AQI.Median), size=4) + 
      geom_point(data=aqi1, aes(Year, AQI.Max, color=AQI.Max), size=4) + 
      labs(title="Air Quality Index (AQI)", x="Year", y="AQI") + 
      ylim(0, max(c(max(aqi1$AQI.Max), max(aqi2$AQI.Max))) + 10) + 
      theme(legend.title=element_blank())
    p1
  })
  
  output$chart2 <- renderPlot({
    aqi1 <- aqi[which(aqi$County==input$choice1),]
    aqi2 <- aqi[which(aqi$County==input$choice2),]
    
    p2 <- ggplot() + 
      geom_point(data=aqi2, aes(Year, AQI.Median, color=AQI.Median), size=4) + 
      geom_point(data=aqi2, aes(Year, AQI.Max, color=AQI.Max), size=4) + 
      labs(title="Air Quality Index (AQI)", x="Year", y="AQI") + 
      ylim(0, max(c(max(aqi1$AQI.Max), max(aqi2$AQI.Max))) + 10) + 
      theme(legend.title=element_blank())
    p2
  })
  
  output$rgraph1 <- renderPlot({
    resp1 <- resp[which(resp$County==input$choice1),]
    resp2 <- resp[which(resp$County==input$choice2),]
    
    rg1 <- ggplot() +  
      geom_point(data=resp1, aes(Year, Rate, color=Rate), size=4) +
      labs(title="Respiratory Disease Rate", x="Year", y="Rate per 100,000 people") + 
      ylim(0, max(c(max(resp1$Rate), max(resp2$Rate))) + 5) + 
      theme(legend.title=element_blank())
    rg1
  })
  
  output$rgraph2 <- renderPlot({
    resp1 <- resp[which(resp$County==input$choice1),]
    resp2 <- resp[which(resp$County==input$choice2),]
    
    rg2 <- ggplot() + 
      geom_point(data=resp2, aes(Year, Rate, color=Rate), size=4) +
      labs(title="Respiratory Disease Rate", x="Year", y="Rate per 100,000 people") + 
      ylim(0, max(c(max(resp1$Rate), max(resp2$Rate))) + 5) + 
      theme(legend.title=element_blank())
    rg2
  })
  
  output$cgraph1 <- renderPlot({
    circ1 <- circ[which(circ$County==input$choice1),]
    circ2 <- circ[which(circ$County==input$choice2),]
    
    cg1 <- ggplot() + 
      geom_point(data=circ1, aes(Year, Rate, color=Rate), size=4) +
      labs(title="Circulatory Disease Rate", x="Year", y="Rate per 100,000 people") +
      ylim(0, max(c(max(circ1$Rate), max(circ2$Rate))) + 5) + 
      theme(legend.title=element_blank())
    cg1
  })
  
  output$cgraph2 <- renderPlot({
    circ1 <- circ[which(circ$County==input$choice1),]
    circ2 <- circ[which(circ$County==input$choice2),]
    
    cg2 <- ggplot() +  
      geom_point(data=circ2, aes(Year, Rate, color=Rate), size=4) +
      labs(title="Circulatory Disease Rate", x="Year", y="Rate per 100,000 people") +
      ylim(0, max(c(max(circ1$Rate), max(circ2$Rate))) + 5) + 
      theme(legend.title=element_blank())
    cg2
  })  
  
  output$image1 <- renderImage({
    list(src = "C:/Users/user name/Documents/Codeathon/dcwalkscore.png", height=600, width=600)
  }, deleteFile=F)
  
  output$image2 <- renderImage({
    list(src = "C:/Users/user name/Documents/Codeathon/lawalkscore.png", height=600, width=600)
  }, deleteFile=F)
}

shinyApp(ui, server)