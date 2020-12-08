#----------------------------------------------------------------------
#BAN420 - Introduction to R
#September 2020
#----------------------------------------------------------------------
#References:
#Covid19.analytics package:
#https://www.rdocumentation.org/packages/covid19.analytics/versions/1.0
#Shiny:
#https://shiny.rstudio.com/tutorial/



#Installing missing packages (if any) that are needed for the application
#Retrieved from 
#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

list.of.packages <- c("covid19.analytics", "magrittr", "tidyr", "ggplot2",
                      "shiny", "shinyWidgets", "data.table", "scales", "wpp2019",
                      "RColorBrewer", "rworldmap", "dplyr", "plotly")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


#Load libraries 
library(covid19.analytics)
library(magrittr)
library(tidyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(data.table)
library(scales)
library(wpp2019)
library(RColorBrewer)
library(rworldmap)
library(dplyr)
library(plotly)

##Input-data
covid <- covid19.data()

#Creating a dataset for the satatistics and one for the contries
covid_num <- covid  %>% select("Confirmed", "Deaths", "Recovered")
countries <- covid  %>% select(Country_Region)

##Population data
data("pop")
population_df <- pop %>% select("name", "2020" ) %>%
  dplyr::rename(Country.Region ="name", Population = "2020") 



####UI#######
ui <- fluidPage( 
  headerPanel(
    img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcSsx0X2qtaA3tNn_i3YeEcDh2_F0AXEVDYemQ&usqp=CAU", height = "140px", width = "31.9%")
  ), #collects image from specified webpage
  #Source: https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
  sidebarPanel(
    helpText("Select", em("Confirmed, Deaths"), "or", em("Recovered"), "and a country to examine.
             
             The data is continously updating."), #Help text with italic text for the options
    
    selectInput('Stat', 'Data:', names(covid_num )), #Select data type 
    selectInput('PlotType', 'Data Visualization:', c('Map', 'Graph')), #Select the data visualization
    
    #will only show this panel if the data visualization chosen is "Graph"
    conditionalPanel(
      condition = "input.PlotType == 'Graph'", 
      selectInput('Country', 'Country', countries), #Select country 
      dateRangeInput("dates",
                     "Date range",
                     start = "2020-01-22", #start date of the dataset
                     end = as.character(Sys.Date())) #Ends at todays date by default
    )),
  #Creates the space and size of the output 
  mainPanel(strong(
    plotOutput('plot',height = '500px'),
    verbatimTextOutput('text'),
    width = 5))
)


#####Functions######

#Renaming specific countries in order to match the dataset "countries". 
#This is in order to include the specified countries in the map that we are going to create 
population_df$Country.Region[population_df$Country.Region == "United States of America"] <- "US"
population_df$Country.Region[population_df$Country.Region == "Russian Federation"] <- "Russia"
population_df$Country.Region[population_df$Country.Region == "Bolivia (Plurinational State of)"] <- "Bolivia"
population_df$Country.Region[population_df$Country.Region == "Congo"] <- "Congo (Kinshasa)"
population_df$Country.Region[population_df$Country.Region == "United Republic of Tanzania"] <- "Tanzania"

##retrieving data from a country containing daily cases for a specific stat
getDailyDataForCountry = function(stat, country, date_range ){
  
  
  ts_data <- covid19.data(paste0("ts-", tolower(as.character(stat)))) %>% #Reading the dataset with time series
    subset(select = -Province.State) # Removes column
  
  
  temp <- ts_data %>%
    filter(Country.Region == country) %>% # Filters by country given
    group_by(Country.Region) %>% # Adds data from different states/provinces
    summarise_all(funs(sum)) %>% # as data for the country as a whole
    pivot_longer(cols = colnames(.)[-(1:4)], #Turns date columns into rows
                 names_to = "Date",  
                 values_to = "Cases") %>%
    select(Country.Region, Date, Cases) #Selects wanted data
  temp$Date <- as.Date(temp$Date) # Converts date column from class character to date
  
  
  sum <- as.integer(temp[1,3]) #Used to calculate daily new cases
  
  for (i in 2:(nrow(temp))){ #Iterates through and calculates daily new cases confirmed
    if (temp[i,3] == sum){ #No increase sets value to 0
      temp[i,3] = 0
    } else { #Calculates increase
      temp[i,3] = temp[i,3] - sum
      sum <- sum + temp[i,3]
    }
  }
  temp <- filter(temp, Date >= date_range[1] & Date <=date_range[2])
  
  return (temp)
  
}


##retrieving data from all countries containing accumulated cases for a specific stat
globalData = function(stat){
  #The time series data is accumulated, to retrieve e.g number of confirmed cases yesterdays
  # date is used
  mapData <- covid19.data(paste0("ts-", tolower(as.character(stat)))) %>%
    select(Country.Region, Lat, Long, as.character(Sys.Date()-1)) %>% #Selects wanted columns
    group_by(Country.Region) %>% 
    summarise_all(funs(sum)) #Adds data for different country regions together
  
  names(mapData)[names(mapData) == as.character(Sys.Date()-1)] <- "Cases"
  
  
  
  mapData <-left_join(mapData, population_df, by.x = "Country.Region") %>% drop_na() %>% 
    mutate("Cases per 100 000 capita" = Cases/Population*100) #Adding a column for cases per 100 0000. 
  #Multiplying by 100 because the population data is in 1000
  
  return (mapData)
  
}


#plotting graph by given data and titles
plotGraph = function(data,xvar, yvar, title = "", xlab= "", ylab= "") {
  
  plot <- data %>%
    ggplot(aes(x=xvar,
               y=yvar,
               ymin=0)) +
    ggtitle(title)+
    xlab(xlab)+
    ylab(ylab)+
    geom_line(col = "red")+
    theme_classic() +
    theme(axis.text = element_text(size =14, face = "bold"),
          axis.title = element_text(size = 14,face = "bold")) +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) +
    theme(plot.margin = unit(c(0,0,2,2),"cm"))
  
  
  return (plot)
  
}

#drawing a map 
drawMap = function(mapData, plotBy, title){
  
  worldMap <- joinCountryData2Map(mapData,
                                  nameJoinColumn = "Country.Region", #Selects column to join by
                                  joinCode = "NAME") #defines what countries are referenced by
  
  colors <- RColorBrewer::brewer.pal(10,'Spectral') #Retrieves a color palette for the map
  
  map <- mapCountryData(worldMap,
                        nameColumnToPlot = plotBy, #Joins data and country to the map
                        mapTitle = title,
                        catMethod = 'fixedwidth', #width of map
                        colourPalette = colors, 
                        numCats = 10) #Number of categories to color the map by
  
  return(map)
  
}


#####Server######
server <- function(input, output) {
  
  datainputDaily <- reactive(
    getDailyDataForCountry(input$Stat, input$Country, input$dates)
    
  )
  
  datainputMap <- reactive(
    globalData(input$Stat)
    
  )
  
  
  output$plot <- renderPlot({
    
    if(input$PlotType == 'Graph'){ #plots if input is 'Graph'
      data <-datainputDaily()
      if (input$Stat == 'Deaths'){
        plotGraph(data, data$Date, data$Cases,paste0("Daily number of deaths in ",  as.character(input$Country)), "Time horizon", paste0(as.character(input$Stat)))
      }else{
        plotGraph(data, data$Date, data$Cases,paste0("Daily ", tolower(as.character(input$Stat)) ," cases in ",  as.character(input$Country)), "Time horizon", paste0(as.character(input$Stat))  )}
    }else { ##if input is 'Map'
      data <- datainputMap()
      drawMap(data, 'Cases per 100 000 capita', paste0("Number of ", input$Stat, " per 100 000 capita"))
      
    }
    
  })
  
  
}

shinyApp(ui = ui, server = server)


