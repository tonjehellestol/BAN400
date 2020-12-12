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



library("COVID19")
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
library(gganimate)
library(ggthemes)
library(forecast)
library(zoo)
library(stringr)
library(lubridate)

##Input-data
covid <- covid19.data()

#Creating a dataset for the satatistics and one for the countries
covid_num <- covid  %>% select("Confirmed", "Deaths")
countries <- covid  %>% select(Country_Region)

#Population data
#data("pop")
#population_df <- pop %>% select("name", "2020" ) %>%
#dplyr::rename(Country.Region ="name", Population = "2020") 


######## Retrieving datasets

#Column names wanted for the data
column_names = c("ID", "date", "confirmed_cases", "confirmed_deaths", "country_name", "population")


#Dataset from covid19 package
#Petter sin versjon
# Crossgovsources_df2 <- covid19() %>%
#   select("id","date","tests","confirmed","deaths","administrative_area_level_1","population") %>%
#   rename_at(vars(c("id","date","confirmed","deaths","administrative_area_level_1","population")), ~ column_names) %>%
#   replace(is.na(.),0) %>%
#   mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths)))

na.locf2 <- function(x) na.locf(x, na.rm = FALSE)


Crossgovsources_df <- covid19() %>% 
  select("id","date","tests","confirmed","deaths","administrative_area_level_1","population") %>% 
  rename_at(vars(c("id","date","confirmed","deaths","administrative_area_level_1","population")), ~ column_names) %>%
  filter(country_name != "Costa Atlantica",country_name != "Diamond Princess") %>% #not countries
  group_by(country_name) %>% 
  do(na.locf2(.)) %>% #replace NA by previous cumulative value
  replace(is.na(.),0) %>% #replace NAs with not previous values by 0 
  mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths))) %>% #calculate daily increase in number of deaths and confirmed cases
  ungroup() 




##############bruker forel??pig kun Crossgov som datasett #########

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
    plotlyOutput(outputId = "plot" ,height = '500px'),
    verbatimTextOutput('text'),
    width = 5))
)




##########Graphs#########


graph_dailyConfirmed <- function(df, country){
  
  
  temp_df <- df %>% 
    filter(country_name == country)
  temp_df$average <- ma(temp_df$daily_cases, 7)
  
  
  #Creating graph  
  graph_cases <- temp_df %>%
    ggplot(aes(x=date)) +
    geom_bar(aes(y=daily_cases, text = paste0("New Cases: ", daily_cases, "\nDate: ", date)), stat ="identity", colour = "#DD8888", fill = "#e74c4c", alpha = 0.4) +
    geom_line(aes(y = average), size = 1.5, colour = "red", alpha = 0.6) +
    labs (title = paste0("Daily Cases of Covid19 In ", country),
          x = "Date",
          y = "Daily Confirmed Cases") +
    theme_hc() 
  
  
  ## Creating font for the interactive tooltip
  interactive_font = list(
    family = "DM Sans",
    size = 16,
    color = "white"
  )
  
  ## Creating the layout for the interactive tooltip
  interactive_label = list(
    bgcolor = "#595959",
    bordercolor = "transparent",
    font = interactive_font
  )
  
  
  
  
  ## Turning graph_cases to an interactive graph
  interactive_plot <- ggplotly(graph_cases, tooltip = c("text")) %>% 
    style(hoverlabel = interactive_label) %>% 
    layout(font = interactive_font,
           yaxis = list(fixedrange = TRUE)) %>% 
    config(displayModeBar = FALSE)
  
  
  return(interactive_plot)
}


graph_dailyDeaths <- function(df, country){
  
  
  temp_df <- df %>% 
    filter(country_name == country)
  temp_df$average <- ma(temp_df$daily_deaths, 7)
  
  
  #Creating graph  
  graph_cases <- temp_df %>%
    ggplot(aes(x=date)) +
    geom_bar(aes(y=daily_deaths, text = paste0("New Cases: ", daily_cases, "\nDate: ", date)), stat ="identity", colour = "#DD8888", fill = "#e74c4c", alpha = 0.4) +
    geom_line(aes(y = average), size = 1.5, colour = "red", alpha = 0.6) +
    labs (title = paste0("Daily deaths of Covid19 In ", country),
          x = "Date",
          y = "Daily deaths") +
    theme_hc() 
  
  
  ## Creating font for the interactive tooltip
  interactive_font = list(
    family = "DM Sans",
    size = 16,
    color = "white"
  )
  
  ## Creating the layout for the interactive tooltip
  interactive_label = list(
    bgcolor = "#595959",
    bordercolor = "transparent",
    font = interactive_font
  )
  
  
  ## Turning graph_cases to an interactive graph
  interactive_plot <- ggplotly(graph_cases, tooltip = c("text")) %>% 
    style(hoverlabel = interactive_label) %>% 
    layout(font = interactive_font,
           yaxis = list(fixedrange = TRUE)) %>% 
    config(displayModeBar = FALSE)
  
  
  return(interactive_plot)
}





##########MAP#############

getGlobalConfirmed<- function(df){
  
  map_data <- df %>% 
    #mutate(last_date_in_month = ceiling_date(ymd(date), 'month') -1)%>% 
    #filter(date == Sys.Date()-1 | date == last_date_in_month) %>% 
    #filter((date == Sys.Date()-1 | date == last_date_in_month), country_name != "Costa Atlantica",country_name != "Diamond Princess" ) %>% 
    filter(date == Sys.Date()-2 ) %>% 
    mutate(casesPer100k = round(confirmed_cases/population*100000,2),)%>% 
    select(ID, date, country_name, ID, casesPer100k) %>% 
    mutate(hover = paste0(country_name, "\n", casesPer100k))
  
  return (map_data)
  
}


getGlobalDeaths<- function(df){
  
  map_data <- df %>% 
    mutate(last_date_in_month = ceiling_date(ymd(date), 'month') -1)%>% 
    #filter(date == Sys.Date()-1 | date == last_date_in_month) %>% 
    #filter((date == Sys.Date()-1 | date == last_date_in_month), country_name != "Costa Atlantica",country_name != "Diamond Princess" ) %>% 
    filter(date == Sys.Date()-2) %>% 
    mutate(casesPer100k = round(confirmed_deaths/population*100000,2),)%>% 
    select(ID, date, country_name, ID, casesPer100k) %>% 
    mutate(hover = paste0(country_name, "\n", casesPer100k))
  
  return (map_data)
  
}



drawMap <- function(map_data, main_title, colorbar_title){
  #print("hei, skal tegne map")
  
  map <- plot_ly(map_data, 
                 type='choropleth', 
                 #frame = map_data$date,
                 locations=map_data$ID,
                 z=map_data$casesPer100k, 
                 zmin=0,
                 zmax = max(map_data$casesPer100k),
                 colorscale = list(c(0, 0.1, 0.2,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                                   c(brewer.pal(11,'Spectral'))),
                 color = map_data$casesPer100k,
                 text = map_data$hover,
                 hoverinfo = 'text') %>% 
    colorbar(title = colorbar_title, len = 0.75) %>% 
    layout(title = main_title)
  
  #print("retunerer map")
  return(map)
  
}



#####Server######
server <- function(input, output) {
  
  data_graph = reactive({
    Crossgovsources_df %>% mutate(date = as.Date(date)) %>% 
   filter( date >= input$dates[1] & date <=input$dates[2] )
  })
  
  
  
  output$plot <- renderPlotly({
    if(input$PlotType == 'Graph'){
      data <- data_graph()
      if (input$Stat == 'Deaths'){
        graph_dailyDeaths(data,input$Country)
    }else{ #confirmed
      graph_dailyConfirmed(data,input$Country)
    }}else{#map
      if (input$Stat == 'Deaths'){
        Crossgovsources_df %>% getGlobalDeaths() %>%
          drawMap( paste0("Confirmed number of deaths per 100.000 as of ", as.character(Sys.Date()-2)), "Number of deaths" )
      }else{ #confirmed cases
        Crossgovsources_df %>% getGlobalConfirmed() %>%
          drawMap( paste0("Confirmed cases per 100.000 as of ", as.character(Sys.Date()-2)), "Confirmed cases")
      }
    }
  })
  
}

shinyApp(ui = ui, server = server)