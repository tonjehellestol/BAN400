
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

#Legger inn for norge 

### Retrieved from https://github.com/thohan88/covid19-nor-data
norwaydata <- read.csv("https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_and_district.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
norwaydata$date <- as.Date(norwaydata$date)

### Det er noen feil med data, kumulativ antall cases blir mindre i noen kommuner p? enkelte datoer feks B?rum
### Ved utregning av antall nye daglige tilfeller f?r jeg derfor negative tall, noe som ikke gir mening
### Evt kan dette testes ved ? se om data[x] > data[y] hvor x > y, evt sette data[x] == data[y] dersom f?rste statement er TRUE


norway <- norwaydata %>% 
  select("date","kommune_name","fylke_name","cases") %>% 
  group_by(kommune_name, date, fylke_name) %>% 
  summarise_at(vars(cases),             
               list(cases = sum)) %>% 
  ungroup() %>% 
  group_by(kommune_name) %>% 
  mutate(daily_cases = c(0,diff(cases)))

#Creating a dataset for the satatistics and one for the municipalities 
#prøvde å gjøre det samme som for covid, men kommune blir lagt til i norway_cases
norway_cases <- norway %>% select("cases")
kommune <- norway %>% 
  group_by(kommune_name) %>%
  select("kommune_name")




####UI#######
ui <- fluidPage( 
  titlePanel(
    img(src = "https://www.siv.no/PublishingImages/Nyheter/Praksisnytt/Koronavirus.png?RenditionID=3", height = "200px", width = "99.9%")
  ), #collects image from specified webpage
  #Source: https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
  navbarPage("COVID-19 Statistics",
             tabPanel("Global", icon=icon("home"),
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
                        )
                      ),
                      #Creates the space and size of the output 
                      mainPanel(strong(
                        plotlyOutput('plot',height = '500px'),
                        verbatimTextOutput('text'),
                        width = 5)
                      ),
                      
                      hr(),
                      
                      fluidRow(column(width=3),
                               #column(width=1),
                               column(br(),
                                      strong(p("Number of deaths:")), 
                                      textOutput("death"),
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                               column(widt=4),
                               column(br(),
                                      strong(p("Total confirmed cases:")), 
                                      textOutput("TCC"),
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                               br(),
                               column(widt=2),
                               
                               column(br(),
                                      strong(p("Last week:")), 
                                      textOutput("lastweek"),
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                      ),
                      
                      br(),
                      br(),
                      
                      fluidRow(column(width=3),
                               column(br(),
                                      p("Test status:    ",style="color:black"),
                                      width=9,style="background-color:lightyellow;border-radius: 10px",
                                      actionButton("action", "Test statistic")),
                               br(),
                      ),
                      
                      br(),
                      br(),
                      
             ),
             
             tabPanel("Norway", icon=icon("bar-chart-o"),
                      sidebarPanel(
                        helpText("Choose"),
                        selectInput('Stat', 'Data:', names(norway_cases)), #c('Map', 'Graph')), #Select data type 
                        selectInput('PlotType', 'Data Visualization:', c('Top 10', 'MunGraph')),
                        
                        #will only show this panel if the data visualization chosen is "Graph"
                        #denne funker ikke før den er lagt inn i serveren, den må hete noe annet enn Graph, ellers responderer den på global fanen
                        conditionalPanel(
                          condition = "input.PlotType == 'MunGraph'", 
                          selectInput('Municipality', 'Municipality', kommune), 
                          dateRangeInput("dates",
                                         "Date range",
                                         start = "2020-01-22", #start date of the dataset
                                         end = as.character(Sys.Date())) #Ends at todays date by default
                          
                          
                        )
                      ),
                      
                      mainPanel(
                        h3(p(strong('Plot output',style="color:salmon")),align="center"),
                        column(plotlyOutput("Legg til navn pa plot"),width = 12,style="border:1px solid black"), 
                      ),
                      
                      hr(),
                      
                      fluidRow(column(width=3),
                               #column(width=1),
                               column(br(),
                                      strong(p("Number of deaths:")), 
                                      textOutput("deathnor"),
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                               column(widt=4),
                               column(br(),
                                      strong(p("Total confirmed cases:")), 
                                      textOutput("TCCnor"),
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                               br(),
                               column(widt=2),
                               
                               column(br(),
                                      strong(p("Last week:")), 
                                      textOutput("lastweeknor"),
                                      br(),
                                      width = 3,style="background-color:	lightgray;border-left:6px solid gray;border-top: 1px solid black;border-right:1px solid black;border-bottom: 1px solid black"),
                      ),
                      br(),
                      br(),
                      
                      fluidRow(column(width=3),
                               column(br(),
                                      p("Test status:    ",style="color:black"),
                                      width=9,style="background-color:lightyellow;border-radius: 10px",
                                      actionButton("action", "Test statistic"),
                               ),
                               
                               br(),
                      ),
                      
                      
                      br(),
                      br(),
                      
             ),
             tabPanel("About this site", icon=icon("arrow-right"),
                      tags$div(
                        tags$h4("Last update"),
                        h6(paste0(as.character(Sys.Date()))),
                        p("This site uses data for Norway retrived from", a(href="https://github.com/thohan88/covid19-nor-data", "https://github.com/thohan88/covid19-nor-data", target="_blank"), "and the built in data packages in R, covid19() and covid19.data(). The data is updated once daily, and numbers till yesterday is included in the data visualization."),
                        
                      
             )
  )))


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
  
  #Outfor for the boxes below the graph/map
  #Bruk renderText istedet hvis det skal være if funksjon e.l., for renderPrint kan man feks skrive summary(totaldeaths)
  #Denne linken kan sjekkes ut https://github.com/RiveraDaniel/Regression/blob/master/server.R
  output$death <- renderPrint({p("hundre")}) #legg inn fnavn på unksjon for antall døde
  output$TCC <- renderPrint({p("10")})
  output$lastweek <- renderPrint({p("hundre")}) 
  
  output$deathnor <- renderPrint({p("hundre")}) #legg inn funksjon for antall døde
  output$TCCnor <- renderPrint({p("10")}) 
  output$lastweeknor <- renderPrint({p("100")}) 
  
}

shinyApp(ui = ui, server = server)

