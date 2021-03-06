#----------------------------------------------------------------------
  #BAN400 - Introduction to R
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





#---------------------------------- Retriving datasets ----------------------------------#

### Retriving data for global statistics ###

#Column names wanted for the data
column_names = c("ID", "date", "confirmed_cases", "confirmed_deaths", "country_name", "population")

na.locf2 <- function(x) na.locf(x, na.rm = FALSE) #replace NA by previous value


Crossgovsources_df <- covid19() %>% 
  select("id","date","tests","confirmed","deaths","administrative_area_level_1","population") %>% 
  rename_at(vars(c("id","date","confirmed","deaths","administrative_area_level_1","population")), ~ column_names) %>%
  filter(country_name != "Costa Atlantica",country_name != "Diamond Princess") %>% #not countries
  group_by(country_name) %>% 
  do(na.locf2(.)) %>% #replace NA by previous cumulative value
  replace(is.na(.),0) %>% #replace NAs with not previous values by 0 
  mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths))) %>% #calculate daily increase in number of deaths and confirmed cases
  ungroup() 


#options for user - countries in the dataset
countries <- Crossgovsources_df  %>% select(country_name)


### Retriving data for Norwegian statistics ###


### Retrieved from https://github.com/thohan88/covid19-nor-data
norwaydata <- read.csv("https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_and_district.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
norwaydata$date <- as.Date(norwaydata$date)


norway <- norwaydata %>% 
  select("date","kommune_name","fylke_name","cases") %>% 
  group_by(kommune_name, date, fylke_name) %>% 
  summarise_at(vars(cases),             
               list(cases = sum)) %>% 
  ungroup() %>% 
  group_by(kommune_name) %>% 
  mutate(daily_cases= c(0,diff(cases)))



#Creating a dataset for the statistics and one for the municipalities 

kommune <- norway %>% select("kommune_name")




#---------------------------------- UI ----------------------------------#
ui <- fluidPage( 
  titlePanel(
    img(src = "https://www.siv.no/PublishingImages/Nyheter/Praksisnytt/Koronavirus.png?RenditionID=3", height = "200px", width = "99.9%")
  ), #collects image from specified webpage
  #Source: https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
  navbarPage("COVID-19 Statistics",
             tabPanel("Global", icon=icon("home"),
                      sidebarPanel(
                        helpText("Select", em("Confirmed"), "or", em("Deaths"), "and a country to examine.
               
               The data is continously updating."), #Help text with italic text for the options
                        
                        selectInput('Stat', 'Data:', c('Confirmed', 'Deaths')), #Select data type 
                        selectInput('PlotType', 'Data Visualization:', c('Map', 'Graph')), #Select the data visualization
                        
                        #will only show this panel if the data visualization chosen is "Graph"
                        conditionalPanel(
                          condition = "input.PlotType == 'Graph'", 
                          selectInput('Country', 'Country', countries, selected = countries[1]), #Select country 
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
                                      textOutput("tsglobal"),
                                      width=9,style="background-color:lightyellow;border-radius: 10px",
                               ),
                               br(),
                      ),
                      
                      br(),
                      br(),
                      
             ),
             
             tabPanel("Norway", icon=icon("bar-chart-o"),
                      sidebarPanel(
                        helpText("Choose"),
                        #selectInput('StatNorway', 'Data:', c("Confirmed","Deaths")), #c('Map', 'Graph')), #Select data type 
                        selectInput('PlotTypeNorway', 'Data Visualization:', c('Top 10', 'Graph')),
                        
                        #will only show this panel if the data visualization chosen is "Graph"
                        #denne funker ikke f??r den er lagt inn i serveren, den m?? hete noe annet enn Graph, ellers responderer den p?? global fanen
                        conditionalPanel(
                          condition = "input.PlotTypeNorway == 'Graph'", 
                          selectInput('Municipality', 'Municipality', kommune, selected = kommune[1]), 
                          dateRangeInput("dates",
                                         "Date range",
                                         start = "2020-01-22", #start date of the dataset
                                         end = as.character(Sys.Date())) #Ends at todays date by default
                          
                          
                        )
                      ),
                      
                      mainPanel(strong(
                        plotlyOutput('PlotNor',height = '500px'),
                        verbatimTextOutput('text2'),
                        width = 5)
                      ),
                     
                      # mainPanel(
                      #   h3(p(strong('Plot output',style="color:salmon")),align="center"),
                      #   column(plotlyOutput("PlotNor", height = '500px'),width = 12), 
                      # ),
                      
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
                                      textOutput("tsnorway"),
                                      width=9,style="background-color:lightyellow;border-radius: 10px",
                               ),
                               
                               br(),
                      ),
                      
                      
                      br(),
                      br(),
                      
             ),
             tabPanel("Diagnostics", icon=icon("chart-line"),
                      sidebarPanel(
                        helpText("Choose the testdata you want to examine."),
                        selectInput('dataset', 'Data:', c('Global', 'Norway')),  
                        selectInput('PlotTypeDiagnostics', 'Data Visualization:', c('Summary', 'Graph')),
                        
                        
                        conditionalPanel(
                          condition = "input.dataset == 'Global'", 
                          selectInput('countryDiagnostic', 'Country', countries, selected = countries[1]), 
                          selectInput('statDiagnostic', 'Statistics', c("Confirmed","Deaths"), selected = countries[1])
                          
                        ),
                        
                        conditionalPanel(
                          condition = "input.dataset == 'Norway'", 
                          selectInput('Municipality', 'Municipality', kommune, selected = kommune[1])
                          
                        ),
                        
                        dateRangeInput("dates",
                                       "Date range",
                                       start = "2020-01-22", #start date of the dataset
                                       end = as.character(Sys.Date())) #Ends at todays date by default
                        
                        
                      ),
                      
                      mainPanel(
                        h3(p(strong('Diagnostics',style="color:salmon")),align="center"),
                        column(plotlyOutput("Navn p?? testplot"),width = 12,style="border:1px solid black"),
                      ),
                      hr(),
                      
                      fluidRow(column(width=3),
                               column(br(),
                                      p("La inn denne dersom vi skal skrive en forklaring, og for ?? f?? litt luft p?? siden.
                                        G??r ikke an ?? f?? space under grafen uten denne",style="color:black"),
                                      textOutput("noe?"),
                                      width=9,style="background-color:lightyellow;border-radius: 10px",
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



#---------------------------------- Functions ----------------------------------#

##########Graphs#########


graph_dailyConfirmed <- function(df, country){
  
  
  temp_df <- df 
    #filter(country_name == country)
  temp_df$average <- ma(temp_df$daily_cases, 7)
  
  
  #Creating graph  
  graph_cases <- temp_df %>%
    ggplot(aes(x=date)) +
    geom_bar(aes(y=daily_cases, text = paste0("New Cases: ", daily_cases, "\nDate: ", date)), stat ="identity", colour = "#DD8888", fill = "#e74c4c", alpha = 0.4) +
    geom_line(aes(y = average), size = 1.5, colour = "red", alpha = 0.6) +
    labs (title = paste("Daily Confirmed Cases of Covid19 in", country, sep = " "),
          x = "Date\n",
          y = "Confirmed Cases\n") +
    theme_hc() +
    theme(axis.title.x =element_blank(),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title =element_text(hjust = 0.5, face = "bold", size = 16)) +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
  
  
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
  
  ##
  
  ## Turning graph_cases to an interactive graph
  interactive_plot <- ggplotly(graph_cases, tooltip = c("text"), layerData = 1) %>% 
    style(hoverlabel = interactive_label) %>% 
    layout(font = interactive_font,
           yaxis = list(fixedrange = TRUE)) %>% 
    config(displayModeBar = FALSE)
  
  
  
  return(interactive_plot)
}


graph_dailyDeaths <- function(df, country){
  
  
  temp_df <- df 
  temp_df$average <- ma(temp_df$daily_deaths, 7)
  
  
  #Creating graph  
  graph_cases <- temp_df %>%
    ggplot(aes(x=date)) +
    geom_bar(aes(y=daily_deaths, text = paste0("New Cases: ", daily_cases, "\nDate: ", date)), stat ="identity", colour = "#DD8888", fill = "#e74c4c", alpha = 0.4) +
    geom_line(aes(y = average), size = 1.5, colour = "red", alpha = 0.6) +
    labs (title = paste("Daily Confirmed Deaths of Covid19 in", country, sep = " "),
          x = "Date\n",
          y = "Confirmed deaths\n") +
    theme_hc() +
    theme(axis.title.x =element_blank(),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title =element_text(hjust = 0.5, face = "bold", size = 16)) +
    scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
  
  
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
  
  ##
  
  ## Turning graph_cases to an interactive graph
  interactive_plot <- ggplotly(graph_cases, tooltip = c("text"), layerData = 1) %>% 
    style(hoverlabel = interactive_label) %>% 
    layout(font = interactive_font,
           yaxis = list(fixedrange = TRUE)) %>% 
    config(displayModeBar = FALSE)
  
  
  
  
  return(interactive_plot)
}





##########MAP#############

getGlobalConfirmed<- function(df){
  
  map_data <- df %>% 
    filter(date == Sys.Date()-2 ) %>% 
    mutate(casesPer100k = round(confirmed_cases/population*100000,0),)%>% 
    select(ID, date, country_name, ID, casesPer100k) %>% 
    mutate(hover = paste0(country_name, "\n", casesPer100k))
  
  return (map_data)
  
}


getGlobalDeaths<- function(df){
  
  map_data <- df %>% 
    filter(date == Sys.Date()-2) %>% 
    mutate(casesPer100k = round(confirmed_deaths/population*100000,0),)%>% 
    select(ID, date, country_name, ID, casesPer100k) %>% 
    mutate(hover = paste0(country_name, "\n", casesPer100k))
  
  return (map_data)
  
}



drawMap <- function(map_data, main_title, colorbar_title){

  
  map <- plot_ly(map_data, 
                 type='choropleth', 
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
    add_annotations(
      y=1.05, 
      x=0.5, 
      text= main_title, 
      showarrow=F,
      font=list(size=15)
    ) %>% 
    config(displayModeBar = FALSE)
   
  
  
  return(map)
  
}



#####Server######
server <- function(input, output) {
  
  
  ##GLOBAL
  
  data_graph = reactive({
    Crossgovsources_df %>% mutate(date = as.Date(date)) %>% 
      filter( country_name == input$Country, date >= input$dates[1] & date <=input$dates[2] ) 
  })
  
  
  
  output$plot <- renderPlotly({
    if(input$PlotType == 'Graph'){
      data <- data_graph()
      if (input$Stat == 'Deaths'){
        graph_dailyDeaths(data, input$Country)
      }else{ #confirmed
        graph_dailyConfirmed(data, input$Country)
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
  #Bruk renderText istedet hvis det skal v??re if funksjon e.l., for renderPrint kan man feks skrive summary(totaldeaths)
  #Denne linken kan sjekkes ut https://github.com/RiveraDaniel/Regression/blob/master/server.R
  output$death <- renderPrint({p("hundre")}) #legg inn fnavn p?? unksjon for antall d??de
  output$TCC <- renderPrint({p("10")})
  output$lastweek <- renderPrint({p("hundre")}) 
  
  
  ##NORWAY
  
  data_graphNorway = reactive({
    norway %>% mutate(date = as.Date(date)) %>% 
      filter(kommune_name == input$Municipality , date >= input$dates[1] & date <=input$dates[2] )
  })
  
 
  
   output$PlotNor <- renderPlotly({

    if(input$PlotTypeNorway == 'Graph'){
      data <- data_graphNorway()
      #data <- norway %>% filter(kommune_name == "Bergen")
      #graph_dailyConfirmed(data, "Bergen")
      graph_dailyConfirmed(data,input$Municipality)
    }

    })

    
    
  #})
  
  
  # tabPanel("Norway", icon=icon("bar-chart-o"),
  #          sidebarPanel(
  #            helpText("Choose"),
  #            #selectInput('StatNorway', 'Data:', c("Confirmed","Deaths")), #c('Map', 'Graph')), #Select data type 
  #            selectInput('PlotTypeNorway', 'Data Visualization:', c('Top 10', 'Graph')),
  #            
  #            #will only show this panel if the data visualization chosen is "Graph"
  #            #denne funker ikke f??r den er lagt inn i serveren, den m?? hete noe annet enn Graph, ellers responderer den p?? global fanen
  #            conditionalPanel(
  #              condition = "input.PlotTypeNorway == 'Graph'", 
  #              selectInput('Municipality', 'Municipality', kommune, selected = kommune[1]), 
  #              dateRangeInput("dates",
  #                             "Date range",
  #                             start = "2020-01-22", #start date of the dataset
  #                             end = as.character(Sys.Date())) #Ends at todays date by default
  #              
  # 
  # 
  
 
  
  output$deathnor <- renderPrint({p("hundre")}) #legg inn funksjon for antall d??de
  output$TCCnor <- renderPrint({p("10")}) 
  output$lastweeknor <- renderPrint({p("100")}) 
  
  #Output for the diagnostics tab
  #det g??r ikke an ?? ha samme output for begge faner, da funker ikke conditionalpanel
  output$tsnorway <- renderPrint({p("The difference between the data is ... farge her?",
                                    br(), "To examine the full diagnostics of the data, go to the diagnostics tab")}) 
  output$tsglobal <- renderPrint({p("The difference between the data is ... farge her?",
                                    br(), "To examine the full diagnostics of the data, go to the diagnostics tab")}) 
  
}

shinyApp(ui = ui, server = server)


