######## Retrieving datasets

library(plotly)
library(dplyr)
library(lubridate)
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

#Column names wanted for the data
column_names = c("ID", "date", "confirmed_cases", "confirmed_deaths", "country_name", "population")




#Dataset from covid19 package
#Petter sin versjon
# Crossgovsources_df2 <- covid19() %>%
#   select("id","date","tests","confirmed","deaths","administrative_area_level_1","population") %>%
#   rename_at(vars(c("id","date","confirmed","deaths","administrative_area_level_1","population")), ~ column_names) %>%
#   replace(is.na(.),0) %>%
#   mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths)))


#Forslag fra Tonje - for ?? fikse litt mer p?? NAs. Usikker p?? hvordan det b??r l??ses

na.locf2 <- function(x) na.locf(x, na.rm = FALSE)

Crossgovsources_df <- covid19() %>% 
  select("id","date","tests","confirmed","deaths","administrative_area_level_1","population") %>% 
  rename_at(vars(c("id","date","confirmed","deaths","administrative_area_level_1","population")), ~ column_names) %>%
  group_by(country_name) %>% 
  do(na.locf2(.)) %>% #replace NA by previous accumulative value
  ungroup()%>%
  replace(is.na(.),0) %>% #replace NAs with not previous values by 0 
  mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths))) 






# 
# map_data <- Crossgovsources_df %>% 
#   mutate(last_date_in_month = ceiling_date(ymd(date), 'month') -1)%>% 
#   #filter(date == Sys.Date()-1 | date == last_date_in_month) %>% 
#   #filter((date == Sys.Date()-1 | date == last_date_in_month), country_name != "Costa Atlantica",country_name != "Diamond Princess" ) %>% 
#   filter(date == Sys.Date()-1 , country_name != "Costa Atlantica",country_name != "Diamond Princess" ) %>% 
#   mutate(casesPer100k = round(confirmed_cases/population*100000,2),)%>% 
#   select(ID, date, country_name, ID, casesPer100k) %>% 
#   mutate(hover = paste0(country_name, "\n", casesPer100k))
# 
# map <- plot_ly(map_data, 
#                type='choropleth', 
#                #frame = map_data$date,
#                locations=map_data$ID,
#                z=map_data$casesPer100k, 
#                zmin=0,
#                zmax = max(map_data$casesPer100k),
#                colorscale = list(c(0, 0.1, 0.2,0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
#                                  c(brewer.pal(11,'Spectral'))),
#                color = map_data$casesPer100k,
#                text = map_data$hover,
#                hoverinfo = 'text') %>% 
#   colorbar(title = 'Confirmed cases', len = 0.75) %>% 
#   layout(title = "Confirmed cases per 100.000")
# 
# map
# 
# 
# 


  
getGlobalConfirmed<- function(df){
  
  map_data <- df %>% 
    mutate(last_date_in_month = ceiling_date(ymd(date), 'month') -1)%>% 
    #filter(date == Sys.Date()-1 | date == last_date_in_month) %>% 
    #filter((date == Sys.Date()-1 | date == last_date_in_month), country_name != "Costa Atlantica",country_name != "Diamond Princess" ) %>% 
    filter(date == Sys.Date()-1 , country_name != "Costa Atlantica",country_name != "Diamond Princess" ) %>% 
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
    filter(date == Sys.Date()-1 , country_name != "Costa Atlantica",country_name != "Diamond Princess" ) %>% 
    mutate(casesPer100k = round(confirmed_deaths/population*100000,2),)%>% 
    select(ID, date, country_name, ID, casesPer100k) %>% 
    mutate(hover = paste0(country_name, "\n", casesPer100k))
  
  return (map_data)
  
}




  

drawMap <- function(map_data, main_title, colorbar_title){
  
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
  
  
    return(map)
  
}


##If stat == confirmed
Crossgovsources_df %>% getGlobalConfirmed() %>% 
drawMap( "Confirmed cases per 100.000", "Confirmed cases")

##If stat == deaths
Crossgovsources_df %>% getGlobalDeaths() %>% 
  drawMap( "Number of deaths per 100.000", "Number of deaths")





              # frame= map_data$date)

