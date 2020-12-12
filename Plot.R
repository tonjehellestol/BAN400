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

#Forslag fra Tonje - for ?? fikse litt mer p?? NAs. Usikker p?? hvordan det b??r l??ses
Crossgovsources_df <- covid19() %>% 
  select("id","date","tests","confirmed","deaths","administrative_area_level_1","population") %>% 
  rename_at(vars(c("id","date","confirmed","deaths","administrative_area_level_1","population")), ~ column_names) %>%
  group_by(country_name) %>% 
  do(na.locf2(.)) %>% #replace NA by previous accumulative value
  ungroup()%>%
  replace(is.na(.),0) %>% #replace NAs with not previous values by 0 
  mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths)))


#Adding column "negative_daily_cases" and "negative_daily_deaths", holds the value 1 if daily_cases/daily_deaths are negative, 0 otherwise
Crossgovsources_df <- Crossgovsources_df %>% group_by(country_name) %>% mutate(negative_daily_cases = (ifelse( daily_cases < 0, 1, 0)), negativ_daily_deaths = (ifelse( daily_deaths < 0, 1, 0))) %>% ungroup()

#Correction: changing negative daily_deaths and negative daily_cases to 0
Crossgovsources_df <- Crossgovsources_df %>% 
  mutate( daily_deaths = replace(daily_deaths , daily_deaths < 0, 0), daily_cases = replace(daily_cases , daily_cases < 0, 0))


#Dataset from John Hopkins 
JHD_df_cleaning <- function(df, case_type){
  df %>% 
    group_by(Country.Region) %>% 
    select(-"Province.State", - "Lat", - "Long") %>% 
    mutate_at(vars(-group_cols()), sum) %>% 
    distinct() %>% 
    pivot_longer(cols = colnames(.)[-(1:1)], 
                 names_to = "date",  
                 values_to = case_type)
}

JHD_df_confirmed <- JHD_df_cleaning(covid19.data("TS-confirmed"), "confirmed_cases")

JHD_df_deaths <- JHD_df_cleaning(covid19.data("TS-deaths"), "confirmed_deaths")

JHD_df_full <- JHD_df_confirmed %>% 
  full_join(JHD_df_deaths) %>% 
  rename(country_name = Country.Region) %>% 
  mutate(date=as.Date(date, format = "%Y-%m-%d"), country_name = as.character(country_name)) %>% 
  mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths)))





### Dataset for municipalities in Norway
### Retrieved from https://github.com/thohan88/covid19-nor-data
norwaydata <- read.csv("https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_and_district.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
norwaydata$date <- as.Date(norwaydata$date)


norway <- norwaydata %>% 
  select("date","kommune_name","fylke_name","cases") %>% 
  rename("country_name" = kommune_name) %>% 
  group_by(country_name, date, fylke_name) %>% 
  summarise_at(vars(cases),             
               list(cases = sum)) %>% 
  ungroup() %>% 
  group_by(country_name) %>% 
  mutate(daily_cases = c(0,diff(cases)))




####### Finished retrieving datasets




graph_data <- function(df, country){
  
  
  temp_df <- df %>% 
    filter(country_name == country)
  temp_df$average <- ma(temp_df$daily_cases, 7)

  
  #Creating graph  
  graph_cases <- temp_df %>%
    ggplot(aes(x=date)) +
    geom_bar(aes(y=daily_cases, text = paste0("New Cases: ", daily_cases, "\nDate: ", date)), stat ="identity", colour = "#DD8888", fill = "#e74c4c", alpha = 0.4) +
    geom_line(aes(y = average), size = 1.5, colour = "red", alpha = 0.6) +
    labs (title = "Daily Cases of Covid19 In XXXXXX",
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



a <- graph_data(JHD_df_full,"Sweden")




