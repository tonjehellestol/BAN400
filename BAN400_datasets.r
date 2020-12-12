
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


na.locf2 <- function(x) na.locf(x, na.rm = FALSE) #replace NA with previous value if exist

Crossgovsources_df <- covid19() %>% 
  select("id","date","tests","confirmed","deaths","administrative_area_level_1","population") %>% 
  rename_at(vars(c("id","date","confirmed","deaths","administrative_area_level_1","population")), ~ column_names) %>%
  filter(country_name != "Costa Atlantica",country_name != "Diamond Princess") %>% 
  group_by(country_name) %>% 
  do(na.locf2(.)) %>% #replace NA by previous cumulative value
  replace(is.na(.),0) %>% #replace NAs with not previous values by 0 
  mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths))) %>% 
  ungroup() 

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


