
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


#Forslag fra Tonje - for ?? fikse litt mer p?? NAs. Usikker p?? hvordan det b??r l??ses
Crossgovsources_df <- covid19() %>% 
  select("id","date","tests","confirmed","deaths","administrative_area_level_1","population") %>% 
  rename_at(vars(c("id","date","confirmed","deaths","administrative_area_level_1","population")), ~ column_names) %>%
  group_by(country_name) %>% 
  do(na.locf2(.)) %>% #replace NA by previous accumulative value
  ungroup()%>%
  replace(is.na(.),0) %>% #replace NAs with not previous values by 0 
  mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths)))




#Datasettet ECDC blir ikke oppdatert regelmessig, g?r fullstendig over til daglige oppdateringer 14 des, denne b?r dermed ikke brukes


#dataset from European central for disease control
ECDC_df <-  read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>% 
  select("countryterritoryCode","dateRep","cases","deaths","countriesAndTerritories","popData2019") %>% 
  rename_at(vars(c("countryterritoryCode","dateRep","cases","deaths","countriesAndTerritories","popData2019")), ~column_names) 

ECDC_df$ID <- as.character(ECDC_df$ID)
ECDC_df$date <- as.Date(ECDC_df$date, tryFormats = "%d/%m/%Y")
ECDC_df$country_name <- as.character(ECDC_df$country_name)

ECDC_df <- ECDC_df %>% 
  replace(is.na(.),0) %>%  
  mutate(daily_cases = c(0,diff(confirmed_cases)), daily_deaths = c(0,diff(confirmed_deaths)))



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


####### Finished retrieving datasets