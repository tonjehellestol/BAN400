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
library(gganimate)





### Finner top 10 kommuner og plotter graf av deres utvikling i ??r

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




temp_df <- norway %>% 
  filter(date == Sys.Date()-1) %>% 
  arrange(desc(cases)) %>% 
  head(3)

top_5 <- as.vector(temp_df[["country_name"]])


temp_df <- norway %>% 
  filter(country_name %in% top_5)

graph1 = temp_df %>% 
  ggplot(aes(x = date, y = cases, color = country_name)) +
  geom_line( size = 2, alpha = 0.9) +
  theme_hc() +
  labs (title = "Cumulative number of cases in top 5 municipalities in Norway\n",
        y = "Number of cases",
        color = "Location:   ") +
  theme(axis.title.x =element_blank(),
        plot.title =element_text(hjust = 0.5))





