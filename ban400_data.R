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

Crossgovsources_df <- covid19()



ECDC_df <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

JHU_df <- covid19.data()




### Retrieved from https://github.com/thohan88/covid19-nor-data
norwaydata <- read.csv("https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_and_district.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
norwaydata$date <- as.Date(norwaydata$date)

### Det er noen feil med data, kumulativ antall cases blir mindre i noen kommuner p?? enkelte datoer feks B??rum
### Ved utregning av antall nye daglige tilfeller f??r jeg derfor negative tall, noe som ikke gir mening
### Evt kan dette testes ved ?? se om data[x] > data[y] hvor x > y, evt sette data[x] == data[y] dersom f??rste statement er TRUE


norway <- norwaydata %>% 
  select("date","kommune_name","fylke_name","cases") %>% 
  group_by(kommune_name, date, fylke_name) %>% 
  summarise_at(vars(cases),             
               list(cases = sum)) %>% 
  ungroup() %>% 
  group_by(kommune_name) %>% 
  mutate(daily_cases = c(0,diff(cases)))





###### start of function 



selected_input <- norway %>% 
  filter(kommune_name == "Oslo") %>% 
  ungroup()



selected_input$average <- ma(selected_input$daily_cases, 7) ## calculating moving averages over past week for each datapoint


# Creating graph
graph_cases = selected_input %>%
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
ggplotly(graph_cases, tooltip = c("text")) %>% 
  style(hoverlabel = interactive_label) %>% 
  layout(font = interactive_font,
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)







