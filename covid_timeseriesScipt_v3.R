#loading packages
library(readr)
library(plyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(lubridate)


#importing table and assigning to raw_table
raw_table <- read_csv("time_series_covid19_confirmed_global_narrow.csv")

#cleaning raw_table to exclude the first row
raw_table <- raw_table[-1,] 


#assigning vector containing Province/State and Country/Region to variable New_Varaible
 new_variable <- ifelse (is.na(raw_table$`Province/State`),
                         paste(raw_table$`Country/Region`),
                         paste(raw_table$`Province/State`,raw_table$`Country/Region`)
 )



#Creating vector named spanish speaking with spanish speaking countries
Spanish_speaking <- c("Dominican Republic", "El Salvador", "Spain", "Venezuela", "Colombia", "Ecuador", "Guatemala", "Honduras", "Panama", "Peru", "Bolivia", "Chile", "Costa Rica", "Cuba", "Haiti", "Mexico", "Uruguay")

raw_table$Date <- ymd(raw_table$Date)
raw_table$Value <- as.numeric(raw_table$Value)

spanish_covid <- raw_table %>%
  filter(`Country/Region` %in% Spanish_speaking)
 
country_pop <- data.frame("Country" = Spanish_speaking, "Population" = c(10847910, 6486205, 46754778, 28435940, 50882891, 17643054, 17915568, 9904607, 4314767, 32971854, 11673021, 19116201, 5094118, 11326616, 11402528, 128932753,3473730))

spanish_covid_n <- spanish_covid %>%
  inner_join(country_pop, by = c("Country/Region" = "Country")) %>%
    mutate(Relative_value = Value / Population * 100)

spanish_covid_n %>%
  ggplot( aes(x = Date, y = Relative_value, color = `Country/Region`)) + 
  geom_line() + labs(x = "Date", y = "Relative Confirmed Cases (cases/population)")




spanish_covid %>%
    ggplot( aes(x = Date, y = Value, group = `Country/Region`, color = `Country/Region`)) +
    geom_line() + labs(x = "Date", y = "Confirmed Cases (cases)")


