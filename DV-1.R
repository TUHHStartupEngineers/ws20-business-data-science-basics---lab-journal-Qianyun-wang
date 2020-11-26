library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)


covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

cum_covid_data_tbl <- covid_data_tbl %>%
  select(dateRep, day, month, year, cases, countriesAndTerritories)%>%
  rename("Country"= countriesAndTerritories)%>%
  filter(Country %in% c("France","Germany","Italy","Spain","United_Kingdom","United_States_of_America"))%>%
  filter(year == 2020)%>%
  mutate(date = dmy(dateRep))%>%
  arrange(Country,date)%>%
  group_by(Country)%>%
  mutate(cum_case = cumsum(cases))

cum_covid_data_tbl %>%
  ggplot(aes(x=date ,y=cum_case,color=Country))+
  geom_line() +
  scale_x_date(breaks = date_breaks("months"),
               labels = date_format("%B"))+
  scale_y_continuous(breaks=c(seq(0,13000000,2500000)), 
                     limits = c(0,13000000),
                     labels = scales::dollar_format(0.1,scale = 1e-6, 
                                                    decimal.mark = ",", 
                                                    prefix = "",
                                                    suffix = "M")) +
  labs(
    title = 'CVOID-19 confirmed cases worldwide',
    subtitle = "USA grows much faster after April",
    x="Year 2020",
    y="Cumulative Cases"
  )+
  theme_light()+
  theme(legend.position = "bottom",
        panel.grid=element_line(color = "white"),
        panel.background=element_rect(fill="black"),
        panel.border = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
death_covid_date_tbl <- covid_data_tbl %>%
  select(deaths,popData2019,countriesAndTerritories)%>%
  group_by(countriesAndTerritories)%>%
  summarise(deaths_tol=sum(deaths),
            pop=mean(popData2019))%>%
  ungroup()%>%
  mutate(death_rate = deaths_tol/pop)%>%
  rename('region'=countriesAndTerritories)%>%
  mutate(across(region, str_replace_all, "_", " ")) %>%
  mutate(region = case_when(
    region == "United Kingdom" ~ "UK",
    region == "United States of America" ~ "USA",
    region == "Czechia" ~ "Czech Republic",
    TRUE ~ region))

world <- map_data("world")

world_death_rate <-world %>%
  left_join(death_covid_date_tbl,by="region")


world_death_rate%>%  
  ggplot()+
  geom_map(map=world,
           aes(map_id=region, fill= death_rate))+
  expand_limits(x = world$long, y = world$lat) +
  labs(
    title = 'Confirmed CVOID-19 deaths relative to the size of the population',
    subtitle = "More than 1.4 Million confirmed COVID-19 deaths worldwide",
    caption = "Date: 26/11/2020",
    x = "",
    y = ""
  )+
  scale_fill_distiller(palette = "Reds", direction = 1,
                       labels = scales::dollar_format(0.001,scale = 1e2, 
                                                    decimal.mark = ".", 
                                                    prefix = "",
                                                    suffix = "%"),
                       breaks=c(seq(0,0.0014,0.0003))) +
  theme(panel.grid=element_line(color = "grey"),
        panel.background=element_rect(fill="black"),
        panel.border = element_blank(),
        axis.text = element_blank())
  

