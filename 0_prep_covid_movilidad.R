
# Dirección: http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/historicos/07/datos_abiertos_covid19_10.10.2020.zip
# COVID

library(dplyr)
library(ggplot2)

################################################################################

setwd('datamexico-covid')

################################################################################

covid <- read.csv('data_mexico_covid_states.csv',stringsAsFactors = F)
mobile <- read.csv('2020_MX_Region_Mobility_Report.csv',stringsAsFactors = F)

covid$fecha <- covid$Time %>% as.Date()
mobile$fecha <- mobile$date %>% as.Date()
covid$semana <- covid$fecha %>% data.table::week()
mobile$semana <- mobile$fecha %>% data.table::week()

################################################################################

mobile %>% 
  filter(sub_region_1=='Aguascalientes') %>% 
  group_by(semana) %>% 
  summarise(mobil=mean(retail_and_recreation_percent_change_from_baseline,na.rm=T)) %>% 
  ggplot(aes(semana,mobil)) +
  geom_line() +
  theme_bw()

mobile %>% 
  filter(sub_region_1=='Aguascalientes') %>% 
  select(semana,retail_and_recreation_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline,
         workplaces_percent_change_from_baseline,
         residential_percent_change_from_baseline) %>% 
  reshape2::melt(id=1) %>%
  rename(tipo_viaje=variable) %>% 
  group_by(semana,tipo_viaje) %>% 
  summarise(mobil=mean(value,na.rm=T)) %>% 
  ggplot(aes(semana,mobil,col=tipo_viaje)) +
  geom_line() +
  theme_bw()

covid %>% 
  filter(State =='aguascalientes-ag') %>% 
  select(semana,Daily.Cases,Daily.Deaths,Daily.Hospitalized,Daily.Suspect) %>% 
  reshape2::melt(id=1) %>%
  rename(tipo_caso=variable) %>% 
  group_by(semana,tipo_caso) %>% 
  summarise(casos=mean(value,na.rm=T)) %>% 
  ggplot(aes(semana,casos,col=tipo_caso)) +
  geom_line() +
  geom_point() +
  theme_bw()
  
covid %>% 
  filter(State =='aguascalientes-ag') %>% 
  group_by(semana) %>% 
  summarise(casos=sum(Daily.Deaths,na.rm=T)) %>% 
  ggplot(aes(semana,casos)) +
  geom_line() +
  theme_bw()  
