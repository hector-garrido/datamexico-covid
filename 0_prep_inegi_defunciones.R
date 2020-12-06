
library(dplyr)
library(ggplot2)

################################################################################

setwd('datamexico-covid')

################################################################################

# cuidado con los paths

Def <- foreign::read.dbf('defunciones_base_datos_2019_dbf/DEFUN19.DBF',as.is=T)

lista_mex <- foreign::read.dbf('defunciones_base_datos_2019_dbf/LISTAMEX.dbf',as.is=T)
lista_int <- foreign::read.dbf('defunciones_base_datos_2019_dbf/LISTA1.dbf',as.is=T)

################################################################################

lista_mex <- read.csv('lista_mex.csv',stringsAsFactors = F)
lista_mex$aux <- lista_mex$CVE %>% nchar()
lista_mex$CVE[lista_mex$aux==1] <- paste0('0',lista_mex$CVE[lista_mex$aux==1])
lista_mex$aux <- NULL

A$semana <- A$Fecha %>% data.table::week()
A %>% 
  filter(ENT_OCURR=='01') %>% dim
  filter(LISTA_MEX %in% lista_mex$CVE[lista_mex$RELEVANTE==1]) %>% 
#  filter(LISTA_MEX=='33B') %>% 
  group_by(DESCRIP.y) %>% 
  count %>% View
  ggplot(aes(semana,n)) + 
  geom_line() + 
  theme_bw()

################################################################################

Def <- Def %>% 
  select(ENT_OCURR,MUN_OCURR,CAUSA_DEF,LISTA_MEX,DIA_OCURR,MES_OCURR,ANIO_OCUR) %>% 
  left_join(lista_int,by=c('CAUSA_DEF'='CVE')) %>% 
  left_join(lista_mex,by=c('LISTA_MEX'='CVE'))

Def$Fecha <- paste(Def$DIA_OCURR,Def$MES_OCURR,Def$ANIO_OCUR,sep='-') %>% 
  as.Date('%d-%m-%Y')

Def %>% 
  filter(Fecha>'2019-01-01') %>% 
  group_by(Fecha) %>% 
  count %>% 
  ggplot(aes(Fecha,n)) + 
  geom_line() + 
  theme_bw()
