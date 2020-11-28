
library(dplyr)
library(ggplot2)

################################################################################

# cuidado con los paths

Def <- foreign::read.dbf('../Desktop/defunciones_base_datos_2019_dbf/DEFUN19.DBF',as.is=T)

lista_mex <- foreign::read.dbf('../Desktop/defunciones_base_datos_2019_dbf/LISTAMEX.dbf',as.is=T)
lista_int <- foreign::read.dbf('../Desktop/defunciones_base_datos_2019_dbf/LISTA1.dbf',as.is=T)

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
