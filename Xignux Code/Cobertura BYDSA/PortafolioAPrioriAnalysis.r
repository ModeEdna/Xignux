setwd('C:\\Downloads\\Portafolio BYDSA')
library(tidyverse)
library(lubridate)
library(stringr)
library(arules)
library(arulesViz)
#library(plyr)
library(tidyr)
library(openxlsx)

######################################

retail = read_csv('Reporte Venta Producto.csv')
head(retail)

retail = retail %>% filter(Sucursal!='NULL')

#############################################
ventaXGrupo = retail %>%
  group_by(Region, Sucursal,Grupo,Canal,MarcaProducto,Submarca)%>%
  summarise(Frecuencia = n()/n_distinct(NombreSemana)) %>%
  arrange(Region, Sucursal,Grupo,Canal,MarcaProducto,-Frecuencia)

#############################################
ventaXSucursal = retail %>%
  group_by(Region, Sucursal,Canal,MarcaProducto,Submarca)%>%
  summarise(Frecuencia = n()/n_distinct(NombreSemana)) %>%
  arrange(Region, Sucursal,Canal,MarcaProducto,-Frecuencia)

#############################################
ventaXRegion = retail %>%
  group_by(Region, Canal,MarcaProducto,Submarca)%>%
  summarise(Frecuencia = n()/n_distinct(NombreSemana)) %>%
  arrange(Region, Canal,MarcaProducto,-Frecuencia)

#############################################
sucursales = unique(retail$Sucursal)
for(suc in sucursales){
  data_sucursal =  retail %>% filter(Sucursal == suc,MarcaProducto=='LEO')

  data_sucursal_gp = data_sucursal %>%
    group_by(NombreSemana, IdCliente) %>%
    summarise(Submarca = paste(unique(Submarca), collapse=';'),
              Num = n()) %>%
    ungroup()

  num = max(data_sucursal_gp$Num)
  lista = data_sucursal_gp %>% select(Submarca) %>%
    separate(Submarca, into=paste('c', 1:num,sep='_'), sep = ';')
  write_csv(lista, paste('transacciones//Leo//',suc,'.csv', sep=''),col_names = FALSE,na = '')
}

#############################################
sucursales = unique(retail$Sucursal)
for(suc in sucursales){
  data_sucursal =  retail %>% filter(Sucursal == suc,MarcaProducto=='ENCANTO')
  
  data_sucursal_gp = data_sucursal %>%
    group_by(NombreSemana, IdCliente) %>%
    summarise(Submarca = paste(unique(Submarca), collapse=';'),
              Num = n()) %>%
    ungroup()
  
  num = max(data_sucursal_gp$Num)
  lista = data_sucursal_gp %>% select(Submarca) %>%
    separate(Submarca, into=paste('c', 1:num,sep='_'), sep = ';')
  write_csv(lista, paste('transacciones//Encanto//',suc,'.csv', sep=''),col_names = FALSE,na = '')
}


#################################################################

columnas = c("sucursal","lhs","rhs","support","confidence","lift","count")
ruledf = data.frame(sucursal=character(0),lhs=character(0),
                    rhs=character(0),support=numeric(0),
                    confidence=numeric(0),lift=numeric(0),
                    count=numeric(0))

#suc = 'Guadalupe'
for(suc in sucursales){
  tr <- read.transactions(paste('transacciones//Leo//',suc,'.csv', sep=''),
                          format = 'basket', sep=',')
  
  #itemFrequencyPlot(tr, topN=10, type='absolute')
  
  
  rules <- apriori(tr, parameter = list(supp=0.25, conf=0.8))
  rules <- sort(rules, by=c('lift','support','confidence'), decreasing = TRUE)
  #summary(rules)
  
  if(length(rules)>0){
    topRules = rules[1:min(length(rules),15)]
    
    #plot(topRules)
    productos = str_split( paste(str_replace_all(as.vector(labels(lhs(topRules))), '[{}]',''),
                                 str_replace_all(as.vector(labels(rhs(topRules))), '[{}]',''),
                                 sep=','), pattern = ',')
    productos_vect = character(length(productos))
    for(k in 1:length(productos)){
      productos_vect[k] = paste(sort(productos[[k]]), collapse=',') 
    }
    
    ruledf = bind_rows(ruledf,
                       data.frame(
                         sucursal = suc,
                         lhs = str_replace_all(as.vector(labels(lhs(topRules))), '[{}]',''),
                         rhs = str_replace_all(as.vector(labels(rhs(topRules))), '[{}]',''),
                         topRules@quality,
                         productos = productos_vect)
    )
    #ruledf
  }

}

write_csv(ruledf, 'Reporte_Reglas_Asociacion_Leo_Sucursal.csv')


#################################################################

columnas = c("sucursal","lhs","rhs","support","confidence","lift","count")
ruledf = data.frame(sucursal=character(0),lhs=character(0),
                    rhs=character(0),support=numeric(0),
                    confidence=numeric(0),lift=numeric(0),
                    count=numeric(0))

#suc = 'Guadalupe'
for(suc in sucursales){
  tr <- read.transactions(paste('transacciones//Encanto//',suc,'.csv', sep=''),
                          format = 'basket', sep=',')
  
  #itemFrequencyPlot(tr, topN=10, type='absolute')
  
  
  rules <- apriori(tr, parameter = list(supp=0.25, conf=0.8))
  rules <- sort(rules,by=c('lift','support','confidence'), decreasing = TRUE)
  #summary(rules)
  
  if(length(rules)>0){
    topRules = rules[1:min(length(rules),15)]
    
    #plot(topRules)
    productos = str_split( paste(str_replace_all(as.vector(labels(lhs(topRules))), '[{}]',''),
                                 str_replace_all(as.vector(labels(rhs(topRules))), '[{}]',''),
                                 sep=','), pattern = ',')
    productos_vect = character(length(productos))
    for(k in 1:length(productos)){
      productos_vect[k] = paste(sort(productos[[k]]), collapse=',') 
    }
    
    ruledf = bind_rows(ruledf,
                       data.frame(
                         sucursal = suc,
                         lhs = str_replace_all(as.vector(labels(lhs(topRules))), '[{}]',''),
                         rhs = str_replace_all(as.vector(labels(rhs(topRules))), '[{}]',''),
                         topRules@quality,
                         productos = productos_vect)
    )
    
  }
}

write_csv(ruledf, 'Reporte_Reglas_Asociacion_Encanto_Sucursal.csv')

#############################################################
write.xlsx(list(ventaXGrupo, ventaXSucursal, ventaXRegion),
           'Frecuencia de compra X Zona.xlsx')
