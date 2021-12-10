#GUILLERMO GARCIA
#24_11_21


pacman::p_load(httr,tidyverse,jsonlite,xml2,janitor,leaflet,leaflet.extras,pacman)




#A -----------------------------------------------------------------------####



##1 -----------------------------------------------------------------------####

url <- 'https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/'

#cargo url
GET(url)

#guardo info de la url
df_gas <- url %>% 
  fromJSON() %>% 
  .$ListaEESSPrecio %>% 
  tibble()  

#limpio ds 
df_gas <- df_gas %>% 
  clean_names() %>% 
  type_convert(locale = locale(decimal_mark = ',')) %>% 
  rename(longitud=longitud_wgs84)

##2 -----------------------------------------------------------------------####



##3 -----------------------------------------------------------------------####

#creo una variable que contiene los precio y el idccaa del df
precio_combustible <- df_gas[9:22] %>% 
  mutate(df_gas$idccaa) %>% 
  rename(idccaa=`df_gas$idccaa`) 
  

#agrupo por idccaa y calculo la media de precios
precio_combustible <- precio_combustible %>% 
  group_by(idccaa) %>% 
  summarise_all(mean,na.rm=TRUE)
  



#creo una variable con los nuevos nombre
nuevos_nombre <- c('media_precio_biodiesel','media_precio_bioetanol', 'media_precio_gas_natural_comprimido',
                   'media_precio_gas_natural_licuado','media_precio_gases_licuados_del_petroleo', 'media_precio_gasoleo_a',
                   'media_precio_gasoleo_b','media_precio_gasoleo_premium','media_precio_gasolina_95_e10', 'media_precio_gasolina_95_e5', 
                   'media_precio_gasolina_95_e5_premium', 'media_precio_gasolina_98_e10', 'media_precio_gasolina_98_e5', 'media_precio_hidrogeno' )


#cambio los nombre antiguos por los nuevos
for(i in 2:16){
  precio_combustible <- precio_combustible %>% 
    rename_at(i, function(x) nuevos_nombre[i-1])
}


#uno el df original con el que trabajado los precios
df_gas <- merge(x = df_gas, y = precio_combustible, by = c("idccaa", "idccaa")) %>% 
  as_tibble()
 
  





nuevos_nombre <- c('valoracion_precio_biodiesel','valoracion_precio_bioetanol', 'valoracion_precio_gas_natural_comprimido',
                   'valoracion_precio_gas_natural_licuado','valoracion_precio_gases_licuados_del_petroleo', 'valoracion_precio_gasoleo_a',
                   'valoracion_precio_gasoleo_b','valoracion_precio_gasoleo_premium','valoracion_precio_gasolina_95_e10', 'valoracion_precio_gasolina_95_e5', 
                   'valoracion_precio_gasolina_95_e5_premium', 'valoracion_precio_gasolina_98_e10', 'valoracion_precio_gasolina_98_e5', 'valoracion_precio_hidrogeno' )



#true ofalse si es mas caro o no que la media
df_gas<-df_gas %>% mutate(valoracion_precio_biodiesel= (df_gas$precio_biodiesel>df_gas$media_precio_biodiesel))
df_gas<-df_gas %>% mutate(valoracion_precio_bioetanol= (df_gas$precio_bioetanol>df_gas$media_precio_bioetanol))
df_gas<-df_gas %>% mutate(valoracion_precio_gas_natural_comprimido= (df_gas$precio_gas_natural_comprimido>=df_gas$media_precio_gas_natural_comprimido))
df_gas<-df_gas %>% mutate(valoracion_precio_gas_natural_licuado= (df_gas$precio_gasoleo_b>df_gas$media_precio_gasoleo_b))
df_gas<-df_gas %>% mutate(valoracion_precio_gases_licuados_del_petroleo= (df_gas$precio_gases_licuados_del_petroleo>=df_gas$media_precio_gases_licuados_del_petroleo))
df_gas<-df_gas %>% mutate(valoracion_precio_gasoleo_a= (df_gas$precio_gasoleo_a>df_gas$media_precio_gasoleo_a))
df_gas<-df_gas %>% mutate(valoracion_precio_gasoleo_b= (df_gas$precio_gasoleo_b>df_gas$media_precio_gasoleo_b))
df_gas<-df_gas %>% mutate(valoracion_precio_gasoleo_premium= (df_gas$precio_gasoleo_premium>df_gas$media_precio_gasoleo_premium))
df_gas<-df_gas %>% mutate(valoracion_precio_gasolina_95_e10= (df_gas$precio_gasolina_95_e10>df_gas$media_precio_gasolina_95_e10))
df_gas<-df_gas %>% mutate(valoracion_precio_gasolina_95_e5= (df_gas$precio_gasolina_95_e5>df_gas$media_precio_gasolina_95_e5))
df_gas<-df_gas %>% mutate(valoracion_precio_gasolina_95_e5_premium= (df_gas$precio_gasolina_95_e5_premium>=df_gas$media_precio_gasolina_95_e5_premium))
df_gas<-df_gas %>% mutate(valoracion_precio_gasolina_98_e10= (df_gas$precio_gasolina_98_e10>df_gas$media_precio_gasolina_98_e10))
df_gas<-df_gas %>% mutate(valoracion_precio_gasolina_98_e5= (df_gas$precio_gasolina_98_e5>df_gas$media_precio_gasolina_98_e5))
df_gas<-df_gas %>% mutate(valoracion_precio_hidrogeno= (df_gas$precio_hidrogeno>df_gas$media_precio_hidrogeno))






for(i in 0:13){
  df_gas[47+i] = ifelse(df_gas[47+i] == 'TRUE','no_low_cost','low_cost')
}


##4 -----------------------------------------------------------------------####


write_excel_csv(df_gas,'low-cost_21937978.xlsx')   
#write_rds(df_gas,'2_low-cost_21937978.xlsx')




#B -----------------------------------------------------------------------####

##1 -----------------------------------------------------------------------####

ccaa_nombre=c('ANDALUCIA','ARAGON','PRINCIPADO DE ASTURIAS', 'ILLES BALEARS', 'CANARIAS', 'CANTABRIA', 'CASTILLA Y LEON',
                   'CASTILLA-LA MANCHA', 'CATALU�A', 'COMUNITAT VALENCIANA', 'EXTREMADURA','GALICIA', 'COMUNIDAD DE MADRID', 
                   'REGION DE MURCIA', 'COMUNIDAD FORAL DE NAVARRA', 'PAIS VASCO', 'LA RIOJA', 'CEUTA', 'MELILLA')



df_gas <-df_gas%>%
  mutate(ccaa=case_when(idccaa=='01'~ccaa_nombre[1], idccaa=='02'~ccaa_nombre[2], idccaa=='03'~ccaa_nombre[3], idccaa=='04'~ccaa_nombre[4], 
                        idccaa=='05'~ccaa_nombre[5], idccaa=='06'~ccaa_nombre[6], idccaa=='07'~ccaa_nombre[7], idccaa=='08'~ccaa_nombre[8], 
                        idccaa=='09'~ccaa_nombre[9], idccaa=='10'~ccaa_nombre[10], idccaa=='11'~ccaa_nombre[11], idccaa=='12'~ccaa_nombre[12], 
                        idccaa=='13'~ccaa_nombre[13], idccaa=='14'~ccaa_nombre[14], idccaa=='15'~ccaa_nombre[15], idccaa=='16'~ccaa_nombre[16], 
                        idccaa=='17'~ccaa_nombre[17], idccaa=='18'~ccaa_nombre[18], idccaa=='19'~ccaa_nombre[19]))  




precio_madrid_andalucia_1 <- df_gas %>% 
  filter(ccaa  %in% c('COMUNIDAD DE MADRID', 'ANDALUCIA'))  %>% 
  group_by(ccaa, valoracion_precio=.$valoracion_precio_gasoleo_a) %>%
  summarise(cantidad_gas=n()) 
 
  
    
   
##2 -----------------------------------------------------------------------#### 
   
   
precio_madrid_andalucia_2 <- df_gas %>% 
  filter(ccaa  %in% c('COMUNIDAD DE MADRID', 'ANDALUCIA'))  %>%
  group_by(ccaa) %>% 
  summarise(min_precio_gasoleo_a=min(.$precio_gasoleo_a, na.rm=TRUE),
            media_precio_gasoleo_a=mean(.$precio_gasoleo_a, na.rm=TRUE),
            max_precio_gasoleo_a=max(.$precio_gasoleo_a, na.rm=TRUE),
            min_precio_gasoleo_a=min(.$precio_gasolina_95_e5_premium, na.rm=TRUE),
            media_precio_gasolina_95_e5_premium=mean(.$precio_gasolina_95_e5_premium, na.rm=TRUE),
            max_precio_gasoleo_a=max(.$precio_gasolina_95_e5_premium, na.rm=TRUE))



##3 -----------------------------------------------------------------------####   
  



precio_madrid_andalucia <-merge(x=precio_madrid_andalucia_1, 
                                y=precio_madrid_andalucia_2, 
                                by = c("ccaa", "ccaa")) %>% 
  as_tibble()

 
 
write_excel_csv(precio_madrid_andalucia,'informe_CAM_21937978.xlsx')   
#write_rds(precio_madrid_andalucia,'2_informe_CAM_21937978.xlsx')
  
 
#C -----------------------------------------------------------------------#### 
 
##1 -----------------------------------------------------------------------#### 
 




`%notin%` <- Negate(`%in%`)

df_gas %>%
  filter(valoracion_precio_biodiesel == "low_cost" | valoracion_precio_bioetanol == "low_cost"
         | valoracion_precio_gas_natural_comprimido == "low_cost" | valoracion_precio_gas_natural_licuado == "low_cost"
         | valoracion_precio_gases_licuados_del_petroleo == "low_cost" | valoracion_precio_gasoleo_a == "low_cost"
         | valoracion_precio_gasoleo_b == "low_cost" | valoracion_precio_gasoleo_a == "low_cost"
         | valoracion_precio_gasolina_95_e10 == "low_cost" | valoracion_precio_gasolina_95_e5 == "low_cost"
         | valoracion_precio_gasolina_95_e5_premium == "low_cost" | valoracion_precio_gasolina_98_e10 == "low_cost"
         | valoracion_precio_gasolina_98_e5 == "low_cost"| valoracion_precio_hidrogeno == "low_cost") %>%
  group_by(provincia) %>%
  filter(provincia  %in% c( 'MADRID')) 
  
  
 

df_gas %>%
  filter(valoracion_precio_biodiesel == "low_cost" | valoracion_precio_bioetanol == "low_cost"
         | valoracion_precio_gas_natural_comprimido == "low_cost" | valoracion_precio_gas_natural_licuado == "low_cost"
         | valoracion_precio_gases_licuados_del_petroleo == "low_cost" | valoracion_precio_gasoleo_a == "low_cost"
         | valoracion_precio_gasoleo_b == "low_cost" | valoracion_precio_gasoleo_a == "low_cost"
         | valoracion_precio_gasolina_95_e10 == "low_cost" | valoracion_precio_gasolina_95_e5 == "low_cost"
         | valoracion_precio_gasolina_95_e5_premium == "low_cost" | valoracion_precio_gasolina_98_e10 == "low_cost"
         | valoracion_precio_gasolina_98_e5 == "low_cost"| valoracion_precio_hidrogeno == "low_cost") %>%
  filter(provincia  %in% c( 'MADRID', 'BARCELONA')) %>% 
  nrow() 





#D-----------------------------------------------------------------------#### 

##1-----------------------------------------------------------------------#### 

#gasolineras abiertas 24h
si_24_horas <- df_gas %>%
  filter(.$horario %in% 'L-D: 24H') 


#ds sin columna horario
no_24_horas <- df_gas %>% 
  select(-horario)
  

##2-----------------------------------------------------------------------#### 

write_csv(no_24_horas,'no_24_horas_21937978.xlsx')
#write_rds(no_24_horas,'2_no_24_horas.xlsx')



#E-----------------------------------------------------------------------####

##1-----------------------------------------------------------------------####

poblacion_municipios <- read.csv('21937978_poblacion_municipios_2020.csv')

poblacion_municipios <- poblacion_municipios %>%
  as_tibble() %>%
  clean_names() %>%
  rename(municipio=nombre) 
  



df_gas <- merge(x = df_gas, y = poblacion_municipios, by = c("municipio", "municipio")) %>% 
  as_tibble()


#write_rds(df_gas,'punto_guardado_1')

##2-----------------------------------------------------------------------#### 




TopTen  <- df_gas %>% 
  filter(provincia %notin% c('PALMAS (LAS)', 
                             'SANTA CRUZ DE TENERIFE', 
                             'BALEARS (ILLES)'), 
         horario %in% 'L-D: 24H') %>% 
  select(municipio, starts_with('valoracion')) 
  
  

















