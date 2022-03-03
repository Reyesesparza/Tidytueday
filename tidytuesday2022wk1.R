library(httr)
library(jsonlite)
library(rjson)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(grid)



####### Obtener Datos #################################################################################################################
##Token de Inegi ##
token_inegi <- "token de inegi"

##indicadores de tasas de ocupacion por actividad economica ##

n_indicadore <- seq(from = 668906, to = 668918, by = 1)


valor <- "a"
descripcion <- "b"
indicadores <- data.frame()

## COnsulta a Api Inegi informacion Indicadores ##
for(i in seq_along(n_indicadore)){
  
  url_base <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/CL_INDICATOR/"
  indicador <- n_indicadore[i]
  url_cont <- "/es/BIE/2.0/"
  url_fin <- "?type=json"
  
    url <- paste0(url_base, indicador, url_cont, token_inegi, url_fin)

      respuesta<-GET(url)
      datosGenerales<-content(respuesta,"text", encoding = "UTF-8")
      flujoDatos<-paste(datosGenerales,collapse = " ")
      flujoDatos<-fromJSON(flujoDatos)
      series <- flujoDatos$CODE


        valor[i] <-  series[[1]]$value
        descripcion[i]<- series[[1]]$Description

df_i <- data.frame(valor = valor, descripcion = descripcion)

indicadores <- rbind(df_i)


}



ind <- "a"
periodo <- "b"
valor <- "c"
tasa_empleo <- data.frame()

### Informacion sobre poblacion ocupada por industria ##



for(i in seq_along(n_indicadore)){
  


  url_base <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/"
  indicador <- n_indicadore[i]
  url_cont <- "/es/0700/false/BIE/2.0/"
  url_fin <- "?type=json"

    url <- paste0(url_base, indicador, url_cont, token_inegi, url_fin)
      
    respuesta<-GET(url)
    datosGenerales<-content(respuesta,"text", encoding = "UTF-8")
    flujoDatos<-paste(datosGenerales,collapse = " ")
    flujoDatos<-fromJSON(flujoDatos)
    series <- flujoDatos$Series
    obs <- series[[1]]
    
      
      
      for( j in seq_along(obs$OBSERVATIONS)){
        
        periodo[j] <- obs$OBSERVATIONS[[j]]$TIME_PERIOD
        valor[j]   <- obs$OBSERVATIONS[[j]]$OBS_VALUE
        ind[j] <- obs$INDICADOR

      }    
      
        df <- data.frame(ind = ind, periodo = periodo, valor = valor)
        tasa_empleo <- rbind(tasa_empleo, df)
}











## Poblacion Total ###

  url_base <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/"
  indicador <- 289250
  url_cont <- "/es/0700/false/BIE/2.0/"
  url_fin <- "?type=json"

    url <- paste0(url_base, indicador, url_cont, token_inegi, url_fin)

      respuesta<-GET(url)
      datosGenerales<-content(respuesta,"text", encoding = "UTF-8")
      flujoDatos<-paste(datosGenerales,collapse = " ")
      flujoDatos<-fromJSON(flujoDatos)
      series <- flujoDatos$Series
      obs <- series[[1]]
        
        periodo <- "a"
        poblacion <- "b"
        n_pop <- data.frame()
        
        for( i in seq_along(obs$OBSERVATIONS)){
          periodo[i] <- obs$OBSERVATIONS[[i]]$TIME_PERIOD
          poblacion[i] <- obs$OBSERVATIONS[[i]]$OBS_VALUE
          
          df <- data.frame(periodo = periodo, poblacion = poblacion)
          
          n_pop <- rbind(df)
          
        }

#######################################################################################################################################
        
######### Trabajar datos #############################################################################################################
      
     
primarias <- c("Primario (agricultura, ganadería, silvicultura, caza y pesca)")
secundarias <- c("Industria extractiva y de la electricidad", "Industria manufacturera","Construcción")        
tercearias <- c("Comercio", "Restaurantes y servicios de alojamiento", "Transportes, comunicaciones, correo y almacenamiento",
                "Servicios profesionales, financieros y corporativos", "Servicios sociales", "Servicios diversos")        
        
        
        tasas <- tasa_empleo %>% 
          left_join( df_i, by = c("ind" = "valor")) %>%
          mutate(anio_mes = ym(periodo),
                 trim = quarter(anio_mes),
                 qtr = paste0(substring(year(anio_mes),1,4),"/0",quarter(anio_mes)),
                 ani_trim = ym(paste0(year(anio_mes),"/", quarter(anio_mes))),
                 mes = month(anio_mes),
                 valor = as.numeric(valor),
                 tipo = if_else( ind == "668907", "primarias",
                                 if_else( ind == "668908", "secundarias",
                                          if_else( ind == "668912", "tercearias", 
                                                   if_else( ind == "668906","total", "sin agregacion")))))
        
        
        
meses_considerar <- c(3,6,9,12)
        
poblacion_total <- tasas %>%
  filter(tipo == "total") %>%
  left_join( n_pop, by= c("qtr" = "periodo")) %>%
  filter(mes %in% meses_considerar)
        

poblacion_primario <- tasas %>%
  filter( tipo == "primarias") %>%
  filter( mes %in% meses_considerar) %>%
  left_join( n_pop, by= c( "qtr" = "periodo")) %>%
  mutate( popr_pob = as.numeric(valor)/100,
          poblacion = as.numeric(poblacion),
          poblacion_sector = popr_pob * poblacion) %>%
  select(qtr, poblacion_sector)




        
poblacion_secundario <- tasas %>%
  filter( tipo == "secundarias") %>%
  filter( mes %in% meses_considerar) %>%
  left_join( n_pop, by= c( "qtr" = "periodo")) %>%
  mutate( popr_pob = as.numeric(valor)/100,
          poblacion = as.numeric(poblacion),
          poblacion_sector = popr_pob * poblacion) %>%
  select(qtr, poblacion_sector)




poblacion_terceareas <- tasas %>%
  filter( tipo == "tercearias") %>%
  filter( mes %in% meses_considerar) %>%
  left_join( n_pop, by= c( "qtr" = "periodo")) %>%
  mutate( popr_pob = as.numeric(valor)/100,
          poblacion = as.numeric(poblacion),
          poblacion_sector = popr_pob * poblacion) %>%
  select(qtr, poblacion_sector)
        
 
       
        poblacion_ins_secundarias <- tasas %>%
          filter( descripcion %in% secundarias) %>%
          filter( mes %in% meses_considerar) %>%
          left_join(poblacion_secundario, by = "qtr") %>%
          mutate( prop_pob = as.numeric(valor) / 100,
                  poblacion_sec_ind = prop_pob * poblacion_sector,
                  pob_ind_sec_mil = round(poblacion_sec_ind / 1000,2),
                  sector = "secundario")
        
        
        
        
        poblacion_ins_terceareas <- tasas %>%
          filter( descripcion %in% tercearias) %>%
          filter( mes %in% meses_considerar) %>%
          left_join(poblacion_terceareas, by = "qtr") %>%
          mutate( prop_pob = as.numeric(valor) / 100,
                  poblacion_sec_ind = prop_pob * poblacion_sector,
                  pob_ind_sec_mil = round(poblacion_sec_ind / 1000,2),
                  sector = "terceario")
        
        
        poblacion_ins_primarias <- tasas %>%
          filter( descripcion %in% primarias) %>%
          filter( mes %in% meses_considerar) %>%
          left_join(poblacion_primario, by = "qtr") %>%
          mutate( prop_pob = as.numeric(valor) / 100,
                  poblacion_sec_ind = prop_pob * poblacion_sector,
                  pob_ind_sec_mil = round(poblacion_sec_ind / 1000,2),
                  sector = "primario")
        
        
        

poblacion_ocupada <- rbind(poblacion_ins_primarias, poblacion_ins_secundarias, poblacion_ins_terceareas)
        

crecimiento <- poblacion_ocupada %>%
  filter( qtr == "2019/04" | qtr == "2021/04") %>%
  select(qtr, descripcion, poblacion_sec_ind) %>%
  arrange( qtr) %>%
  pivot_wider(names_from = qtr, values_from = poblacion_sec_ind) %>%
  mutate( crec = round(`2021/04` / `2019/04` - 1,5),
          prop_pob = round(`2021/04` / sum(`2021/04`),2)) %>%
  arrange(desc(`2021/04`))

        
        
        
        
      
        
        
        
        

## Tema graficos ##


tema_1 <- function(base_size = 10, base_family = "Raleway"){
  
  color_background = "#FFFFFF"
  color_grid_major = "#EBEBEB" 
  color_axis_text = "#837F7F"  
  color_axis_title = "#837F7F" 
  color_title = "#575757"
  color_subtitle = "#575757"
  strip_background_color = "#575757"
  
  
  aqui <- theme_minimal(base_size = base_size) + 
    theme( panel.background = element_rect(fill = color_background, color = color_background))+
    theme(plot.background = element_rect(fill = color_background, color = color_background)) +
    theme( panel.grid.major = element_line(color = color_grid_major)) +
    theme( panel.grid.minor = element_line(color = color_grid_major)) +
    theme( axis.ticks = element_blank())+
    theme(plot.title = element_text(  color = color_title, family = base_family, size = rel(1.6), vjust = 1.5)) +
    theme(plot.subtitle = element_text( color = color_subtitle, family = base_family, size = base_size + 2, hjust = 0)) +
    theme(plot.caption = element_text( color = color_subtitle, family = base_family, size = base_size)) +
    theme(axis.text.x = element_text( color = color_axis_text, family = base_family, size = base_size)) +
    theme(axis.text.y = element_text( color = color_axis_text, family = base_family, size = base_size)) +
    theme(axis.text = element_text( color = color_axis_text, family = base_family, size = base_size)) +
    theme(axis.title.x = element_text(color = color_axis_title, family = base_family, size = base_size + 1.2, vjust = 1.25)) +
    theme(axis.title.y = element_text(color = color_axis_title, family = base_family, size = base_size + 1.2, vjust = 1.25)) +
    theme( axis.title = element_text(color = color_axis_title, family = base_family, size = base_size + 1.2))
  
  
  
  aqui
  
}




##Graficos ##

# Poblacion Ocupada ##

n <- seq(from = 1, to = length(n_pop$periodo), by = 1)

n_pop <- n_pop %>%
  mutate( n = n,
          et = str_sub(periodo, start = 6, end = 7),
          label = if_else(et == "01", periodo, "" ),
          n_pop = as.numeric(poblacion),
          año = str_sub(periodo, start = 1, end = 4),
          label = if_else(año == "2020"| año == "2021", periodo, label)) %>%
  arrange(periodo)




pandemia <- c("2020/01", "2020/02", "2020/03")

df_pob_pand <- n_pop %>%
  filter(periodo %in% pandemia) %>%
  mutate( po_1 =  poblacion[.$periodo == "2020/03"],
          poblacion = if_else( periodo == "2020/02", po_1, poblacion))



n_pop$poblacion[ n_pop$periodo== "2020/02"] <- df_pob_pand$poblacion[df_pob_pand$periodo == "2020/02" ] 

periodo_pande_label <- c("2019/04","2021/04")

g1 <- ggplot(data = n_pop, aes( x = periodo, y = poblacion, group = 1))+
  geom_point(color = "#837F7F", na.rm = TRUE)+
  geom_line(color = "#837F7F", na.rm = TRUE)+
  geom_point( data = df_pob_pand, aes( x = periodo, y = poblacion), color = "#C19C06")+
  geom_line(data = df_pob_pand, aes( x = periodo, y = poblacion), color = "#C19C06")+
  tema_1()+
  annotate("rect", xmin ="2019/04", xmax ="2021/02", ymax = max(poblacion), ymin = 0 , color = "#F6F6F6", alpha =1/7 )+
  annotate("text", x = "2019/04", y = min(poblacion), hjust=.1, vjust =-1, size = 3.5, color = "#575757",label ="Periodo de restriccion")+
  annotate("text", x = "2020/02", y = df_pob_pand$poblacion[df_pob_pand$periodo=="2020/02"], label = " 2020/02 Dato no disponible", vjust =-1.8, size = 3.5, color = "#434343" )+
  theme( axis.text.y = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5))+
  scale_x_discrete(labels = n_pop$label)+
  geom_text(data = n_pop[n_pop$periodo %in% periodo_pande_label,], aes(x = periodo, y = poblacion, label = paste0(round(as.numeric(poblacion) /1000000,1)," M")), color = "#434343", size = 3, vjust = 1.2, hjust= .5)+
  labs(title = ""
    ,subtitle = "El total de la poblacion ocupada en la economia esta en su maximo historico\nrecuperandose del impacto de las medidas restrictivas implementadas a inicios de la pandemia", x = "trimestre")
  
g1





### Manufacturas ###
manufacturas <- poblacion_ocupada %>%
  filter( descripcion == "Industria manufacturera")


man_pandemia <- manufacturas %>%
  filter( qtr %in% pandemia) %>%
  mutate( po_1 =  poblacion_sec_ind[.$qtr == "2020/03"],
          poblacion_sec_ind = if_else( qtr == "2020/02", po_1, poblacion_sec_ind))


manufacturas$poblacion_sec_ind[  manufacturas$qtr== "2020/02"] <- man_pandemia$poblacion_sec_ind [man_pandemia$qtr == "2020/02" ] 


## Restaurantes y servicios Alojamiento ##

restaurants <- poblacion_ocupada %>%
  filter( descripcion == "Restaurantes y servicios de alojamiento")


rest_pandemia <- restaurants %>%
  filter( qtr %in% pandemia) %>%
  mutate( po_1 =  poblacion_sec_ind[.$qtr == "2020/03"],
          poblacion_sec_ind = if_else( qtr == "2020/02", po_1, poblacion_sec_ind))


restaurants$poblacion_sec_ind[  restaurants$qtr== "2020/02"] <- rest_pandemia$poblacion_sec_ind [rest_pandemia$qtr == "2020/02" ] 


## Comercio ####

comercio <- poblacion_ocupada %>%
  filter( descripcion == "Comercio")


com_pandemia <- comercio %>%
  filter( qtr %in% pandemia) %>%
  mutate( po_1 =  poblacion_sec_ind[.$qtr == "2020/03"],
          poblacion_sec_ind = if_else( qtr == "2020/02", po_1, poblacion_sec_ind))


comercio$poblacion_sec_ind[  comercio$qtr== "2020/02"] <- com_pandemia$poblacion_sec_ind [com_pandemia$qtr == "2020/02" ] 





g2 <-ggplot(data = manufacturas, aes( x = qtr, y = poblacion_sec_ind, group = 1))+
  geom_point(color = "#837F7F", na.rm = TRUE)+
  geom_line(color = "#837F7F", na.rm = TRUE)+
  geom_point(data = man_pandemia, aes(x = qtr, y = poblacion_sec_ind),color = "#C19C06" )+
  geom_line(data = man_pandemia, aes(x = qtr, y = poblacion_sec_ind),color = "#C19C06")+
  tema_1()+
  annotate("rect", xmin ="2019/04", xmax ="2021/02", ymax = max(manufacturas$poblacion_sec_ind) , ymin = min(manufacturas$poblacion_sec_ind), color = "#F6F6F6", alpha =1/7 )+
  theme( axis.text.y = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_blank())+
  geom_text(data = manufacturas[manufacturas$qtr %in% periodo_pande_label,], aes( x = qtr, y = poblacion_sec_ind, label = paste0(round(poblacion_sec_ind/1000000,1)," M")), color = "#434343", size = 3, vjust = 1.1, hjust= 1)+
  labs(subtitle = "Comportamiento de la poblacion ocupada en la Industria manufacturera",
       y = "",
       x = "")
  g2
  
  
g3 <-ggplot(data = comercio, aes( x = qtr, y = poblacion_sec_ind, group = 1))+
  geom_point(color = "#837F7F", na.rm = TRUE)+
  geom_line(color = "#837F7F", na.rm = TRUE)+
  geom_point(data = com_pandemia, aes(x = qtr, y = poblacion_sec_ind),color = "#C19C06" )+
  geom_line(data = com_pandemia, aes(x = qtr, y = poblacion_sec_ind),color = "#C19C06")+
  tema_1()+
  annotate("rect", xmin ="2019/04", xmax ="2021/02", ymax = max(comercio$poblacion_sec_ind) , ymin = min(comercio$poblacion_sec_ind), color = "#F6F6F6", alpha =1/7 )+
  theme( axis.text.y = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_blank())+
  geom_text(data = comercio[comercio$qtr %in% periodo_pande_label,], aes( x = qtr, y = poblacion_sec_ind, label = paste0(round(poblacion_sec_ind/1000000,1)," M")), color = "#434343", size = 3, vjust = 1.1, hjust= 1)+
  labs(subtitle = "Comportamiento de la poblacion ocupada en Comercio",
       y = "",
       x = "")



g4 <- ggplot(data = restaurants, aes( x = qtr, y = poblacion_sec_ind, group = 1))+
  geom_point(color = "#837F7F", na.rm = TRUE)+
  geom_line(color = "#837F7F", na.rm = TRUE)+
  geom_point(data = rest_pandemia, aes(x = qtr, y = poblacion_sec_ind),color = "#C19C06" )+
  geom_line(data = rest_pandemia, aes(x = qtr, y = poblacion_sec_ind),color = "#C19C06")+
  tema_1()+
  annotate("rect", xmin ="2019/04", xmax ="2021/02", ymax = max(restaurants$poblacion_sec_ind) , ymin = min(restaurants$poblacion_sec_ind), color = "#F6F6F6", alpha =1/7 )+
  theme( axis.text.y = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle = 90, hjust = .5, vjust = .5))+
  geom_text(data = restaurants[restaurants$qtr %in% periodo_pande_label,], aes( x = qtr, y = poblacion_sec_ind, label = paste0(round(poblacion_sec_ind/1000000,1)," M")), color = "#434343", size = 3, vjust = 1.1, hjust= 1)+
  labs(subtitle = "Comportamiento de la poblacion ocupada en Restaurantes y servicios de alojamiento",
       y = "",
       x = "trimestre")



tasa_crecimiento <- crecimiento %>%
  mutate( `2019/04 Pob ocupada Mill` = round(`2019/04`/1000000,1),
          `2021/04 pob cupada Mill` = round(`2021/04`/1000000,1),
          `% crec` = round(crec * 100,2),
          `% pob sector` = (prop_pob * 100)) %>%
  select(-c(crec, `2019/04`, `2021/04`, prop_pob)) %>%
  rename("sector economico" = descripcion)



graficos <- grid.arrange(g2, g3, g4, ncol = 1, nrow = 3, layout_matrix = rbind(1,2,3,3))

hj <- matrix(c(0, 0.5, 0.5, 0.5, 0.5), ncol=5, nrow=nrow(tasa_crecimiento), byrow=TRUE)
x <- matrix(c(0, 0.5, 0.5, 0.5, 0.5), ncol=5, nrow=nrow(tasa_crecimiento), byrow=TRUE)



tt_grobe <- ttheme_minimal(base_size = 10  ,base_colour = "#575757", core = list(fg_params = list(hjust = as.vector(hj),
                                                                                                  x = as.vector(x))),
                          rowhead=list(fg_params=list(hjust=1.1, x=0)))



tabla <- tableGrob(tasa_crecimiento , rows = NULL , theme = tt_grobe)

tb <- grid.arrange(top = textGrob("Crecimiento de la poblacion ocupada por sector economico en millones de personas \n del cuarto trimestre del 2019 al cuarto trimestre del 2021.", gp = gpar(col ="#575757"  )), tabla)
tb

c <- grid.arrange(tb, graficos, ncol = 2, widths =c(1.5, 1))

titulo<- textGrob("¿Como ha evolucionado la poblacion ocupada en la economia Mexicana? \nTras los efectos de la pandemia" ,gp = gpar(fontsize = 20, font = 1, col = "#575757" ))
nota <- textGrob("viz por: @ReyesEsparza10     fuente: INEGI",gp = gpar(fontsize = 10, font = 1, col = "#575757" ) )


grafico_final <- grid.arrange(g1, c, top = titulo, bottom = nota)

ggsave(grafico_final, filename =  "poblacion_ocupada.png", width = 17, height = 10)








