
install.packages("lubridate")
install.packages("plotly")
install.packages("extrafont")

library(highcharter)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(extrafont)

#archivo en bruto
femicidio <- read.csv('femicidios.csv')
view(femicidio)

#archivo completo
femicidio2 <- read.csv('registrofemicidios.csv')
view(femicidio2)

# renombro las columnas 
names (femicidio2) = c("casoNro", "edadVictima", "generoVictima", "tipoVictima", "provincia", "modalidad", "fechaHecho", "anio", "mes")
view(femicidio2)

# Ordena los casos por fecha del hecho
gpdos <- femicidio2 %>%
  arrange(desc(fechaHecho)) %>%
  distinct(casoNro, .keep_all = TRUE)
view(gp2)

#pasos para probar luego highcharter REVISAR
gp2 <- femicidioSin_NA %>%
  select(casoNro, anio, modalidad) %>% 
  nest(-casoNro) %>%
  mutate(
    data = map(data, mutate_mapping, hcaes(x = anio, y = modalidad), drop = TRUE),
    data = map(data, list_parse)
  ) %>%
  rename(ttdata = data)
#highcharter

# agrupa y cuenta por provincia y por género, hay value missing por lo que me dan más categorías que las realmente existentes 
#sería interesante completar los NA o eliminarlos
gptres <- gpdos %>% 
    group_by(provincia)  %>% 
    count(generoVictima)%>% 

prueba <- femicidio2 %>% 
  group_by(anio)  %>% 
  count(tipoVictima) %>% 
  order_by(anio)
View(prueba)

# agrupa y sumariza por provincia y por tipo de victima
gpcuatro <- gpdos %>% 
    group_by(provincia)%>% 
    count(tipoVictima)
  view(gpcuatro)
# renombro los nombres de los campos de gpcuatro, 'n' pasa a llamarse 'total'  
names (gpcuatro) = c("provincia", "tipoVictima", "total") 
view(gpcuatro) 


# agrupa y cuenta por provincia y por género 
gp5 <- gp %>% 
   group_by(provincia)%>% 
   count(provincia) %>% 
  order_by(provincia)
  view(gp5)  

# calcula totales por provincia 
gpcin <- gpdos %>% 
    group_by(provincia) %>% 
    count(provincia) 
  view(gpcin) 
# renombro los nombres de los campos de gpcin, 'n' pasa a llamarse 'total'    
names (gpcin) = c("provincia", "total") 
view(gpcin)  
  

 # primera visualización: visualizacion cant. por provincia y tipo de victima 
ggplot(data = gpcuatro, aes(x=provincia, y=total, color = tipoVictima)) + 
    geom_point ()+
    theme(aspect.ratio = 1)+
    labs (x = "Provincias", y = "Total de femicidios", color = "Tipo de víctima")+
    ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+ #las provincias se visualizan en forma vertical en el eje x
    theme(plot.title = element_text(family="Comic Sans MS",
                                    size=rel(1),       #Tamaño relativo de la letra del título
                                    vjust=2,           #Justificación vertical, para separarlo del gráfico
                                    position_identity(center),   
                                    face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                    color="black",    #Color del texto  color=maroon, lightblue
                                    lineheight=1.0)) #Separación entre líneas
  
# visualizacion cant. por provincia y tipo de victima, el tamaño de los puntos varía de acuerdo a la cantidad
  ggplot(data = gpcuatro, aes(x=total, y=provincia, color = tipoVictima)) + 
    geom_point (size = 5, alpha = 1 / 3) + theme(aspect.ratio = 1)+
    labs (x = "Total de femicidios", y = "Provincias", color = "Tipo de víctima")+
    theme (text = element_text(size=11)) +  # Tamaño de la fuente de las leyendas de  x e y
  ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
  theme (plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1.5),       #Tamaño relativo de la letra del título
                                     vjust=2,            #Justificación vertical, para separarlo del gráfico
                                     position_identity(center),   
                                     face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                     color="black", #Color del texto  color=maroon, lightblue
                                     lineheight=1.0))  #Separación entre líneas
  
  # visualización de totales por provincia ordenado por orden alfabetico, utilizo face_wrap por tipo de victima
  ggplot(data = gpcuatro, aes(x=provincia, y=total, color = tipoVictima)) + 
    geom_point ()+
    theme(aspect.ratio = 1)+
    facet_wrap( ~tipoVictima)+
    labs (x = "Provincias", y = "Total de femicidios", color = "Tipo de víctima")+
    ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+ #las provincias se visualizan en forma vertical en el eje x
    theme(plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1),       #Tamaño relativo de la letra del título
                                     vjust=1,           #Justificación vertical, para separarlo del gráfico
                                     position_identity(center),   
                                     face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                     color="black",    #Color del texto  color=maroon, lightblue
                                     lineheight=1.0)) #Separación entre líneas
    
  
  # visualización totales por provincia y utilizo face_wrap por provincia, agrego títulos y etiquetas (mejorarlo)
  ggplot(data = gpcuatro, aes(x=provincia, y=total, color = tipoVictima)) + 
    geom_point ()+
    theme(aspect.ratio = 1)+
    labs (x = "Provincias", y = "Total de femicidios", color = "Tipo de víctima")+
    theme (text = element_text(size=10)) +  # Tamaño de la fuente de las leyendas de  x e y
    ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
    theme (plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1),       #Tamaño relativo de la letra del título
                                     vjust=1,            #Justificación vertical, para separarlo del gráfico
                                     position_identity(center),   
                                     face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                     color="black", #Color del texto  color=maroon, lightblue
                                     lineheight=1.0))+  #Separación entre líneas
    facet_wrap(~year)+
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
  
 
  
# visualización con diagramas de barras, totales por provincia y tipo de victima
ggplot(data = gpcuatro, aes(x=provincia, y=total, fill= tipoVictima)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values =c("darkgreen", "purple")) +
    labs (x = "Provincias", y = "Total de femicidios", fill = "Tipo de víctima")+
    ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
    theme (plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1),       #Tamaño relativo de la letra del título
                                     vjust=1,            #Justificación vertical, para separarlo del gráfico
                                     position_identity(center),   
                                     face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                     color="black", #Color del texto  color=maroon, lightblue
                                     lineheight=1.0))+  #Separación entre líneas
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) #las provincias se visualizan en forma vertical en el eje x
 
# visualización con diagramas de barras, totales por provincia y tipo de victima utilizando geom_col (ambos valores en una misma barra)
ggplot(data = gpcuatro, aes(x=provincia, y=total, fill= tipoVictima)) + 
    geom_col(width = .3) +
    scale_fill_manual(values =c("darkgreen", "purple")) +
    labs (x = "Provincias", y = "Total de femicidios", fill = "Tipo de víctima")+
    ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
    theme (plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1),       #Tamaño relativo de la letra del título
                                     vjust=1,            #Justificación vertical, para separarlo del gráfico
                                     position_identity(center),   
                                     face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                     color="black", #Color del texto  color=maroon, lightblue
                                     lineheight=1.0))+  #Separación entre líneas
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+ #las provincias se visualizan en forma vertical en el eje x

  
  
  
  
  
  
  # visualización totales por provincia y tipo de victima con geom_bar y distintos atributos para manejar las barras
  ggplot(data = gpcuatro, aes(x=provincia, y=total)) + 
    geom_bar(width= 0.5,stat="identity",aes(fill = tipoVictima), position = position_stack(reverse = TRUE)) + #con el atributo fill indico que la barra va a estar formada por valores de principal y vinculado
    coord_flip()+
    labs (x = "Provincia", y = "Total de femicidios", fill = "Tipo de víctima")+
    theme (text = element_text(size=11)) +  # Tamaño de la fuente de las leyendas de  x e y
    ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
    theme (plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1.5),       #Tamaño relativo de la letra del título
                                     vjust=1,            #Justificación vertical, para separarlo del gráfico
                                     position_identity(center),   
                                     face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                     color="black", #Color del texto  color=maroon, lightblue
                                     lineheight=1.0))  #Separación entre líneas
 
 
# separo fechaHecho en año y mes, luego agrupo por año y provincia, ordenado en orden ascendente
femicidioanioprov<- femicidio2 %>%
    separate(fechaHecho, c("year", "month"), sep = "-", remove = FALSE) %>% #se extrae año y mes de una fecha del tipo yyyy-mm-dd
    group_by(year, provincia) %>%   #agrupa por año y  provincia
    slice(1)
View( femicidioanioprov)

# Omito las filas con años vacíos para poder trabajar mejor
femicidioSin_NA<- femicidioanioprov %>% na.omit()
View(femicidioSin_NA)  

femicount<- femicidioSin_NA %>%
 group_by(provincia) %>%   #agrupa por año y  provincia
  count(tipoVictima)
View( femicount)

femicount2<- femicidioSin_NA %>%
  group_by(anio,provincia) %>%   #agrupa por año y  provincia
 count(tipoVictima)
View( femicount2)




gpcuatro <- gpdos %>% 
  group_by(provincia)%>% 
  count(tipoVictima)
view(gpcuatro)

summarise(avg = mean(femicidioSin_NA),
          stddev = sd(femicidioSin_NA)) %>%
  filter(year == "2018")




# revisar el código a continuación  
  
ggplot(data = gpcuatro, aes(x=provincia, y=total, fill= provincia)) + 
    geom_bar(width=1,stat="identity", position=position_dodge()) +
    scale_fill_manual(values =c("darkgreen", "purple", "blue","black", "maroon", "lightblue","pink", "yellow","orange", "chocolate2", "brown","darkgrey", "pink2", "darkgreen","cyan", "maroon", "lightgreen","pink", "yellow","orchid", "red", "skyblue2", "green","turquoise")) 
  

gptot <- left_join(gpdos, gptres, by = "country")
view(gptot)

