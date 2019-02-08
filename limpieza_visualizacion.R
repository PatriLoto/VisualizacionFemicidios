
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
#archivo sin provincias sin especificar
femicidio2 <- read.csv('registrofemicidios.csv')
view(femicidio)
view(femicidio2)

names (femicidio) = c("casoNro", "edadVictima", "generoVictima", "tipoVictima", "provincia", "modalidad", "fechaHecho", "medio","link", "id_indec")
names (femicidio2) = c("casoNro", "edadVictima", "generoVictima", "tipoVictima", "provincia", "modalidad", "fechaHecho", "anio", "mes")

# Ordena los casos por fecha del hecho
gp <- femicidio %>%
  arrange(desc(fechaHecho)) %>%
  distinct(casoNro, .keep_all = TRUE)
view(gp)


gpdos <- femicidio2 %>%
  arrange(desc(fechaHecho)) %>%
  distinct(casoNro, .keep_all = TRUE)
view(gpdos)


#no sirve la informacion
gp1 <- femicidio %>%
  arrange(desc(fechaHecho)) %>%
(provincia,keep_all = TRUE)
view(gp1)


gp2 <- gapminder %>%
  select(casoNro, fechaHecho, modalidad) %>% 
  nest(-casoNro) %>%
  mutate(
    data = map(data, mutate_mapping, hcaes(x = year, y = pop), drop = TRUE),
    data = map(data, list_parse)
  ) %>%
  rename(ttdata = data)


# agrupa y cuenta por provincia y por género, hay value missing por lo que me da más categorías que las realmente existentes 
#sería interesante completar los NA o eliminarlos

gp3 <- gp %>% 
  group_by(provincia)  %>% 
  count(generoVictima)
  view(gp3)
  
  
gptres <- gpdos %>% 
    group_by(provincia)  %>% 
    count(generoVictima)
  view(gptres)

# agrupa y cuenta por provincia y por tipo de victima

gp4 <- gp %>% 
  group_by(provincia)  %>% 
  count(tipoVictima)
  view(gp4)
  
gpcuatro <- gpdos %>% 
    group_by(provincia)  %>% 
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
  
 # primera visualización 
  ggplot(data = gpcin, aes(x=provincia, y=n)) + 
    geom_point() +
    theme(aspect.ratio = 1)
  
  #visualizacion cant. por provincia y tipo de victima
  ggplot(data = gpcuatro, aes(x=provincia, y=n, color = tipoVictima)) + 
    geom_point ()+
    theme(aspect.ratio = 1)
  
# visualizacion cant. por provincia y tipo de victima, el tamaño de los puntos varía de acuerdo a la cantidad
  ggplot(data = gpcuatro, aes(x=total, y=provincia, color = tipoVictima)) + 
    geom_point (size = 5, alpha = 1 / 3) + theme(aspect.ratio = 1)+
    labs (x = "Total de femicidios", y = "Provincia", color = "Tipo de víctima")+
    theme (text = element_text(size=11)) +  # Tamaño de la fuente de las leyendas de  x e y
  ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
  theme (plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1.5),       #Tamaño relativo de la letra del título
                                     vjust=2,            #Justificación vertical, para separarlo del gráfico
                                     position_identity(center),   
                                     face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                     color="black", #Color del texto  color=maroon, lightblue
                                     lineheight=1.0))  #Separación entre líneas
  
  # visualización totales por provincia y utilizo face_wrap por tipo de victima
  ggplot(data = gpcuatro, aes(x=provincia, y=total, color = tipoVictima)) + 
    geom_point ()+
    theme(aspect.ratio = 1)+
    facet_wrap( ~tipoVictima)+
    labs (x = "Provincia", y = "Total de femicidios", color = "Tipo de víctima")
  
  
  # visualización totales por provincia y utilizo face_wrap por provincia, agrego títulos y etiquetas 
  ggplot(data = gpcuatro, aes(x=provincia, y=total, color = tipoVictima)) + 
    geom_point ()+
    theme(aspect.ratio = 1)+
    facet_wrap( ~provincia)+
    labs (x = "Total de femicidios", y = "Provincia", color = "Tipo de víctima")+
    theme (text = element_text(size=10)) +  # Tamaño de la fuente de las leyendas de  x e y
    ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
    theme (plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1),       #Tamaño relativo de la letra del título
                                     vjust=2,            #Justificación vertical, para separarlo del gráfico
                                     position_identity(center),   
                                     face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                     color="black", #Color del texto  color=maroon, lightblue
                                     lineheight=1.0))  #Separación entre líneas
  
  
 
  
    #
  ggplot(data = gpcuatro, aes(x=provincia, y=total, fill= tipoVictima)) + 
  geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values =c("darkgreen", "purple")) +
    labs (x = "Provincia", y = "Total de femicidios", color = "Tipo de víctima")+
    ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
    theme (plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1),       #Tamaño relativo de la letra del título
                                     vjust=2,            #Justificación vertical, para separarlo del gráfico
                                     position_identity(center),   
                                     face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                     color="black", #Color del texto  color=maroon, lightblue
                                     lineheight=1.0))  #Separación entre líneas
  
  #
  ggplot(data = gpcuatro, aes(x=provincia, y=total, fill= provincia)) + 
    geom_bar(width=1,stat="identity", position=position_dodge()) +
    scale_fill_manual(values =c("darkgreen", "purple", "blue","black", "maroon", "lightblue","pink", "yellow","orange", "chocolate2", "brown","darkgrey", "pink2", "darkgreen","cyan", "maroon", "lightgreen","pink", "yellow","orchid", "red", "skyblue2", "green","turquoise"))  
  
  # visualización totales por provincia, utilizo geom_bar y distintos atributos para manejar las barras
  ggplot(data = gpcuatro, aes(x=provincia, y=total)) + 
    geom_bar(width= 0.5,stat="identity",aes(fill = tipoVictima), position = position_stack(reverse = TRUE)) + #con el atributo fill indico que la barra va a estar formada por valores de principal y vinculado
    coord_flip()+
    labs (x = "Provincia", y = "Total de femicidios", fill = "Tipo de víctima")+
    theme (text = element_text(size=11)) +  # Tamaño de la fuente de las leyendas de  x e y
    ggtitle ("Femicidios ocurridos en la Argentina durante 2012 y 2019")+ # agrego título al gráfico
    theme (plot.title = element_text(family="Comic Sans MS",
                                     size=rel(1.5),       #Tamaño relativo de la letra del título
                                     vjust=2,            #Justificación vertical, para separarlo del gráfico
                                     position_identity(center),   
                                     face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                     color="black", #Color del texto  color=maroon, lightblue
                                     lineheight=1.0))  #Separación entre líneas
 
     
  
  

gptot <- left_join(gp, gp2, by = "country")


# utilizando boxplot, no me convence
ggplot(data = gpcuatro, aes(x=provincia, y=total, color = tipoVictima)) + 
  geom_boxplot() +
  theme(aspect.ratio = 1)  