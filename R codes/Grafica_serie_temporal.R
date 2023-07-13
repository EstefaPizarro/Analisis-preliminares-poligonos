# GRAFICA DE NDVI DE POLIGONOS DE ESPECIES GENERADOS---------------

# CONFIGURACIÓN INICIAL ---------------------------------------------------
rm(list = ls())
graphics.off()
options(scipen = 999)

# CARGAR LIBRERIAS --------------------------------------------------------
pacman::p_load(ggplot2
               ,dplyr
               ,lubridate
               ,tidyr
               ,stringr
               ,forcats
               ,plotly)

# Setear ruta inicial
path <- "Inputs/"

# Listar archivos a trabajar
lista <- list.files(path)


# CICLO PARA GRAFICA ------------------------------------------------------
for(i in 1:length(lista)){
  
  ## Leo base
  datos <- read.csv(paste0(path,lista[i]))
  
  # Cambio de estructura de fecha 
  datos <- datos %>% as_tibble() %>%
    separate(system.index
             , sep = "_"
             ,into = c("Poligono","Fecha1", "Fecha2", "Sat")) %>% 
    filter(Sat %in% c("T19HBA")) %>% 
    mutate(Date = ymd(Date)
           ,Poligono = as.factor(Poligono)
           ,Poligono = fct_inorder(Poligono)) %>% 
    select(-c(Fecha1, Fecha2))
  
  
  # Obtener nombre de cada archivo para exportacion
  name <- str_remove(lista[i], "\\.csv$")
  
  # Ploteo
  ggplot(datos, aes(x=Date, y=NDVI, color=Poligono)) +      
    geom_line() + 
    labs(title= paste0('Serie temporal NDVI ',name), 
         subtitle='Comportamiento histórico de ubicación de polígono', 
         caption='Instituto Nacional de Estadísticas, 2023',
         x='Fecha', y='NDVI') + 
    theme(plot.title=element_text(family='Anton', size=20),
          plot.subtitle=element_text(family='Anton'),
          axis.title.x = element_text(family='Anton', face='bold', hjust=0.5),
          axis.title.y = element_text(family='Anton', face='bold', hjust=0.5))  -> p
  
  #plot <- p + theme(legend.position='none')
  
  ggplot2::ggsave(filename = paste0("Outputs/Serie_junta/",name,".png"),
                  plot = plot, 
                  device = "png",
                  width = 22, height = 10)
  
  plot2 <- ggplotly(plot)
  
  htmlwidgets::saveWidget(
    widget = plot2, #the plotly object
    file = paste0("Outputs/Serie_junta//",name,".html"), #the path & file name
    selfcontained = TRUE #creates a single html file
  )
  
  message(paste0("Grafico_", name, "_generado"))
  
}

# CICLO PARA GRAFICA GRID------------------------------------------------------
for(i in 1:length(lista)){
  
## Leo base
datos <- read.csv(paste0(path,lista[i]))


# Cambio de estructura de fecha 
datos <- datos %>% as_tibble() %>%
  separate(system.index
           , sep = "_"
           ,into = c("Poligono","Fecha1", "Fecha2", "Sat")) %>% 
  filter(Sat %in% c("T19HBA")) %>% 
  mutate(Date = ymd(Date)
         ,Poligono = as.factor(Poligono)
         ,Poligono = fct_inorder(Poligono)) %>% 
  select(-c(Fecha1, Fecha2))


# Obtener nombre de cada archivo para exportacion
name <- str_remove(lista[i], "\\.csv$")

# Ploteo
ggplot(datos, aes(x=Date, y=NDVI, color=Poligono)) +      
  geom_line() + 
  labs(title= paste0('Serie temporal NDVI ',name), 
       subtitle='Comportamiento histórico de ubicación de polígono', 
       caption='Instituto Nacional de Estadísticas, 2023',
       x='Fecha', y='NDVI') + 
  theme(plot.title=element_text(family='Anton', size=20),
        plot.subtitle=element_text(family='Anton'),
        axis.title.x = element_text(family='Anton', face='bold', hjust=0.5),
        axis.title.y = element_text(family='Anton', face='bold', hjust=0.5)) +
  facet_grid('Poligono') -> p

plot <- p + theme(legend.position='none')
 
 ggplot2::ggsave(filename = paste0("Outputs/Serie_separada/",name,".png"),
                 plot = plot, 
                 device = "png",
                 width = 22, height = 10)
 
 plot2 <- ggplotly(plot)
 
 htmlwidgets::saveWidget(
   widget = plot2, #the plotly object
   file = paste0("Outputs/Serie_separada/",name,".html"), #the path & file name
   selfcontained = TRUE #creates a single html file
 )
 
 message(paste0("Grafico_", name, "_generado"))
 
}
 
