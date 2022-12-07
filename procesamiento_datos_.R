# procesamiento Censo de Población y CONAPO
library(tidyverse)

cdmx <- sf::st_read("G:\\.shortcut-targets-by-id\\1X4qSsPI_0kCFeoFf5t5oY0QMRYx79sDW\\Proyectos_data_science\\INEGI_Marco_Geo_dic_2021\\mg2021_integrado\\conjunto_de_datos\\00a.shp") %>% 
  filter(CVE_ENT=="09")

# Población 
#Resultados por AGEB 09 CDMX 
poblacion <- read_csv("G:\\.shortcut-targets-by-id\\1X4qSsPI_0kCFeoFf5t5oY0QMRYx79sDW\\Proyectos_data_science\\RESULTADOS_AGEB\\RESULTADOS_AGEB\\RESAGEBURB_09CSV20.csv") %>% 
  janitor::clean_names() %>% 
  filter(nom_loc=="Total AGEB urbana") %>% 
  select(1:18) %>% 
  mutate(cvegeo=paste0(entidad,mun,loc,ageb))%>% 
  select(1:11,cvegeo)


marginacion <-  read_csv("01_input/marginacion_cdmx.csv")

# psdss % Población sin derechohabiencia a los servicios de salud
# SBASC % Población  de 15 años o más sin educación básica

marginacion %>% 
  select(cve_ageb,sbasc,psdss,im_2020,gm_2020)

datos_final <- poblacion %>% 
  left_join(marginacion %>% 
              select(cve_ageb,sbasc,psdss,im_2020,gm_2020,imn_2020),by=c("cvegeo"="cve_ageb"))

datos_proyecto <- cdmx %>% 
  left_join(datos_final,by=c("CVEGEO"="cvegeo"))


pal <- wesanderson::wes_palette("Zissou1", 40, type = "continuous")

histograma <- datos_proyecto %>% 
  ggplot(aes(pobtot))+
  geom_histogram(aes(fill=..count..),
                 bins = 40,
                 alpha=.8,
                 col="black")+
  scale_x_continuous(labels = scales::comma,
                     breaks = seq(from=0,to=25000,by=2500))+
  scale_y_continuous(breaks = seq(from=0,to=300,by=40))+
  scale_fill_gradientn(colours = pal,
                       guide = guide_legend( keyheight = unit(3, units = "mm"), 
                                             keywidth=unit(12, units = "mm"), 
                                             label.position = "bottom", 
                                             title.position = 'top', 
                                             nrow=1))+
  theme_bw()+
  labs(x="Distribución de Población de los AGEBS CDMX",
       y="Frecuencia",
       fill="Frecuencia Población AGEBS",
       caption = "Elaboración propia con Datos del CENSO 2020")+
  theme(
    legend.position = "top",
    axis.text.x = element_text(color="black",size=10,face="bold"),
    axis.text.y = element_text(color="black",size=10,face="bold"),
    axis.title.x = element_text(color="black",size=12,face="bold"),
    axis.title.y = element_text(color="black",size=12,face="bold"),
    plot.caption = element_text(color="black",size=12,face="bold",hjust = 0),
    legend.text = element_text(color="black",size=10,face="bold"),
    legend.title = element_text(color="black",size=12,face="bold"),
    )

histograma

pal <- wesanderson::wes_palette("Zissou1", 30, type = "continuous")

quantile(datos_proyecto$pobtot,na.rm = T,probs = c(0:1))

datos_proyecto$gm_2020 <- factor(datos_proyecto$gm_2020,levels = c("Muy bajo",
                                                                   "Bajo",
                                                                   "Medio",
                                                                   "Alto",
                                                                   "Muy alto"))
maginacion_mapa <- datos_proyecto %>%
  ggplot()+
  geom_sf(fill="grey")+
  geom_sf(aes(fill=gm_2020),col="black",size=.51)+
  theme_bw()+
  viridis::scale_fill_viridis(option = "inferno",
                              discrete = T)+
  theme_void()

ggsave("marginacion_mapa.png",
       plot = maginacion_mapa,
       width = 12,
       height = 16)

marginacion_plot <- datos_proyecto %>% 
  filter(!is.na(gm_2020)) %>% 
  group_by(gm_2020) %>% 
  summarise(total=n()) %>% 
  ggplot(aes(gm_2020,total))+
  geom_col(aes(fill=gm_2020),
           col="black",
           alpha=.8)+
  theme_bw()+
  viridis::scale_fill_viridis(option = "inferno",
                              discrete = T)+
  theme(
    legend.position = "top",
    axis.text.x = element_text(color="black",size=10,face="bold"),
    axis.text.y = element_text(color="black",size=10,face="bold"),
    axis.title.x = element_text(color="black",size=12,face="bold"),
    axis.title.y = element_text(color="black",size=12,face="bold"),
    plot.caption = element_text(color="black",size=12,face="bold",hjust = 0),
    legend.text = element_text(color="black",size=10,face="bold"),
    legend.title = element_text(color="black",size=12,face="bold"),
  )

ggsave(filename = "marginacion_plot.png",
       plot = marginacion_plot,
       width = 10,
       height = 8)


datos_proyecto %>% 
  glimpse()


poblacion_mapa <- datos_proyecto %>%
  ggplot()+
  geom_sf(fill="grey")+
  geom_sf(aes(fill=pobtot),col="black",size=.51)+
  theme_bw()+
  viridis::scale_fill_viridis(option = "inferno",
                              discrete = F)+
  theme_void()

ggsave("Poblacion_mapa.png",
       plot = poblacion_mapa,
       width = 12,
       height = 16)
