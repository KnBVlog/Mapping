
 library(ggplot2)
 library(dplyr)
 library(tidyverse)
 library(readxl)
 library(sf)
 library(rgdal)
 library(leaflet)


Denpasar <- readOGR('Enter_Here\\ADMINISTRASIKECAMATAN.shp')
Denpasar.sf <-st_as_sf(Denpasar)


#Warna
mybinspop <- c(0,100000,200000,300000,Inf)
mypalettepop <- colorBin( palette="Reds", 
                          domain=Denpasar@data$PRIA2020, 
                          na.color="transparent", bins=mybinspop)

mybinspuskesmas <- c(2,3,4,Inf)
mypalettepuskesmas <- colorBin( palette="YlOrBr", 
                                domain=Denpasar@data$PRIA2020, 
                                na.color="transparent", 
                                bins=mybinspuskesmas)


#Leaflet
leaflet(Denpasar) %>%
  #Group Base Map
  addTiles(group = "OSM (default)")%>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
  addProviderTiles(providers$Stamen.Terrain, group = "Stamen") %>%
  setView(lat=-8.665550, lng=115.217919,zoom=11.5) %>%
  
  #Group Polygon
  addPolygons(data=Denpasar, color="black"
              , fillColor=0, fillOpacity = 0.05
              , weight=1.3, opacity=1, group="Kecamatan") %>%
  addPolygons(data=Denpasar, group="Total Populasi 2020",
              fillColor = ~mypalettepop(TOTAL2020), 
              stroke=TRUE, fillOpacity = 0.5, color="white", 
              weight=0.3) %>%
  addPolygons(data=Denpasar, group="Jumlah Puskesmas",
              fillColor = ~mypalettepuskesmas(JUMLAH_PUS), 
              stroke=TRUE, fillOpacity = 0.5, color="white", 
              weight=0.3) %>%
  
  #Group Legend
    addLegend(pal=mypalettepop, values=~TOTAL2020, opacity=0.9, 
              title="Total Populasi Tahun 2020", 
              position="bottomleft",group="Total Populasi 2020") %>%
    addLegend(pal=mypalettepuskesmas, values=~JUMLAH_PUS, opacity=0.9, 
              title="Jumlah Puskesmas",position="bottomleft",
              group = "Jumlah Puskesmas") %>%
  
  #Layers Control
  addLayersControl(
    baseGroups = c("OSM (default)","CartoDB", "Stamen"),
    overlayGroups = c("Kecamatan","Total Populasi 2020","Jumlah Puskesmas"),
    options = layersControlOptions(collapsed = TRUE)) %>% 
  #Menambahkan icon home button di peta
  addEasyButton(easyButton(icon="fa-home", title="Return",
    onClick=JS("function(btn, map){ map.setView([lat=-8.665550, 
               lng=115.217919], 11.5); }"))) %>%
  htmlwidgets::onRender("function() {$('.leaflet-control-layers-overlays').
    prepend('<label style=\"text-align:left\">Layers:</label>'),
    $('.leaflet-control-layers-list').
    prepend('<label style=\"text-align:left\">Base Map Theme:</label>');}") %>% 
  hideGroup(c("Kecamatan","Total Populasi 2020","Jumlah Puskesmas")) 

library(ggplot2)
library(ggspatial) #Untuk menambahkan annotation scale and arrow

view(Denpasar.sf)
ggplot(Denpasar)+
  ggspatial::annotation_map_tile("cartolight") +
  geom_sf(data=Denpasar.sf,aes(fill=TOTAL2020),alpha=0.5)+
  scale_fill_continuous(name=bquote("Total Population in 2022"),
                        high="darkblue",low="lightblue",
                        labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  theme_bw()+
  theme(legend.position="right",
        legend.text=element_text(size=14),
        legend.box.background = element_rect(color="black", size=1),
        text=element_text(size=14,family="serif")
  )+
  annotation_scale(location = "br", width_hint = 0.5,
                   bar_cols = c("grey60", "white"),
                   text_family="serif") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering())

