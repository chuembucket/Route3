

library(tidyverse)
library(plotKML)
library(transformr)
library(sf)
library(rgdal)
library(tigris)
options(tigris_use_cache = TRUE)
sf::sf_use_s2(FALSE)
library(ggrepel)
library(tidycensus)
library(tidyverse)
library(gganimate)
library(gginnards)
library(raster)
library(geojsonR)
library(geojsonsf)

mapTheme <- theme(plot.title =element_text(size=12),
                  plot.subtitle = element_text(size=8),
                  plot.caption = element_text(size = 6),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background= element_rect(colour = 'lightgrey'),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "none",
                  plot.margin = margin(1, 1, 1, 1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))

parking <- geojson_sf("C:/Users/Charlie/Documents/GIS/twitter/cecilbus/parking.geojson")
buildings <- readOGR("C:/Users/Charlie/Documents/GIS/PhillyData", layer = "LI_BUILDING_FOOTPRINTS_1", verbose = T) %>% st_as_sf()
landuse <- readOGR("C:/Users/Charlie/Documents/GIS/PhillyData", layer = "Land_Use", verbose = T) %>% st_as_sf()
unis <- readOGR("C:/Users/Charlie/Documents/GIS/PhillyData", layer = "unis", verbose = T) %>% st_as_sf() 
schools<- readOGR("C:/Users/Charlie/Documents/GIS/PhillyData", layer = "Schools", verbose = T) %>% st_as_sf()
parks <- readOGR("C:/Users/Charlie/Documents/GIS/PhillyData", layer = "parks", verbose = T) %>% st_as_sf()%>%
  st_transform(crs=4326)
fr8rail <- readOGR("C:/Users/Charlie/Documents/GIS/PhillyData", layer = "fr8rail", verbose = T) %>% st_as_sf() %>%
  st_transform(crs=4326)


sbs <- st_join(buildings, schools)

unibs <- st_join(buildings, unis) %>% filter(!is.na(BRT_ID))


unibs %>% st_crop(y = bounds) %>% filter(!is.na(BRT_ID)))+
  geom_sf()

sbs <- sbs %>% filter(!is.na(TYPE))%>% 
  st_crop(y = bounds)




bounds <- c(ymax = 39.985, ymin = 39.975, xmin =-75.195, xmax = -75.13)

#import schools

ggplot()+
  geom_sf(data = builds, color = NA, fill = 'white')+
  geom_sf(data = uni, color = NA, fill = 'maroon', alpha = .7)+
  geom_sf(data = sbs, color = NA, fill = 'orange', alpha = .7)+
  mapTheme
  




#load data with st_read
#r = st_read(busgpx, layer = "tracks")
#ggplot(r)+geom_sf()

#load data with readGPX (outdated?)

#GPX operations
#loading
busgpx <-'C:/Users/Charlie/Documents/GIS/twitter/cecilbus/cb.gpx'
route <- readGPX(busgpx) 
route <- route$tracks[[1]][[1]] 

#c <- route %>% st_as_sf(coords = c('lon','lat')) %>% st_set_crs(4326) %>% st_transform(crs=32736)
#vars
# plot (c)
# ds <- spDistsN1(c, longlat = FALSE)
# 
# lsf = st_sf(st_set_geometry(p[2, ], NULL), st_sfc(l1))
# plot(p$geometry[1:2], add = T)

speed1 <- c(rep(NA, nrow(route)))
speed1[1] <- 0

for(i in 2:nrow(route)){
  speed1[i] <- sqrt((route$lon[i]-route$lon[i-1])^2+(route$lat[i]-route$lat[i-1])^2)
}
route$speed <- speed1




route <- route %>% st_as_sf(coords = c('lon','lat')) %>% st_set_crs(4326) %>% st_transform(crs=32736)
route$id <- seq(1,nrow(route))

dist1 <- c(rep(NA, nrow(route)))

for(i in 1:nrow(route)-1){
  dist1[i] <- st_distance(x = route$geometry[i], y = route$geometry[i+1])
}


route$speedmph <- dist1

dist2 <- c(rep(NA, nrow(route)))

for(i in 3:nrow(route)-2){
  dist2[i] <- (dist1[i-2] + dist1[i-1] + dist1[i] +dist1[i+1] + dist1[i+2]/ 5)
}





back$speedcol <- c(rep('red', nrow(back)- 1000), rep('blue', 1000))


busmotion <- c(rep(NA, nrow(route)))
               
for(i in 1:nrow(route)-1){
  busmotion[i] <- ifelse(route$speedmph[i] < route$speedmph[i+1], 'acc', 
                         ifelse(route$speedmph[i] > 5, 'slow','stop')
                         )
}  

route$motion <- busmotion



there <- route[100:1000,]
back <- route[8000:9500,]
smool <- route[8100:8200,]

ggplot(back[1:300,], aes(y = speedmph, x = id, color = motion))+
  geom_point()+
  scale_color_manual(values = c("#008450", "#EFB700", "#B81D13", NA))
  




# geom_sf(data = back, aes(color = cut(speedmph, c(0,5,15,60))), size = 2)+
#scale_color_manual(values = c("red", "yellow", "green", NA))+
############
#river
river <- area_water('PA', county = 'Philadelphia') %>%
  st_as_sf()%>%
  st_transform(crs=4326)

#rail
mflStatskml <- "C:/Users/Charlie/Documents/GIS/Transit/mflStats.kml"
mflStats <- st_read(mflStatskml)%>% 
  st_crop(y = bounds)

mflkml <- "C:/Users/Charlie/Documents/GIS/Transit/mfl.kml"
mfl <- st_read(mflkml)%>% 
  st_crop(y = bounds)

bslStatskml <- "C:/Users/Charlie/Documents/GIS/Transit/bslStats.kml"
bslStats <- st_read(bslStatskml)%>% 
  st_crop(y = bounds)

bslkml <- "C:/Users/Charlie/Documents/GIS/Transit/bsl.kml"
bsl <- st_read(bslkml)%>% 
  st_crop(y = bounds)

rrkml <- "C:/Users/Charlie/Documents/GIS/Transit/regionalrail.kml"
rr <- st_read(rrkml) %>% st_crop(y = bounds)

residential <- lu %>% filter(C_DIG1 == 1)
residential$reslevel <- residential$C_DIG2 %>% 
  factor(levels = c(11, 12, 13), labels = c('Low','Medium','High'))

#Boundsing!
#c(ymax = 39.99, ymin = 39.97, xmin =-75.195, xmax = -75.13)
bounds <- c(ymax = 39.985, ymin = 39.975, xmin =-75.195, xmax = -75.13)
builds <- buildings %>% st_crop(y = bounds)
lu <- landuse %>% st_crop(y = bounds)
riv <- river %>% st_crop(y = bounds)
uni <- unis %>% st_crop(y = bounds)
parkin <- parking %>% st_crop(y = bounds)
park <- parks %>% st_crop(y = bounds)
fr8 <- fr8rail %>% st_crop(y = bounds)
unib <- unibs %>% st_crop(y = bounds)



#geoms

    #rail
  # geom_sf(data = bsl,linetype = "dashed", size = 1.1)+
  # geom_sf(data = mfl,linetype = "dashed", size = 1.1)+
  # geom_sf(data = rr,linetype = "dashed", size = 1.1)+

    #schools builds
  # geom_sf(data = uni, fill = 'maroon', alpha = .3)+
  
  #geom_sf(data = riv, fill = 'blue', color = 'NA')+
  # geom_sf(data = parking, color = NA, fill = 'red')+



ggplot()+
  geom_sf(data = residential, aes(fill = reslevel), color = NA)+
  geom_sf(data = back)+  
  mapTheme

ggplot()+
  geom_sf(data = builds, fill = 'grey', aes(color = alpha("black", MAX_HGT)))+
  geom_sf(data = back)+  
  mapTheme




ggplot()+
  geom_sf(data = lu, aes(fill = as.character(C_DIG1)), color = NA)+
  geom_sf(data = residential, fill = 'grey', color = alpha("black",0.2))+
  geom_sf(data = route)+
  mapTheme







#anim
p <- ggplot()+
  # geom_sf(data = builds, color = NA, fill = 'white')+
  # geom_sf(data = parkin, color = alpha('black', 0.3), fill = 'grey')+
  # geom_sf(data = uni, color = NA, fill = 'maroon', alpha = .7)+
  # geom_sf(data = sbs, color = NA, fill = 'orange', alpha = .7)+
  geom_sf(data = bsl, linetype = "dashed", size = 1.1)+
  geom_sf(data = mfl, linetype = "dashed", size = 1.1)+
  geom_sf(data = rr, linetype = "dashed", size = 1.1)+
  geom_sf(data = back, aes(color = motion), size = 3)+
  scale_color_manual(values = c("#008450", "#EFB700", "#B81D13"))+
  labs(title = 'time = {frame_time}', subtitle = 'speed = {back %>% filter(id == frame_time) %>% pull(speedmph) %>% round(0)} mph')+
  mapTheme+
  transition_time(id)+
  ease_aes('linear')+
  shadow_wake(.03)
gganimate::animate(p, duration = 50, fps = 10)


p <- ggplot()+
  geom_sf(data = riv, fill = 'blue', color = 'NA')+
  geom_sf(data = park %>% filter(ACREAGE > 0), color = NA, fill = 'lightgreen', alpha = .7)+
  geom_sf(data = bsl, color = '#ee6737', size = 1.2)+
  geom_sf(data = builds, color = NA, fill = 'white')+
  geom_sf(data = unib, color = NA, fill = 'maroon', alpha = .3)+
  geom_sf(data = sbs, color = NA, fill = 'gold', alpha = .3)+
  geom_sf(data = parkin, color = NA, fill = 'orange')+
  geom_sf(data = fr8,linetype = "twodash", size = 1.1)+
  geom_sf(data = mfl, color = '#2e99d1', size = 1.2)+
  geom_sf(data = rr, color = '#477997', size = 1.1)+
  geom_sf(data = back, aes(color = motion), size = 1.5, alpha = .8)+
  labs(title = 'time = {frame_time}', subtitle = 'speed = {back %>% filter(id == frame_time) %>% pull(speed)}')+
  scale_color_manual(values = c("#008450", "#EFB700", "#B81D13"))+
  mapTheme+
  transition_time(id)+
  ease_aes('linear')+
  shadow_wake(.03)

anim_save('9-8.gif', p, duration = 30 , fps = 10)


#aniamtions
p <- ggplot()+
  #geom_sf(data = builds, fill = 'grey', color = "NA")+
  geom_sf(data = smool)+
  labs(title = 'time = {frame_time}', subtitle = 'speed = ')+
  theme( panel.background=element_blank())+
  transition_time(id)+
  ease_aes('linear')+
  view_follow(exclude_layer = c(1))

gganimate::animate(p, duration = 10, fps = 100)


ggplot(back, aes(x = id, y = speed))+
  geom_line()+
  labs(title = 'time = {frame_time}', subtitle = '{back %>% filter(id == frame_time) %>% pull(speed)}')+
  theme( panel.background=element_blank())+
  transition_time(id)+
  ease_aes('linear') 

