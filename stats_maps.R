library(tidyverse)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(jsonlite)
library(RCurl)
library(scales)
library(grid)
library(RColorBrewer)

theme_set(theme_bw(base_size = 14, base_family = "Arial"))


f<-read.csv('person_education_final.csv')

# country stats:
country_stats<-f %>% group_by(country) %>% summarize(count=n()) %>% arrange(desc(count))

# take top 20
top_10<-head(country_stats,n=10)$country

# filter out 
ff<-f%>% filter(country %in% top_10)

#ff$country<-droplevels(ff$country)

png("University_by_final_country.png",width=800,height=800)
# plot barplots
ggplot(ff,aes(x=country,color=country,fill=country))+
  geom_bar()+facet_wrap(~code)+scale_y_log10(breaks=c(10,50,100,500,1000))+
  theme_bw()+
   theme(
     axis.text  = element_text(vjust = 0.2, size = 18),
     axis.title = element_text(face = 'bold', vjust = 0.2, size = 18),
     plot.title = element_text(face = 'bold', vjust = 2.0, size = 20),
     strip.text = element_text(face = 'bold',  size = 18),
     plot.margin = unit(c(1.0,0.2,0.2,0.2), "cm")
     )

country_stats_code<-f %>% 
 group_by(country,code) %>% 
 summarize(count=n()) 
 
  
URL <- "https://github.com/nvkelso/natural-earth-vector/raw/v2.0/geojson/ne_50m_admin_0_countries.geojson"
file <- basename(URL)
if( !file.exists(file)) {
    download.file(URL, file) 
}

world <- readOGR(file, "OGRGeoJSON")

# remove antarctica
world <- world[!world$iso_a3 %in% c("ATA"),]
#project(?)
#world <- spTransform(world, CRS("+proj=wintri"))


outline <- bbox(world)
outline <- data.frame(xmin=outline["x","min"],
                      xmax=outline["x","max"],
                      ymin=outline["y","min"],
                      ymax=outline["y","max"])

map <- fortify(world, region='iso_a2')

png("University_by_final_country_map.png",width=1200,height=600)

gg <- ggplot(country_stats_code)
gg <- gg + geom_rect(data=outline, 
                     aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
                     color=1, fill="lightblue", size=0.3)
gg <- gg + geom_map(data=map, map=map,
                    aes(x=long, y=lat, map_id=id, group=group),
                    fill="lightblue", color='black',alpha=0.5)
gg <- gg + geom_map(data=country_stats_code, map=map, color="white", size=0.15,
                    aes(fill=count, group=country, map_id=country))
gg <- gg + scale_fill_gradientn(trans='log10',name="Graduates population",colours = brewer.pal(11, "Spectral"))
gg <- gg + coord_map("mercator")
gg <- gg + facet_wrap(~code)
gg <- gg + theme_bw()
gg <- gg + theme(
     axis.text    = element_text(vjust = 0.2, size = 18),
     axis.title   = element_text(face = 'bold', vjust = 0.2, size = 18),
     plot.title   = element_text(face = 'bold', vjust = 2.0, size = 20),
     strip.text   = element_text(face = 'bold',  size = 18),
     legend.text  = element_text(size = 18,angle = 45, hjust = 1),
     legend.title = element_text(face = 'bold',size=18),
     plot.margin  = unit(c(1.0,0.2,0.2,0.2), "cm")
     )
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())
gg <- gg + theme(legend.position="bottom")
gg <- gg + labs(x=NULL, y=NULL)
gg

