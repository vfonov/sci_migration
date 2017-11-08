library(tidyverse)
#library(ggthemes)

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

 
# let's see proportions of people who left by year
f<-read.csv('person_education_final_grad.csv')

f<-f %>% mutate(left= (code=='MIT'&country!='US') | (code!='MIT'&country!='RU'))

total<-f%>%group_by(code,graduated) %>% summarise(count=n())
staid<-f%>%filter(!left)%>%group_by(code,graduated) %>% summarise(count=n())
prop<-left_join(total,staid,by=c('code','graduated')) %>% 
      rename(total=count.x,staid=count.y) %>% mutate( remain=staid/total) %>%
      filter(graduated>1970 & graduated<2017)

png("patriots.png",width=800,height=800)

ggplot(prop,aes(x=graduated,y=remain))+
  geom_line()+facet_wrap(~code)+
  theme_bw()+
   theme(
     axis.text  = element_text(vjust = 0.2, size = 18),
     axis.title = element_text(face = 'bold', vjust = 0.2, size = 18),
     plot.title = element_text(face = 'bold', vjust = 2.0, size = 20),
     strip.text = element_text(face = 'bold',  size = 18),
     plot.margin = unit(c(1.0,0.2,0.2,0.2), "cm")
     )
