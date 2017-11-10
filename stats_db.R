library(DBI)
library(tidyverse)
library(countrycode)
library(grid)


decode_ru<-function(code) { return(gsub(' ',"\n",countrycode(code,'iso2c',"country.name.ru")) ) }

theme_set(theme_bw(base_size = 14, base_family = "Arial"))


migration <- dbConnect(RSQLite::SQLite(), "orcid_2016.sqlite3")

# query by country
by_country<-dbGetQuery(migration, 'select country,count(*) as count from person_last_education group by 1 order by 2 desc limit 10')

by_country<-by_country %>% mutate(country_ru = decode_ru(country))

png('top_10_country.png',width=800,height=400)
ggplot(by_country,aes(x=country_ru,y=count))+geom_col()+ggtitle('все выпускники по странам')+ylab('')+xlab('')


by_country_phd<-dbGetQuery(migration, 'select country,count(*) as count from person_last_education where is_phd=1 group by 1 order by 2 desc limit 10')
by_country_phd<-by_country_phd %>% mutate(country_ru = decode_ru(country))

png('top_10_country_phd.png',width=800,height=400)
ggplot(by_country_phd,aes(x=country_ru,y=count))+geom_col()+ggtitle('PhD по странам')+ylab('')+xlab('')

destination_by_country<-dbGetQuery(migration, "select o.country as origin,l.country as destination, count(*) as count from person_last_education as o left join person_last_location as l on l.orcid_id=o.orcid_id where o.country in ('RU','CN','GB') and o.is_phd=1 group by 1,2")

origin_by_country<-dbGetQuery(migration, "select o.country as origin,l.country as destination, count(*) as count from person_last_education as o left join person_last_location as l on l.orcid_id=o.orcid_id where  l.country in ('RU','CN','GB') and o.is_phd=1 group by 1,2")

by_origin_total <- destination_by_country  %>% 
    group_by(origin) %>% summarize(total=sum(count))

    
    
totals <- destination_by_country  %>% 
    group_by(destination) %>% summarize(total=sum(count)) %>% 
    arrange(desc(total))

country_by_destination_top_10 <- head(totals,n=10)$destination

destination_by_country_n <- destination_by_country %>% 
            left_join(by_origin_total,by=c('origin')) %>% 
            filter(destination %in% country_by_destination_top_10) %>%
            mutate(frac=100*count/total,origin=as.factor(origin),destination=as.factor(destination)) 

by_destination_total <- origin_by_country  %>% 
    group_by(destination) %>% summarize(total=sum(count))
            
totals <- origin_by_country  %>% 
    group_by(origin) %>% summarize(total=sum(count)) %>% 
    arrange(desc(total))

country_by_origin_top_10 <- head(totals,n=10)$origin

origin_by_country_n <- origin_by_country %>% 
            left_join(by_destination_total,by=c('destination')) %>% 
            filter(origin %in% country_by_origin_top_10) %>%
            mutate(frac=100*count/total,origin=as.factor(origin),destination=as.factor(destination)) 

            
png('by_destination_phd.png',width=800,height=900)
ggplot(destination_by_country_n, aes(x=decode_ru(destination), y=frac))+
  geom_col()+facet_grid(decode_ru(origin)~.) + ggtitle('PhD по странам убытия')+ylab('')+xlab('')

png('by_origin_phd.png',width=800,height=900)
ggplot(origin_by_country_n, aes(x=decode_ru(origin), y=frac))+
  geom_col()+facet_grid(decode_ru(destination)~.) + ggtitle('PhD по странам прибытия')+ylab('')+xlab('')

  
q()  
by_destination_total <- origin_by_country  %>% 
    group_by(destination) %>% summarize(total=sum(count))

