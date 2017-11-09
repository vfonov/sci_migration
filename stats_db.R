library(DBI)
library(tidyverse)

library(grid)


theme_set(theme_bw(base_size = 14, base_family = "Arial"))


migration <- dbConnect(RSQLite::SQLite(), "orcid_2016.sqlite3")

# query by country
by_country<-dbGetQuery(migration, 'select country,count(*) from person_last_education group by 1 order by 2 desc limit 10')

png('top_10_country.png',width=800,height=400)
ggplot(by_country,aes(x=country,y=`count(*)`))+geom_col()

destination_by_country<-dbGetQuery(migration, "select o.country as origin,l.country as destination, count(*) as count from person_last_education as o left join person_last_location as l on l.orcid_id=o.orcid_id where o.country in ('RU','CN','GB') group by 1,2")

origin_by_country<-dbGetQuery(migration, "select o.country as origin,l.country as destination, count(*) as count from person_last_education as o left join person_last_location as l on l.orcid_id=o.orcid_id where l.country in ('RU','CN','GB') group by 1,2")

by_origin_total <- destination_by_country  %>% 
    group_by(origin) %>% summarize(total=sum(count))

totals <- destination_by_country  %>% 
    group_by(destination) %>% summarize(total=sum(count)) %>% 
    arrange(desc(total))

country_by_destination_top_10 <- head(totals,n=10)$destination

destination_by_country_n <- destination_by_country %>% 
            left_join(by_country_total,by=c('origin')) %>% 
            filter(destination %in% by_destination_top_10) %>%
            mutate(frac=100*count/total,origin=as.factor(origin),destination=as.factor(destination)) 
            
ggplot(destination_by_country_n, aes(x=destination, y=frac))+
  geom_col()+facet_grid(~origin) 

  
  
by_destination_total <- origin_by_country  %>% 
    group_by(destination) %>% summarize(total=sum(count))

