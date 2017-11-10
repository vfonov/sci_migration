library(DBI)
library(tidyverse)
library(countrycode)
library(grid)
library(plotly)

source('plotly_par.R')

decode_ru<-function(code) { return(gsub(' ',"\n",countrycode(code,'iso2c','country.name.ru')) ) }
to_iso3c<-function(code)  { return(countrycode(code,'iso2c','iso3c')) }

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

destination_by_country<-dbGetQuery(migration, "select l.country as destination, count(*) as count from person_last_education as o left join person_last_location as l on l.orcid_id=o.orcid_id where o.country='RU' and o.is_phd=1 group by 1")

origin_by_country<-dbGetQuery(migration, "select o.country as origin, count(*) as count from person_last_education as o left join person_last_location as l on l.orcid_id=o.orcid_id where l.country='RU' and o.is_phd=1 group by 1")

total_from_ru <- destination_by_country  %>% summarize(n=sum(count))
fraction_remained <- destination_by_country %>% 
    filter(destination=='RU') %>% mutate(frac=round(100*count/total_from_ru$n,2))

destination_by_country_n <- destination_by_country %>% filter(destination!='RU') %>%
            mutate(frac=round(100*count/total_from_ru$n,3),destination3c=to_iso3c(destination),COUNTRY=decode_ru(destination)) 

total_to_ru <- origin_by_country  %>%  summarize(n=sum(count))
fraction_local <- origin_by_country %>% 
    filter(origin=='RU') %>% mutate(frac=round(100*count/total_to_ru$n,2))
            
origin_by_country_n <- origin_by_country %>% filter(origin!='RU') %>%
            mutate(frac=round(100*count/total_to_ru$n,3),origin3c=to_iso3c(origin),COUNTRY=decode_ru(origin)) 


# specify map projection/options
p <- plot_geo(destination_by_country_n) %>%
  add_trace(
    z = ~frac, color = ~frac, colorscale  = 'Bluered',
    text = ~COUNTRY, locations = ~destination3c, marker = list(line = list(color = toRGB("grey"), width = 0.5))
  ) %>%
  colorbar(title = 'Отбыло', ticksuffix = '%') %>%
  layout(
    title = paste('PhD по странам убытия, осталось ',fraction_remained$frac,'%'),
    geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'Mercator')
        )
  )
api_create(p, filename = "phds_from_russia")


p <- plot_geo(origin_by_country_n) %>%
  add_trace(
    z = ~frac, color = ~frac, colorscale  = 'Bluered',
    text = ~COUNTRY, locations = ~origin3c, marker = list(line = list(color = toRGB("grey"), width = 0.5))
  ) %>%
  colorbar(title = 'Прибыло', ticksuffix = '%') %>%
  layout(
    title = paste('PhD по странам прибытия, местных ',fraction_local$frac,'%'),
    geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'Mercator')
        )
  )
api_create(p, filename = "phds_to_russia")


png('by_destination_phd.png',width=800,height=900)
ggplot(destination_by_country_n, aes(x=decode_ru(destination), y=frac))+
  geom_col()+facet_grid(decode_ru(origin)~.) + ggtitle('PhD по странам убытия')+ylab('')+xlab('')

png('by_origin_phd.png',width=800,height=900)
ggplot(origin_by_country_n, aes(x=decode_ru(origin), y=frac))+
  geom_col()+facet_grid(decode_ru(destination)~.) + ggtitle('PhD по странам прибытия')+ylab('')+xlab('')


left_country<-dbGetQuery(migration, "select o.country as country,count(distinct o.orcid_id) as departed from person_last_education as o left join person_last_location as l on l.orcid_id=o.orcid_id where o.country!=l.country and o.is_phd=1 group by 1")

arrived_to_country<-dbGetQuery(migration, "select l.country as country, count(distinct l.orcid_id) as arrived from person_last_education as o left join person_last_location as l on l.orcid_id=o.orcid_id  where o.country!=l.country and o.is_phd=1 group by 1")

by_country<-dbGetQuery(migration, 'select l.country as country, count(distinct l.orcid_id) as total from person_last_education as o left join person_last_location as l on l.orcid_id=o.orcid_id  where o.is_phd=1 group by 1')

balance_by_country <- by_country %>% 
        left_join(left_country,by='country') %>% 
        left_join(arrived_to_country,by='country') %>%
        mutate(arrived=ifelse(is.na(arrived),0,arrived),departed=ifelse(is.na(departed),0,departed)) %>%
        mutate(balance=round(100*(arrived-departed)/total,2)) %>% 
        filter(total>10) %>% 
        mutate(country3c=to_iso3c(country),country.ru=decode_ru(country))
        

p <- plot_geo(balance_by_country) %>%
  add_trace(
    z = ~balance, color = ~balance, colorscale  = 'Bluered',
    text = ~country.ru, locations = ~country3c, marker = list(line = list(color = toRGB("grey"), width = 0.5))
  ) %>%
  colorbar(title = 'Баланс', ticksuffix = '%',limits=c(-100,100)) %>%
  layout(
    title = 'PhD (приехало-уехало)/осталось',
    geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'Mercator')
        )
  )
api_create(p, filename = "phds_balance")
  
  
mipt_graduates<-dbGetQuery(migration, "select country,count(*) as count from person_education_final where code='MIPT' group by 1") %>%
          mutate(country3c=to_iso3c(country),country.ru=decode_ru(country)) 

p <- plot_geo(mipt_graduates) %>%
  add_trace(
    z = ~count, color = ~count, colorscale  = 'Bluered',
    text = ~country.ru, locations = ~country3c, marker = list(line = list(color = toRGB("grey"), width = 0.5))
  ) %>%
  colorbar(title = 'Физтехи') %>%
  layout(
    title = 'Количество физтехов по странам ',
    geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'Mercator')
        )
  )
api_create(p, filename = "mipt_migration")

# let's see what happend with MIPT graduates  
