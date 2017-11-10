library(DBI)
library(tidyverse)
library(countrycode)
library(grid)
library('igraph')

decode_ru<-function(code) { return(gsub(' ',"\n",countrycode(code,'iso2c','country.name.ru')) ) }
to_iso3c<-function(code)  { return(countrycode(code,'iso2c','iso3c')) }

theme_set(theme_bw(base_size = 14, base_family = "Arial"))


migration <- dbConnect(RSQLite::SQLite(), "orcid_2016.sqlite3")

# build graph , first extract list of all countries with number of people who studied there as a weigh
countries<-dbGetQuery(migration, 'select country, count(distinct orcid_id) as weight from person_last_education group by 1')
# add country names 
countries <- countries %>% mutate(country3c=to_iso3c(country), country.ru=decode_ru(country),id=country)

# build connectivity matrix, counting directed connection from each country to another country
connections<-dbGetQuery(migration, "select o.country as origin,d.country as destination, count(distinct o.orcid_id) as count from migration as o left join migration as d on o.orcid_id=d.orcid_id  where o.country!=d.country  and d.start_year > o.start_year group by 1,2")

connections <- connections %>% filter(origin %in% countries$country,destination %in% countries$country)

net <- graph_from_data_frame(d=connections, vertices=countries, directed=T) 


# give labels to nodes
V(net)$label<-V(net)$country3c
# give size and weight to all vertices and edges
V(net)$size<-log10(V(net)$weight)*10
E(net)$width <- log10(E(net)$count)
