## Analysis of ORCID migration data for MIPT

Idea from the article in [Science:"Vast set of public CVs reveals the world’s most migratory
scientists"](http://www.sciencemag.org/news/2017/05/vast-set-public-cvs-reveals-world-s-most-migratory-scientists)

# based on data that comes from "Introducing ORCID"

Original data from https://doi.org/10.5061/dryad.48s16

# Prerequisites

* Required R packages: 
 * for flat graphs: `tidyverse`  
 * for world map view: `c('maptools','mapproj','rgeos','rgdal','jsonlite','RCurl','RColorBrewer')`

 
# Results
 
* Scientists from 4 universities population by country
 ![](https://user-images.githubusercontent.com/628822/32561690-e3b02622-c47a-11e7-95eb-9c9797308653.png "By Country")
 
* Percent of scientists who remained in the home country
 ![](https://user-images.githubusercontent.com/628822/32561688-e3662cf2-c47a-11e7-923e-23bdd4fd4bfc.png "Remained")
 
 
* Top 10 destination countries
 ![](https://user-images.githubusercontent.com/628822/32561689-e383b39e-c47a-11e7-8732-b27d6bfa3399.png "Top 10 destinations")
 