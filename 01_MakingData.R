rm(list=ls())
########################
#Llibreries 
########################
library(dplyr)
library(tibble)
library(sp)
library(INLA)
library(maptools) 
library(rmapshaper)
library(rgdal)
library(vroom)
library(spdep)
library(tidyr)
library(openxlsx)
#--------------------

#------------Cases by ABS--------------

dat <- vroom("dat.csv")%>% 
  rename(ABS=absdescripcio,CODIABS=abscodi,SEXE=sexedescripcio,TIPUS=resultatcoviddescripcio) %>% 
  #Hi ha missings de ABS, els treiem pel mapa per ABS (si volguéssim calcular el total els hauríem d'incloure)
  subset(ABS!="No classificat" & TIPUS!="Sospitós") %>% 
  as.data.frame()

#Code to download automatically updated data. To run the following code 
#a Socrata account is needed. One can sign up free in: https://support.socrata.com/hc/en-us

# library(RSocrata)
# dat <- read.socrata(
#   "https://analisi.transparenciacatalunya.cat/resource/xuwf-dxjd.json",
#   email     = YOUREMAIL
#   password  = YOURPASSWORD
# ) %>% 
#   rename(ABS=absdescripcio,CODIABS=abscodi,SEXE=sexedescripcio,TIPUS=resultatcoviddescripcio) %>% 
#   subset(ABS!="No classificat" & TIPUS!="Sospitós") %>% 
#   mutate(data=as.Date(data,format="%d/%m/%Y")+1,
#          numcasos=as.integer(numcasos)) %>%
#   as.data.frame()
#----------------

#------Geography of the map by ABS-----
shapefileT <- rgdal::readOGR("ABS_2018/ABS_2018.shp",stringsAsFactors = FALSE,encoding = "UTF-8",use_iconv = T)

#Bind Montcada i Reixac 1 and 2:

#Modify el @data because the rows of shapefileT@data have to correspond to the ID's of spatial polygons
shapefileT_data<-shapefileT@data %>%
  subset(CODIABS!=382) %>%
  mutate(CODIABS=ifelse(CODIABS==381,302,CODIABS))
shapefileT_data<-rbind(shapefileT_data[shapefileT_data$CODIABS!=302,],shapefileT_data[shapefileT_data$CODIABS=="302",])
shapefileT_data$OBJECTID<-1:dim(shapefileT_data)[1]
rownames(shapefileT_data)<-shapefileT_data$OBJECTID

shapefileT_mont<-raster::aggregate(shapefileT[grep("Montcada i Reixac",shapefileT$NOMABS),],dissolve=T)
shapefileT<-shapefileT[!grepl("Montcada i Reixac",shapefileT$NOMABS),]
shapefileT <- spChFIDs(shapefileT, as.character(1:dim(shapefileT@data)[1]))
shapefileT_mont <- spChFIDs(shapefileT_mont, as.character(dim(shapefileT@data)[1]+1))
shapefileT<-spRbind(shapefileT,shapefileT_mont)

shapefileT<-SpatialPolygonsDataFrame(shapefileT,shapefileT_data)

#Let's add Castellbisbal in Martorell

shapefileT_data<-shapefileT@data %>%
  subset(CODIABS!=399)

shapefileT_data<-rbind(shapefileT_data[shapefileT_data$CODIABS!=149,],shapefileT_data[shapefileT_data$CODIABS==149,])
shapefileT_data$OBJECTID<-1:dim(shapefileT_data)[1]
rownames(shapefileT_data)<-shapefileT_data$OBJECTID

shapefileT_mart<-raster::aggregate(shapefileT[shapefileT$CODIABS%in%c(399,149),],dissolve=T)
shapefileT<-shapefileT[!shapefileT$CODIABS%in%c(399,149),]
shapefileT <- spChFIDs(shapefileT, as.character(1:dim(shapefileT@data)[1]))
shapefileT_mart <- spChFIDs(shapefileT_mart, as.character(dim(shapefileT@data)[1]+1))
shapefileT<-spRbind(shapefileT,shapefileT_mart)

shapefileT<-SpatialPolygonsDataFrame(shapefileT,shapefileT_data)

names(shapefileT@data)[ncol(shapefileT@data)-1]<-"ÀREA"

sum<-dat %>%
  group_by(ABS) %>%
  summarise(CODIABS=unique(CODIABS))

shapefileT@data<-merge(shapefileT@data %>% dplyr::select(-NOMABS),sum,by="CODIABS",all.x=TRUE)

shapefileT<-spTransform(shapefileT, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

shapefileT@data<-shapefileT@data %>%
  arrange(OBJECTID)

shapefileT@data[c("Coord_X","Coord_Y")]<-coordinates(shapefileT)

#-----------------

#---------Cases by age----------
dat_edat <- vroom("dat_edat.csv")%>% 
  rename(SEXE=sexedescripcio,edat=edatrang,TIPUS=resultatcoviddescripcio) %>% 
  #Hi ha missings de ABS, els treiem pel mapa per ABS (si volguéssim calcular el total els hauríem d'incloure)
  subset(TIPUS!="Sospitós") %>% 
  as.data.frame()

#Code to download automatically updated data. To run the following code 
#a Socrata account is needed. One can sign up free in: https://support.socrata.com/hc/en-us
# 
# dat_edat <- read.socrata(
#   "https://analisi.transparenciacatalunya.cat/resource/qwj8-xpvk.json",
#   email     = "psatorra@idibell.cat",
#   password  = "Pau.satorra24"
# ) %>% 
#   rename(SEXE=sexedescripcio,edat=edatrang,TIPUS=resultatcoviddescripcio) %>% 
#   #Hi ha missings de ABS, els treiem pel mapa per ABS (si volguéssim calcular el total els hauríem d'incloure)
#   subset(TIPUS!="Sospitós") %>% 
#   mutate(data=as.Date(data,format="%d/%m/%Y")+1,
#          numcasos=as.integer(numcasos)) %>%
#   as.data.frame()
#----------------

#----- Population by ABS-----
pob_abs<-vroom("Registre_central_de_poblaci__del_CatSalut.csv",delim=",") %>%
  subset(any==2020) %>%
  dplyr::select(CODIABS="codi Àrea Bàsica de Saut",SEXE="gènere",edat,total="població oficial")

pob_abs$edat<-cut(pob_abs$edat,breaks=c(min(pob_abs$edat),seq(10,90,10),max(pob_abs$edat)),labels=unique(dat_edat$edat),right=FALSE,include.lowest=TRUE)
pob_abs<-pob_abs %>%
  group_by(SEXE,CODIABS,edat) %>%
  summarise(total=sum(total))
#-----------------------

#Compile the code with the functions needed to estimate the results: 

source("models.R")

#------------RESULTS CALCULATION---------------------

#Cumulative results for the last day of each period:

#-------1st wave--------------
sdatT<-dat %>% subset(data<=as.Date("2020-06-23"))
#Total
dat_casos1<-sir(sdatT)%>% 
  mutate(reactive=1)
sum_casos1<-smooth_sir(dat_casos1)%>% 
  mutate(reactive=1)
#Last 7 days
sdat<-sdatT %>% subset(data>max(data)-7)
dat_casos2<-sir(sdat)%>% 
  mutate(reactive=2)
sum_casos2<-smooth_sir(dat_casos2)%>% 
  mutate(reactive=2)
#Last 14 days
sdat<-sdatT %>% subset(data>max(data)-14)
dat_casos3<-sir(sdat)%>% 
  mutate(reactive=3)
sum_casos3<-smooth_sir(dat_casos3)%>% 
  mutate(reactive=3)

#-------2nd wave-------------
#Total
sdatT<-dat %>% subset(data>as.Date("2020-06-23"))
dat_casos4<-sir(sdatT)%>% 
  mutate(reactive=4)
sum_casos4<-smooth_sir(dat_casos4)%>% 
  mutate(reactive=4)
#Last 7 days
sdat<-sdatT %>% subset(data>max(data)-7)
dat_casos5<-sir(sdat)%>% 
  mutate(reactive=5)
sum_casos5<-smooth_sir(dat_casos5)%>% 
  mutate(reactive=5)
#Last 14 days
sdat<-sdatT %>% subset(data>max(data)-14)
dat_casos6<-sir(sdat)%>% 
  mutate(reactive=6)
sum_casos6<-smooth_sir(dat_casos6)%>% 
  mutate(reactive=6)

dat_casos<-rbind(dat_casos1,dat_casos2,dat_casos3,dat_casos4,dat_casos5,dat_casos6)
sum_casos<-rbind(sum_casos1,sum_casos2,sum_casos3,sum_casos4,sum_casos5,sum_casos6) 

#Evolution of results by each period:

dat<-dat %>% arrange(data)
#-------1st wave--------------
sdatT<-dat %>% subset(data<=as.Date("2020-06-23"))

#Total
dates<-unique(sdatT$data)
dat_evo1<-NULL
#We leave two weeks of margin from the first date to begin calculation:
for(i in 14:length(dates)){
  sdat<-sdatT %>% subset(data<=dates[i])
  sdat_casos<-sir(sdat) 
  ssum_casos<-smooth_sir(sdat_casos) 
  
  add<-ssum_casos %>%
    tibble::add_column(data=dates[i],
                       .before="ABS")
  
  dat_evo1<-rbind(dat_evo1,add)
}
dat_evo1$reactive<-1

#Last 7 days
sdates<-dates[dates>max(dates)-7]
dat_evo2<-NULL
for(i in 1:length(sdates)){
  sdat<-sdatT %>% subset(data>(sdates[i]-7) & data<=sdates[i])
  sdat_casos<-sir(sdat) 
  ssum_casos<-smooth_sir(sdat_casos) 
  
  add<-ssum_casos %>%
    tibble::add_column(data=sdates[i],
                       .before="ABS")
  dat_evo2<-rbind(dat_evo2,add)
}
dat_evo2$reactive<-2

#Last 14 days
sdates<-dates[dates>max(dates)-14]
dat_evo3<-NULL
for(i in 1:length(sdates)){
  sdat<-sdatT %>% subset(data>(sdates[i]-14) & data<=sdates[i])
  sdat_casos<-sir(sdat) 
  ssum_casos<-smooth_sir(sdat_casos) 
  
  add<-ssum_casos %>%
    tibble::add_column(data=sdates[i],
                       .before="ABS")
  
  dat_evo3<-rbind(dat_evo3,add)
}
dat_evo3$reactive<-3


#-------2nd wave--------------
sdatT<-dat %>% subset(data>as.Date("2020-06-23"))
#Total
dates<-unique(sdatT$data)
dat_evo4<-NULL
#We leave two weeks of margin from the first date to begin calculation:
for(i in 14:length(dates)){
  sdat<-sdatT %>% subset(data<=dates[i])
  sdat_casos<-sir(sdat) 
  ssum_casos<-smooth_sir(sdat_casos) 
  
  add<-ssum_casos %>%
    tibble::add_column(data=dates[i],
                       .before="ABS")
  
  dat_evo4<-rbind(dat_evo4,add)
}
dat_evo4$reactive<-4

#Last 7 days
sdates<-dates[dates>max(dates)-7]
dat_evo5<-NULL
for(i in 1:length(sdates)){
  sdat<-sdatT %>% subset(data>(sdates[i]-7) & data<=sdates[i])
  sdat_casos<-sir(sdat) 
  ssum_casos<-smooth_sir(sdat_casos) 
  
  add<-ssum_casos %>%
    tibble::add_column(data=sdates[i],
                       .before="ABS")
  
  dat_evo5<-rbind(dat_evo5,add)
}
dat_evo5$reactive<-5

#Last 14 days
sdates<-dates[dates>max(dates)-14]
dat_evo6<-NULL
for(i in 1:length(sdates)){
  sdat<-sdatT %>% subset(data>(sdates[i]-14) & data<=sdates[i])
  sdat_casos<-sir(sdat) 
  ssum_casos<-smooth_sir(sdat_casos) 
  
  add<-ssum_casos %>%
    tibble::add_column(data=sdates[i],
                       .before="ABS")
  
  dat_evo6<-rbind(dat_evo6,add)
}
dat_evo6$reactive<-6

dat_evo<-rbind(dat_evo1,dat_evo2,dat_evo3,dat_evo4,dat_evo5,dat_evo6)

#Calculation for the total of Catalonia:

tot_evo<-tot(dat_evo) 

tot_evo<-as.data.frame(tot_evo)

#-------------------------

#---------------------Save datasets----------------
#Datasets that don't need to be updated:

openxlsx::write.xlsx(x=unique(dat$ABS),file="APP/llista_abs.xlsx")
#MAP:
shapefileT<-shapefileT %>% ms_simplify(keep=0.05,keep_shapes=TRUE)
save(shapefileT, file="APP/shapefileT.Rda")
#POPULATION:
save(pob_abs, file="APP/pob_abs.Rda")

#Datasets that need to be updated daily:

save(dat_casos, file="APP/dat_casos.Rda")
save(sum_casos, file="APP/sum_casos.Rda")

save(dat_evo, file="APP/dat_evo.Rda")
save(tot_evo, file="APP/tot_evo.Rda")

#----------------
