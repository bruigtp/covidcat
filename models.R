###################################################
#File with the functions that generate the results:
###################################################

#------------SIR-----------------

#Function to calculate given a dataset of cases in a period the standard incidence ratio (sir):
sir<-function(dat){
  #-----------Case ratio of total POPULATION by age----------------
  dat_edat_pob<-dat_edat%>%
    subset(dat_edat$data%in%dat$data) %>% 
    group_by(SEXE,edat) %>%
    summarise(numcasos=sum(numcasos))
  
  sumpob<-pob_abs %>% 
    group_by(SEXE,edat) %>% 
    summarise(total=sum(total)) 
  
  sumpob<-merge(sumpob,dat_edat_pob,by=c("SEXE","edat"),all=TRUE) %>%
    mutate_if(is.numeric,~replace_na(.x,0)) %>% 
    mutate(resp=numcasos/total) %>% 
    dplyr::select(SEXE,edat,resp)
  
  #------------Expected and population by ABS----------------
  sumpob_abs<-merge(pob_abs,sumpob,by=c("SEXE","edat"),all=TRUE) %>% 
    mutate(esp=total*resp) %>% 
    group_by(SEXE,CODIABS) %>% 
    summarise(total=sum(total),esp=sum(esp))
  
  
  #-----------RAW TAXES AND POPULATION BY ABS----------------
  dat<-dat %>% group_by(ABS,SEXE) %>% summarise(casos=sum(numcasos)) %>% as.data.frame()
  #We put as NA the ABS that don't have reported cases in dat
  dat_mapa<-as.data.frame(rbind(cbind(ABS=shapefileT$ABS,CODIABS=shapefileT$CODIABS,SEXE="Dona"),cbind(ABS=shapefileT$ABS,CODIABS=shapefileT$CODIABS,"Home"))) %>% mutate(borrar=1)
  
  dat<-merge(dat,dat_mapa,by=c("ABS","SEXE"),all=T) %>% dplyr::select(-borrar)
  
  dat<-dat %>% arrange(SEXE,ABS)
 
  #-------------------------SIR----------------------

  dat<-merge(dat,sumpob_abs,by=c("CODIABS","SEXE"),all.x=TRUE) %>% 
    mutate(esp=round(esp,2)) %>% 
    dplyr::select(-CODIABS) %>% 
    as.data.frame()
  
  dat
  
}

#--------------------------- 

#-----------SMOOTH SIR--------------

#Parameters needed for INLA modeling
nb <- poly2nb(shapefileT)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
shapefileT$re_u <- 1:nrow(shapefileT@data)
sdunif = "expression: logdens=log(0.5)-log_precision/2; return(logdens);"

#Function to calculate given a dataset of the cumulative cases of a period and their respective SIR, the smooth SIR and the smooth tax with INLA models:

smooth_sir<-function(dat){
  
  sum_t<-dat %>% 
    group_by(ABS) %>%
    summarise(casos=sum(casos,na.rm=T),esp=sum(esp,na.rm=T),total=sum(total,na.rm=T),ratio=casos/esp) %>%
    as.data.frame()
  
  sum_h<-dat %>%
    subset(SEXE=="Home") %>% 
    group_by(ABS) %>%
    summarise(casos_h=sum(casos,na.rm=T),esp_h=sum(esp,na.rm=T),total_h=sum(total,na.rm=T),ratio_h=casos_h/esp_h) %>%
    as.data.frame()
  
  sum_d<-dat %>%
    subset(SEXE=="Dona") %>% 
    group_by(ABS) %>%
    summarise(casos_d=sum(casos,na.rm=T),esp_d=sum(esp,na.rm=T),total_d=sum(total,na.rm=T),ratio_d=casos_d/esp_d) %>%
    as.data.frame()
  
  sum<-merge(sum_t,merge(sum_h,sum_d,by="ABS"),by="ABS")%>% 
    mutate_if(is.numeric,~ifelse(is.infinite(.x) | is.nan(.x),NA,.x))
  
  #SMOOTH SIR
  
  map<-shapefileT
  
  order<-map@data$ABS
  map@data<-merge(map@data,sum,by="ABS") %>% mutate(ABS=factor(ABS,levels=order)) %>% arrange(ABS) %>% mutate(ABS=as.character(ABS))

  sum<-sum %>% 
    mutate(ABS=factor(ABS,levels=order)) %>% 
    arrange(ABS) %>% 
    mutate(ABS=as.character(ABS))
  
  formula = casos ~ f(re_u, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)), cdf=c(log(1)))
  
  res <- inla(formula, family="poisson", data=map@data, E=esp, control.compute=list(dic=T, cpo=TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))), control.fixed = list(prec.intercept = 0.01))
  
  sum$RR<-res$summary.fitted.values[,"mean"]
  sum$LL<-res$summary.fitted.values[,"0.025quant"]
  sum$UL<-res$summary.fitted.values[,"0.975quant"]
  sum$p<-res$summary.fitted.values[,grep("cdf",names(res$summary.fitted.values))]  
  
  formula = casos_h ~ f(re_u, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)), cdf=c(log(1)))
  
  res <- inla(formula, family="poisson", data=map@data, E=esp_h, control.compute=list(dic=T, cpo=TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))), control.fixed = list(prec.intercept = 0.01))
  
  sum$RR_h<-res$summary.fitted.values[,"mean"]
  sum$LL_h<-res$summary.fitted.values[,"0.025quant"]
  sum$UL_h<-res$summary.fitted.values[,"0.975quant"]
  sum$p_h<-res$summary.fitted.values[,grep("cdf",names(res$summary.fitted.values))]  
  
  
  formula = casos_d ~ f(re_u, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)), cdf=c(log(1)))
  
  res <- inla(formula, family="poisson", data=map@data, E=esp_d, control.compute=list(dic=T, cpo=TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))), control.fixed = list(prec.intercept = 0.01))
  
  sum$RR_d<-res$summary.fitted.values[,"mean"]
  sum$LL_d<-res$summary.fitted.values[,"0.025quant"]
  sum$UL_d<-res$summary.fitted.values[,"0.975quant"]
  sum$p_d<-res$summary.fitted.values[,grep("cdf",names(res$summary.fitted.values))]  
  
  #SMOOTH TAX:
  
  formula = casos ~ f(re_u, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)), cdf=c(log(0.005),log(0.0025)))
  
  res <- inla(formula, family="poisson", data=map@data, E=total, control.compute=list(dic=T, cpo=TRUE), control.predictor=list(compute=TRUE, cdf=c(log(0.005),log(0.0025))), control.fixed = list(prec.intercept = 0.01))
  
  sum$stax<-res$summary.fitted.values[,"mean"]
  sum$staxL<-res$summary.fitted.values[,"0.025quant"]
  sum$staxU<-res$summary.fitted.values[,"0.975quant"]
  sum$p2<-res$summary.fitted.values[,grep("cdf",names(res$summary.fitted.values))[1]]
  sum$p3<-res$summary.fitted.values[,grep("cdf",names(res$summary.fitted.values))[2]]
  
  ia_cat_h<-sum_h %>% 
    summarise(casos_h=sum(casos_h),total_h=sum(total_h)) %>% 
    mutate(tax_h=casos_h/total_h) %>% 
    pull(tax_h)
  
  formula = casos_h ~ f(re_u, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)), cdf=c(log(ia_cat_h)))
  
  res <- inla(formula, family="poisson", data=map@data, E=total_h, control.compute=list(dic=T, cpo=TRUE), control.predictor=list(compute=TRUE, cdf=c(log(ia_cat_h))), control.fixed = list(prec.intercept = 0.01))
  
  sum$stax_h<-res$summary.fitted.values[,"mean"]
  sum$staxL_h<-res$summary.fitted.values[,"0.025quant"]
  sum$staxU_h<-res$summary.fitted.values[,"0.975quant"]
  
    ia_cat_d<-sum_d %>%
    summarise(casos_d=sum(casos_d),total_d=sum(total_d)) %>%
    mutate(tax_d=casos_d/total_d) %>%
    pull(tax_d)
  
  formula = casos_d ~ f(re_u, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)), cdf=c(log(ia_cat_d)))
  
  res <- inla(formula, family="poisson", data=map@data, E=total_d, control.compute=list(dic=T, cpo=TRUE), control.predictor=list(compute=TRUE, cdf=c(log(ia_cat_d))), control.fixed = list(prec.intercept = 0.01))
  
  sum$stax_d<-res$summary.fitted.values[,"mean"]
  sum$staxL_d<-res$summary.fitted.values[,"0.025quant"]
  sum$staxU_d<-res$summary.fitted.values[,"0.975quant"]

  sum %>% 
    arrange(ABS) 
  
}

#------------------


#------------RESULTS FOR THE TOTAL OF CATALONIA----------
#Function that given the evolution of the results by ABS calculates the evolution for the total of Catalonia:

tot<-function(dat_evo){
  
  #ci evo total population
  tot_evo<-dat_evo %>% 
    group_by(reactive,data) %>% 
    summarise(casos=sum(casos),total=sum(total),casos_h=sum(casos_h),total_h=sum(total_h),casos_d=sum(casos_d),total_d=sum(total_d))
  
  tot_evo$Ttax<-NULL
  tot_evo$TtaxL<-NULL
  tot_evo$TtaxU<-NULL
  
  tot_evo$Ttax_h<-NULL
  tot_evo$TtaxL_h<-NULL
  tot_evo$TtaxU_h<-NULL
  
  tot_evo$Ttax_d<-NULL
  tot_evo$TtaxL_d<-NULL
  tot_evo$TtaxU_d<-NULL
  
  for(i in 1:nrow(tot_evo)){
    formula <- casos ~ 1
    res <- inla(formula, family = "poisson", data = tot_evo[i,], E = total, control.predictor = list(compute = TRUE))
    tot_evo$Ttax[i]<-res$summary.fitted.values[,"mean"]
    tot_evo$TtaxL[i]<-res$summary.fitted.values[,"0.025quant"]
    tot_evo$TtaxU[i]<-res$summary.fitted.values[,"0.975quant"]
    #Home
    formula_h <- casos_h ~ 1
    res_h <- inla(formula_h, family = "poisson", data = tot_evo[i,], E = total_h, control.predictor = list(compute = TRUE))
    tot_evo$Ttax_h[i]<-res_h$summary.fitted.values[,"mean"]
    tot_evo$TtaxL_h[i]<-res_h$summary.fitted.values[,"0.025quant"]
    tot_evo$TtaxU_h[i]<-res_h$summary.fitted.values[,"0.975quant"]
    #Dona
    formula_d<- casos_d ~ 1
    res_d <- inla(formula_d, family = "poisson", data = tot_evo[i,], E = total_d, control.predictor = list(compute = TRUE))
    tot_evo$Ttax_d[i]<-res_d$summary.fitted.values[,"mean"]
    tot_evo$TtaxL_d[i]<-res_d$summary.fitted.values[,"0.025quant"]
    tot_evo$TtaxU_d[i]<-res_d$summary.fitted.values[,"0.975quant"]
  }
  
  
  tot_evo %>% 
    as.data.frame()
}

#------------------
