rm(list=ls(all=TRUE))
  ## Modify Code of to estimate age-specific mortality rates by county in California for 1999--2016. 
  ## Monica Alexander, September 2018
  # load in packages and functions
  library(tidyverse)
  library(rjags)
  library(R2jags)
  require(coda)
  library(ggplot2)
  library(mcmcplots)
  ## Leer los datos con nombre de municipio,codigo DANE, grupo de edad, ano, 
  #muertes,
  #poblacion y cambiar el nombre de cada columna
  options(scipen = 999)
  d <-read_csv2("~/DM2.csv")
  d[,1]<-NULL
  d <- d %>% 
  rename(county = County,
     code = County_Code,
     age_group = Age_Group,
     year = Year,
     deaths = Deaths,
     pop = Population)
# seleccionar algunas ciudades de Colombia
# Codigos DANE ciudades de colombia
  M<-c(11001,5001, 76001,  8001, 13001,  8758, 54001, 68001, 73001, 25754, 
  66001, 50001, 52001, 5088, 47001, 17001, 23001, 20001, 76109, 41001, 
  76520, 19001, 63001,  5360, 68276, 70001, 5266, 76834, 41551, 54128,
  47170, 15469,  5475, 68575, 86001, 50287)
  d <- d %>% 
    filter(code %in% M)
  # Restriccion de grupos de edad
  age_groupt<-c("20-24", "25-29", "30-34", "35-39", "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79",
                "80+")
  d <- d %>% 
    filter(age_group %in% age_groupt)
  # Agrega la columna age con los grupos de edad 0,5,10,...,80 
  # Y agrega la columna mx calculando la tasa de mortalidad de cada en cada fila 
  d <- d %>% 
    rowwise() %>% 
    mutate(age = ifelse(age_group== "0-4", 0, 
           ifelse(age_group == "80+", 80, 
                               as.numeric(strsplit(age_group, "-")[[1]][1]))),
           mx = deaths/pop) 
        
  # Lee los datos de la poblacion y cambia los nombres de las columnas 
  pop <- d %>%
    select(county, code, age_group, year, age, pop)
  #
  # Obtener los datos ?nicos de los grupos de edad, edades, a?os y municipios 
  age_groups <- unique(d$age_group)
  age <- seq(20, 80, by = 5) # ojo mirar esto!!
  years <- unique(d$year)
  counties <- unique(d$county)
  
  # make sure counties overlap across the two datasets
  pop <- pop %>% filter(county %in% counties) 
  ## Poner los datos en el formato de JAGS
  ## Crear los arreglos de dimensi?n age x time x area(municipio) x state (departemento)
  #Para las muertes
  y.xtas <- array(NA, c(length(age_groups), length(years), length(counties), 1))
  #Para la poblacion
  pop.xtas <- array(NA, c(length(age_groups), length(years), length(counties), 1))
  #
  ## Llenar los arreglos 
  #Primero selecciona de cada ano del conjunto de datos del municipio, el grupo de edad y el numero de muertes
  #y los guarda en this_value
  #Luego selecciona de cada a?o del conjunto de datos pop el municipo, el grupo de edad y la poblacion
  #y los guarda en this_pop
  #Despues llena los arreglos y.xtas y pop.xtas con la informacion para cada uno de los municipios
  for(j in 1:length(years)){
    this_value <- as.matrix(d %>% filter(year == years[j]) %>% select(county, age, deaths) %>% spread(county, deaths) %>% select(-age))
    this_pop <- as.matrix(pop %>% filter(year == years[j]) %>% select(county, age, pop) %>% spread(county, pop) %>% select(-age))
    these_counties <- colnames(this_value)
    these_counties_pop <- colnames(this_pop)
    for(k in 1:length(these_counties)){
      y.xtas[,j,which(counties==these_counties[k]),1] <- this_value[,k]
      }
    for(l in 1:length(these_counties_pop)){
      pop.xtas[,j,which(counties==these_counties_pop[l]),1] <- this_pop[,l]
      }
  }
  
  # Cargar los componentes principales calculados con la descomposicion SVD
  pcs <- read.csv("~/LAC_pcs.DM2.4p.csv")[6:18,]
  pcs[,1]<-NULL
  ###Datos que necesita el modelo
  #Las muertes guardadas en el arreglo y.xtas
  #La poblacion guardada en el arreglo pop.xtas
  #Los Yx del modelo que son los componentes principales
  #S el numero de departamentos en este caso 1
  #X el numero de grupos de edad
  #T el numero de a?os considerados
  #n.a y n.amax el numero de municipios
  #P el numero de componentes principales 
  start.time <- Sys.time()
  jags.data <- list(
    y.xtas = y.xtas, 
    pop.xtas = pop.xtas, 
    Yx = pcs,
    S = 1, 
    X= length(age_groups), 
    T = length(years), 
    n.a=length(counties), 
    n.amax=length(counties), P=3)
    
  # Parametros a monitorear 
  parnames <- c("beta.tas", "mu.beta" ,"sigma.beta", "tau.mu", "u.xtas", "mx.xtas")
  ############################################################################
  # Correr el modelo con 30.000 iteraciones
  set.seed(2^29) # Semilla primo de mersene
    mod <- jags(data = jags.data, 
                parameters.to.save=parnames,
                n.burnin = 10000,
                n.iter = 30000,
                model.file = "~/modelo.bn.txt")
    ## Stop the clock          
    end.time <- Sys.time()
    (end.time - start.time)
    # summary del modelo, estimacion de los par?metros de interes
    #mcmc.array <- mod$BUGSoutput$summary
    ## Verificar que los Rhats que es una estadistica para el diagnostico de 
    #la convergencia de las cadenas
    ## es menor a 1.1 (es decir convergen), si no es asi se debe aumentar el 
    #numero de iteraciones por cadena 
    summary(mod$BUGSoutput$summary[,"Rhat"])
    q1<-quantile(mod$BUGSoutput$summary[,"Rhat"], 0.05) 
    q2<-quantile(mod$BUGSoutput$summary[,"Rhat"], 0.975)   
    max(mod$BUGSoutput$summary[,"Rhat"])
    paste("IC 95% para Rhat :(", q1, ";", q2,")")
    boxplot(mod$BUGSoutput$summary[,"Rhat"], horizontal = T)
    
    # graficos de ajuste
    #mcmcplot(mod)
    # Obtener las muestras posteriores
    mcmc.array <- mod$BUGSoutput$sims.array
    ####
    #los mx.xtas son las estimaciones de la tasa de mortalidad, toca sacarles el ln
    estimaciones<-as.data.frame(mod$BUGSoutput$mean$mx.xtas)
    #write_csv(estimaciones, "/home/jjsandoval/Documentos/Demografia/Proyecto_diabetes/Diabetes/Programas/est_mujeres_grandes_Poisson.csv")
    
    # Obtener las mx estimados  y los intervalos de credibilidad al 95% para cada municipio para el 2017
    res <- tibble(age = rep(unique(d$age), each = length(counties)), 
            county = rep(counties, times = length(age_groups)),
            median = rep(NA, length(age_groups)*length(counties)), 
            upper = rep(NA, length(age_groups)*length(counties)), 
            lower = rep(NA, length(age_groups)*length(counties)))
      for(i in 1:length(age_groups)){
        for(j in 1:length(counties)){
          sms <-  c(mcmc.array[,,paste0("mx.xtas[",i,",13,",j,",1]")]  ) # ejemplo 2010 == years[6]
          res$median[res$age==unique(d$age)[i]&res$county==counties[j]] <- median(log(sms))
          res$upper[res$age==unique(d$age)[i]&res$county==counties[j]] <- quantile(log(sms), 0.975)
          res$lower[res$age==unique(d$age)[i]&res$county==counties[j]] <- quantile(log(sms), 0.025)
        }
      }
    #write_csv(res, "~/int_mujeres_grandes_2022_1_Pois.csv")
    
    # plot the results
    lst<-c(1,7,8,12,13,21,36) # seleccion de municipios
    ggplot(res %>% filter(county %in% counties[lst]), aes(age, median))+
      geom_line(aes(color = county)) + 
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = county), alpha = 0.3) + 
      facet_wrap(~county) + 
      geom_point(data = d %>% filter(year==2017, county %in% counties[lst]), 
      aes(age, log(mx), color = county), size = 2) + ylim(-40,0)+ xlim(20,80)+
      theme_bw(base_size = 12) + theme(legend.position="none")+ 
      labs(y="Log tasa de mortalidad", x= "Grupos de Edad") + 
      ggtitle("Estimación Tasa de Mortalidad DM mujeres 2017")
   #setwd("~/graficos_36_ciudades")
   #ggsave("Muj_cidGZ2017_pois.png",  width = 10, height = 6)
    #
    d$mx100m<-d$mx*100000
    write_csv(d, "~/tasas_brutas_mujeres_grandes_pois.csv") 
