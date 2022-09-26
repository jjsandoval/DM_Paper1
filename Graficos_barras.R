library(data.table)
library(ggplot2)
library(dplyr)
tabla<-fread(file.choose(), dec=",")
tabla$ano<-as.character(tabla$ano)

tabla <- tabla %>%
  mutate(modelo = recode(modelo, 'Alexander' = 'AZB', 'Bnegativo' = 'BN', 'Cero Inf' =  'ZIP' ))


# 75-79 anos
# Medellin
ggplot(tabla %>% 
         filter(municipio=="Medellin-Antioquia",
                edad=="75-79"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.2,
                position=position_dodge(.9)) + labs(x= "Años", y="Mediana (ICR95%)")+
  facet_wrap(vars(modelo))+theme(legend.position = "none")


##########################################################################
ggplot(tabla %>% 
         filter(municipio=="Medellin-Antioquia",
                edad=="75-79"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.1,
                position=position_dodge(.9)) +
  geom_point(size = 2)+
  facet_wrap(vars(modelo))+theme(legend.position = "none")+
  labs(x= "Años", y= "Mediana (ICR 95%)")
##########################################################################

# Valledupar-Cesar
ggplot(tabla %>% 
         filter(municipio=="Valledupar-Cesar",
                edad=="75-79"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(vars(modelo))+theme(legend.position = "none")+labs(x= "Años", y= "Mediana (ICR 95%)")

ggplot(tabla %>% 
         filter(municipio=="Valledupar-Cesar",
                edad=="75-79"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.1,
                position=position_dodge(.9)) +
  geom_point(size = 2)+ labs(x= "Años", y= "Mediana (ICR 95%)")+
  facet_wrap(vars(modelo))+theme(legend.position = "none")+labs(x= "Años", y= "Mediana (ICR 95%)")

# Fuente De Oro-Meta
ggplot(tabla %>% 
         filter(municipio=="Fuente De Oro-Meta",
                edad=="75-79"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.2,
                position=position_dodge(.9)) +labs(x= "Años", y= "Mediana (ICR 95%)")+
  facet_wrap(vars(modelo))+theme(legend.position = "none")

ggplot(tabla %>% 
         filter(municipio=="Fuente De Oro-Meta",
                edad=="75-79"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.1,
                position=position_dodge(.9)) +
  geom_point(size = 2)+
  facet_wrap(vars(modelo))+theme(legend.position = "none")+
  labs(x= "Años", y= "Mediana (ICR 95%)")

# 20-24 a?os
# Medellin
ggplot(tabla %>% 
              filter(municipio=="Medellin-Antioquia",
                     edad=="20-24"), 
            aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(vars(modelo))+theme(legend.position = "none")+
  labs(x= "Años", y= "Mediana (ICR 95%)")

ggplot(tabla %>% 
         filter(municipio=="Medellin-Antioquia",
                edad=="20-24"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.1,
                position=position_dodge(.9)) +
  geom_point(size = 2)+
  facet_wrap(vars(modelo))+theme(legend.position = "none")+
  labs(x= "Años", y= "Mediana (ICR 95%)")

# Valledupar-Cesar
ggplot(tabla %>% 
         filter(municipio=="Valledupar-Cesar",
                edad=="20-24"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(vars(modelo))+theme(legend.position = "none")+
  labs(x= "Años", y= "Mediana (ICR 95%)")

ggplot(tabla %>% 
         filter(municipio=="Valledupar-Cesar",
                edad=="20-24"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.1,
                position=position_dodge(.9)) +
  geom_point(size = 2)+
  facet_wrap(vars(modelo))+theme(legend.position = "none")+
  labs(x= "Años", y= "Mediana (ICR 95%)")
  


# Fuente De Oro-Meta
ggplot(tabla %>% 
         filter(municipio=="Fuente De Oro-Meta",
                edad=="20-24"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(vars(modelo))+theme(legend.position = "none")+
  labs(x= "Años", y= "Mediana (ICR 95%)")

ggplot(tabla %>% 
         filter(municipio=="Fuente De Oro-Meta",
                edad=="20-24"), 
       aes(x=ano, y=Mediana, fill=modelo)) + 
  geom_errorbar(aes(ymin=Mediana-LI95, ymax=Mediana+LS95), width=.1,
                position=position_dodge(.9)) +
  geom_point(size = 2)+ ylim(-1,40)+
  facet_wrap(vars(modelo))+theme(legend.position = "none")+
  labs(x= "Años", y= "Mediana (ICR 95%)")
