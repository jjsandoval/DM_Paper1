setwd("~/graficos_municipiosg")
res<-fread("~/int_mujeres_GrandesCiud2017.csv")

# plot the results
lst<-1:9 # seleccion de municipios
ggplot(res %>% filter(county %in% counties[lst]), aes(age, median))+
  geom_line(aes(color = county)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = county), alpha = 0.2) + 
  facet_wrap(~county) + 
  geom_point(data = d %>% filter(year==2017, county %in% counties[lst]), 
             aes(age, log(mx), color = county), size = 2) + ylim(-20,0)+ xlim(40,80)+
  theme_bw(base_size = 14) + theme(legend.position="none")+ 
  labs(y="Log tasa de mortalidad", x= "Grupos de Edad") + 
  ggtitle("Estimación Tasa de Mortalidad mujeres 2017")
ggsave("Muj_munG2017_1p.png",  width = 10, height = 6)

# plot the results
lst<-10:18 # seleccion de municipios
ggplot(res %>% filter(county %in% counties[lst]), aes(age, median))+
  geom_line(aes(color = county)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = county), alpha = 0.2) + 
  facet_wrap(~county) + 
  geom_point(data = d %>% filter(year==2017, county %in% counties[lst]), 
             aes(age, log(mx), color = county), size = 2) + ylim(-20,0)+ xlim(40,80)+
  theme_bw(base_size = 14) + theme(legend.position="none")+ 
  labs(y="Log tasa de mortalidad", x= "Grupos de Edad") + 
  ggtitle("Estimación Tasa de Mortalidad mujeres 2017")
ggsave("Muj_munG2017_2p.png",  width = 10, height = 6)