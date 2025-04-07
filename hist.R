# INDICE DE DESENV. AMBIENTAL - IDA

# Meio Aquático

# Carregamento dos Dados
library(readxl)
dados <- read_excel("Arq1.xlsx", sheet = 6) 
dados

library(ggplot2)
library(plyr) # Desativá-lo depois
library(ggthemes)

# Histogramas de Temp:##########################################################
mean_profun <- ddply(dados,"Profun.m",summarise,grp.mean=mean(Temp))
mean_profun

ggplot(dados, aes(x = Temp))+
  geom_histogram(aes(y = ..density.., fill = Profun.m),color="gray",bins = 10) +
  geom_vline(data=mean_profun,aes(xintercept = grp.mean), color = "red",
             linetype="dashed", size=1) +
  labs(x = "Temperatura (ºC)", y = "Frequência",
       fill = "Profun.m", color = "Profun.m") +
  stat_density(geom = "line",color="red",size=0.7) +
  facet_wrap(~Profun.m, ncol = 5, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")  # Remove a legenda

# Histogramas de Condutibilidade ###############################################
mean_profun <- ddply(dados,"Profun.m",summarise,grp.mean=mean(Condut))
mean_profun

ggplot(dados, aes(x = Condut))+
  geom_histogram(aes(y = ..density.., fill = Profun.m),color="gray",bins = 10) +
  geom_vline(data=mean_profun,aes(xintercept = grp.mean), color = "red",
             linetype="dashed", size=1) +
  labs(x = "Condutividade (x Mil MSiemens/cm)", y = "Frequência",
       fill = "Profun.m", color = "Profun.m") +
  stat_density(geom = "line",color="red",size=0.7) +
  scale_y_continuous(labels=scales::scientific_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ",",
                                                  scale = 1e-3))+
  facet_wrap(~Profun.m, ncol = 5, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")  # Remove a legenda

# Histogramas de Condutância Específica ########################################
mean_profun <- ddply(dados,"Profun.m",summarise,grp.mean=mean(Cond.esp))
mean_profun

ggplot(dados, aes(x = Cond.esp))+
  geom_histogram(aes(y = ..density.., fill = Profun.m),color="gray",bins = 10) +
  geom_vline(data=mean_profun,aes(xintercept = grp.mean), color = "red",
             linetype="dashed", size=1) +
  labs(x = "Condutância Esp. (MSiemens/cm)", y = "Frequência",
       fill = "Profun.m", color = "Profun.m") +
  stat_density(geom = "line",color="red",size=0.7) +
  scale_y_continuous(labels=scales::scientific_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ",",
                                                  scale = 1e-3))+
  facet_wrap(~Profun.m, ncol = 5, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")  # Remove a legenda

# Histogramas de Salinidade ####################################################
mean_profun <- ddply(dados,"Profun.m",summarise,grp.mean=mean(Salin))
mean_profun

ggplot(dados, aes(x = Salin))+
  geom_histogram(aes(y = ..density.., fill = Profun.m),color="gray",bins = 10) +
  geom_vline(data=mean_profun,aes(xintercept = grp.mean), color = "red",
             linetype="dashed", size=1) +
  labs(x = "Salinidade (Practical Salinity Scale)", y = "Frequência",
       fill = "Profun.m", color = "Profun.m") +
  stat_density(geom = "line",color="red",size=0.7) +
  scale_y_continuous(labels=scales::scientific_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ",",
                                                  scale = 1e-3))+
  facet_wrap(~Profun.m, ncol = 5, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")  # Remove a legenda

# Histogramas de Propag do Som #################################################
mean_profun <- ddply(dados,"Profun.m",summarise,grp.mean=mean(Vel.som))
mean_profun

ggplot(dados, aes(x = Vel.som))+
  geom_histogram(aes(y = ..density.., fill = Profun.m),color="gray",bins = 10) +
  geom_vline(data=mean_profun,aes(xintercept = grp.mean), color = "red",
             linetype="dashed", size=1) +
  labs(x = "Vel. Som (m/s)", y = "Frequência",
       fill = "Profun.m", color = "Profun.m") +
  stat_density(geom = "line",color="red",size=0.7) +
  scale_y_continuous(labels=scales::scientific_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ",",
                                                  scale = 1e-3))+
  facet_wrap(~Profun.m, ncol = 5, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")  # Remove a legenda

# Histogramas de Densidade #####################################################
mean_profun <- ddply(dados,"Profun.m",summarise,grp.mean=mean(Dens))
mean_profun

ggplot(dados, aes(x = Dens))+
  geom_histogram(aes(y = ..density.., fill = Profun.m),color="gray",bins = 10) +
  geom_vline(data=mean_profun,aes(xintercept = grp.mean), color = "red",
             linetype="dashed", size=1) +
  labs(x = "Densidade (kg/m³)", y = "Frequência",
       fill = "Profun.m", color = "Profun.m") +
  stat_density(geom = "line",color="red",size=0.7) +
  scale_y_continuous(labels=scales::scientific_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ",",
                                                  scale = 1e-3))+
  facet_wrap(~Profun.m, ncol = 5, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")  # Remove a legenda

# Dsabilitando o 'plyr'
detach("package:plyr", unload = TRUE)



