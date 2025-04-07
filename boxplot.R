# INDICE DE DESENV. AMBIENTAL - IDA

# Meio Aquático

# Carregamento dos Dados
library(readxl)
dados <- read_excel("Arq1.xlsx", sheet = 6) 
dados

# Boxplot Temperatura ##########################################################
library(ggplot2)
dados$Profun.m <-as.factor(dados$Profun.m)

ggplot(dados,aes(y = Temp,x = Profun.m)) +
  geom_errorbar(stat="boxplot",width=0.2)+
  geom_boxplot(width=0.6, aes(fill=Profun.m), outlier.shape = 1,
               outlier.size = 2) +
  geom_point(stat="summary",fun="mean",shape = 7,size=2,color="red") +
  labs(y="Temp (ºC)",x="Profundidade", fill = "Profun. (m)") +
  theme(legend.position = "none") + # Remove a legenda
  theme(legend.position = "none",                 # Remove a legenda
        axis.text.x = element_blank(),            # Esconde os rótulos do eixo x
        axis.ticks.x = element_blank(),           # Esconde os ticks do eixo x
        axis.line.x = element_blank())            # Esconde a linha do eixo x




