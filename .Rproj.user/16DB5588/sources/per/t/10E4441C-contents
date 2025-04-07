# INDICE DE DESENV. AMBIENTAL - IDA

# Meio Aquático

# Carregamento dos Dados
library(readxl)
dados <- read_excel("Arq1.xlsx", sheet = 6) 
dados

# Correlação
library(PerformanceAnalytics)
m<-dados[,c(2, 4:9)]
m
chart.Correlation(m,histogram=T)

# Dispersão c/ Ajuste Linear: Temp ~ Profun ####################################
library(ggplot2)
library(ggpubr)
library(ggthemes)

ggplot(dados,aes(x = Profun, y = Temp)) +
  geom_point(color = "gray1", size = 2, shape = 1) +
  geom_smooth(method = "lm",se=F,color="red",size=0.5) + # alternar 'se = T e F'
  scale_y_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  stat_regline_equation(aes(label = ..eq.label..), label.x = 0.9, label.y = 29.5) + # Equação da reta
  stat_regline_equation(aes(label = ..rr.label..), label.x = 0.9, label.y = 29.2) + # R²
  labs(y = "Temperatura (ºC)", x = 'Profundidade (m)') +
  theme_classic2()

# Dispersão c/ Ajuste Linear: Condut ~ Profun ##################################
library(ggplot2)
library(ggpubr)
library(ggthemes)

ggplot(dados,aes(x = Profun, y = Condut)) +
  geom_point(color = "green4", size = 2, shape = 1) +
  geom_smooth(method = "lm",se=F,color="red",size=0.5) + # alternar 'se = T e F'
  scale_y_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  stat_regline_equation(aes(label = ..eq.label..), label.x = 0.8, label.y = 60000) + # Equação da reta
  stat_regline_equation(aes(label = ..rr.label..), label.x = 0.8, label.y = 57000) + # R²
  labs(y = "Condutividade (MSiemens/cm)", x = 'Profundidade (m)') +
  theme_classic2()

# Dispersão c/ Ajuste Linear: Cond.esp ~ Profun ################################
library(ggplot2)
library(ggpubr)
library(ggthemes)

ggplot(dados,aes(x = Profun, y = Cond.esp)) +
  geom_point(color = "orange4", size = 2, shape = 1) +
  geom_smooth(method = "lm",se=F,color="red",size=0.5) + # alternar 'se = T e F'
  scale_y_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  stat_regline_equation(aes(label = ..eq.label..), label.x = 0.8, label.y = 60000) + # Equação da reta
  stat_regline_equation(aes(label = ..rr.label..), label.x = 0.8, label.y = 57000) + # R²
  labs(y = "Condutância Esp. (MSiemens/cm)", x = 'Profundidade (m)') +
  theme_classic2()

# Dispersão c/ Ajuste Linear: Salin ~ Profun ###################################
library(ggplot2)
library(ggpubr)
library(ggthemes)

ggplot(dados,aes(x = Profun, y = Salin)) +
  geom_point(color = "yellow4", size = 2, shape = 1) +
  geom_smooth(method = "lm",se=F,color="red",size=0.5) + # alternar 'se = T e F'
  scale_y_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  stat_regline_equation(aes(label = ..eq.label..), label.x = 0.8, label.y = 37) + # Equação da reta
  stat_regline_equation(aes(label = ..rr.label..), label.x = 0.8, label.y = 35) + # R²
  labs(y = "Salinidade (Practical Salinity Scale)", x = 'Profundidade (m)') +
  theme_classic2()

# Dispersão c/ Ajuste Linear: Vel.som ~ Profun #################################
library(ggplot2)
library(ggpubr)
library(ggthemes)

ggplot(dados,aes(x = Profun, y = Vel.som)) +
  geom_point(color = "red4", size = 2, shape = 1) +
  geom_smooth(method = "lm",se=F,color="red",size=0.5) + # alternar 'se = T e F'
  scale_y_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  stat_regline_equation(aes(label = ..eq.label..), label.x = 0.8, label.y = 1545) + # Equação da reta
  stat_regline_equation(aes(label = ..rr.label..), label.x = 0.8, label.y = 1542) + # R²
  labs(y = "Vel. Som (m/s)", x = 'Profundidade (m)') +
  theme_classic2()

# Dispersão c/ Ajuste Linear: Dens ~ Profun ####################################
library(ggplot2)
library(ggpubr)
library(ggthemes)

ggplot(dados,aes(x = Profun, y = Dens)) +
  geom_point(color = "purple4", size = 2, shape = 1) +
  geom_smooth(method = "lm",se=F,color="red",size=0.5) + # alternar 'se = T e F'
  scale_y_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ","))+
  stat_regline_equation(aes(label = ..eq.label..), label.x = 0.8, label.y = 1025) + # Equação da reta
  stat_regline_equation(aes(label = ..rr.label..), label.x = 0.8, label.y = 1023) + # R²
  labs(y = "Densidade (kg/m³)", x = 'Profundidade (m)') +
  theme_classic2()

# Dispersão c/ Ajuste Linear: Condutividade=f(Salinidade,Temperatura) ##########
# Versão 3D:
library(plotly)

# Supondo que seus dados estejam em um data frame chamado `dados`
plot_ly(data = dados, x = ~Salin, y = ~Temp, z = ~Condut, 
        type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~Condut, colorscale = 'Viridis')) %>%
  layout(scene = list(xaxis = list(title = 'Salin(EP)'),
                      yaxis = list(title = 'Temp(ºC)'),
                      zaxis = list(title = 'Cond(µS/cm)')))

# Versão 2D:
library(ggplot2)

ggplot(dados, aes(x = Salin, y = Condut, color = Temp)) +
  geom_point(size = 3, shape = 16) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Salinidade (EP)", y = "Condutividade (µS/cm)", color = "Temp (ºC)") +
  theme_minimal() +
  facet_wrap(~ cut(Temp, breaks = 3), scales = "free") + # Divide Temperatura em 3 intervalos
  theme_minimal()

# Dispersão c/ Ajuste Linear: Vel.Som = f(Temp, Salin, Pressao) #########################
# Carregar biblioteca
library(plotly)

# Gráfico 3D: Velocidade do Som em função de Temperatura, Salinidade e Pressão
plot_ly(data = dados, 
        x = ~Salin, y = ~Temp, z = ~Vel.som, 
        type = 'scatter3d', mode = 'markers',
        color = ~Pressao, colorscale = 'Viridis',
        marker = list(size = 5)) %>%
  layout(scene = list(
    xaxis = list(title = 'Salin.(EP)'),
    yaxis = list(title = 'Temp.(ºC)'),
    zaxis = list(title = 'Vel.som(m/s)')
  ))

# Carregar bibliotecas
library(ggplot2)
library(dplyr)

# Criar intervalos para pressão
dados <- dados %>%
  mutate(Pressao_intervalos = cut(Pressao, breaks = 4))

# Gráfico 2D: Velocidade do Som vs Salinidade com Temperatura como cor
ggplot(dados, aes(x = Salin, y = Vel.som, color = Temp)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Salinidade (EP)", 
       y = "Vel.som (m/s)", 
       color = "Temp.(ºC)") + 
  theme_minimal() +
  facet_wrap(~Pressao_intervalos, scales = "free")  # Facetas para intervalos de pressão

# Dispersão c/ Ajuste Linear: Dens = f(Temp, Salin, Pressao) ###################
# Versão 3D:
library(plotly)

plot_ly(data = dados, 
        x = ~Salin, y = ~Temp, z = ~Dens, 
        type = 'scatter3d', mode = 'markers',
        color = ~Pressao, colorscale = 'Viridis',
        marker = list(size = 5)) %>%
  layout(scene = list(
    xaxis = list(title = 'Salin.(EP)'),
    yaxis = list(title = 'Temp.(ºC)'),
    zaxis = list(title = 'Dens.(kg/m³)')
  ))

# Versão 2D:
library(ggplot2)
library(dplyr)

# Criar intervalos para pressão
dados <- dados %>%
  mutate(Pressao_intervalos = cut(Pressao, breaks = 4))

# Gráfico 2D com facetas
ggplot(dados, aes(x = Salin, y = Dens, color = Temp)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Salinidade (EP)", 
       y = "Dens.(kg/m³)", 
       color = "Temp(ºC)") + 
  theme_minimal() +
  facet_wrap(~Pressao_intervalos, scales = "free")  # Facetas para diferentes intervalos de pressão

# Dispersão c/ Ajuste Linear: Cond.esp = f(Cond, Temp) #########################
# Versão 3D:
# Gráfico 3D para Condutividade Específica
library(plotly)

plot_ly(data = dados, 
        x = ~Condut, y = ~Temp, z = ~Cond.esp, 
        type = 'scatter3d', mode = 'markers',
        color = ~Temp, colorscale = 'Viridis',  # Colorido pela Temperatura
        marker = list(size = 5)) %>%
  layout(scene = list(
    xaxis = list(title = 'Cond.(S/m)'),
    yaxis = list(title = 'Temp.(ºC)'),
    zaxis = list(title = 'Cond.esp.(S/m)')
  ))

# Versão 2D:
library(ggplot2)
library(dplyr)

# Criar intervalos para Condutividade
dados <- dados %>%
  mutate(Cond_intervalos = cut(Condut, breaks = 4))

# Gráfico 2D com facetas
ggplot(dados, aes(x = Condut, y = Cond.esp, color = Temp)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Condutividade (S/m)", 
       y = "Condutância Específica (S/m)", 
       color = "Temp(ºC)") +
  theme_minimal() +
  facet_wrap(~Cond_intervalos, scales = "free")  # Facetas para diferentes intervalos de Condutividade








