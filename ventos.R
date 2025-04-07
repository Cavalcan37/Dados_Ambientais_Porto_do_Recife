# DADOS ATMOSFÉRICOS

# Carregamento dos Dados
library(readxl)
dados <- read_excel("ATM.xlsx", sheet = 2) 
dados
str(dados)

# Correlação
# Correlação
library(PerformanceAnalytics)
m<-dados
m
chart.Correlation(dados,histogram=T)

# Previsão de Ventos no Porto do Recife - Próximos 12 meses
# Modelagem SARIMA - Ventos no Porto do Recife
# Pacotes
if(!require(forecast))install.packages("forecast")
library(forecast)
if(!require(lmtest))install.packages("lmtest")
library(lmtest)
if(!require(readxl))install.packages("readxl")
library(readxl)
# Dados
ventos <- read_excel("ATM.xlsx", sheet = 3)
ventos

# Convertendo em ST
ventos <- ts(ventos,frequency = 12, start = c(2022,1))
ventos
str(ventos)

ts.plot(ventos)

decomp.ventos<-decompose(ventos)
plot(decomp.ventos)

ggtsdisplay(ventos)

# Aferição Gráfica de Sazonalidade
seasonplot(ventos, col = rainbow(3), year.labels = T, type = "o", 
           pch = 22)

# Transformação Box-Cox
L <- BoxCox.lambda(ventos)
L

ventos_BC <- BoxCox(ventos, lambda = L)
ventos_BC

ts.plot(ventos_BC)

decomp.ventos_BC<-decompose(ventos_BC)
plot(decomp.ventos_BC)

ggtsdisplay(ventos_BC)

# Nº de Integrações (Diferenciações)
ndiffs(ventos_BC)
ventos_DIFF <- diff(ventos_BC,1)
ts.plot(ventos_DIFF)

decomp.ventos_DIFF<-decompose(ventos_DIFF)
plot(decomp.ventos_DIFF)

ggtsdisplay(ventos_DIFF) # "contar" "p" e "q"

# Nº de Diferenciações Sazonais # usando a série original
nsdiffs(ventos) # Acho que tenho que fazer isso com a série original!!!

ventos_DIFF2 <- diff(ventos,12)
seasonplot(ventos_DIFF2, col = rainbow(3), year.labels = T, type = "o", 
           pch = 22)

ggtsdisplay(ventos_DIFF2) # "contar" "P" e "Q"

# Ajustando ARIMA/SARIMA
# Separando os Dados em Treino (train) e Teste (test)
ventos_train <- ts(ventos[1:28], frequency = 12)
ventos_train

ventos_test <- ts(ventos[29:36], frequency = 12)
ventos_test

FIT <- Arima(y = ventos_train, order = c(1,1,0), seasonal = c(0,1,1), lambda = L)
summary(FIT)

coeftest(FIT)

# Plotar a Série Treino e Acurácia
plot(ventos_train)

lines(FIT$fitted, col = "blue")

accuracy(ventos_train, FIT$fitted)

# Calcular o coeficiente de determinação (R²)
# Fazer previsões no conjunto de treino
fitted_values <- FIT$fitted
ss_total <- sum((ventos_train - mean(ventos_train))^2)
ss_residual <- sum((ventos_train - fitted_values)^2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared

# Realizando Agora Forecast para 86 Previsões para Compara com o Teste (test)
predi <- forecast(FIT, h = 8)
predi

plot(predi)

# Plotar as Previsões com a Base de Teste
plot(as.numeric(ventos_test), type = "l")

lines(as.numeric(predi$mean), col = "red")

accuracy(as.numeric(ventos_test), as.numeric(predi$mean))

# Diagnóstico dos Resíduos do Modelo
tsdiag(FIT)

qqnorm(FIT$residuals)
qqline(FIT$residuals, col = "red")

# Fazer 12 previsões além do banco de dados
FIT_full <- Arima(y = ventos, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
forecast_full <- forecast(FIT_full, h = 12)
forecast_full

plot(forecast_full)
lines(forecast_full$mean, col = "red", type = "o", pch = 16)

# HISTOGRAMAS DE PROBABILIDADE (Ventos)
# 13. Probabilidades da Incidência de Ventos
# 13.1. Prev. Ventos de fev/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(ventos = rnorm(5000, mean = 0.9173058, sd = 0.6113405))
dados

# Valor de referência para destaque
valor_referencia <- 1.94

media_valor <- mean(dados$ventos) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 0.9173058, sd = 0.6113405)
probabilidade

# Gerar o gráfico ajustado
VEN_fev <- ggplot(dados, aes(x = ventos)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 0.9173058, sd = 0.6113405),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 0.9173058, sd = 0.6113405),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$ventos))
  ) +
  labs(
    x = "Ventos - Fevereiro/25 (m/s)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 2.8, y = 0.1,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 0, y = 0.6,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
VEN_fev

# Prev. Ventos de mar/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(ventos = rnorm(5000, mean = 0.7160, sd = 0.5490905))
dados

# Valor de referência para destaque
valor_referencia <- 1.80

media_valor <- mean(dados$ventos) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 0.7160, sd = 0.5490905)
probabilidade

# Gerar o gráfico ajustado
VEN_mar <- ggplot(dados, aes(x = ventos)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 0.7160, sd = 0.5490905),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 0.7160, sd = 0.5490905),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$ventos))
  ) +
  labs(
    x = "Ventos - Março/25 (m/s)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 2.4, y = 0.1,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 0, y = 0.65,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
VEN_mar

# Prev. Ventos de abr/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(ventos = rnorm(5000, mean = 0.4350, sd = 0.55))
dados

# Valor de referência para destaque
valor_referencia <- 1

media_valor <- mean(dados$ventos) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 0.4350, sd = 0.55)
probabilidade

# Gerar o gráfico ajustado
VEN_abr <- ggplot(dados, aes(x = ventos)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 0.4350, sd = 0.55),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 0.4350, sd = 0.55),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$ventos))
  ) +
  labs(
    x = "Ventos - Abril/25 (m/s)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 2.1, y = 0.1,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = -0.5, y = 0.6,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
VEN_abr

# Prev. Ventos de mai/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(ventos = rnorm(5000, mean = 0.5249, sd = 0.6159))
dados

# Valor de referência para destaque
valor_referencia <- 1.5

media_valor <- mean(dados$ventos) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 0.5249, sd = 0.6159)
probabilidade

# Gerar o gráfico ajustado
VEN_mai <- ggplot(dados, aes(x = ventos)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 0.5249, sd = 0.6159),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 0.5249, sd = 0.6159),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$ventos))
  ) +
  labs(
    x = "Ventos - Maio/25 (m/s)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 2.5, y = 0.1,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = -0.3, y = 0.6,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
VEN_mai

library(patchwork)
library(magrittr)
(VEN_fev + VEN_mar)/(VEN_abr + VEN_mai)



