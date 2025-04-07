# DADOS ATMOSFÉRICOS

# Carregamento dos Dados
library(readxl)
dados <- read_excel("ATM.xlsx", sheet = 5) 
dados
str(dados)

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
temp <- read_excel("ATM.xlsx", sheet = 5)
temp

# Convertendo em ST
temp <- ts(temp,frequency = 12, start = c(2022,1))
temp
str(temp)

ts.plot(temp)

decomp.temp<-decompose(temp)
plot(decomp.temp)

ggtsdisplay(temp)

# Aferição Gráfica de Sazonalidade
seasonplot(temp, col = rainbow(3), year.labels = T, type = "o", 
           pch = 22)

# Transformação Box-Cox
L <- BoxCox.lambda(temp)
L

temp_BC <- BoxCox(temp, lambda = L)
temp_BC

ts.plot(temp_BC)

decomp.temp_BC<-decompose(temp_BC)
plot(decomp.temp_BC)

ggtsdisplay(temp_BC)

# Nº de Integrações (Diferenciações)
ndiffs(temp_BC)
temp_DIFF <- diff(temp_BC,1)
ts.plot(temp_DIFF)

decomp.temp_DIFF<-decompose(temp_DIFF)
plot(decomp.temp_DIFF)

ggtsdisplay(temp_DIFF) # "contar" "p" e "q"

# Nº de Diferenciações Sazonais # usando a série original
nsdiffs(temp) # Acho que tenho que fazer isso com a série original!!!

temp_DIFF2 <- diff(temp,12)
seasonplot(temp_DIFF2, col = rainbow(3), year.labels = T, type = "o", 
           pch = 22)

ggtsdisplay(temp_DIFF2) # "contar" "P" e "Q"

# Ajustando ARIMA/SARIMA
# Separando os Dados em Treino (train) e Teste (test)
temp_train <- ts(temp[1:28], frequency = 12)
temp_train

temp_test <- ts(temp[29:36], frequency = 12)
temp_test

FIT <- Arima(y = temp_train, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
summary(FIT)

coeftest(FIT)

# Plotar a Série Treino e Acurácia
plot(temp_train)

lines(FIT$fitted, col = "blue")

accuracy(temp_train, FIT$fitted)

# Calcular o coeficiente de determinação (R²)
# Fazer previsões no conjunto de treino
fitted_values <- FIT$fitted
ss_total <- sum((temp_train - mean(temp_train))^2)
ss_residual <- sum((temp_train - fitted_values)^2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared

# Realizando Agora Forecast para 86 Previsões para Compara com o Teste (test)
predi <- forecast(FIT, h = 8)
predi

plot(predi)

# Plotar as Previsões com a Base de Teste
plot(as.numeric(temp_test), type = "l")

lines(as.numeric(predi$mean), col = "red")

accuracy(as.numeric(temp_test), as.numeric(predi$mean))

# Diagnóstico dos Resíduos do Modelo
tsdiag(FIT)

qqnorm(FIT$residuals)
qqline(FIT$residuals, col = "red")

# Fazer 12 previsões além do banco de dados
FIT_full <- Arima(y = temp, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
forecast_full <- forecast(FIT_full, h = 12)
forecast_full

plot(forecast_full)
lines(forecast_full$mean, col = "red", type = "o", pch = 16)

# HISTOGRAMAS DE PROBABILIDADE (Temp)
# 13. Probabilidades da Incidência de Temp
# 13.1. Prev. Temp de fev/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(temp = rnorm(5000, mean = 26.69, sd = 2))
dados

# Valor de referência para destaque
valor_referencia <- 29.07

media_valor <- mean(dados$temp) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 26.69, sd = 2)
probabilidade

# Gerar o gráfico ajustado
TEMP_fev <- ggplot(dados, aes(x = temp)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.8, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 26.69, sd = 2),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 26.69, sd = 2),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$temp))
  ) +
  labs(
    x = "Temperatura - Fevereiro/25 (ºC)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 31, y = 0.1,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 22, y = 0.1,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
TEMP_fev

# Prev. Temp de mar/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(temp = rnorm(5000, mean = 30.24, sd = 1))
dados

# Valor de referência para destaque
valor_referencia <- 31.13

media_valor <- mean(dados$temp) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 30.24, sd = 1)
probabilidade

# Gerar o gráfico ajustado
TEMP_mar <- ggplot(dados, aes(x = temp)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 30.24, sd = 1),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 30.24, sd = 1),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$temp))
  ) +
  labs(
    x = "Temperatura - Março/25 (ºC)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 33, y = 0.1,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 28.4, y = 0.35,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
TEMP_mar

# Prev. Temp de abr/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(temp = rnorm(5000, mean = 29.63, sd = 1))
dados

# Valor de referência para destaque
valor_referencia <- 29.8

media_valor <- mean(dados$temp) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 29.63, sd = 1)
probabilidade

# Gerar o gráfico ajustado
TEMP_abr <- ggplot(dados, aes(x = temp)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 29.63, sd = 1),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 29.63, sd = 1),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$temp))
  ) +
  labs(
    x = "Temperatura - Abril/25 (ºC)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 33, y = 0.1,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 28, y = 0.35,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
TEMP_abr

# Prev. Temp de mai/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(temp = rnorm(5000, mean = 28.69, sd = 1))
dados

# Valor de referência para destaque
valor_referencia <- 27.57

media_valor <- mean(dados$temp) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 28.69, sd = 1)
probabilidade

# Gerar o gráfico ajustado
TEMP_mai <- ggplot(dados, aes(x = temp)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 28.69, sd = 1),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 28.69, sd = 1),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$temp))
  ) +
  labs(
    x = "Temperatura - Maio/25 (ºC)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 31.4, y = 0.1,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 27.5, y = 0.35,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
TEMP_mai

library(patchwork)
library(magrittr)
(TEMP_fev + TEMP_mar)/(TEMP_abr + TEMP_mai)
