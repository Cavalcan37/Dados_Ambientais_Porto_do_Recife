# DADOS ATMOSFÉRICOS

# Carregamento dos Dados
library(readxl)
dados <- read_excel("ATM.xlsx", sheet = 7) 
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
HR <- read_excel("ATM.xlsx", sheet = 7)
HR

# Convertendo em ST
HR <- ts(HR,frequency = 12, start = c(2022,1))
HR
str(HR)

ts.plot(HR)

decomp.HR<-decompose(HR)
plot(decomp.HR)

ggtsdisplay(HR)

# Aferição Gráfica de Sazonalidade
seasonplot(HR, col = rainbow(3), year.labels = T, type = "o", 
           pch = 22)

# Transformação Box-Cox
L <- BoxCox.lambda(HR)
L

HR_BC <- BoxCox(HR, lambda = L)
HR_BC

ts.plot(HR_BC)

decomp.HR_BC<-decompose(HR_BC)
plot(decomp.HR_BC)

ggtsdisplay(HR_BC)

# Nº de Integrações (Diferenciações)
ndiffs(HR_BC)
HR_DIFF <- diff(HR_BC,1)
ts.plot(HR_DIFF)

decomp.HR_DIFF<-decompose(HR_DIFF)
plot(decomp.HR_DIFF)

ggtsdisplay(HR_DIFF) # "contar" "p" e "q"

# Nº de Diferenciações Sazonais # usando a série original
nsdiffs(HR) # Acho que tenho que fazer isso com a série original!!!

HR_DIFF2 <- diff(HR,12)
seasonplot(HR_DIFF2, col = rainbow(3), year.labels = T, type = "o", 
           pch = 22)

ggtsdisplay(HR_DIFF2) # "contar" "P" e "Q"

# Ajustando ARIMA/SARIMA
# Separando os Dados em Treino (train) e Teste (test)
HR_train <- ts(HR[1:28], frequency = 12)
HR_train

HR_test <- ts(HR[29:36], frequency = 12)
HR_test

FIT <- Arima(y = HR_train, order = c(1,1,0), seasonal = c(0,1,1), lambda = L)
summary(FIT)

coeftest(FIT)

# Plotar a Série Treino e Acurácia
plot(HR_train)

lines(FIT$fitted, col = "blue")

accuracy(HR_train, FIT$fitted)

# Calcular o coeficiente de determinação (R²)
# Fazer previsões no conjunto de treino
fitted_values <- FIT$fitted
ss_total <- sum((HR_train - mean(HR_train))^2)
ss_residual <- sum((HR_train - fitted_values)^2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared

# Realizando Agora Forecast para 86 Previsões para Compara com o Teste (test)
predi <- forecast(FIT, h = 8)
predi

plot(predi)

# Plotar as Previsões com a Base de Teste
plot(as.numeric(HR_test), type = "l")

lines(as.numeric(predi$mean), col = "red")

accuracy(as.numeric(HR_test), as.numeric(predi$mean))

# Diagnóstico dos Resíduos do Modelo
tsdiag(FIT)

qqnorm(FIT$residuals)
qqline(FIT$residuals, col = "red")

# Fazer 12 previsões além do banco de dados
FIT_full <- Arima(y = HR, order = c(1,1,0), seasonal = c(0,1,1), lambda = L)
forecast_full <- forecast(FIT_full, h = 12)
forecast_full

plot(forecast_full)
lines(forecast_full$mean, col = "red", type = "o", pch = 16)


# HISTOGRAMAS DE PROBABILIDADE (HR)
# 13. Probabilidades da Incidência de HR
# 13.1. Prev. HR de fev/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(HR = rnorm(5000, mean = 83.57, sd = 5))
dados

# Valor de referência para destaque
valor_referencia <- 77.83

media_valor <- mean(dados$HR) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 83.57, sd = 5)
probabilidade

# Gerar o gráfico ajustado
HR_fev <- ggplot(dados, aes(x = HR)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 83.57, sd = 5),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 83.57, sd = 5),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$HR))
  ) +
  labs(
    x = "Humidade Rel. - Fevereiro/25 (%)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 95, y = 0.05,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 75, y = 0.065,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
HR_fev

# 13.1. Prev. HR de mar/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(HR = rnorm(5000, mean = 82.72, sd = 5))
dados

# Valor de referência para destaque
valor_referencia <- 82.07

media_valor <- mean(dados$HR) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 82.72, sd = 5)
probabilidade

# Gerar o gráfico ajustado
HR_mar <- ggplot(dados, aes(x = HR)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 82.72, sd = 5),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 82.72, sd = 5),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$HR))
  ) +
  labs(
    x = "Humidade Rel. - Março/25 (%)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 95, y = 0.05,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 75, y = 0.065,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
HR_mar

# 13.1. Prev. HR de abr/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(HR = rnorm(5000, mean = 85.22, sd = 5))
dados

# Valor de referência para destaque
valor_referencia <- 84

media_valor <- mean(dados$HR) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 85.22, sd = 5)
probabilidade

# Gerar o gráfico ajustado
HR_abr <- ggplot(dados, aes(x = HR)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 85.22, sd = 5),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 85.22, sd = 5),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$HR))
  ) +
  labs(
    x = "Humidade Rel. - Abril/25 (%)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 95, y = 0.05,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 77, y = 0.065,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
HR_abr

# 13.1. Prev. HR de abr/24
# Geração dos dados simulados
# set.seed(123) # Para reprodutibilidade
dados <- data.frame(HR = rnorm(5000, mean = 84.08, sd = 5))
dados

# Valor de referência para destaque
valor_referencia <- 85.15

media_valor <- mean(dados$HR) # Calcular a média dos dados
media_valor

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 84.08, sd = 5)
probabilidade

# Gerar o gráfico ajustado
HR_mai <- ggplot(dados, aes(x = HR)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
  stat_function(
    fun = dnorm,
    args = list(mean = 84.08, sd = 5),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 84.08, sd = 5),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$HR))
  ) +
  labs(
    x = "Humidade Rel. - Maio/25 (%)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 95, y = 0.05,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 76, y = 0.065,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()
HR_mai

library(patchwork)
library(magrittr)
(HR_fev + HR_mar)/(HR_abr + HR_mai)
