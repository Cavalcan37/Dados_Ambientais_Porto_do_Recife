
# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# Criar vetor com os dados
dados_vento <- c(172.7, 171.3, 160, 157.2, 160, 154.4, 151.6, 157.2, 160, 157.2, 
                 165.7, 168.5, 157.2, 143.2, 155.8, 153, 155.8, 160, 162.9, 161.5, 
                 161.5, 154.4, 157.2, 151.6, 147.4, 147.4, 151.6, 155.8, 150.2, 153, 
                 154.4, 151.6, 162.9, 154.4, 157.2, 151.6, 157.2, 157.2, 158.6, 155.8, 
                 157.2, 153, 161.5, 157.2, 158.6, 154.4, 151.6, 148.8, 137.6, 143.2, 
                 150.2, 148.8, 157.2, 148.8, 143.2, 154.4, 151.6, 151.6, 150.2, 146, 
                 148.8, 155.8, 154.4, 154.4, 158.6, 158.6, 160, 144.6, 151.6, 150.2, 
                 151.6, 141.8, 140.4, 146, 140.4, 139, 143.2, 141.8, 136.2, 134.8, 139, 
                 140.4, 148.8, 134.8, 146, 141.8, 134.8, 130.6, 132, 133.4, 127.8, 
                 126.4, 127.8, 123.5, 130.6, 129.2, 124.9, 120.7, 116.5, 122.1, 117.9, 
                 106.7, 102.5, 96.9, 99.7, 96.9, 101.1, 103.9, 105.3, 101.1, 103.9, 
                 102.5, 106.7, 106.7, 109.5, 117.9, 112.3, 110.9, 122.1, 117.9, 102.5, 
                 105.3, 103.9, 102.5, 102.5, 103.9, 99.7, 105.3, 108.1, 108.1, 110.9, 
                 120.7, 110.9, 117.9, 117.9, 113.7, 109.5, 103.9, 105.3, 102.5, 101.1, 
                 99.7, 96.9, 92.7, 101.1, 102.5, 98.3, 98.3, 102.5, 99.7, 115.1, 105.3, 
                 113.7, 113.7, 122.1)

# Criar um dataframe
df <- data.frame(direcao = dados_vento)

# Criar histograma circular com escala de cores por frequência
ggplot(df, aes(x = direcao, fill = ..count..)) +
  geom_histogram(binwidth = 10, color = "white") +  # Define intervalo de 10 graus
  coord_polar(start = 0) +  # Mantém 360° (ou 0°) no topo
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 360, by = 30)) +  # Ajuste da escala angular
  scale_fill_gradient(low = "yellow", high = "red") +  # Gradiente de cor para indicar frequência
  labs(title = "Histograma Circular da Direção do Vento",
       x = "Direção (graus)",
       y = "Frequência",
       fill = "Frequência") +
  theme_minimal()





