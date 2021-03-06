# Estimativas de Localiza��o (m�dia, m�dia ponderada, mediana, mediana ponderada)

# este dataset cont�m um conjunto de dados da popula��o e as taxas
# de homic�dio (em homic�dios/100 mil pessoas) em cada estado.

# Exemplo p�gina 12
# Calcule a m�dia, a m�dia aparada e a mediana para a popula��o

state <- read.csv(file='C:/Users/Renato/Dropbox/pos_usp/usp_data_open/statistic/estatistica_pratica_para_ciencieas_de_dados/data/state.csv')

# m�dia
media = mean(state[["Population"]])

# m�dia ponderada
media_pon = mean(state[["Population"]], trim=0.1) # exlui os 5 maiores e 5 menores estados

# mediana
mediana = median(state[["Population"]]) 

# Pacote para achar a mediana ponderada
if(!require(matrixStats))install.packages("matrixStats")
library(matrixStats)

media_pon_2 = weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])
mediana_pon = weightedMedian(state[["Murder.Rate"]], w=state[["Population"]])  
