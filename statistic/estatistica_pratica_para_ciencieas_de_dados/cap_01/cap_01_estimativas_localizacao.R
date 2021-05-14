# Estimativas de Localização (média, média ponderada, mediana, mediana ponderada)

# este dataset contém um conjunto de dados da população e as taxas
# de homicídio (em homicídios/100 mil pessoas) em cada estado.

# Exemplo página 12
# Calcule a média, a média aparada e a mediana para a população

state <- read.csv(file='C:/Users/Renato/Dropbox/pos_usp/usp_data_open/statistic/estatistica_pratica_para_ciencieas_de_dados/data/state.csv')

# média
media = mean(state[["Population"]])

# média ponderada
media_pon = mean(state[["Population"]], trim=0.1) # exlui os 5 maiores e 5 menores estados

# mediana
mediana = median(state[["Population"]]) 

# Pacote para achar a mediana ponderada
if(!require(matrixStats))install.packages("matrixStats")
library(matrixStats)

media_pon_2 = weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])
mediana_pon = weightedMedian(state[["Murder.Rate"]], w=state[["Population"]])  
