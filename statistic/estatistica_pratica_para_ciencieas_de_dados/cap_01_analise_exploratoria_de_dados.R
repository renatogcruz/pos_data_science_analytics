# Estimativas de Localiza??o (m?dia, m?dia ponderada, mediana, mediana ponderada)

# este dataset cont?m um conjunto de dados da popula??o e as taxas
# de homic?dio (em homic?dios/100 mil pessoas) em cada estado.

# Exemplo p?gina 12
# Calcule a m?dia, a m?dia aparada e a mediana para a popula??o

state <- read.csv(file='C:/Users/Renato/Dropbox/pos_usp/usp_data_open/statistic/estatistica_pratica_para_ciencieas_de_dados/data/state.csv')

# m?dia
media = mean(state[["Population"]])

# m?dia ponderada
media_pon = mean(state[["Population"]], trim=0.1) # exlui os 5 maiores e 5 menores estados

# mediana
mediana = median(state[["Population"]]) 

# Pacote para achar a mediana ponderada
if(!require(matrixStats))install.packages("matrixStats")
library(matrixStats)

media_pon_2 = weighted.mean(state[["Murder.Rate"]], w=state[["Population"]])
mediana_pon = weightedMedian(state[["Murder.Rate"]], w=state[["Population"]])  


# Exemplo: Estimativas de variabilidade de população Estadual

# desvio padrão
desvio_padrao <- sd(state[['Population']])

# MAD - desvio absoluto mediano da mediana
mad <- mad(state[['Population']])

# O desvio padrão é quase o dobro do MAD. Motivo - desvio padrão é sensível aos
# autliers

# Percentis e BoxPlots - página 20
# Percentis são valiosos para resumir toda a distribuição
# É comum registrar os quatis (25º, 50º e 70º percentis) e os decis (10º, 20°, 
# ... percentis).
# Percentis são especialmente valiosos para resumir as caudas (a amplitude externa)
# da dsitribuição

# em R, utiliza-se função 'qualite' para se obter percentis
percentis <- quantile(state[["Murder.Rate"]], p=c(.05, .25, .5, .75, .95))
percentis

# na saída percentis nota-se que a mediana é de 4 homicídios por 100 mil
# o 5º percentil é de 1.6 e o 95º é de 6.51

# os boxplots são baseados em percentis e são um modo rápido de visualizar a 
# distribuição dos dados.

par("mar")          # corrigindo erro
par(mar=c(1,1,1,1)) # corrigindo erro
boxplot(state[["Population"]]/1000000, ylab="Population (millions)")


# Tabela de frequências e histogramas - página 22
# 