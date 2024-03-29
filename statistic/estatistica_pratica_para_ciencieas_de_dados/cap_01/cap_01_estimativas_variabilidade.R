# Estimativas de variabilidade

# Emxemplo: Estimativas de variabilidade de popula��o estadual
# C�lculo estimativas de variabilidade para os dados de popula��o estaduais

state <- read.csv(file='C:/Users/Renato/Dropbox/pos_usp/usp_data_open/statistic/estatistica_pratica_para_ciencieas_de_dados/data/state.csv')

# desvio padr�o
desvio_padrao <- sd(state[['Population']])

# MAD - desvio absoluto mediano da mediana
mad <- mad(state[['Population']])

# O desvio padr�o � quase o dobro do MAD. Motivo - desvio padr�o � sens�vel aos
# autliers