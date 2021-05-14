# Estimativas de variabilidade

# Emxemplo: Estimativas de variabilidade de população estadual
# Cálculo estimativas de variabilidade para os dados de população estaduais

state <- read.csv(file='C:/Users/Renato/Dropbox/pos_usp/usp_data_open/statistic/estatistica_pratica_para_ciencieas_de_dados/data/state.csv')

# desvio padrão
desvio_padrao <- sd(state[['Population']])

# MAD - desvio absoluto mediano da mediana
mad <- mad(state[['Population']])

# O desvio padrão é quase o dobro do MAD. Motivo - desvio padrão é sensível aos
# autliers