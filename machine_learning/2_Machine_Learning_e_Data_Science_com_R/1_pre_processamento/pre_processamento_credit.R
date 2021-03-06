# BASE DE DADOS 

base = read.csv("credit_data.csv")

base$clientid = NULL # apagar este atributo (n�o � importante para ML)

summary(base) # mesma fun��o do .describe pandas/python

# TRATAMENTO DE VALORES INCONSISTENTES
idade_invalida = base[base$age < 0 & !is.na(base$age), ] # m�scara [linhas com valores idade negativas, 
                                                         # e valores n�o nulos
                                                         # todas as colunas]
# em R os �ndices come�am em 1
# maneiras de trabalhar com valores invalidos
# 1 - apagar toda a coluna que contenha dados inv�lidos (n�o � a mais inteligente)
# base$age = NULL
# 2 - apagar somente os registros com problemas (apagar as linhas)(n�o muito boa)
# base = base[base$age > 0, ]
# 3 - preencher os dados manualmente (maneira correto, por�m, em bases grandes seria invi�vel)
# 4 - calcular a m�dia da idade e substituir os valores incons. com a m�dia
mean(base$age, na.rm = TRUE)                  # media, tirando valores NA (isso inclui os dados incons. na m�dia)
mean(base$age[base$age>0], na.rm = TRUE)       # m�dia sem dados incons. e valores NA
base$age = ifelse(base$age<0, 40.92, base$age) # alterando valores

# TRATAMENTO DE VALORES FALTANTES
base[is.na(base$age), ]
base$age = ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)

# ESCALONAMENTO DE ATRIBUTOS
# tratando dados com escalas diferentes
# padroniza��o (standardisation)
# normaliza��o (normalization)

base[, 1:3] = scale(base[, 1:3])

# Base de treinamento e teste
if(!require(caTools)) install.packages("caTools") # Instala��o do pacote caso n�o esteja instalado
library(caTools) 

set.seed(1)
divisao = sample.split(base$default, SplitRatio = 0.75)

base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)