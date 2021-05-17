# BASE DE DADOS 

base = read.csv("credit_data.csv")

base$clientid = NULL # apagar este atributo (não é importante para ML)

summary(base) # mesma função do .describe pandas/python

# TRATAMENTO DE VALORES INCONSISTENTES
idade_invalida = base[base$age < 0 & !is.na(base$age), ] # máscara [linhas com valores idade negativas, 
                                                         # e valores não nulos
                                                         # todas as colunas]
# em R os índices começam em 1
# maneiras de trabalhar com valores invalidos
# 1 - apagar toda a coluna que contenha dados inválidos (não é a mais inteligente)
# base$age = NULL
# 2 - apagar somente os registros com problemas (apagar as linhas)(não muito boa)
# base = base[base$age > 0, ]
# 3 - preencher os dados manualmente (maneira correto, porém, em bases grandes seria inviável)
# 4 - calcular a média da idade e substituir os valores incons. com a média
mean(base$age, na.rm = TRUE)                  # media, tirando valores NA (isso inclui os dados incons. na média)
mean(base$age[base$age>0], na.rm = TRUE)       # média sem dados incons. e valores NA
base$age = ifelse(base$age<0, 40.92, base$age) # alterando valores




