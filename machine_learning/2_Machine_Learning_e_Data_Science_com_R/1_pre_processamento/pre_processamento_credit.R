base = read.csv("credit_data.csv")

base$clientid = NULL # apagar este atributo (não é importante para ML)

summary(base) # mesma função do .describe pandas/python
