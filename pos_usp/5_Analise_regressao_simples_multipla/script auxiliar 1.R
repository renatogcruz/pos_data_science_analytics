#analysis of variance (teste F):

anova(modelo_tempodist)

f <- rf(100000, df1 = 10, df2 = 70)
hist(f, breaks = 100)

qf(0.05, df1 = 1, df2 = 8, lower.tail = F)
pf(5.317655, df1 = 1, df2 = 8, lower.tail = F)

#teste t de Student:

t <- rt(100000, df = 8)
hist(t, breaks = 100)

pt(6.025, df = 8, lower.tail = F)*2


# Exemplo 3:
aggregate(corrupcao$cpi ~ corrupcao$regiao, FUN = mean)

corrupcao_dummies <- dummy_columns(.data = corrupcao,
                                   select_columns = "regiao",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = F)

corrupcao_dummies <- corrupcao_dummies[,-3]