# Complemento Exemplo 04:

bebes$zcomprimento <- scale(bebes$comprimento)
summary(bebes$zcomprimento)
sd(bebes$zcomprimento)

sf.test(bebes$comprimento)
sf.test(bebes$zcomprimento)

#############################################################################

# Complemento Exemplo 05:

modelo_auxiliar1 <- lm(formula = retorno ~ endividamento,
                      data = empresas)
summary(modelo_auxiliar1)

modelo_auxiliar2 <- lm(formula = retorno ~ . - empresa - endividamento,
                      data = empresas)
summary(modelo_auxiliar2)

modelo_auxiliar3 <- lm(formula = retorno ~ . - empresa - endividamento - disclosure,
                       data = empresas)
summary(modelo_auxiliar3)

modelo_auxiliar4 <- lm(formula = retorno ~ disclosure,
                       data = empresas)
summary(modelo_auxiliar4)


empresas$zativos <- scale(empresas$ativos)
empresas$zliquidez <- scale(empresas$liquidez)

modelo_auxiliar5 <- lm(formula = retorno ~ zativos + zliquidez,
                       data = empresas)
summary(modelo_auxiliar5)
confint(modelo_auxiliar5, level = 0.95)

empresas$zativos <- NULL
empresas$zliquidez <- NULL

