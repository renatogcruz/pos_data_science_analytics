modelo1_step <- step(modelo1, k = (qchisq(p = 0.05, df = 1, lower.tail = F)))
summary(modelo1_step)

modelo_econ1 <- lm(formula = salario ~ econometria1,
                   data = salarios)

summary(modelo_econ1)

###############

modelo_rh3 <- lm(formula = salario ~ rh3,
                 data = salarios)

summary(modelo_rh3)

modelo3_step <- step(modelo3, k = (qchisq(p = 0.05, df = 1, lower.tail = F)))
summary(modelo3_step)

modelo_aux3 <- lm(formula = rh3 ~ econometria3,
                  data = salarios)

summary(modelo_aux3)

tolerance <- 1 - summary(modelo_aux3)$r.squared
tolerance
#Tolerance varia de 1 a 0

VIF <- 1/tolerance
VIF
#VIF varia de 1 a +(infinito)



######

modelo2_aux <- lm(formula = econometria2 ~ rh2,
                  data = salarios)

summary(modelo2_aux)

modelo2_step <- step(modelo2, k = 3.841459)
summary(modelo2_step)

export_summs(modelo2,modelo2_step, scale = F)



######################
saeb_rend$codigo <- as.character(saeb_rend$codigo)
saeb_rend$rede <- as.factor(saeb_rend$rede)


########################

saeb_rend$yhat <- predict(object = modelosaeb, newdata = saeb_rend)
saeb_rend$resid <- saeb_rend$saeb - saeb_rend$yhat

saeb_rend$up <- ((saeb_rend$resid)^2)/((sum(saeb_rend$resid^2,na.rm = T))/(25530))
modelo_aux <- lm(formula = up ~ yhat,
                 data = saeb_rend)
anova(modelo_aux)

pchisq(33.441/2, df = 1, lower.tail = F)


saeb_rend_dummies_uf <- dummy_columns(.data = saeb_rend,
                                      select_columns = "uf",
                                      remove_selected_columns = T)

saeb_rend_dummies_uf <- saeb_rend_dummies_uf[,-11]

#Modelo considerando as UF's 
modelosaeb_dummies_uf <- lm(formula = saeb ~ . -municipio -codigo -escola -rede,
                            data = saeb_rend_dummies_uf)

summary(modelosaeb_dummies_uf)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelosaeb_dummies_uf)


##################
planosaude$plano <- as.factor(planosaude$plano)

modelo_renda <- lm(despmed ~ renda, planosaude_dummies)

#Parâmetros do modelo_planosaude
summary(modelo_renda)


ols_vif_tol(step_bc_planosaude)

modelo_aux_plano <- lm(dcron ~ plano_esmeralda + plano_ouro,
                       data = planosaude_dummies)
summary(modelo_aux_plano)


