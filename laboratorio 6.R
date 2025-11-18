# install.packages("haven")
# install.packages("dplyr")
# install.packages("estimatr")
# install.packages("car")

library(haven)
library(dplyr)
library(estimatr)
library(car)


casen <- read_dta("casen_2022_clean_lab4.dta")

casen <- casen %>%
  filter(!is.na(y1), !is.na(educ), !is.na(edad), !is.na(mujer),
         y1 > 0, educ > 0, edad > 0)

# FORMAS CUADRÁTICAS
# Queremos ver si el efecto de la edad sobre el ingreso (y1)
# es NO lineal. Para eso agregamos edad^2 al modelo.


casen$edad2 <- casen$edad^2


# 2) Modelo lineal (sin edad^2)



modelo_lin <- lm_robust(y1 ~ educ + edad + mujer,
                        data = casen, se_type = "HC1")
summary(modelo_lin)



# 3) Modelo con forma cuadrática: edad + edad^2


modelo_quad <- lm_robust(y1 ~ educ + edad + edad2 + mujer,
                         data = casen, se_type = "HC1")
summary(modelo_quad)





b1 <- coef(modelo_quad)["edad"]
b2 <- coef(modelo_quad)["edad2"]






# Efecto marginal a los 20 años
ef_20 <- b1 + 40*b2
ef_20

# Efecto marginal a los 50 años
ef_50 <- b1 + 100*b2
ef_50

# Punto donde el efecto cambia de signo (máximo o mínimo)
p_inf <- -b1 / (2*b2)
p_inf




# Para edad = 20 → H0: edad + 40*edad2 = 0
linearHypothesis(modelo_quad, "edad + 40*edad2 = 0",
                 vcov.=vcov(modelo_quad), test="F")

# Para edad = 50 → H0: edad + 100*edad2 = 0
linearHypothesis(modelo_quad, "edad + 100*edad2 = 0",
                 vcov.=vcov(modelo_quad), test="F")

# Test conjunto: ¿Edad y edad2 importan en conjunto?
linearHypothesis(modelo_quad,
                 c("edad = 0", "edad2 = 0"),
                 vcov.=vcov(modelo_quad), test="F")











# B: casen$educ_mujer <- casen$educ * casen$mujer

# Modelo con interacción educ*mujer
casen$educ_mujer <- casen$educ * casen$mujer


modelo_int <- lm_robust(y1 ~ educ + edad + mujer + educ_mujer,
                        data = casen, se_type = "HC1")
summary(modelo_int)

b1 <-  # educ
b4 <-  # educ_mujer     

b1 # efecto hombre
b1 + b4 #efecto mujer






# Test individual: H0: educ_mujer = 0                # No hay diferencia de género 
linearHypothesis(modelo_int, "educ_mujer = 0",
                 vcov.=vcov(modelo_int), test="F")


# educ × mujer NO es estadísticamente significativa. 

# Test conjunto: H0: educ = 0 y educ_mujer = 0         # Educación no importa para nadie
linearHypothesis(modelo_int,
                 c("educ = 0", "educ_mujer = 0"),
                 vcov.=vcov(modelo_int), test="F")


# La educación sí tiene un efecto significativo sobre el ingreso, ya sea para hombres, mujeres o ambos.
