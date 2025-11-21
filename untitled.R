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

# INTERACCION VARIABLE CONTINUA 