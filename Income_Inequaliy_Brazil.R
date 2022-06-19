
## DATA PREPARATION
# Installing the Packages
# install.packages("PNADcIBGE")
# install.packages("survey")
# install.packages("convey")
# install.packages("magrittr")

# Loading the Packages
library(PNADcIBGE) # CRAN v0.7.0
library(survey) # CRAN v4.0
library(convey) # CRAN v0.2.3
library(magrittr) # CRAN v2.0.1

# Downloading the database
dados_pnadc <- get_pnadc(year = 2021, quarter = 3, vars = c("VD4020", "V2007"))


## DATA ANALYSIS
# Classe do objeto
class(dados_pnadc) # útil para análises de dados amostrais complexos

# Obter nº total de homens e mulheres
svytotal(x = ~V2007, design = dados_pnadc, na.rm = TRUE)

# Estimar o índice de Gini
dados_pnadc %>%
  convey_prep() %>%
  svygini(formula = ~VD4020, na.rm = TRUE)

