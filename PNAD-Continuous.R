###################################################################
## Continuous National Household Sample Survey (PNAD-Contínua)
# Author: Tharcisio Leone
###################################################################


## 1. DATA PREPARATION

# Installing the Packages
install.packages("PNADcIBGE")

# Loading the Packages
library(PNADcIBGE)
library(survey)

# Downloading the database
help("get_pnadc") 
dadosPNADc_anual_trimestre <- get_pnadc(year=2017, topic=2) # Only topic 2 (education) of 2017.
dadosPNADc <- get_pnadc(year=2017, quarter=4) # Only 4th quarter of 2017
dadosPNADc # Showing the data structure
class(dadosPNADc) # Showing the type of object

# Importing only the necessary variables for the investigation
variaveis_selecionadas <- c("UF","V2007","V2009","V2010","V3007","VD3004","VD4001","VD4002","VD4020","VD4035")
dadosPNADc <- get_pnadc(year=2017, quarter=4, vars=variaveis_selecionadas)



## DATA ANALYSIS

# Estimating Total Population
totalsexo <- svytotal(x=~V2007, design=dadosPNADc, na.rm=TRUE) # Population by Gender
totalsexo
totalsexoraca <- svytotal(x=~V2007+V2010, design=dadosPNADc, na.rm=TRUE) # Population by Gender AND Race
totalsexoraca
totalsexoEraca <- svytotal(x=~interaction(V2007,V2010), design=dadosPNADc, na.rm=TRUE) # Interaction by Gender AND Race
ftable(x=totalsexoEraca)

# Estimating Mean Values for the Population
propsexo <- svymean(x=~V2007, design=dadosPNADc, na.rm=TRUE) # Proportion by Gender
propsexo

# Estimating Total Monthly Income for people aged 14 and over
totalrenda <- svytotal(x=~VD4020, design=dadosPNADc, na.rm=TRUE)
totalrenda
cv(object=totalrenda) # Computing the Coefficient of Variation
confint(object=totalrenda) # Computing the confidence intervals
confint(object=totalrenda, level=0.99)

# Estimating Average Monthly Income for people aged 14 and over
mediarenda <- svymean(x=~VD4020, design=dadosPNADc, na.rm=TRUE)
mediarenda # Average Income = R$ 2,182.6 per Month
cv(object=mediarenda) # Computing the ratio of the standard deviation to the mean.
confint(object=mediarenda) # Computing the confidence intervals
