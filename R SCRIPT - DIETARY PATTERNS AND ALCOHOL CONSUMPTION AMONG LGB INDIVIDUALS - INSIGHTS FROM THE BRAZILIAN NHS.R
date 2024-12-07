#FEDERAL UNIVERSITY OF PARAÍBA
#HEALTH SCIENCES CENTER
#DEPARTMENT OF NUTRITION

#AUTHOR: PAULO GUSTAVO COSTA E SILVA CRUZ
#ADVISOR: PROF. DR. SÁVIO MARCELINO GOMES

#PAPER TITLE:
#DIETARY PATTERNS AND ALCOHOL CONSUMPTION AMONG LGB INDIVIDUALS: 
#INSIGHTS FROM THE BRAZILIAN NATIONAL HEALTH SURVEY

#JOÃO PESSOA - PARAÍBA - BRASIL / 2024

#SCRIPT CREATED FOR DATA EXTRACTION, CLEANING, AND ANALYSIS OF THE PNS (2019)

# 1 - Selecting variables based on the PNS variable dictionary:

variables <- c("C006","Y008","V00291",
               
               "C009","V0001","V0026","D00901","VDF003","C008",
               
               "P006","P00901","P018","P01601","P023","P015","P013",
               "P01101","P02001","P02002","P02501","P02801")

#C006 - GENDER
#Y008 - SEXUAL ORIENTATION
#V00291 - SELECTED RESIDENT WEIGHT WITH CALIBRATION

#SOCIODEMOGRAPHIC VARIABLES:
#C009 - RACE OR SKIN COLOR
#V0001 - FEDERATION UNIT
#V0026 - CENSUS TRACT
#D00901 - EDUCATION LEVEL
#VDF003 - PER CAPITA HOUSEHOLD INCOME
#C008 - RESIDENT'S AGE

#CONSUMPTION VARIABLES:
#P006 - NUMBER OF DAYS BEANS ARE CONSUMED
#P00901 - NUMBER OF DAYS VEGETABLES ARE CONSUMED
#P018 - NUMBER OF DAYS FRUITS ARE CONSUMED
#P01601 - NUMBER OF DAYS FRUIT JUICE IS CONSUMED
#P023 - NUMBER OF DAYS MILK IS CONSUMED
#P015 - NUMBER OF DAYS FISH IS CONSUMED
#P013 - NUMBER OF DAYS CHICKEN IS CONSUMED
#P01101 - NUMBER OF DAYS RED MEAT IS CONSUMED
#P02001 - NUMBER OF DAYS BOXED JUICE/POWDERED DRINK IS CONSUMED
#P02002 - NUMBER OF DAYS SODA IS CONSUMED
#P02501 - NUMBER OF DAYS SWEETS AND TREATS ARE CONSUMED
#P02801 - NUMBER OF DAYS ALCOHOLIC BEVERAGES ARE CONSUMED

#2 - Extracting raw data from the Brazilian Institute of Geography 
# and Statistics (IBGE) website:


install.packages("PNSIBGE")
library(PNSIBGE)

data_without_weights <- get_pns(year=2019, vars=variables, design=F)

# 3 - Modifying variables and stratifying the datasets into two: 
#     men and women

# Grouping the Education variable for analysis:

data_without_weights$D00901 <- factor(data_without_weights$D00901, labels = c("Primary or Lower Education", "Primary or Lower Education", "Primary or Lower Education", "Primary or Lower Education", "Primary or Lower Education",
                                                                    "Secondary", "Elementary", "Elementary", "Secondary", "Secondary", "Secondary",
                                                                    "Higher Education", "Higher Education", "Higher Education", "Higher Education", "Ignorado ou Não Aplicável", "Ignorado ou Não Aplicável") , 
                                 levels = c("Creche", "Pré-escola", "Classe de alfabetização – CA", "Alfabetização de jovens e adultos", "Antigo primário (elementar)", "Antigo ginasial (médio 1º ciclo)", 
                                            "Regular do ensino fundamental ou do 1º grau", "Educação de jovens e adultos (EJA) ou supletivo do ensino fundamental", "Antigo científico, clássico etc. (médio 2º ciclo)", 
                                            "Regular do ensino médio ou do 2º grau", "Educação de jovens e adultos (EJA) ou supletivo do ensino médio", "Superior – graduação", "Especialização de nível superior (duração mínima de 360 horas)", 
                                            "Mestrado", "Doutorado", "Ignorado", "Não aplicável"))


# Grouping the Federation Unit variable to analyze as Macroregions:

data_without_weights$V0001 <- factor(data_without_weights$V0001, labels = c("North","North","North","North","North","North","North",
                                                                  "Northeast","Northeast","Northeast","Northeast","Northeast","Northeast","Northeast","Northeast","Northeast",
                                                                  "Southeast","Southeast","Southeast","Southeast",
                                                                  "South","South","South",
                                                                  "Midwest","Midwest","Midwest","Midwest") , 
                                levels = c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins",
                                           "Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe","Bahia",
                                           "Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo",
                                           "Paraná","Santa Catarina","Rio Grande do Sul",
                                           "Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal"))

# Converting fruit consumption to numeric (Was as character in the dataset)

data_without_weights$P018 <- as.numeric(data_without_weights$P018)

# Creating the object by assigning weights corresponding to the complex sampling design.

install.packages("survey")
library(survey)

data <- svydesign(ids = ~UPA_PNS, strata = ~V0024, data = data_without_weights, 
                   nest = TRUE, weights = ~V00281)

# Splitting the data by gender:

men_data <- subset(data, subset = C006 == "Homem")

women_data <- subset(data, subset = C006 == "Mulher")

# 4 - Description of socioeconomic data by gender and sexual orientation:
# This step needs to be repeated for all variables analyzed in Table 1
# and for both genders.

object <- svyby(~ D00901, ~ Y008 , women_data, svymean, na.rm=T) #proporcao

object #returns the dependent variable by gender and sexual orientation

confint(object) #returns the 95% confidence interval

# 5 - Running the GLM model with Poisson distribution:

install.packages("jtools")
library(jtools)

model_poisson = svyglm(formula = P018 ~ Y008 + C008 + C009 + V0001 + V0026 + D00901, design = women_data, family = quasipoisson())

# Using the summ::jtools function to analyze the exponential of the model

summ(model_poisson, confint = TRUE, exp = TRUE)

