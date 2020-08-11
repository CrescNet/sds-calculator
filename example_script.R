library(openxlsx)
library(childsds)
library(stringr)

file <- 'example.xlsx'
data <- read.xlsx(file, sep.names = ' ')

data$`Länge SDS` <- sds(
  value = data$`Länge in m` * 100,
  age = data$`Kind Alter in Jahren`,
  sex = data$Geschlecht, male = 'Junge', female = 'Mädchen',
  item = 'height',
  ref = kro.ref
)
data$`Länge Perz.` <- pnorm(data$`Länge SDS`) * 100

data$'Gewicht SDS' <- sds(
  value = data$`Gewicht in kg`,
  age = data$`Kind Alter in Jahren`,
  sex = data$Geschlecht, male = 'Junge', female = 'Mädchen',
  item = 'weight',
  ref = kro.ref
)
data$`Gewicht Perz.` <- pnorm(data$`Gewicht SDS`) * 100

data$`BMI in kg pro m²` <- data$`Gewicht in kg` / data$`Länge in m` ^ 2
data$'BMI SDS' <- sds(
  value = data$`BMI in kg pro m²`,
  age = data$`Kind Alter in Jahren`,
  sex = data$Geschlecht, male = 'Junge', female = 'Mädchen',
  item = 'bmi',
  ref = kro.ref
)
data$`BMI Perz.` <- pnorm(data$`BMI SDS`) * 100

sds_file <- str_replace(file, '.xlsx', '_SDS.xlsx')
sheet_name <- getSheetNames(file)[1]

write.xlsx(data, sds_file, sheetName = sheet_name)
