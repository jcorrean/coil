library(readr)
coildata <- coildata <- read_csv("Data/coildata.csv")
library(lslx)

modelCO <-"
   SP =~ SP1 + SP2 +SP3 + SP4 + SP5
   CR =~ CR1 + CR2 + CR3 + CR4 + CR5
   CD =~ CD1 + CD2 + CD3 + CD4 + CD5
SP ~~ 1 * SP
CR ~~ 1 * CR
CD ~~ 1 * CD
"