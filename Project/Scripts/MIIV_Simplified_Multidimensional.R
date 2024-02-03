library(readr)
coildata <- coildata <- read_csv("Data/coildata.csv")

library(lavaan)
modelData <- coildata ;
modelCO2 <-
  "
! regressions 
   SP=~SP__SP1*SP1
   SP=~SP__SP3*SP3
   SP=~SP__SP4*SP4
   SP=~SP__SP5*SP5
   CR=~CR__CR1*CR1
   CR=~CR__CR2*CR2
   CR=~CR__CR3*CR3
   CD=~CD__CD1*CD1
   CD=~CD__CD2*CD2
   CD=~CD__CD4*CD4
! residuals, variances and covariances
   SP1 ~~ VAR_SP1*SP1
   SP3 ~~ VAR_SP3*SP3
   SP4 ~~ VAR_SP4*SP4
   SP5 ~~ VAR_SP5*SP5
   CR1 ~~ VAR_CR1*CR1
   CR2 ~~ VAR_CR2*CR2
   CD1 ~~ VAR_CD1*CD1
   CD2 ~~ VAR_CD2*CD2
   CD4 ~~ VAR_CD4*CD4
   SP ~~ 1.0*SP
   CR ~~ 1.0*CR
   CD ~~ 1.0*CD
   CR3 ~~ VAR_CR3*CR3
   SP ~~ COV_SP_CR*CR
   CR ~~ COV_CR_CD*CD
   SP ~~ COV_SP_CD*CD
! observed means
   SP1~1;
   SP3~1;
   SP4~1;
   SP5~1;
   CR1~1;
   CR2~1;
   CD1~1;
   CD2~1;
   CD4~1;
   CR3~1;
";

result8 <- cfa(modelCO2, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE, std.lv = TRUE);

CO2 <- summary(result8, fit.measures=TRUE)

library(MIIVsem)
miivs(modelCO2)
model1_miiv <- miive(model = modelCO2, data = coildata, sarg.adjust = "holm")
Model1_miiv <- estimatesTable(model1_miiv)

ML.lhs <- Model1_miiv$lhs
ML.rhs <- Model1_miiv$rhs
ML.es <- Model1_miiv$est 
ML.se <- Model1_miiv$se
ML.p <- Model1_miiv$pvalue
lava1 <- data.frame(ML.lhs, ML.rhs, ML.es, ML.se, ML.p)
colnames(lava1)[1] <- "lhs"
colnames(lava1)[2] <- "rhs"

MIIVE.lhs <- Model1_miiv$lhs
MIIVE.rhs <- Model1_miiv$rhs
MIIVE.es <- Model1_miiv$est
MIIVE.se <- Model1_miiv$se
MIIVE.p <- Model1_miiv$pvalue
miiv1 <- data.frame(MIIVE.lhs, MIIVE.rhs, MIIVE.es, MIIVE.se, MIIVE.p)
colnames(miiv1)[1] <- "lhs"
colnames(miiv1)[2] <- "rhs"

## merge lava1 and miiv1
MethodsComparison <- merge(lava1, miiv1, by = c("lhs", "rhs"), all.x = TRUE, all.y = TRUE)
