library(readr)
coildata <- coildata <- read_csv("Data/coildata.csv")

library(lavaan);
modelData <- coildata
model<-"
! regressions 
   SP=~SP__SP1*SP1
   SP=~SP__SP2*SP2
   SP=~SP__SP3*SP3
   SP=~SP__SP4*SP4
   SP=~SP__SP5*SP5
! residuals, variances and covariances
   SP1 ~~ VAR_SP1*SP1
   SP2 ~~ VAR_SP2*SP2
   SP3 ~~ VAR_SP3*SP3
   SP4 ~~ VAR_SP4*SP4
   SP5 ~~ VAR_SP5*SP5
   SP ~~ 1.0*SP
! observed means
   SP1~1;
   SP2~1;
   SP3~1;
   SP4~1;
   SP5~1;
";
result <- cfa(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = FALSE, std.lv = FALSE)
summary(result, fit.measures=TRUE)
ML <- summary(result, fit.measures=TRUE)
MLestimates <- data.frame(ML$pe)

library(MIIVsem)
miivs(model)
model1_miiv <- miive(model = model, data = coildata, sarg.adjust = "holm")
Model1_miiv <- estimatesTable(model1_miiv)

ML.lhs <- Model1_miiv$lhs
ML.rhs <- Model1_miiv$rhs
ML.es <- head(MLestimates$est, 5)
ML.se <- head(ML$pe[7], 5)
ML.p <- head(ML$pe[9], 5)
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
MethodsComparison <- head(MethodsComparison, 5)

library(dplyr)
MethodsComparison <- MethodsComparison %>%
  mutate(ML.MIIVE.dif = ML.es - MIIVE.es, .after = MIIVE.p)

library(ggplot2)
ggplot(MethodsComparison, aes(x = ML.MIIVE.dif)) +
  geom_density()

# SIMPLIFIED MODEL

modelSP <-"
! regressions 
   SP=~SP__SP1*SP1
   SP=~SP__SP3*SP3
   SP=~SP__SP4*SP4
   SP=~SP__SP5*SP5
! residuals, variances and covariances
   SP1 ~~ VAR_SP1*SP1
   SP3 ~~ VAR_SP3*SP3
   SP4 ~~ VAR_SP4*SP4
   SP5 ~~ VAR_SP5*SP5
   SP ~~ 1.0*SP
! observed means
   SP1~1;
   SP3~1;
   SP4~1;
   SP5~1;
"
result2 <- cfa(modelSP, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = FALSE, std.lv = FALSE)

SP2 <- summary(result2, fit.measures=TRUE)
SP2 <- data.frame(SP2$fit)

ML2 <- summary(result2, fit.measures=TRUE)
ML2estimates <- data.frame(ML2$pe)

library(MIIVsem)
miivs(modelSP)
model2_miiv <- miive(model = modelSP, data = coildata, sarg.adjust = "holm")
Model2_miiv <- estimatesTable(model2_miiv)

ML2.lhs <- Model2_miiv$lhs
ML2.rhs <- Model2_miiv$rhs
ML2.es <- head(ML2estimates$est, 4)
ML2.se <- head(ML2$pe[7], 4)
ML2.p <- head(ML2$pe[9], 4)
lava2 <- data.frame(ML2.lhs, ML2.rhs, ML2.es, ML2.se, ML2.p)
colnames(lava2)[1] <- "lhs"
colnames(lava2)[2] <- "rhs"

MIIVE2.lhs <- Model2_miiv$lhs
MIIVE2.rhs <- Model2_miiv$rhs
MIIVE2.es <- Model2_miiv$est
MIIVE2.se <- Model2_miiv$se
MIIVE2.p <- Model2_miiv$pvalue
miiv2 <- data.frame(MIIVE2.lhs, MIIVE2.rhs, MIIVE2.es, MIIVE2.se, MIIVE2.p)
miiv2 <- head(miiv2, 4)
colnames(miiv2)[1] <- "lhs"
colnames(miiv2)[2] <- "rhs"

## merge lava1 and miiv1
MethodsComparison2 <- merge(lava2, miiv2, by = c("lhs", "rhs"), all.x = TRUE, all.y = TRUE)
MethodsComparison2 <- head(MethodsComparison2, 4)

library(dplyr)
MethodsComparison2 <- MethodsComparison2 %>%
  mutate(ML.MIIVE.dif = ML2.es - MIIVE2.es, .after = MIIVE2.p)

library(ggplot2)
ggplot(MethodsComparison2, aes(x = ML.MIIVE.dif)) +
  geom_density()
