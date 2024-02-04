library(readr)
coildata <- coildata <- read_csv("Data/coildata.csv")

library(lavaan);
modelData <- coildata
model<-"
! regressions 
   CR=~CR__CR1*CR1
   CR=~CR__CR2*CR2
   CR=~CR__CR3*CR3
   CR=~CR__CR4*CR4
   CR=~CR__CR5*CR5
! residuals, variances and covariances
   CR1 ~~ VAR_CR1*CR1
   CR2 ~~ VAR_CR2*CR2
   CR3 ~~ VAR_CR3*CR3
   CR4 ~~ VAR_CR4*CR4
   CR5 ~~ VAR_CR5*CR5
   CR ~~ 1.0*CR
! observed means
   CR1~1;
   CR2~1;
   CR3~1;
   CR4~1;
   CR5~1;
";

result3 <- cfa(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE, std.lv = TRUE);
summary(result3, fit.measures=TRUE);

ML <- summary(result3, fit.measures=TRUE)
MLestimates <- data.frame(ML$pe)

library(MIIVsem)
miivs(model)
model1_miiv <- miive(model = model, data = coildata, sarg.adjust = "holm")
Credibility <- estimatesTable(model1_miiv)

ML.lhs <- Credibility$lhs
ML.rhs <- Credibility$rhs
ML.es <- head(MLestimates$est, 5)
ML.se <- head(ML$pe[7], 5)
ML.p <- head(ML$pe[9], 5)
lava1 <- data.frame(ML.lhs, ML.rhs, ML.es, ML.se, ML.p)
colnames(lava1)[1] <- "lhs"
colnames(lava1)[2] <- "rhs"

MIIVE.lhs <- Credibility$lhs
MIIVE.rhs <- Credibility$rhs
MIIVE.es <- Credibility$est
MIIVE.se <- Credibility$se
MIIVE.p <- Credibility$pvalue
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
modelCR<-"
! regressions 
   CR=~CR__CR1*CR1
   CR=~CR__CR2*CR2
   CR=~CR__CR3*CR3
! residuals, variances and covariances
   CR1 ~~ VAR_CR1*CR1
   CR2 ~~ VAR_CR2*CR2
   CR3 ~~ VAR_CR3*CR3
   CR ~~ 1.0*CR
! observed means
   CR1~1;
   CR2~1;
   CR3~1;
";

result4 <- cfa(modelCR, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE, std.lv = TRUE);

CR2 <- summary(result4, fit.measures=TRUE)
CR2 <- data.frame(CR2$fit)

ML2 <- summary(result4, fit.measures=TRUE)
ML2estimates <- data.frame(ML2$pe)

library(MIIVsem)
miivs(modelCR)
model2_miiv <- miive(model = modelCR, data = coildata, sarg.adjust = "holm")
Model2_miiv <- estimatesTable(model2_miiv)

ML2.lhs <- Model2_miiv$lhs
ML2.rhs <- Model2_miiv$rhs
ML2.es <- head(ML2estimates$est, 3)
ML2.se <- head(ML2$pe[7], 3)
ML2.p <- head(ML2$pe[9], 3)
lava2 <- data.frame(ML2.lhs, ML2.rhs, ML2.es, ML2.se, ML2.p)
colnames(lava2)[1] <- "lhs"
colnames(lava2)[2] <- "rhs"

MIIVE2.lhs <- Model2_miiv$lhs
MIIVE2.rhs <- Model2_miiv$rhs
MIIVE2.es <- Model2_miiv$est
MIIVE2.se <- Model2_miiv$se
MIIVE2.p <- Model2_miiv$pvalue
miiv2 <- data.frame(MIIVE2.lhs, MIIVE2.rhs, MIIVE2.es, MIIVE2.se, MIIVE2.p)
miiv2 <- head(miiv2, 3)
colnames(miiv2)[1] <- "lhs"
colnames(miiv2)[2] <- "rhs"

## merge lava1 and miiv1
MethodsComparison2 <- merge(lava2, miiv2, by = c("lhs", "rhs"), all.x = TRUE, all.y = TRUE)
MethodsComparison2 <- head(MethodsComparison2, 3)

library(dplyr)
MethodsComparison2 <- MethodsComparison2 %>%
  mutate(ML.MIIVE.dif = ML2.es - MIIVE2.es, .after = MIIVE2.p)

library(ggplot2)
ggplot(MethodsComparison2, aes(x = ML.MIIVE.dif)) +
  geom_density()
