library(readr)
coildata <- coildata <- read_csv("Data/coildata.csv")

library(lavaan)
modelData <- coildata ;
modelCO <-"
! regressions 
   SP=~SP__SP1*SP1
   SP=~SP__SP2*SP2
   SP=~SP__SP3*SP3
   SP=~SP__SP4*SP4
   SP=~SP__SP5*SP5
   CR=~CR__CR1*CR1
   CR=~CR__CR2*CR2
   CR=~CR__CR3*CR3
   CR=~CR__CR4*CR4
   CR=~CR__CR5*CR5
   CD=~CD__CD1*CD1
   CD=~CD__CD2*CD2
   CD=~CD__CD3*CD3
   CD=~CD__CD4*CD4
   CD=~CD__CD5*CD5
! residuals, variances and covariances
   SP1 ~~ VAR_SP1*SP1
   SP2 ~~ VAR_SP2*SP2
   SP3 ~~ VAR_SP3*SP3
   SP4 ~~ VAR_SP4*SP4
   SP5 ~~ VAR_SP5*SP5
   CR1 ~~ VAR_CR1*CR1
   CR2 ~~ VAR_CR2*CR2
   CR3 ~~ VAR_CR3*CR3
   CR4 ~~ VAR_CR4*CR4
   CR5 ~~ VAR_CR5*CR5
   CD1 ~~ VAR_CD1*CD1
   CD2 ~~ VAR_CD2*CD2
   CD3 ~~ VAR_CD3*CD3
   CD4 ~~ VAR_CD4*CD4
   CD5 ~~ VAR_CD5*CD5
   SP ~~ 1.0*SP
   CR ~~ 1.0*CR
   CD ~~ 1.0*CD
   SP ~~ COV_SP_CR*CR
   CR ~~ COV_CR_CD*CD
   SP ~~ COV_SP_CD*CD
! observed means
   SP1~1;
   SP2~1;
   SP3~1;
   SP4~1;
   SP5~1;
   CR1~1;
   CR2~1;
   CR3~1;
   CR4~1;
   CR5~1;
   CD1~1;
   CD2~1;
   CD3~1;
   CD4~1;
   CD5~1;
"
result7 <- cfa(modelCO, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE, std.lv = TRUE);

CO <- summary(result7, fit.measures=TRUE)
CollaborationParEst <- data.frame(CO$pe)
CollaborationFIT <- data.frame(CO$fit)

library(semPlot)
png("Results/F1.png", width = 35, height = 20, units = 'in', res = 300)
semPaths(result7, whatLabels = "none", layout = "tree", color = list(
  lat = c(rgb(0, 204, 0, maxColorValue = 255),
          rgb(255, 255, 0, maxColorValue = 255),
          rgb(255, 0, 0, maxColorValue = 255)),
  man = c(rgb(155, 255, 175, maxColorValue = 255),
          rgb(155, 255, 175, maxColorValue = 255),
          rgb(155, 255, 175, maxColorValue = 255),
          rgb(155, 255, 175, maxColorValue = 255),
          rgb(155, 255, 175, maxColorValue = 255),
          rgb(255, 255, 153, maxColorValue = 255),
          rgb(255, 255, 153, maxColorValue = 255),
          rgb(255, 255, 153, maxColorValue = 255),
          rgb(255, 255, 153, maxColorValue = 255),
          rgb(255, 255, 153, maxColorValue = 255),
          rgb(255, 102, 102, maxColorValue = 255),
          rgb(255, 102, 102, maxColorValue = 255),
          rgb(255, 102, 102, maxColorValue = 255),
          rgb(255, 102, 102, maxColorValue = 255),
          rgb(255, 102, 102, maxColorValue = 255))),
  edge.color = "black",
  edge.label.cex = 1,
  edge.width = 1.5,
  label.cex = 1,
  node.width = 1,
  node.height = 1,
  mar = c(5, 1, 5, 1), intercepts = FALSE, residuals = FALSE, nCharNodes = 0)
dev.off()







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
ParEst2 <- data.frame(CO2$pe)
FIT2 <- data.frame(CO2$fit)

library(semPlot)
png("Results/F3.png", width = 35, height = 20, units = 'in', res = 300)
semPaths(result8, whatLabels = "std", layout = "tree", color = list(
  lat = c(rgb(0, 204, 0, maxColorValue = 255),
          rgb(255, 255, 0, maxColorValue = 255),
          rgb(255, 0, 0, maxColorValue = 255)),
  man = c(rgb(155, 253, 175, maxColorValue = 255),
          rgb(155, 255, 175, maxColorValue = 255),
          rgb(155, 255, 175, maxColorValue = 255),
          rgb(155, 255, 175, maxColorValue = 255),
          rgb(255, 255, 153, maxColorValue = 255),
          rgb(255, 255, 153, maxColorValue = 255),
          rgb(255, 255, 153, maxColorValue = 255),
          rgb(255, 102, 102, maxColorValue = 255),
          rgb(255, 102, 102, maxColorValue = 255),
          rgb(255, 102, 102, maxColorValue = 255))),
  edge.color = "black",
  edge.label.cex = 1,
  edge.width = 1.5,
  label.cex = 1,
  node.width = 1,
  node.height = 1,
  mar = c(5, 1, 5, 1), intercepts = FALSE, residuls = FALSE, nCharNodes = 0)
dev.off()

