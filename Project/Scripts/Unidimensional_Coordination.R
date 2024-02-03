library(readr)
coildata <- coildata <- read_csv("Data/coildata.csv")

library(lavaan);
modelData <- coildata
model<-"
! regressions 
   CD=~CD__CD1*CD1
   CD=~CD__CD2*CD2
   CD=~CD__CD3*CD3
   CD=~CD__CD4*CD4
   CD=~CD__CD5*CD5
! residuals, variances and covariances
   CD1 ~~ VAR_CD1*CD1
   CD2 ~~ VAR_CD2*CD2
   CD3 ~~ VAR_CD3*CD3
   CD4 ~~ VAR_CD4*CD4
   CD5 ~~ VAR_CD5*CD5
   CD ~~ 1.0*CD
! observed means
   CD1~1;
   CD2~1;
   CD3~1;
   CD4~1;
   CD5~1;
";

result2 <- cfa(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
summary(result2, fit.measures=TRUE);

CD <- summary(result2, fit.measures=TRUE)
CD$test
CD <- data.frame(CD$fit)
CD$Index <- rownames(CD)
