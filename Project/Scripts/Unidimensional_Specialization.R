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

result2 <- cfa(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
summary(result2, fit.measures=TRUE);

SP <- summary(result2, fit.measures=TRUE)
SP <- data.frame(SP$fit)
SP$Index <- rownames(SP)
