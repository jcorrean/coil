library(readr)
coildata <- coildata <- read_csv("Data/coildata.csv")

library(lavaan);
modelData <- read.table(DATAFILENAME, header = TRUE) ;
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

result2 <- cfa(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
summary(result2, fit.measures=TRUE);

CR <- summary(result2, fit.measures=TRUE)
CR$test
CR <- data.frame(CR$fit)
CR$Index <- rownames(CR)
