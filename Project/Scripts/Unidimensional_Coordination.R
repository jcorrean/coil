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


library(lavaan);
modelData <- coildata
modelCD<-"
! regressions 
   CD=~CD__CD1*CD1
   CD=~CD__CD2*CD2
   CD=~CD__CD4*CD4
! residuals, variances and covariances
   CD1 ~~ VAR_CD1*CD1
   CD2 ~~ VAR_CD2*CD2
   CD4 ~~ VAR_CD4*CD4
   CD ~~ 1.0*CD
! observed means
   CD1~1;
   CD2~1;
   CD4~1;
";



result6 <- cfa(modelCD, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE, std.lv = TRUE);

CD2 <- summary(result6, fit.measures=TRUE)
CD2 <- data.frame(CD2$fit)
CD2

library(semPlot)
png("Results/F4C.png", width = 15, height = 10, units = 'in', res = 300)
semPaths(result6, 
         whatLabels = "std", 
         layout = "tree", 
         color = list(lat = rgb(255, 0, 0, maxColorValue = 255),
                      man = rgb(255, 102, 102, maxColorValue = 255)),
         edge.color = "black",
         shapeMan = "rectangle",
         edge.label.cex = 1.5,
         edge.width = 1.5,
         label.cex = 1.5,
         node.width = 2,
         node.height = 2,
         mar = c(6, 1.2, 9, 1.2), title = FALSE, intercepts = FALSE, residuals = TRUE, nCharNodes = 0)
dev.off()