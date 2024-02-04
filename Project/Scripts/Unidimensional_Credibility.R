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

result2 <- cfa(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE);
summary(result2, fit.measures=TRUE);

CR <- summary(result2, fit.measures=TRUE)
CR$test
CR <- data.frame(CR$fit)
CR$Index <- rownames(CR)


modelData <- coildata
model<-"
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

result4 <- cfa(model, data=coildata, fixed.x=FALSE, estimator="MLM", std.ov = TRUE, std.lv = TRUE);

CR2 <- summary(result4, fit.measures=TRUE)
CR2 <- data.frame(CR2$fit)
CR2

library(semPlot)
png("Results/F4B.png", width = 15, height = 10, units = 'in', res = 300)
semPaths(result4, 
         whatLabels = "std", 
         layout = "tree", 
         color = list(lat = rgb(255, 255, 0, maxColorValue = 255),
                      man = rgb(255, 255, 153, maxColorValue = 255)),
         edge.color = "black",
         shapeMan = "rectangle",
         edge.label.cex = 1.5,
         edge.width = 1.5,
         label.cex = 1.5,
         node.width = 2,
         node.height = 2,
         mar = c(6, 1.2, 9, 1.2), title = FALSE, intercepts = FALSE, residuals = TRUE, nCharNodes = 0)
dev.off()