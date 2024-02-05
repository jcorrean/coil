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

lslx_fa <- lslx$new(model = modelCO,
                    data = coildata)

lslx_fa$print()

lslx_fa$fit(
  penalty_method = "mcp",
  lambda_grid = seq(.01, .60, .01),
  delta_grid = "default"
)

lslx_fa$summarize(selector = "bic", interval = FALSE)

pave <- plsem(model = modelCO,
              data = coildata,
              penalty_method = "mcp",
              lambda_grid = seq(.01, .60, .01),
              delta_grid = c(1.5, 3.0, Inf))

hola <- data.frame(pave$extract_fit_index(selector = "bic"))
EstimatesLSLX <- pave$test_coefficient(selector = "bic")

pave$summarize(selector = "bic")$intercepts
pave$test_lr(selector = "bic")
holas <- pave$test_lr(selector = "bic")


pave$plot_fit_index()
pave$plot_numerical_condition()
pave$plot_coefficient()
rm(list=setdiff(ls(), "EstimatesLSLX"))
save.image("~/Documents/GitHub/coil/Project/Results/lslx_Estimates.RData")

