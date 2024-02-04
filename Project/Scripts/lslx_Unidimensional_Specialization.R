library(readr)
coildata <- coildata <- read_csv("Data/coildata.csv")
library(lslx)
modelSP <-"
   SP =~ SP1 + SP3 + SP4 + SP5
   SP ~~ 1 * SP
"

lslx_fa <- lslx$new(model = modelSP,
                    data = coildata)

lslx_fa$print()

lslx_fa$fit(
  penalty_method = "mcp",
  lambda_grid = seq(.01, .60, .01),
  delta_grid = c(1.5, 3.0, Inf)
)$fitted.lslx()

lslx_fa$summarize(selector = "bic", interval = FALSE)

pave <- plsem(model = modelSP,
              data = coildata,
              penalty_method = "mcp",
              lambda_grid = seq(.01, .60, .01),
              delta_grid = c(1.5, 3.0, Inf))

hola <- data.frame(pave$extract_fit_index(selector = "bic"))

pave$summarize(selector = "bic")
pave$test_lr(selector = "bic")
holas <- pave$test_lr(selector = "bic")


pave$plot_fit_index()
pave$plot_numerical_condition()
pave$plot_coefficient()
