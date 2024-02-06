library(readr)
coildata <- coildata <- read_csv("Data/coildata.csv")

library(ggplot2)
coildata_long <- reshape2::melt(coildata)

png("Results/F2.png", width = 15, height = 8, units = 'in', res = 300)
ggplot(coildata_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free", ncol = 5) +
  theme_minimal() + # Optional: Customize the theme
  theme(legend.position = "none") + # This line removes the legend
  labs(title = "")
dev.off()

library(MVN)
mvn(coildata)
