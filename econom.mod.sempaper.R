data1 = read.csv("C:/Users/elina/Downloads/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_115367/API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_115367.csv", skip = 4)
data2 = read.csv("C:/Users/elina/Downloads/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_121708/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_121708.csv", 
                 skip = 4)

inflation <- subset(data1, Country.Code == "CZE")
gdp <- subset(data2, Country.Code == "CZE")
years <- paste0("X", 2000:2024)

inflation_values <- as.numeric(inflation[1, years])
gdp_values <- as.numeric(gdp[1, years])

df <- data.frame(
  Year = 2000:2024,
  Inflation = inflation_values,
  GDP_Growth = gdp_values
)

df$Inflation_Lag1 <- c(NA, df$Inflation[-nrow(df)])
df <- na.omit(df)
summary(df)
cor(df[, c("Inflation", "GDP_Growth")])
library(ggplot2)
ggplot(df, aes(x = Year, y = Inflation)) +
  geom_line() +
  ggtitle("Inflation in Czechia")
ggplot(df, aes(x = Year, y = GDP_Growth)) +
  geom_line() +
  ggtitle("GDP Growth in Czechia")
ggplot(df, aes(x = Inflation, y = GDP_Growth)) +
  geom_point() +
  geom_smooth(method = "lm")

model1 <- lm(GDP_Growth ~ Inflation, data = df)

summary(model1)

model2 <- lm(GDP_Growth ~ Inflation + Inflation_Lag1, data = df)

summary(model2)

install.packages("stargazer")
library(stargazer)

stargazer(model1, model2, type = "text")

ggplot(df, aes(x = Year, y = Inflation)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Inflation in Czechia (2000–2024)",
    x = "Year",
    y = "Inflation Rate (%)"
  ) +
  theme_minimal()

ggplot(df, aes(x = Year, y = GDP_Growth)) +
  geom_line() +
  geom_point() +
  labs(
    title = "GDP Growth in Czechia (2000–2024)",
    x = "Year",
    y = "GDP Growth (%)"
  ) +
  theme_minimal()


ggplot(df, aes(x = Inflation, y = GDP_Growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Inflation and GDP Growth in Czechia",
    x = "Inflation Rate (%)",
    y = "GDP Growth (%)"
  ) +
  theme_minimal()

stargazer(
  model1,
  model2,
  type = "text",
  title = "Regression Results",
  dep.var.labels = "GDP Growth",
  column.labels = c("OLS", "FDL"),
  digits = 3,
  omit.stat = c("f", "ser")
)


install.packages("lmtest")
library(lmtest)
bptest(model2)

dwtest(model2)

stats_table <- data.frame(
  Variable = c("Inflation", "GDP Growth", "Inflation Lag"),
  
  Mean = c(
    mean(df$Inflation),
    mean(df$GDP_Growth),
    mean(df$Inflation_Lag1)
  ),
  
  Median = c(
    median(df$Inflation),
    median(df$GDP_Growth),
    median(df$Inflation_Lag1)
  ),
  
  Min = c(
    min(df$Inflation),
    min(df$GDP_Growth),
    min(df$Inflation_Lag1)
  ),
  
  Max = c(
    max(df$Inflation),
    max(df$GDP_Growth),
    max(df$Inflation_Lag1)
  )
)

round(stats_table, 3)
stats_table[, -1] <- round(stats_table[, -1], 3)

stats_table

stargazer(
  model1,
  model2,
  type = "html",
  out = "regression_table.html",
  
  title = "Regression Results",
  
  dep.var.labels = "GDP Growth",
  
  column.labels = c("OLS", "FDL"),
  
  covariate.labels = c(
    "Inflation",
    "Lagged Inflation"
  ),
  
  digits = 3,
  
  omit.stat = c("f", "ser"),
  
  no.space = TRUE
)












