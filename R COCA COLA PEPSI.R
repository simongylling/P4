library(tidyverse)
library(ggplot2)
library(quantmod)
library(moments)
library(tseries)
library(GGally)

# Hent aktier
getSymbols("KO", src = "yahoo", from = "2020-01-01")
getSymbols("PEP", src = "yahoo", from = "2020-01-01")

# Daglige priser
P_ko  <- Ad(KO)
P_pep <- Ad(PEP)

# Daglige log-afkast
r_ko  <- diff(log(P_ko))
r_pep <- diff(log(P_pep))

# Saml i en matrix og fjern NA
returns <- na.omit(merge(r_ko, r_pep))
colnames(returns) <- c("KO", "PEP")

# Annualisering
trading_days <- 252

cov_annual <- cov(returns) * trading_days
var_ko_annual  <- cov_annual[1,1]
var_pep_annual <- cov_annual[2,2]
cor_matrix <- cor(returns)  # Korrelationskoefficient ændres ikke

# Print annualiserede værdier
cov_annual
cor_matrix

# Scatterplot matrix
returns_df <- as.data.frame(returns)
ggpairs(returns_df,
        title = "Scatterplot Matrix: Coca-Cola vs Pepsi",
        upper = list(continuous = wrap("cor", size = 5)),
        lower = list(continuous = wrap("points", alpha = 0.5)),
        diag = list(continuous = wrap("densityDiag")))