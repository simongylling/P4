library(tidyverse)
library(ggplot2)
library(quantmod)
library(moments)
library(tseries)
library(GGally)

# Hent aktier
getSymbols("DX-Y.NYB", src = "yahoo", from = "2020-01-01")
getSymbols("GLD", src = "yahoo", from = "2020-01-01")

# Daglige priser
P_usd <- Ad(`DX-Y.NYB`)
P_gld <- Ad(GLD)

# Daglige log-afkast
r_usd <- diff(log(P_usd))
r_gld <- diff(log(P_gld))

# Saml i en matrix og fjern NA
returns <- na.omit(merge(r_usd, r_gld))
colnames(returns) <- c("USD", "GOLD")

# Annualisering
trading_days <- 252

cov_annual <- cov(returns) * trading_days
var_nvda_annual <- cov_annual[1,1]
var_amd_annual  <- cov_annual[2,2]
cor_matrix <- cor(returns)  # Korrelation ændres ikke

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
