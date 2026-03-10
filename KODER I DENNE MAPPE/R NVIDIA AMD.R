library(tidyverse)
library(ggplot2)
library(quantmod)
library(moments)
library(tseries)
library(GGally)

# Hent aktier
getSymbols("NVDA", src = "yahoo", from = "2020-01-01")
getSymbols("AMD", src = "yahoo", from = "2020-01-01")

# Daglige priser
P_nvda <- Ad(NVDA)
P_amd <- Ad(AMD)

# Daglige log-afkast
r_nvda <- diff(log(P_nvda))
r_amd <- diff(log(P_amd))

# Saml i en matrix og fjern NA
returns <- na.omit(merge(r_nvda, r_amd))
colnames(returns) <- c("NVDA", "AMD")

# Annualisering
trading_days <- 252

cov_annual <- cov(returns) * trading_days
var_nvda_annual <- cov_annual[1,1]
var_amd_annual  <- cov_annual[2,2]
cor_matrix <- cor(returns)  # Korrelation ændres ikke

# Print annualiserede værdier
cov_annual
cor_matrix
