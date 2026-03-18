# Kode brugt til at lave Markowitz porteføljeoptimering

# Indlæs nødvendige pakker
library(quadprog)
library(tidyverse)
library(ggplot2)
library(quantmod)
library(moments)
library(tseries)
library(GGally)

# Hent aktier
getSymbols("KO", src = "yahoo", from = "2020-01-01")
getSymbols("PEP", src = "yahoo", from = "2020-01-01")
getSymbols("NVDA", src = "yahoo", from = "2020-01-01")
getSymbols("AMD", src = "yahoo", from = "2020-01-01")
getSymbols("DX-Y.NYB", src = "yahoo", from = "2020-01-01")
getSymbols("GLD", src = "yahoo", from = "2020-01-01")

# Daglige priser
returns_list <- na.omit(merge(dailyReturn(Ad(KO)),
                              dailyReturn(Ad(PEP)),
                              dailyReturn(Ad(NVDA)),
                              dailyReturn(Ad(AMD)),
                              dailyReturn(Ad(`DX-Y.NYB`)),
                              dailyReturn(Ad(GLD))))

colnames(returns_list) <- c("KO", "PEP", "NVDA", "AMD", "DX-Y.NYB", "GLD")


# forventede afkast
expected_returns <- colMeans(returns_list)

# Kovariansmatrix
cov_matrix <- cov(returns_list)

# dimensioner
N <- length(expected_returns)

u <- rep(1, N)

#
w <- rep(1/N, N)  # Startvægt (lige vægtet)

# expected return
R_x <- sum(w * expected_returns)

# Varians
var_x <- t(w) %*% cov_matrix %*% w

# standardafvigelse
sd_x <- sqrt(var_x)

# teoretiske løsning
sigma_inv <- solve(cov_matrix)
x_min <- sigma_inv %*% u / as.numeric(t(u) %*% sigma_inv %*% u)

# z*
A <- as.numeric(t(u) %*% sigma_inv %*% expected_returns)
B <- as.numeric(t(expected_returns) %*% sigma_inv %*% expected_returns)
C <- as.numeric(t(u) %*% sigma_inv %*% u)
D <- B * C - A^2

Z <- sigma_inv %*% (expected_returns - (A/C) * (sigma_inv %*% u))

# optimal portfølge
tau <- 3

x_tau <- x_min + (tau/2) * Z
x_tau <- x_tau / sum(x_tau)  # Normaliser så vægtene summerer til 1

# Performance

R_tau <- sum(x_tau * expected_returns)
var_tau <- t(x_tau) %*% cov_matrix %*% x_tau
sd_tau <- sqrt(var_tau)

# forventede afkast
mu_seq <- seq(min(expected_returns), max(expected_returns), length.out = 100)

# varians og risiko for hver forventet afkast
var_seq <- (C * mu_seq^2 - 2 * A * mu_seq + B) / D
sd_seq <- sqrt(var_seq)

# Plot efficient frontier
df <- data.frame(Return = mu_seq, Risk = sd_seq)

mu_min <- df$Return[which.min(df$Risk)]
efficient <- df[df$Return >= mu_min, ]

ggplot(df, aes(Risk, Return)) +
  geom_path(color = "grey", linewidth = 0.8) +
  geom_path(data = efficient, color = "steelblue", linewidth = 1.2) +
  
  annotate("point", x = sd_x, y = R_x, color = "red", size = 3) +
  annotate("point", x = sd_tau, y = R_tau, color = "green", size = 3) +
  
  annotate("text", x = sd_x, y = R_x, label = "Equal Weight", vjust = -1) +
  annotate("text", x = sd_tau, y = R_tau, label = "Optimal Portfolio", vjust = -1) +
  
  labs(title = "Efficient Frontier",
       x = "Risk",
       y = "Return") +
  theme_minimal()