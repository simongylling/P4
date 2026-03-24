library(tidyverse)
library(ggplot2)
library(quantmod)
library(moments)
library(tseries)
library(GGally)

#
output_file <- "C:/Users/Victor/OneDrive/AAU/4. semester/Projekt/Overleaf/Billeder"
set.seed(123)
#

## rho = 1

# Simuler "returns"
r_x1 <- rnorm(500, 0, 0.01)
r_y1 <- 2 * r_x1   

returns_1 <- na.omit(data.frame(X = r_x1, Y = r_y1))

# Kovarians og korrelation
cov_1 <- cov(returns_1)
cor_1 <- cor(returns_1)

cov_1
cor_1

# Variance of sum
var_x1 <- var(r_x1)
var_y1 <- var(r_y1)

cov_xy_1 <- cov(returns_1)[1,2]
vos_1 <- var_x1 + var_y1 + 2 * cov_xy_1
vos_1

# Plot
P1 <- ggpairs(returns_1,
        title = "Simulated Returns: rho = 1",
        upper = list(continuous = wrap("cor", size = 5)),
        lower = list(continuous = wrap("points", alpha = 0.5)),
        diag = list(continuous = wrap("densityDiag")))

ggsave(
  filename = paste0(output_file, "/rho1.png"),
  plot = P1,
  width = 8,
  height = 6,
  dpi = 300
)


## rho = -1

# Simuler "returns"
r_x2 <- rnorm(500, 0, 0.01)
r_y2 <- -2 * r_x2

returns_2 <- na.omit(data.frame(X = r_x2, Y = r_y2))

# Kovarians og korrelation
cov_2 <- cov(returns_2)
cor_2 <- cor(returns_2)
cov_2
cor_2

# Variance of sum
var_x2 <- var(r_x2)
var_y2 <- var(r_y2)

cov_xy_2 <- cov(returns_2)[1,2]
vos_2 <- var_x2 + var_y2 + 2 * cov_xy_2
vos_2

P2 <- ggpairs(returns_2,
        title = "Simulated Returns: rho = -1",
        upper = list(continuous = wrap("cor", size = 5)),
        lower = list(continuous = wrap("points", alpha = 0.5)),
        diag = list(continuous = wrap("densityDiag")))

ggsave(
  filename = paste0(output_file, "/rho_n1.png"),
  plot = P2,
  width = 8,
  height = 6,
  dpi = 300
)


## rho = 0

# Simuler "returns"
r_x0 <- rnorm(500, 0, 0.01)
r_y0 <- rnorm(500, 0, 0.01)

returns_0 <- na.omit(data.frame(X = r_x0, Y = r_y0))

# Kovarians og korrelation
cov_0 <- cov(returns_0)
cor_0 <- cor(returns_0)
cov_0
cor_0

# Variance of sum
var_x0 <- var(r_x0)
var_y0 <- var(r_y0)

cov_xy_0 <- cov(returns_0)[1,2]
vos_0 <- var_x0 + var_y0 + 2 * cov_xy_0
vos_0

P3 <- ggpairs(returns_0,
        title = "Simulated Returns: rho ≈ 0",
        upper = list(continuous = wrap("cor", size = 5)),
        lower = list(continuous = wrap("points", alpha = 0.5)),
        diag = list(continuous = wrap("densityDiag")))

ggsave(
  filename = paste0(output_file, "/rho0.png"),
  plot = P3,
  width = 8,
  height = 6,
  dpi = 300
)