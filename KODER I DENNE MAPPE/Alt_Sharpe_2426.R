prices_2425 <- download_data(type = "stock_prices",
                         symbols = c("NVDA","NXPI","EPAM","BRK-B","PYPL","FDS","CAT","DAL","POOL",
            "AMZN","NCLH","CASY","LLY","BDX","CRL","GOOG","EA","PSKY",
            "WMT","CPB","EL","XOM","OXY","APA","LIN","MLM","MOS",
            "NEE","ATO","AES","WELL","SBAC","ARE"),
                         start_date = "2024-01-01",
                         end_date = "2026-01-01")

returns_2425 <- prices_2425 %>% 
  group_by(symbol) %>% 
  arrange(date) %>% 
  mutate(dailyret = log(adjusted_close/lag(adjusted_close))) %>% 
  select(symbol, date, dailyret) %>% 
  drop_na(dailyret) %>% 
  ungroup()

assets_1 <- returns_2425 %>% 
  group_by(symbol) %>% 
  summarize(mu = mean(dailyret),
            sd = sd(dailyret))

returns_wide_1 <- returns_2425 %>% 
  pivot_wider(names_from = symbol,
              values_from = dailyret)


sigma_1 <- returns_wide_1 %>% 
  select(-date) %>% 
  cov()



u_1 <- rep(1, dim(sigma_1)[1])

sigma_inv_1 <- solve(sigma_1)

x_MIN_1 <- as.vector(sigma_inv_1 %*% u_1) / as.numeric(u_1 %*% sigma_inv_1 %*% u_1)

assets_1 <- assets_1 %>% arrange(match(symbol, colnames(sigma_1)))
assets_1 <- assets_1 %>% 
  mutate(x_MIN_1 = x_MIN_1)

R_1 <- assets_1$mu

z_star_1 <- sigma_inv_1 %*% R_1 -
  (as.numeric(u_1 %*% sigma_inv_1 %*% R_1) /
   as.numeric(u_1 %*% sigma_inv_1 %*% u_1)) *
  sigma_inv_1 %*% u_1

assets_1 <- assets_1 %>% 
  mutate(z_star_1 = z_star_1)

tau_1 <- seq(0,1, by = 0.001)
A_1 <- as.numeric(u_1 %*% sigma_inv_1 %*% R_1)
B_1 <- as.numeric(R_1 %*% sigma_inv_1 %*% R_1)
C_1 <- as.numeric(u_1 %*% sigma_inv_1 %*% u_1)
D_1 <- B_1*C_1-A_1^2

risk_free_daily_1 <- download_data(
  type = "stock_prices", symbol = "^IRX", 
  start_date = "2024-01-01", end_date = "2026-01-01"
) %>% 
  mutate(
    risk_free = (1 + adjusted_close / 100)^(1 / 252) - 1
  ) %>% 
  select(date, risk_free) %>%  
  drop_na()

rf_1 <- mean(risk_free_daily_1$risk_free)

tangency_portfolio_1 <- as.numeric((sigma_inv_1%*%(R_1-rf_1*u_1))/(A_1-rf_1*C_1)) #Weights for the tangency portfolio

sharpe_ratio_1 <- as.numeric((tangency_portfolio%*%R_1 - rf_1)/sqrt((tangency_portfolio%*% sigma_1 %*% tangency_portfolio))) 

###### Hvis du køre denne sammen med Jacobs kode og får den gammle "tangency_portfolio" ude i din data og variable consol til højre kan den kode køres, og du vil få Sharpe ratio på 2.065386.

assets_1 <- assets_1 |>
  mutate(Return_2426 = mu, Variance_2426 = sd^2) |>
  select(Return_2426, Variance_2426)

assets <- assets |>
  mutate(Return = mu, Variance = sd^2) |>
  select(-mu,-sd)

group_1 <- bind_cols(assets, assets_1)

group_1 <- group_1 |>
  mutate(Sharpe_2426 = (Return_2426 - rf_1) / sqrt(Variance_2426)) |>
  select(symbol, x_MIN, z_star, tangency_portfolio, Return, Return_2426, Variance, Variance_2426, sharpe, Sharpe_2426)