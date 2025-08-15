library(tidyverse)
library(Lahman)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

startyear <- 1903
endyear   <- 2024
K <- 10

# Map teamID -> franchID, then keep only CIN franchise
fran <- Teams |>
  filter(yearID >= startyear) |>
  select(teamID, franchID, yearID) |>
  distinct()

reds_pitch <- Pitching |>
  filter(yearID >= startyear, yearID <= endyear, !(lgID %in% c("FL"))) |>
  left_join(fran, by = c("teamID", "yearID")) |>
  filter(franchID == "CIN") |>
  mutate(IPouts = coalesce(IPouts, 0L))

# Build a year-by-year top-K rank table p1..pK
all_years <- tibble(yearID = startyear:endyear)

topk_wide <- reds_pitch |>
  group_by(yearID) |>
  slice_max(n = K, order_by = IPouts, with_ties = FALSE) |>
  ungroup() |>
  arrange(yearID, desc(IPouts)) |>
  group_by(yearID) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  select(yearID, rank, IPouts) |>
  pivot_wider(names_from = rank, values_from = IPouts,
              names_prefix = "p", values_fill = 0)

# Ensure all seasons present
topk_wide <- all_years |>
  left_join(topk_wide, by = "yearID") |>
  mutate(across(starts_with("p"), ~replace_na(., 0)))

# Data for Stan
Y <- topk_wide |>
  select(starts_with("p")) |>
  as.matrix()

# Y is T x K; Stan expects K x T
Y_stan <- t(Y)
T_stan <- ncol(Y_stan)

dt <- list(
  K = K,
  T = T_stan,
  y = Y_stan
)

model_file <- "stan/singleteam_multinomial.stan"
sm <- stan_model(model_file)

fit_reds <- sampling(
  sm, data = dt,
  chains = 4, iter = 3000, warmup = 1000, seed = 740,
  control = list(adapt_delta = 0.98, max_treedepth = 13)
)

save(fit_reds, file = "firstfit.RData")


res_reds <- extract(fit_reds)

summary(fit_reds)
summary(fit_reds, pars = c("sigma", "sigmabeta0", "gamma", "lp__"))
traceplot(fit_reds, pars = c("sigma", "sigmabeta0", "gamma"))


fit_reds2 <- sampling(
  sm, data = dt,
  chains = 4, iter = 3000, warmup = 1000, seed = 740,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

summary(fit_reds2, pars = c("sigma", "sigmabeta0", "gamma", "lp__"))

traceplot(fit_reds2, pars = c("sigma", "sigmabeta0", "gamma"))
