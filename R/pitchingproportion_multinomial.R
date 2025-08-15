library(tidyverse)
library(Lahman)
#library(DT)
#library(gridExtra)
#library(grid)
library(rstan)
library(bayesplot)

head(Teams)

startyear <- 1903
2024-startyear 

TeamsFranchises |>
  filter(franchID %in% c("BFL", "BLT", "BTT", "CHH", "KCP", "NEW", "PBS", "SLT"))

Teams |>
  filter(franchID %in% c("BFL", "BLT", "BTT", "CHH", "KCP", "NEW", "PBS", "SLT"))

fran <- Teams |> 
  filter(yearID >= startyear)|>
  select(teamID, franchID) |>
  distinct()



dt <- Pitching |>
  #filter(yearID == 2023) |>
  filter(yearID >= startyear,
         !(lgID == "FL"))|>
  left_join(fran, by = "teamID") |>
  group_by(franchID, yearID) |>
  slice_max(n = 15, order_by =  IPouts, with_ties = FALSE) |>
  ungroup() |>
  select(franchID, yearID, IPouts) |>
  group_by(franchID, yearID) |>
  mutate(pitcher = paste0("p", row_number())) |>
  ungroup() |>
  pivot_wider(values_from = IPouts, names_from = pitcher)

yearID <- unique(dt$yearID)
franchID <- as.character(unique(dt$franchID))
skeleton <- cross_join(as.data.frame(yearID), as.data.frame(franchID))

dat <- left_join(skeleton, dt, by = c("yearID", "franchID"))
dat[is.na(dat)] <- 0

team_num <- Pitching |>
  filter(yearID >= startyear)|>
  left_join(fran, by = "teamID") |>
  group_by(yearID) |>
  summarize(
    teams = n_distinct(teamID)
  ) |>
  pull(teams)

K <- 15
#N <- team_num # This is gonna get funky in the stan file, may want to make it 30 and deal with NAs or 0s
T <- length(yearID)
y <- array(NA, dim = c(30,K,T))

for (t in 1:T){
  yyy <- startyear - 1 + t
  temp <- dat %>% filter(yearID == yyy) %>% select(p1:p15) %>% as.matrix()
  y[,,t] <- temp
}

n <- matrix(NA, nrow = 30, ncol = T) ## Need to fill n with the total outs pitched for the top 15 players for each team each season since 1903 (do we want NA or 0 for the teams that did not exist)
for (t in 1:T){
  n[,t] <- rowSums(y[,,t])
}

ind <- vector("list", length = T)
for (t in 1:T) {
  ind[[t]] <- which(n[, t] > 0)
}

all_ind <- unlist(ind)
N <- length(all_ind)

ind_start <- cumsum(c(1, sapply(ind, length)[-T]))
ind_end <- cumsum(sapply(ind, length))


dt1 <- list(
  K = K, # Number of pitchers = 15
  T = T, # Year since 1903 
  y = y, # Aggregated list of outs pitched (stick-building)
  n = n,  # Total outs pitched by team by year
  N = N,
  all_ind = all_ind,
  ind_start = ind_start,
  ind_end = ind_end
)

fit2 <- stan(
  file = "stan/pitchingproportion_multinomial.stan", 
  data = dt1,
  chains = 4,
  warmup = 1000,
  iter = 3000,
  cores = 4,
  seed = 740
)

summary(fit3, pars = c("sigma", "sigmabeta0", paste0("beta0[", 1:K, "]"))) 

traceplot(fit, pars = c("sigma", "sigmabeta0", paste0("beta0[", 1:K, "]")),
          inc_warmup = FALSE)


mcmc_trace(as.array(fit2), pars = c("sigma", "sigmabeta0", "gamma"))
mcmc_trace(as.array(fit2), pars = paste0("beta0[", 1:5, "]"))

save(fit, file = "fit.RData")
summary(fit, pars = c("sigma", "sigmabeta0", "gamma"))
draws_fit1 <- extract(fit)


fit2 <- load("fit2.RData")


fit3 <- stan(
  file = "stan/pitchingproportion_multinomial.stan", 
  data = dt1,
  chains = 4,
  warmup = 1000,
  iter = 3000,
  cores = 4,
  seed = 740
)

save(fit3, file = "fit3.RData")
load("fit3.RData")

mcmc_trace(fit3, pars = "gamma")

summary(fit3, pars = c("sigma", "sigmabeta0", "gamma"))
traceplot(fit3, pars = c("sigma", "sigmabeta0", "gamma"))


draws <- extract(fit3)
thetas <- draws$theta
dim(thetas)

apply(thetas, c(2,3), mean) |>
  as.data.frame() |>
  colnames(past0("y_",1:121)) |>
  pivot_longer()

init_fun <- function() list(
  z = matrix(0, nrow = K, ncol = T),
  beta0_raw = rep(0, K),
  sigma = 0.3,
  sigmabeta0 = 3,
  gamma_un = 3
)

model <- stan_model("stan/pitchingproportion_multinomial.stan")
save(model, file = "model.rds")

fit4 <- sampling(
  model,
  data = dt1,
  init = init_fun,
  chains = 4,
  warmup = 1000,
  iter = 3000,
  cores = 4,
  seed = 740
)

save(fit4, file = "fit4.RData")


fit5 <- sampling(
  model,
  data = dt1,
  chains = 4,
  cores = 4,
  warmup = 100,
  iter = 3000,
  seed = 740,
  refresh = 1
)

summary(fit5, pars = c("sigma", "sigmabeta0", "gamma", "lp__"))
traceplot(fit5, pars = c("sigma", "sigmabeta0", "gamma"))

init_fun <- function() list(
  z = matrix(rnorm(K * T, 0, 1), nrow = K, ncol = T),
  beta0_std = rnorm(K, 0, 1),
  sigma = runif(1, 0.15, 0.5),            # around 0.3
  log_sigmabeta0 = rnorm(1, log(3.5), 0.1),
  gamma_un = rnorm(1, 2.3, 0.3)
)

fit6 <- sampling(
  model,
  data = dt1,
  init = init_fun,
  chains = 4,
  cores = 4,
  warmup = 1000,
  iter = 3000,
  seed = 740,
  control = list(adapt_delta = 0.85,
                 max_treedepth = 12),
  refresh = 100
)
