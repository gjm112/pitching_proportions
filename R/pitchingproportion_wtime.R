library(tidyverse)
library(Lahman)
library(DT)
library(gridExtra)
library(grid)

startyear <- 2000
years <- 2024-startyear 

dt <- Pitching |>
  filter(yearID >= startyear)|>
  group_by(teamID, yearID) |>
  slice_max(n = 10,order_by =  IPouts, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    pitcher = rep(paste0("p",1:10), 30*years)
  ) |>
  select(pitcher, yearID, teamID, IPouts) |>
  pivot_wider(values_from = IPouts, names_from = pitcher)


K <- 10
N <- 30
T <- years
y <- array(NA, dim = c(N,K,T))

for (t in 1:T){
  yyy <- startyear - 1 + t
  temp <- dt %>% filter(yearID == yyy) %>% select(p1:p10) %>% as.matrix()
  y[,,t] <- t(apply(temp, 1, cumsum))
}

n <- matrix(NA, nrow = N, ncol = T)
for (t in 1:T){
  n[,t] <- y[,K,t]
}

#yr <- dt$yearID - min(dt$yearID) + 1

dt1 <- list(
  K = K,
  N = N,
  T = T,
  y = y,
  n = n
)

library(rstan)
fit2 <- stan(
  file = "~/stan/pitchingproportion_logisticregression_wtime.stan", 
  data = dt1,
  chains = 4,
  warmup = 2000,
  iter = 4000,
  cores = 4,
  seed = 740
)


plot(fit2)

str(fit2)

library(tidybayes)

names(fit2@sim$samples[[1]])
hist(fit2@sim$samples[[1]]["gamma"][[1]])
mean(fit2@sim$samples[[1]]["gamma"][[1]])

names(fit2@sim$samples[[1]])
results <- data.frame()
for (j in 1:K){
  for (t in 1:T){
    ind <- paste0("beta[",j,",",t,"]")
    results <- rbind(results,data.frame(j = j, t = t,beta = fit2@sim$samples[[1]][[ind]]))
  }
}

summ <- results %>% group_by(j, t) %>% summarize(beta = mean(beta)) %>% mutate(cump = exp(beta)/(1+exp(beta))) %>% arrange(t,j) %>% group_by(t) %>% mutate(p = c(head(cump,1),diff(cump)))
ggplot(aes(x = t + 1990, y = p, color = as.factor(j)), data = summ) + geom_point() + geom_line() + geom_smooth(method = "lm")
