library(tidyverse)
library(Lahman)
library(DT)
library(gridExtra)
library(grid)

head(Teams)

startyear <- 1903
years <- 2024-startyear 

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

dat <- left_join(skeleton, dt, by = c("yearID"))

team_num <- Pitching |>
  filter(yearID >= startyear)|>
  left_join(fran, by = "teamID") |>
  group_by(yearID) |>
  summarize(
    teams = n_distinct(teamID)
  ) |>
  pull(teams)


K <- 15
N <- team_num # This is gonna get funky in the stan file, may want to make it 30 and deal with NAs or 0s
T <- length(years)
y <- array(NA, dim = c(30,K,T))

for (t in 1:T){
  yyy <- startyear - 1 + t
  temp <- dat %>% filter(yearID == yyy) %>% select(p1:p15) %>% as.matrix()
  y[,,t] <- t(apply(temp, 1, cumsum))
}

n <- matrix(NA, nrow = 30, ncol = T) ## Need to fill n with the total outs pitched for the top 15 players for each team each season since 1903 (do we want NA or 0 for the teams that did not exist)

for (t in 1:T){
  n[,t] <- y[,K,t]
}


dt1 <- list(
  K = K, # Number of pitchers = 15
  N = N, # Number of teams
  T = T, # Year since 1903 -- THIS NEEDS FIXED SINCE I CHANGED T TO CREATE y
  y = y, # Aggregated list of outs pitched (stick-building)
  n = n  # Total outs pitched by team by year
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