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
  y[,,t] <- t(apply(temp, 1, cumsum))
}



n <- matrix(NA, nrow = 30, ncol = T) ## Need to fill n with the total outs pitched for the top 15 players for each team each season since 1903 (do we want NA or 0 for the teams that did not exist)
for (t in 1:T){
  n[,t] <- y[,K,t]
}

ind <- list()
for (yyy in 1:ncol(n)){
  ind[[yyy]] <- which(n[,yyy] > 0)
}

# n <- dat |> 
#   replace(is.na(dat), 0) |>
#   mutate(Outs = rowSums(across(p1:p15))) |>
#   select(franchID, yearID, Outs) |>
#   arrange(yearID, franchID) |>
#   pivot_wider(names_from = yearID, values_from = Outs) |>
#   select(-1) |>
#   as.matrix() # This is correct, but there are column names (idk if that works) and 0's instead of NAs


dt1 <- list(
  K = K, # Number of pitchers = 15
  T = T, # Year since 1903 
  y = y, # Aggregated list of outs pitched (stick-building)
  n = n,  # Total outs pitched by team by year
  ind = ind 
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