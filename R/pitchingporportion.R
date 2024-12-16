library(tidyverse)
library(Lahman)
library(DT)
library(gridExtra)
library(grid)

dt <- Pitching |>
  filter(yearID == 2023)|>
  group_by(teamID) |>
  slice_max(n = 5,order_by =  IPouts) |>
  ungroup() |>
  mutate(
    pitcher = rep(c("p1", "p2", "p3", "p4", "p5"), 30)
  ) |>
  select(pitcher, teamID, IPouts) |>
  pivot_wider(values_from = IPouts, names_from = pitcher)

dt1 <- list(
  K = 5,
  N = 30,
  y = as.matrix(dt[,-1])
)

library(rstan)
fit2 <- stan(
  file = "/Users/gregorymatthews/Dropbox/pitchingproportion.stan", 
  data = dt1,
  chains = 4,
  warmup = 2000,
  iter = 4000,
  cores = 1,
  refresh = 0,
  seed = 740
)


plot(fit2)
results <- data.frame(
  p1 = fit2@sim$samples[[1]][[1]],
  p2 = fit2@sim$samples[[1]][[2]],
  p3 = fit2@sim$samples[[1]][[3]],
  p4 = fit2@sim$samples[[1]][[4]],
  p5 = fit2@sim$samples[[1]][[5]])
  
results <- results %>% mutate(theta1 = p1, 
                   theta2 = (1-p1)*p2,
                   theta3 = (1-p1)*(1-p2)*p3,
                   theta4 = (1-p1)*(1-p2)*(1-p3)*p4,
                   theta5 = (1-p1)*(1-p2)*(1-p3)*(1-p4)*p5)

results %>% select(theta1:theta5) %>% pivot_longer(names_to = "p", cols = 1:5) %>% ggplot(aes(x = value, color = p)) + geom_density()


##################

dt <- Pitching |>
  filter(yearID == 2023)|>
  group_by(teamID) |>
  slice_max(n = 10,order_by =  IPouts) |>
  ungroup() |>
  mutate(
    pitcher = rep(paste0("p",1:10), 30)
  ) |>
  select(pitcher, teamID, IPouts) |>
  pivot_wider(values_from = IPouts, names_from = pitcher)

y = as.matrix(dt[,-1])
n <- apply(y,1,sum)
y <- t(apply(y, 1, cumsum))

dt1 <- list(
  K = 10,
  N = 30,
  y = y,
  n = n
)

library(rstan)
fit2 <- stan(
  file = "/Users/gregorymatthews/Dropbox/pitchingproportion_logisticregression.stan", 
  data = dt1,
  chains = 4,
  warmup = 2000,
  iter = 4000,
  cores = 1,
  refresh = 0,
  seed = 740
)


plot(fit2)
results <- data.frame(
  p1 = fit2@sim$samples[[1]][[1]],
  p2 = fit2@sim$samples[[1]][[2]],
  p3 = fit2@sim$samples[[1]][[3]],
  p4 = fit2@sim$samples[[1]][[4]],
  p5 = fit2@sim$samples[[1]][[5]],
  p6 = fit2@sim$samples[[1]][[6]],
  p7 = fit2@sim$samples[[1]][[7]],
  p8 = fit2@sim$samples[[1]][[8]],
  p9 = fit2@sim$samples[[1]][[9]],
  p10 = fit2@sim$samples[[1]][[10]]
  )

apply(results,2,mean)
sum(apply(results,2,mean))
results <- results %>% mutate(theta1 = p1, 
                              theta2 = p2-p1,
                              theta3 = p3-p2,
                              theta4 = p4-p3,
                              theta5 = p5-p4,
                              theta6 = p6-p5,
                              theta7 = p7-p6,
                              theta8 = p8-p7,
                              theta9 = p9-p8,
                              theta10 = p10-p9)



results %>% select(p1:p10) %>% pivot_longer(names_to = "p", cols = 1:10) %>% ggplot(aes(x = value, color = p)) + geom_density()

mn <- results %>% select(p1:p10) 
apply(mn,2,mean)



#### Now add a time component
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
  file = "/Users/gregorymatthews/Dropbox/pitchingproportion_logisticregression_wtime.stan", 
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



results <- data.frame(
  p1 = fit2@sim$samples[[1]][[1]],
  p2 = fit2@sim$samples[[1]][[2]],
  p3 = fit2@sim$samples[[1]][[3]],
  p4 = fit2@sim$samples[[1]][[4]],
  p5 = fit2@sim$samples[[1]][[5]],
  p6 = fit2@sim$samples[[1]][[6]],
  p7 = fit2@sim$samples[[1]][[7]],
  p8 = fit2@sim$samples[[1]][[8]],
  p9 = fit2@sim$samples[[1]][[9]],
  p10 = fit2@sim$samples[[1]][[10]]
)



