library(tidyverse)
library(Lahman)
library(DT)
library(gridExtra)
library(grid)

startyear <- 1903
years <- 2024-startyear 

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


all_ind <- unlist(ind)
ind_start <- cumsum(c(1, sapply(ind, length)[-length(ind)]))
ind_end <- cumsum(sapply(ind, length))

load("results_10000iter.rda")

results <- data.frame()
for (j in 1:K){print(j)
  ind0 <- paste0("beta0[",j,"]")
  for (t in 1:T){
    ind <- paste0("beta[",j,",",t,"]")
    results <- rbind(results,data.frame(j = j, t = t,beta = c(fit10000@sim$samples[[1]][[ind]][-c(1:2000)],
                                                              fit10000@sim$samples[[3]][[ind]][-c(1:2000)],
                                                              fit10000@sim$samples[[4]][[ind]][-c(1:2000)]) + 
                                          c(fit10000@sim$samples[[1]][[ind0]][-c(1:2000)],
                                            fit10000@sim$samples[[3]][[ind0]][-c(1:2000)],
                                            fit10000@sim$samples[[4]][[ind0]][-c(1:2000)])))
  }
}

write.csv(results, "results.csv")

summ <- results %>% 
  group_by(j, t) %>% 
  summarize(beta = mean(beta)) %>% 
  mutate(cump = exp(beta)/(1+exp(beta))) %>% 
  arrange(t,j) %>% 
  group_by(t) %>% 
  mutate(p = c(head(cump,1),diff(cump)))

summ |>
  mutate(color = case_when(
           j <= 5 ~ "Top 5",
           j <= 10 & j >= 6 ~ "P6 - P10",
           j <= 15 & j >= 10 ~ "P11 - P15",
         ),
         color = factor(color, levels = c("Top 5", "P6 - P10", "P11 - P15"))) |>
  ggplot(
    aes(x = t + 1902, y = p, color = color, gorup = as.factor(j))) +
  geom_smooth() +
  labs(
    x = "Season",
    y = "Inning Shares",
    color = "Pitchers"
  ) +
  scale_color_manual(values = c("Top 5" = "red", "P6 - P10" = "blue", "P11 - P15" = "green4")) +
  theme_minimal() + 
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.key.width = unit(0.8, "in"),
    legend.key.height = unit(0.4, "in"),
    strip.text = element_text(size = rel(5)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  ) +
  guides(color = guide_legend(byrow = TRUE))

data <- Pitching |>
  filter(yearID >= 1903,
         !(lgID == "FL"))|>
  left_join(fran, by = "teamID")

total <- data |>
  group_by(franchID, yearID) |>
  summarize(
    Total_IPouts = sum(IPouts)
  )

p5 <- data |>
  group_by(franchID, yearID) |>
  slice_max(n = 5, order_by =  IPouts, with_ties = FALSE) |>
  ungroup() |>
  group_by(franchID, yearID) |>
  summarize(
    P5_IPouts = sum(IPouts)
  )

p15 <- data |>
  group_by(franchID, yearID) |>
  slice_max(n = 15, order_by =  IPouts, with_ties = FALSE) |>
  ungroup() |>
  group_by(franchID, yearID) |>
  summarize(
    P15_IPouts = sum(IPouts)
  )

p_ip <- left_join(total, p15, by = c("yearID", "franchID")) |>
  left_join(p5, by = c("yearID", "franchID")) |>
  mutate(
    `Rotation` = P5_IPouts / Total_IPouts,
    `Top 15` = P15_IPouts / Total_IPouts,
  )

p_ip |>
  pivot_longer(cols = c(`Rotation`, `Top 15`),
               names_to = "type",
               values_to = "prop") |>
  mutate(type = factor(type, levels = c("Top 15", "Rotation"))) |>
  ggplot(
    aes(x = yearID, y = prop, color = type)) + 
  geom_jitter(size = 1, width = 0.2, height = 0) +
  geom_smooth() +
  scale_color_manual(values = c("Top 15" = "aquamarine3", "Rotation" = "red")) +
  theme_minimal() +
  labs(x = "Season",
       y = "Total Inning Shares",
       color = "Pitchers") +
  theme_minimal() + 
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.key.width = unit(0.8, "in"),
    legend.key.height = unit(0.4, "in"),
    strip.text = element_text(size = rel(5)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  ) +
  guides(color = guide_legend(byrow = TRUE))


rot_p <- summ |>
  filter(j <= 5) |>
  select(j, t, p) |>
  rename(
    "p" = j,
    "yearID" = t,
    "prop" = p
  ) |>
  mutate(yearID = yearID + 1902)

t5 <- rot_p |>
  group_by(yearID) |>
  summarize(
    prop = sum(prop)
  )

is <- left_join(p_ip, t5, by = c("yearID")) |>
  mutate(is_plus = Rotation - prop)

is |>
  arrange(-is_plus) |>
  select(franchID, yearID, is_plus) |>
  head(6)

is |>
  arrange(is_plus) |>
  select(franchID, yearID, is_plus) |>
  head(6)

p_props <- rot_p |>
  pivot_wider(names_from = p, values_from = prop, names_prefix = "p")

l <- data |>
  left_join(p15, by = c("franchID", "yearID")) |>
  mutate(is = IPouts / P15_IPouts) |>
  left_join(p_props, by = "yearID") |>
  mutate(P1 = is >= p1,
         P2 = is >= p2,
         P3 = is >= p3,
         P4 = is >= p4,
         P5 = is >= p5
  ) |>
  group_by(playerID) |>
  summarize(
    Szn = n_distinct(yearID),
    P1 = sum(P1),
    P2 = sum(P2),
    P3 = sum(P3),
    P4 = sum(P4),
    P5 = sum(P5),
    prop1 = P1 / Szn,
    prop5 = P5 / Szn
  ) |>
  arrange(-prop1)

head(l, 13)



test <- summ %>% group_by(t) %>% mutate(Hpart = -p*log(p)) %>% summarize(entropy = sum(Hpart))

ggplot(aes(x = t + 1902, y = entropy), data = test) + 
  geom_point() +
  theme_minimal() +
  labs(x = "Season",
       y = "Entropy",
       title ="Top 15 Pitchers") +
  theme_minimal() + 
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.key.width = unit(0.8, "in"),
    legend.key.height = unit(0.4, "in"),
    strip.text = element_text(size = rel(5)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )
  

test <- summ %>% filter(j <= 6) %>%  group_by(t) %>% mutate(p = p/sum(p), Hpart = -p*log(p)) %>% summarize(entropy = sum(Hpart))
ggplot(aes(x = t + 1902, y = entropy), data = test) + 
  geom_point() +
  theme_minimal() +
  labs(x = "Season",
       y = "Entropy",
       title ="Top 5 Pitchers") +
  theme_minimal() + 
  theme(
    text = element_text(size = rel(5)),
    legend.text = element_text(size = rel(3)),
    legend.key.width = unit(0.8, "in"),
    legend.key.height = unit(0.4, "in"),
    strip.text = element_text(size = rel(5)),
    plot.title = element_text(size = rel(9.5), hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25, l = 10))
  )
