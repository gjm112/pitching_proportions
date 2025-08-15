library(Lahman)
library(tidyverse)

head(Pitching)

fran <- Teams |> 
  filter(yearID >= 1903)|>
  select(teamID, franchID) |>
  distinct()

data <- Pitching |>
  filter(yearID >= 1903,
         !(lgID == "FL"))|>
  left_join(fran, by = "teamID")

data |>
  group_by(yearID) |>
  summarize(
    teams = n_distinct(franchID),
    count = sum(IPouts > 600),
    two_per_team = count / teams
  ) |>
  ggplot(
    aes(x = yearID, y = two_per_team)) + 
  geom_jitter(size = 1, width = 0.2, height = 0) +
  geom_smooth(method = "loess",
              se=F) +
  theme_minimal() +
  labs(x = "Season",
       y = "Average Number of Pitchers per Team") +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15))
  )
  
data |>
  filter(yearID %in% c(1903, 1923, 1943, 1963, 1983, 2003, 2023)) |>
  group_by(yearID) |>
  summarize(
    teams = n_distinct(franchID),
    avg_p = n() / teams,
  ) |> view()

data |>
  group_by(playerID, yearID) |>
  summarize(IPouts = sum(IPouts),
            GS = sum(GS)) |>
  ungroup() |>
  filter(yearID %in% c(1903, 1923, 1943, 1963, 1983, 2003, 2023)) |>
  group_by(yearID) |>
  summarize(
    avg_ip = (sum(IPouts) / n()) / 3
  ) |> view()

data |>
  group_by(franchID, yearID) |>
  summarize(
    count = n()
  ) |>
  ggplot(
    aes(x = yearID, y = count)) + 
  geom_jitter(size = 1, width = 0.2, height = 0) +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Season",
       y = "Total Pitchers Used") +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15))
  )

WS <- Teams |>
  filter(yearID >= 1903,
         WSWin == "Y") |>
  select(yearID, franchID)

ws_data <- left_join(WS, data, by = c("yearID", "franchID"))

data |>
  group_by(franchID, yearID) |>
  summarize(
    count = n()
  ) |>
  ggplot() +
  geom_point(
    data = data |>
      group_by(franchID, yearID) |>
      summarize(count = n()),
    aes(x = yearID, y = count),
    alpha = 0.5,
    color = "grey",
    size=1
  ) +
  geom_smooth(
    data = data |>
      group_by(franchID, yearID) |>
      summarize(count = n()),
    aes(x = yearID, y = count)
  ) +
  geom_point(
    data = ws_data |>
      group_by(franchID, yearID) |>
      summarize(count = n()),
    aes(x = yearID, y = count),
    size = 2,
    color = "red"
  ) +
  theme_minimal() +
  labs(
    x = "Season",
    y = "Total Pitchers Used"
  ) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15)))
    



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
  ggplot(
    aes(x = yearID, y = prop, color = type)) + 
  geom_jitter(size = 1, width = 0.2, height = 0) +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Season",
       y = "Inning Shares",
       color = "Pitchers") +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15))
  )

p_ip |>
  filter(yearID %in% c(1903, 1923, 1943, 1963, 1983, 2003, 2023)) |>
  group_by(yearID) |>
  summarize(
    mean_p5 = mean(P5_IPouts),
    p_p5 = sum(P5_IPouts) / sum(Total_IPouts),
    mean_p15 = mean(P15_IPouts),
    p_p15 = sum(P15_IPouts) / sum(Total_IPouts)
  ) |> view()



ws_p <- left_join(WS, p_ip, by = c("yearID", "franchID")) |>
  pivot_longer(cols = c(`Rotation`, `Top 15`),
               names_to = "type",
               values_to = "prop")

ggplot() +
  geom_smooth(
    data = p_ip |>
      pivot_longer(cols = c(`Rotation`, `Top 15`),
                   names_to = "type",
                   values_to = "prop") |>
      filter(type == "Rotation"),
    aes(x = yearID, y = prop)
  ) +
  geom_point(
    data = ws_p|> filter(type == "Rotation"),
    aes(x = yearID, y = prop),
    size = 2,
    color = "red"
  ) +
  theme_minimal() +
  labs(
    x = "Season",
    y = "Inning Shares",
    color = "Pitchers"
  ) +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15))
  )

library(tidybayes)

results <- data.frame()
for (j in 1:K){
  ind0 <- paste0("beta0[",j,"]")
  for (t in 1:T){
    ind <- paste0("beta[",j,",",t,"]")
    results <- rbind(results,data.frame(j = j, t = t,beta = fit2000@sim$samples[[1]][[ind]] + fit2000@sim$samples[[1]][[ind0]]))
  }
}

summ <- results %>% group_by(j, t) %>% summarize(beta = mean(beta)) %>% mutate(cump = exp(beta)/(1+exp(beta))) %>% arrange(t,j) %>% group_by(t) %>% mutate(p = c(head(cump,1),diff(cump)))


ggplot(
  aes(x = t + 1903, y = p, color = as.factor(j)), data = summ |> filter(p>0)) + 
  geom_smooth(se=F) +
  labs(y = "Inning Shares",
       x = "Season",
       color = "Pitcher Rank") +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15))
  )

ggplot(
  aes(x = t + 1903, y = p, color = as.factor(j)), data = summ |> filter(p>0,j>5)) + 
  geom_smooth(se=F) +
  labs(y = "Inning Shares",
       x = "Season",
       color = "Pitcher Rank") +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15))
  )

ggplot(
  aes(x = t + 1903, y = p, color = as.factor(j)), data = summ |> filter(p>0,j<=5)) + 
  geom_smooth(se=F) +
  labs(y = "Inning Shares",
       x = "Season",
       color = "Pitcher Rank") +
  theme(
    axis.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 15))
  )

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
  head(5)

is |>
  arrange(is_plus) |>
  select(franchID, yearID, is_plus) |>
  head(5)

p_props <- rot_p |>
  pivot_wider(names_from = p, values_from = prop, names_prefix = "p")
  
data |>
  filter(playerID %in% c("ryanno01", "seaveto01", "maddugr01", "johnsra05", "martipe02")) |>
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
    P1 = sum(P1),
    P2 = sum(P2),
    P3 = sum(P3),
    P4 = sum(P4),
    P5 = sum(P5)
  )

data |>
  filter(playerID %in% c("greinza01","verlaju01","scherma01","kershcl01")) |>
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
    P1 = sum(P1),
    P2 = sum(P2),
    P3 = sum(P3),
    P4 = sum(P4),
    P5 = sum(P5)
  )
