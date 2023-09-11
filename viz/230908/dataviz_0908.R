### VIZ CHALLENGE


### 라이브러리 세팅 ------------------------------------------------------------
library(readxl)
library(tidyverse)

remotes::install_github("AllanCameron/geomtextpath")
library(geomtextpath)

### 데이터 정리 ----------------------------------------------------------------
births_outside_marriage <- read_excel("230908/SF_2_4_Share_births_outside_marriage.xlsx", 
                                      sheet = "Births_outside_marriage", skip = 3, n_max = 39)

View(births_outside_marriage)

mydata <- births_outside_marriage |>
  select(-Note) |>
  mutate_all(\(x) ifelse(x == "..", NA, x)) |>   # ~ ifelse(. == "..", NA, .) / ~ ifelse(.x == "..", NA, .x)
  as_tibble() |>
  pivot_longer(!Country, names_to = "year", values_to = "rate") |>
  mutate(rate = as.numeric(rate),
         year = as.numeric(year),
         group = ifelse(Country %in% c("Korea", "Denmark", "Sweden", "France", "Chile"), "textline", ""))



### 데이터 시각화 --------------------------------------------------------------
ggplot() +
  geom_line(data = mydata |> filter(group != "textline"), 
            aes(x = year , y = rate, group = Country), alpha = 0.9, color = "grey80") +
  geom_textline(data = mydata |> filter(Country == "Korea"), 
                aes(x = year , y = rate, label = Country, group = Country), linewidth = 1, color = "darkred") +
  geom_textline(data = mydata |> filter(Country %in% c("Denmark", "Sweden", "France","Chile")), 
                aes(x = year , y = rate, label = Country, group = Country), linewidth = 1, color = "darkblue", straight = TRUE) +
  geom_textline(data = mydata |> group_by(year) |> summarise(OECD_rate = mean(rate, na.rm = TRUE)),
                aes(x = year, y = OECD_rate, label = "OECD"), linewidth = 1 , color = "black", linetype = "dashed", straight = TRUE) +
  theme_minimal() +
  scale_x_continuous(limits = c(1960, 2020)) +
  labs(title = "Non-marital birth rates by OECD country",
       subtitle = "1960~2020 OECD 데이터 활용") +
  theme(plot.title = element_text(size = 18, face ="bold", margin = margin(0,0,10,0)),
        plot.subtitle = element_text(size = 12, vjust = 3),
        plot.margin = unit(c(1, .5, 1, .5), "cm"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.2))
