### VIZ CHALLENGE


### 라이브러리 세팅 ------------------------------------------------------------
library(tidyverse)


### 데이터 정리 ----------------------------------------------------------------
terror <- read.delim(pipe("pbpaste"), header = TRUE)

terror$event_date <- lubridate::mdy(terror$event_date)
terror <- terror |>
  mutate(time = lubridate::ymd(paste0("2020/", date_month, "/", date_day)))

terror$time <- as_datetime(terror$time)


### 데이터 시각화 --------------------------------------------------------------
ggplot(terror) +
  geom_point(aes(x = time, y = date_year, size = wounded_high), color = "grey", alpha = 0.5) +
  geom_point(aes(x = time, y = date_year, size = killed_high), color = "#C70050", alpha = 0.5) +
  scale_y_reverse(breaks = c(2020, 2015, 2010, 2005, 2000, 1995, 1990),
                  minor_breaks = (2020:1990),
                  limits = c(2020, 1990)) +
  scale_size(range = c(.1, 18)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Suicide Bombings in Israel (1990-2020)",
       subtitle = "More than 100 terrorist attacks occurred between 2000 and 2004") +
  theme(plot.title = element_text(size = 18, face ="bold", margin = margin(0,0,10,0)),
        plot.subtitle = element_text(size = 12, vjust = 3),
        plot.margin = unit(c(1, .5, 1, .5), "cm"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.2))
