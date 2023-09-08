# 클립보드 바로 데이터 테이블로 읽기
tokyo <- read.delim(pipe("pbpaste"), header = TRUE)

library(tidyverse)
library(lubridate)
library(scales)

tokyo <- tokyo |>
  mutate(date_time = ymd_hm(paste0(date, " ", time)))

ggplot(tokyo) +
  geom_point(aes(x = date_time, y = value, color = group), alpha = 0.5) +
  scale_color_manual(values = c("purple", "green")) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme_minimal() +
  facet_grid(group ~ .) +
  theme(legend.position = "none")

### geom_vline 추가하기
ggplot(tokyo) +
  geom_point(aes(x = date_time, y = value, color = group), alpha = 0.5) +
  geom_vline(xintercept = as_datetime("2011-03-12 15:36:00"), color = "grey20", linetype = "dotted") +
  scale_color_manual(values = c("purple", "green")) +
  scale_x_datetime(limits = c(as_datetime("2010-01-01"), as_datetime("2023-12-31"))) +
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme_minimal() +
  theme(legend.position = "none")
