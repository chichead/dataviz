### VIZ CHALLENGE


### 라이브러리 세팅 ------------------------------------------------------------

devtools::install_github("coolbutuseless/ggblur")
library(ggblur)
library(readxl)
library(tidyverse)



### 데이터 정리 ----------------------------------------------------------------

shooting_DB <- read_excel("230825/Violence Project Mass Shooter Database - Version 7 5.28.23.xlsx", sheet = 5)
shooting_DB <- shooting_DB[-1, ]

shooting_DB$Day <- parse_number(shooting_DB$Day)
shooting_DB$`Number Killed` <- parse_number(shooting_DB$`Number Killed`)

shooting_DB <- shooting_DB |>
  mutate(time = lubridate::ymd(paste0("2020/", Month, "/", Day)))

shooting_DB$time <- as_datetime(shooting_DB$time)



### 시각화 ---------------------------------------------------------------------

ggplot(shooting_DB) +
  geom_point_blur(aes(x = time, y = Year, size = `Number Injured`, blur_size =`Number Injured`), color = "grey", alpha = 0.5) +
  geom_point_blur(aes(x = time, y = Year, size = `Number Killed`, blur_size = `Number Killed`), color = "#C70050", alpha = 0.5) +
  scale_y_reverse(breaks = c(2020, 2010, 2000, 1990, 1980, 1970, 1960),
                  minor_breaks = (2023:1966),
                  limits = c(2023.5, 1965)) +
  scale_size(range = c(.1, 30)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  labs(title = "More frequent shootings",
       subtitle = "1966~2023 Mass Shooting in US") +
  theme(plot.title = element_text(size = 18, face ="bold", margin = margin(0,0,10,0)),
        plot.subtitle = element_text(size = 12, vjust = 3),
        plot.margin = unit(c(1, .5, 1, .5), "cm"),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.2))
