### VIZ CHALLENGE

### 라이브러리 세팅
library(readr)
library(tidyverse)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

ibtracs_WP_list_v04r00 <- read_csv("ibtracs.WP.list.v04r00.csv")
View(ibtracs_WP_list_v04r00)


### PART 1. 태풍 관련 시각화 ---------------------------------------------------

### 충청권 통과 + 남해안 상륙 태풍
tp_list_grand_final <- ibtracs_WP_list_v04r00 |>
  filter(SID %in% c("1945211N23129",
                    "1946222N15152",
                    "1957223N08145",
                    "1961198N18134",
                    "1961204N12147",
                    "1987188N10151",
                    "1989201N11145",
                    "1995193N06156",
                    "2000245N14157",
                    "2002234N14164",
                    "2006180N06140",
                    "2012196N19144",
                    "2012254N09135",
                    "2018227N11145",
                    "2023208N13140")) |>
  select(SID, NAME) |>
  distinct(SID, NAME)

# 태풍 시작점
start_point <- ibtracs_WP_list_v04r00 |> 
  group_by(SID) |>
  slice(1)


### 지도 그래기  
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(world) +
  geom_sf(colour = "grey10", size = 0.2) +
  geom_path(data = ibtracs_WP_list_v04r00 |>
              filter(SID %in% tp_list_grand_final$SID), aes(LON, LAT, group = SID), alpha = 0.5, color = "grey50", linetype = "dotted") +
  geom_path(data = ibtracs_WP_list_v04r00 |>
              filter(SID == "2023208N13140"), aes(LON, LAT, group = SID), color = "#b41a40") +
  coord_sf(xlim = c(110, 170), ylim = c(5, 45), expand = TRUE) +
  geom_point(data = start_point |> filter(SID %in% tp_list_grand_final$SID), aes(LON, LAT)) +
  geom_point(data = start_point |> filter(SID == "2023208N13140"), aes(LON, LAT), color = "#b41a40") +
  ggrepel::geom_text_repel(data = start_point |> 
                             filter(SID %in% tp_list_grand_final$SID), aes(LON, LAT, label = NAME), max.overlaps = Inf) +
  theme_minimal() +
  labs(title = "From EVA(1945) Till KHANUN(2023)",
       subtitle = "1884년부터 2023년까지 태풍 중 남해안에 상륙해 충청권을 통과한 15개의 태풍") +
  theme(plot.title = element_text(size = 18, face ="bold", margin = margin(0,0,10,0)),
        plot.subtitle = element_text(size = 12, vjust = 3))


### PART 2. 한반도 수온 --------------------------------------------------------

# 대해구별 수온 엑셀 파일에서 바로 데이터 긁어오기
sea_temp <- read.delim(pipe("pbpaste"), header = TRUE)

# 지도 시각화
ggplot(world) +
  geom_tile(data = sea_temp, aes(longitude, latitude, fill = temp), color = "white") +
  geom_sf(colour = "grey50", fill = "white", size = 0.2) +
  coord_sf(xlim = c(120, 135), ylim = c(32, 42), expand = TRUE) +
  scale_fill_gradientn(limits = c(0, 32), colors = c("#1448E0", "#FFF7B1", "#FF0017")) +
  theme_minimal() +
  theme(panel.grid = element_blank())