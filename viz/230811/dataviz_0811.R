### VIZ CHALLENGE

# 라이브러리 세팅
library(tidytuesdayR)
library(bertin)
library(sf)
library(ggplot2)
library(maps)
library(mapdata)


# 데이터 불러오기
tt_data <- tt_load("2023-08-01")

states <- tt_data$states
state_name_etymology <- tt_data$state_name_etymology


# 미국 shp 파일 불러오기
usashp <- st_read('cb_2020_us_state_500k.shp')

usashp <- usashp |>
  mutate(ID = str_to_lower(NAME))

usashp <- left_join(usashp, states, by = "ID")
usashp <- usashp |>
  filter(is.na(state) == FALSE)
usashp <- usashp |>
  filter(!STUSPS %in% c("AK", "HI"))

# 전체 인구로 point 만들기
usa_pop <- make_points(polygon = usashp,
                       n = 70,
                       square = FALSE) |>
  mutate(pop_cl = case_when(
    population_2020 < 1000000 ~ 1,
    population_2020 < 2000000 ~ 2, 
    population_2020 < 4000000 ~ 4, 
    population_2020 < 8000000 ~ 8, 
    population_2020 < 10000000 ~ 10, 
    population_2020 < 20000000 ~ 20, 
    population_2020 < 40000000 ~ 40,
    TRUE ~ 41
  ))

# 단위 면적당 인구로 point 만들기
usashp <- usashp |>
  mutate(pop_rate = population_2020/land_area_km2)

usa_poprate <- make_points(polygon = usashp,
                           n = 70,
                           square = FALSE) |>
  mutate(poprate_cl = case_when(
    pop_rate < 10 ~ 1,
    pop_rate < 20 ~ 2, 
    pop_rate < 40 ~ 4, 
    pop_rate < 80 ~ 8, 
    pop_rate < 100 ~ 10, 
    pop_rate < 200 ~ 20, 
    pop_rate < 500 ~ 40,
    TRUE ~ 41
  ))


# 시각화 1(인구수)
ggplot(usa_pop, aes(size = as.factor(pop_cl)))+
  geom_sf(usashp, mapping = aes(geometry = geometry), fill = "#092D61", color = "grey", inherit.aes = FALSE)+
  geom_sf(color = "grey98", shape = "\u2605")+
  scale_size_manual(values = c(0.3, 0.4, 0.5, 0.8, 1, 1.5, 3), labels = c("< 1M", "< 2M", "< 4M", "< 8M", "< 10M", "< 20M", "< 40M"))+
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100")) +
  labs(title = "US Population in 2020",
       subtitle = "Shows population size by state as a star (excluding Alaska and Hawaii)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face ="bold", margin = margin(0,0,10,0)),
        plot.subtitle = element_text(size = 12, vjust = 3),
        legend.position = "none")


# 시각화 2(면적 대비 인구수)
ggplot(usa_poprate, aes(size = as.factor(poprate_cl)))+
  geom_sf(usashp, mapping = aes(geometry = geometry), fill = "#B41A40", color = "grey", inherit.aes = FALSE)+
  geom_sf(color = "grey98", shape = "\u2605")+
  scale_size_manual(values = c(0.3, 0.4, 0.5, 0.8, 1, 1.5, 3), labels = c("< 1M", "< 2M", "< 4M", "< 8M", "< 10M", "< 20M", "< 40M"))+
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100")) +
  theme_minimal() +
  labs(title = "US Population in 1 square kilometer 2020",
       subtitle = "Shows population rate by state as a star (excluding Alaska and Hawaii)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face ="bold", margin = margin(0,0,10,0)),
        plot.subtitle = element_text(size = 12, vjust = 3),
        legend.position = "none")
