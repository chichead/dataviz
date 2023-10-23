library(tidytuesdayR)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load("2023-09-12")

all_countries <- tuesdata$all_countries
country_regions <- tuesdata$country_regions
global_human_day <- tuesdata$global_human_day
global_economic_activity <- tuesdata$global_economic_activity

View(all_countries)
View(country_regions)
View(global_human_day)
View(global_economic_activity)


### 보로노이 다이어그램 --------------------------------------------------------

install.packages("voronoiTreemap")
library(voronoiTreemap)

data(ExampleGDP)
head(ExampleGDP)

# 우리나라로 만들어보기

my_data <- all_countries |>
  filter(country_iso3 == "KOR")

# 카테고리별 컬러 생성
hcl_palette <- hcl.colors(length(unique(my_data$Category)), palette = "Zissou 1")
my_data <- my_data |>
  mutate(color = hcl_palette[as.numeric(factor(Category, level = unique(Category)))])

my_voro <- data.frame(
  h1 = my_data$country_iso3,
  h2 = my_data$Category,
  h3 = my_data$Subcategory,
  color = my_data$color,
  weight = my_data$hoursPerDayCombined,
  codes = my_data$Subcategory
)

my_json <- vt_export_json(vt_input_from_df(my_voro))
vt_d3(my_json,
      title = "KOR",
      size_border = "3px",
      size_circle = "2px")


# 함수화
making_voronoi <- function(country_name) {
  voronoi_data <- all_countries |>
    filter(country_iso3 == country_name)
  
  hcl_palette <- hcl.colors(length(unique(voronoi_data$Category)), palette = "Zissou 1")
  voronoi_data <- voronoi_data |>
    mutate(color = hcl_palette[as.numeric(factor(Category, level = unique(Category)))])
  
  voronoi_df <- data.frame(
    h1 = voronoi_data$country_iso3,
    h2 = voronoi_data$Category,
    h3 = voronoi_data$Subcategory,
    color = voronoi_data$color,
    weight = voronoi_data$hoursPerDayCombined,
    codes = voronoi_data$Subcategory
  )
  
  voronoi_json <- vt_export_json(vt_input_from_df(voronoi_df))
  vt_d3(voronoi_json,
        title = paste0("How do we spend our time? (", country_name, ")"),
        size_border = "3px",
        size_circle = "2px")
}

making_voronoi("KOR")
