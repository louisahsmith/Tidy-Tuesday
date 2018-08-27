library(tidyverse)
library(rvest)
library(lubridate)
library(gganimate)
library(ggrepel)

wiki_table <- function(url, num) {
  url %>%
    read_html() %>%
    html_nodes(
      xpath = str_replace(
      '//*[@id="mw-content-text"]/div/table[num]', "num", num
    )) %>%
    html_table() %>%
    .[[1]]
}

fires <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-21/week21_calfire_frap.csv")
damage <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-21/cal-fire-incidents.csv")
structures <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-21/calfire_damage.csv")

cause_levs <- c(
  "Lightning", "Equipment Use", "Smoking", "Campfire", "Debris",
  "Railroad", "Arson", "Playing with Fire", "Miscellaneous", "Vehicle",
  "Power Line", "Firefighter Training", "Non-Firefighter Training",
  "Unknown/Unidentified", "Structure", "Aircraft", "Escaped Prescribed Burn",
  "Illegal Alien Campfire"
)

dat <- fires %>%
  filter(state == "CA") %>%
  mutate(
    cause = factor(cause, labels = cause_levs),
    alarm_date = parse_date(alarm_date),
    cont_date = parse_date(cont_date),
    year = year_,
    Name = str_to_title(parse_character(fire_name)),
    `Start date` = format(alarm_date, "%B %Y")
  ) %>%
  full_join(damage, by = c("year" = "YEAR")) %>%
  full_join(structures)


wiki <- map(c("1", "3", "4"), wiki_table,
  url = "https://en.wikipedia.org/wiki/List_of_California_wildfires"
) %>%
  reduce(full_join) %>%
  select(-Notes) %>%
  # remove duplicates (first two with wrong numbers of dead)
  filter(
    !(Name == "Witch" & Deaths == 6),
    !(Name == "Thomas" & Deaths == 1),
    !(Name == "Tubbs" & Structures == "5643"),
    parse_date_time(`Start date`, orders = "mY") >
      min(dat$alarm_date, na.rm = T),
    parse_date_time(`Start date`, orders = "mY") <
      max(dat$alarm_date, na.rm = T)
  ) %>%

  # left_join(wiki, dat) %>%
  #   select(Name, `Start date`, Acres, ACRES.BURNED, objectid) %>%
  #   View()
  #
  # filter(dat, str_detect(str_to_lower(Name), "laguna"))
  # filter(dat, str_detect(str_to_lower(comments), "complex"), `Start date` == "August 1999")
  # filter(dat, `Start date` == "August 1999", fire_cause == "Natural") %>% View

  mutate(
    Name = fct_recode(Name,
      "Bear Wallow-Lime Complex" = "Klamath Theater Complex",
      "Marble-Cone" = "Marble Cone",
      "Mcnally" = "McNally",
      "Happy Camp" = "Happy Camp Complex",
      "Harris 2" = "Harris",
      "Redwood Valley" = "Redwood Valley Complex",
      "Bel Air Fire" = "Bel Air",
      "Laguna Fire" = "Laguna",
      "Larson" = "Stanislaus Complex"
    ),
    `Start date` = case_when(
      Name == "Marble-Cone" ~ "August 1977",
      TRUE ~ `Start date`
    ),
    Name = as.character(Name)
  ) %>%
  full_join(dat)

# glimpse(wiki)
# oh no, here months get associated with months, not names!!! FIX
month_dat <- wiki %>%
  group_by(`Start date`) %>%
  summarise(
    acres = sum(gis_acres, na.rm = T),
    deaths = sum(Deaths, na.rm = T),
    alarm = min(alarm_date, na.rm = T)
  ) %>%
  arrange(alarm) %>%
  mutate(
    cum_acres = cumsum(acres),
    cum_deaths = cumsum(deaths),
    date = parse_date_time(`Start date`, orders = "mY")
  ) %>%
  rowid_to_column() %>%
  gather(type, cum_num, cum_acres, cum_deaths)

plot_dat <- full_join(month_dat, wiki) %>%
  mutate(
    Acres = parse_number(Acres),
    post_acres = ifelse(is.na(Acres) | Acres < 100000 |
      type == "cum_deaths", 0, 1),
    pre_acres = lead(post_acres, 1),
    post_deaths = ifelse(is.na(Acres) | Deaths < 1 |
      type == "cum_acres", 0, 1),
    pre_deaths = lead(post_deaths, 1),
    col_new = ifelse(pre_acres == 1 | post_acres == 1 |
      pre_deaths == 1 | post_deaths == 1, cum_num, NA),
    lab = case_when(
      post_acres == 1 & type == "cum_acres" ~ Name,
      post_deaths == 1 & type == "cum_deaths" ~ Name,
      TRUE ~ NA_character_
    ),
    type = factor(type, labels = c("Acres burned", "Deaths"))
  )

p <- ggplot(data = plot_dat) +
  geom_line(aes(x = date, y = cum_num), col = "#f03b20") +
  # geom_text(aes(label = lab, x = date, y = cum_num)) +
  geom_line(aes(x = date, y = col_new), col = "#ffeda0") +
  geom_text_repel(
    aes(label = lab, x = date, y = cum_num),
    size = 3, box.padding = .1, col = "#feb24c"
  ) +
  theme_dark() + theme(panel.grid = element_blank()) +
  labs(
    title = "Major fires in California history",
    y = "Cumulative number since 1950", x = "Year"
  ) +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(type ~ ., scales = "free")
transition_time(rowid) +
  shadow_mark()
p
animate(p, fps = 2, renderer = gifski_renderer(loop = FALSE), ref_frame = -1)