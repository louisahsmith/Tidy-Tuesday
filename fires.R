library(tidyverse)
library(rvest)
library(lubridate)
library(gganimate)
library(ggrepel)
library(grid)

wiki_table <- function(url, num) {
  url %>%
    read_html() %>%
    html_nodes(
      xpath = str_replace('//*[@id="mw-content-text"]/div/table[num]', "num", num)
    ) %>%
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


wiki <- map(c("1", "3", "4"), 
            .f = wiki_table,
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
    pre_acres = lag(cum_acres, 1),
    pre_deaths = lag(cum_deaths, 1),
    date = parse_date_time(`Start date`, orders = "mY")
  ) %>%
  rowid_to_column() %>%
  gather(key = type, value = cum_num, cum_acres:pre_deaths) %>%
  mutate(pre = type %in% c("pre_acres", "pre_deaths"))

pre_dat <- filter(month_dat, pre) %>% 
  rename(pre_val = cum_num) %>% 
  select(-pre) %>%
  mutate(type = fct_recode(type, `Acres burned` = "pre_acres", Deaths = "pre_deaths"))

post_dat <- filter(month_dat, !pre) %>% 
  rename(post_val = cum_num) %>% 
  select(-pre) %>%
  mutate(type = fct_recode(type, `Acres burned` = "cum_acres", Deaths = "cum_deaths"))

full_dat <- full_join(pre_dat, post_dat) %>%
  full_join(wiki) %>%
  mutate(lab = case_when(
    Acres > 100000 & type == "Acres burned" ~ Name,
    Deaths > 0 & type == "Deaths" ~ Name,
    TRUE ~ NA_character_
  ),
  pre_val = ifelse(is.na(lab), NA, pre_val))


p_repel <- ggplot(data = full_dat) +
  geom_line(aes(x = date, y = post_val), col = "#f03b20", size = 1) +
  geom_linerange(aes(ymin = pre_val, ymax = post_val, 
                     x = date), col = "#ffeda0", size = 1) +
  geom_text_repel(aes(label = lab, x = date, y = post_val),
    size = 4, col = "#feb24c", box.padding = 0) +
  theme_dark() + theme(panel.grid = element_blank()) +
  labs(
    title = "Major fires in California history (since 1950)",
    y = "Cumulative number since 1950", 
    x = "Year"
  ) +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(type ~ ., scales = "free") 

p_repel
grid.force()

p_build <- ggplot_build(p_repel)

xrg_acres <- p_build$layout$panel_params[[1]]$x.range
yrg_acres <- p_build$layout$panel_params[[1]]$y.range
xrg_deaths <- p_build$layout$panel_params[[2]]$x.range
yrg_deaths <- p_build$layout$panel_params[[2]]$y.range


# function to get the x and y values of a label's position
# given which panel it is on
get_pos_df <- function(n, panel) {
  grb <- grid.get(n)
  tibble(
    x = xrg_acres[1] + diff(xrg_acres) * 
      convertX(grb$x, "native", valueOnly = TRUE),
    y = switch(panel,
               acres = yrg_acres[1] + diff(yrg_acres) * 
                 convertY(grb$y, "native", valueOnly = TRUE),
               deaths = yrg_deaths[1] + diff(yrg_deaths) * 
                 convertY(grb$y, "native", valueOnly = TRUE)
    ),
    lab = grb$label,
    type = panel
  )
}

# get the positions for each of the labels from each panel
pos_data <- childNames(grid.get(".*", grep = TRUE)) %>%
  str_subset("panel") %>%
  map(~ childNames(grid.get(.))) %>%
  # included "segments" as I was hoping they were the ggrepel lines
  # they are not but I had to use some new functions to do that so 
  # I'm keeping the code!
  list(string = ., pattern = c("textrepel", "segment")) %>%
  cross() %>% 
  map(lift(str_subset)) %>%
  .[c(1,2)] %>% # segment is not what I wanted to just chosing the repels
  map(~ childNames(grid.get(.))) %>%
  map2_dfr(c("acres", "deaths"), ~ map_dfr(.x, get_pos_df, panel = .y)) %>%
  mutate(x = as_datetime(x),
         type = factor(type, labels = c("Acres burned", "Deaths")))


blank_dat <- tibble(a = as_datetime(c(xrg_acres, xrg_deaths),
                                   origin = "1970-01-01 00:00.00 UTC"),
                    b = c(yrg_acres, yrg_deaths),
                    type = rep(c("Acres burned", "Deaths"), each = 2),
                    rowid = 1)

plot_dat <- full_join(full_dat, pos_data) %>% 
  full_join(blank_dat)

p_fixed <- ggplot(data = plot_dat) +
  geom_line(aes(x = date, y = post_val), col = "#f03b20", size = 1) +
  geom_linerange(aes(ymin = pre_val, ymax = post_val, 
                     x = date), col = "#ffeda0", size = 1) +
  geom_text(aes(label = lab, x = x, y = y),
            size = 4, col = "#feb24c") +
  # geom_text(aes(label = lab, x = date, y = post_val),
  #                 size = 4, col = "#feb24c") +
  theme_dark() + theme(panel.grid = element_blank()) +
  labs(
    title = "Major fires in California history (since 1950)",
    y = "Cumulative number since 1950", 
    x = "Year"
  ) +
  # include blank data here so that scales will be the same
  # as the geom_repel plot
  geom_blank(aes(x = a, y = b)) + 
  # but then need to make sure there's no extra space around 
  # the blank data with expand()
  #scale_y_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  scale_x_datetime(expand = c(0, 0)) +
  facet_grid(type ~ ., scales = "free")

p_anim <- p_fixed +
  transition_time(rowid) +
  shadow_mark()
  
gif <- animate(p_anim,
               fps = 4, 
        renderer = gifski_renderer(loop = FALSE), 
        ref_frame = -1)

anim_save("fires.gif", animation = gif)
