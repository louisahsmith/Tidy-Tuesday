library(tidyverse)
library(rvest)
library(lubridate)

wiki_table <- function(url, num){
  url %>%
    read_html() %>%
    html_nodes(xpath = str_replace('//*[@id="mw-content-text"]/div/table[num]', "num", num)) %>%
    html_table() %>%
    .[[1]]
}

fires <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-21/week21_calfire_frap.csv")
damage <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-21/cal-fire-incidents.csv")
structures <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-21/calfire_damage.csv")

glimpse(fires)
glimpse(damage)
glimpse(structures)

cause_levs <- c("Lightning", "Equipment Use", "Smoking", "Campfire", "Debris",
                "Railroad", "Arson", "Playing with Fire", "Miscellaneous", "Vehicle",
                "Power Line", "Firefighter Training", "Non-Firefighter Training", 
                "Unknown/Unidentified", "Structure", "Aircraft", "Escaped Prescribed Burn",
                "Illegal Alien Campfire")

dat <- fires %>%
  filter(state == "CA") %>%
  mutate(cause = factor(cause, levels = cause_levs),
         alarm_date = parse_date(alarm_date),
         cont_date = parse_date(cont_date),
         year = year_,
         Name = str_to_title(parse_character(fire_name)),
         `Start date` = format(alarm_date, "%B %Y")) %>%
  full_join(damage, by = c("year" = "YEAR")) %>%
  full_join(structures)


wiki <- map(c("1", "3", "4"), wiki_table, 
            url = "https://en.wikipedia.org/wiki/List_of_California_wildfires") %>%
  reduce(full_join) %>%
  select(-Notes) %>%
  # remove duplicates (first two with wrong numbers of dead)
  filter(!(Name == "Witch" & Deaths == 6), 
         !(Name == "Thomas" & Deaths == 1),
         !(Name == "Tubbs" & Structures == "5643")) %>%
  mutate(Name = fct_recode(
    "Bear Wallow-Lime Complex" = "Klamath Theater Complex",
    "Marble-Cone" = "Marble Cone",
  ))


left_join(wiki, dat, by = "Start date") %>%
  select(Name.x, Name.y, `Start date`, Acres, ACRES.BURNED) %>% View

filter(dat, `Start date` == "August 1977") %>% View

