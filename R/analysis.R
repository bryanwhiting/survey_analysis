library(googlesheets4)
library(stringr)
library(dplyr)
library(tidyr)

id <- "1HifhX7n-q5zqm_HnFZ1OY5p3C04EIiAZEGXrlYBAEDc"
df <- read_sheet(id)

meta <- df[, c(1:4, 130, 131)]
resp <- df[, c(5:129, 132, 133)]

resp %>%
  rename_with(~str_replace(.,
    fixed("(How many votes would you give this activity? -1, +1, or +2?)"), ""
  )) %>%
  mutate(group = 1) %>%
  gather() %>%
  mutate(
    topic = str_extract(key, "^.*\\["),
    question = str_extract(key, "\\[.*\\]")
    ) %>%
  group_by(topic, question) %>%
  summarize(sum(value))
