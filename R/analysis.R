library(googlesheets4)
library(stringr)
library(dplyr)
library(tidyr)
library(formattable)

id <- "1HifhX7n-q5zqm_HnFZ1OY5p3C04EIiAZEGXrlYBAEDc"
df <- read_sheet(id)

# filter data to remove duplicate rows
df %>%
  pull(2) %>%
  unique() %>%
  length() # 18
# take the last added row
keep_idxs <- df %>%
  select(2) %>%
  setNames("name") %>%
  mutate(idx = row_number()) %>%
  group_by(name) %>%
  mutate(rank = row_number()) %>%
  filter(rank == max(rank))

df <- df %>%
  slice(keep_idxs$idx)

# analyze data
meta <- df[, c(1:4, 130, 131)]
resp <- df[, c(5:129, 132, 133)]

results <- resp %>%
  rename_with(~ str_replace(
    .,
    fixed("(How many votes would you give this activity? -1, +1, or +2?)"), ""
  )) %>%
  mutate(group = 1) %>%
  gather() %>%
  mutate(
    # topic = extract(key, into='hi', regex="^(.*)\\["),
    topic = str_extract(key, "^(.*)\\["),
    question = str_extract(key, "\\[.*\\]"),
    # clean
    topic = str_replace_all(topic,
      pattern = c(
        "\\[" = "",
        "activities" = ""
      )
    ) %>% trimws(),
    question = str_replace_all(question,
                               pattern = c("\\[" = "",
                                           "\\]" = ""))
  ) %>%
  group_by(topic, question) %>%
  summarize(
    count = sum(value),
    n_m1 = sum(ifelse(value == -1, 1, 0)),
    n_p1 = sum(ifelse(value == 1, 1, 0)),
    n_p2 = sum(ifelse(value == 2, 1, 0)),
    ) %>%
  arrange(topic, desc(count)) %>%
  filter(question != "") %>%
  group_by(topic) %>%
  top_n(7) %>%
  ungroup() %>%
  mutate(
    # votes are -1, 1, or 2
    # so min negative = -18
    # max interest = 18 * 2 = 36
    # Interest = Votes/(36+18),
    # min-max
    # Interest = (Votes - min(Votes))/(max(Votes) - min(Votes))
    # Interest = YM with positive votes
    interest = (n_p1 + n_p2)/(n_m1 + n_p1 + n_p2),
    interest = percent(interest, digits=0)
    # TotalVotes = (n_m1 + n_p1 + n_p2)
  ) %>%
  select(
    topic,
    question,
    count,
    interest,
    everything()
  ) %>%
  rename(
    Theme = topic,
    Activity = question,
    Votes = count,
    `% YM Interested \n(% with +1 or +2 vote\nout of 18 YM)` = interest,
    `-1 votes` = n_m1,
    `+1 votes` = n_p1,
    `+2 votes` = n_p2
)
results

write_sheet(
  results,
  ss = id,
  sheet = "Top Ideas"
)

meta %>%
  setNames(
    c('timestamp', 'name', 'ward', 'quorum', 'yw_combined', 'other_activities')
  ) %>%
  group_by(yw_combined) %>%
  count() %>%
  mutate(pct = n/18)
# > 55% (10/18) want monthly or more

meta %>%
  setNames(
    c('timestamp', 'name', 'ward', 'quorum', 'yw_combined', 'other_activities')
  ) %>%
  group_by(yw_combined, ward) %>%
  count() %>%
  mutate(pct = n/18)

meta %>%
  setNames(
    c('timestamp', 'name', 'ward', 'quorum', 'yw_combined', 'other_activities')
  ) %>%
  View()
