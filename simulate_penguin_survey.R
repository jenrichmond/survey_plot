# ------------------------------------------------------------------
# Script written by Claude 2026-10-05
# Penguin Life Inventory: simulate 20 Likert items for each penguin
# ------------------------------------------------------------------
# Five subscales, four items each. The fourth item in every subscale
# is reverse-coded.
#
#   swim_     Swimming Confidence   agreement   Gentoo highest
#   island_   Island Satisfaction   agreement   Torgersen Adelies lowest
#   climate_  Climate Worry         agreement   Adelie > Chinstrap > Gentoo
#   conflict_ Colony Conflict       frequency   Chinstrap > Adelie > Gentoo
#   social_   Social Support        frequency   no differences
#
# Output:
#   penguins_survey.csv   original penguin data + 20 item columns
#   survey_codebook.csv   item names, wording, subscale, scale, reverse flag
# ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(purrr)

set.seed(2026)

penguins <- read_csv("messy_penguins.csv", show_col_types = FALSE)

# ---- Response options ---------------------------------------------

agree_levels <- c("Strongly disagree", "Disagree",
                  "Neither agree nor disagree",
                  "Agree", "Strongly agree")

freq_levels <- c("Never", "Rarely", "Sometimes", "Often", "Always")

# ---- Codebook -----------------------------------------------------
# offset nudges individual items up or down so the items within a
# subscale don't look identical

codebook <- tribble(
  ~item,        ~subscale,              ~scale,      ~reverse, ~offset, ~text,
  "swim_1",     "Swimming Confidence",  "agreement", FALSE,     0.2,    "I feel fast and agile in the water.",
  "swim_2",     "Swimming Confidence",  "agreement", FALSE,     0.0,    "I am comfortable diving to great depths.",
  "swim_3",     "Swimming Confidence",  "agreement", FALSE,    -0.4,    "I can outswim a leopard seal if I need to.",
  "swim_4",     "Swimming Confidence",  "agreement", TRUE,      0.0,    "I find long foraging trips exhausting.",

  "island_1",   "Island Satisfaction",  "agreement", FALSE,     0.1,    "I am happy with the fishing near my island.",
  "island_2",   "Island Satisfaction",  "agreement", FALSE,     0.2,    "I would recommend my island to other penguins.",
  "island_3",   "Island Satisfaction",  "agreement", FALSE,    -0.1,    "My island has good nesting sites.",
  "island_4",   "Island Satisfaction",  "agreement", TRUE,      0.0,    "I have thought about moving to a different island.",

  "climate_1",  "Climate Worry",        "agreement", FALSE,     0.2,    "I have noticed changes in sea ice since I was a chick.",
  "climate_2",  "Climate Worry",        "agreement", FALSE,     0.0,    "I worry about finding enough krill in the future.",
  "climate_3",  "Climate Worry",        "agreement", FALSE,     0.1,    "I am concerned about the future of my species.",
  "climate_4",  "Climate Worry",        "agreement", TRUE,      0.0,    "Things will turn out fine for penguins like me.",

  "conflict_1", "Colony Conflict",      "frequency", FALSE,     0.2,    "How often do you squabble with your neighbours?",
  "conflict_2", "Colony Conflict",      "frequency", FALSE,    -0.3,    "How often do you steal pebbles from other nests?",
  "conflict_3", "Colony Conflict",      "frequency", FALSE,     0.0,    "How often do other penguins steal your pebbles?",
  "conflict_4", "Colony Conflict",      "frequency", TRUE,      0.0,    "How often do you feel at peace in the colony?",

  "social_1",   "Social Support",       "frequency", FALSE,     0.2,    "How often does your partner take their turn on the egg?",
  "social_2",   "Social Support",       "frequency", FALSE,     0.3,    "How often do you huddle with others to keep warm?",
  "social_3",   "Social Support",       "frequency", FALSE,     0.0,    "How often do you feel part of the colony?",
  "social_4",   "Social Support",       "frequency", TRUE,      0.0,    "How often do you feel left out at the edge of the group?"
)

# ---- Group means for each subscale --------------------------------
# These are on a latent scale where each penguin's own tendency has an
# SD of about 1, so 1 unit is a big difference and 0.3 is a small one.
# The island column has a typo ("Torgorsen"), so match on "Torg".

subscale_means <- function(species, island) {
  torgersen <- startsWith(island, "Torg")
  tibble(
    `Swimming Confidence` = case_when(species == "Gentoo"    ~  1.0,
                                      TRUE                   ~ -0.1),
    `Island Satisfaction` = case_when(species == "Adelie" & torgersen ~ -0.6,
                                      species == "Adelie"    ~  0.5,
                                      TRUE                   ~  0.3),
    `Climate Worry`       = case_when(species == "Adelie"    ~  0.9,
                                      species == "Chinstrap" ~  0.6,
                                      species == "Gentoo"    ~ -0.4),
    `Colony Conflict`     = case_when(species == "Chinstrap" ~  0.8,
                                      species == "Adelie"    ~  0.2,
                                      species == "Gentoo"    ~ -0.6),
    `Social Support`      = 0.4
  )
}

# ---- Simulate -----------------------------------------------------

n <- nrow(penguins)
means <- subscale_means(penguins$species, penguins$island)

# Each penguin gets its own tendency on each subscale (group mean +
# individual variation), so items within a subscale hang together
trait <- map(means, \(m) m + rnorm(n, mean = 0, sd = 0.8))

# Turn a continuous score into one of five ordered categories
to_category <- function(x, levels) {
  cut(x, breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labels = levels)
}

simulate_item <- function(subscale, scale, reverse, offset) {
  t <- trait[[subscale]]
  if (reverse) t <- -t
  x <- t + offset + rnorm(n, mean = 0, sd = 0.7)
  levels <- if (scale == "agreement") agree_levels else freq_levels
  as.character(to_category(x, levels))
}

responses <- codebook |>
  select(subscale, scale, reverse, offset) |>
  pmap(simulate_item) |>
  set_names(codebook$item) |>
  as_tibble()

penguins_survey <- bind_cols(penguins, responses)

# ---- Save ---------------------------------------------------------

write_csv(penguins_survey, "penguins_survey.csv")
write_csv(select(codebook, -offset), "survey_codebook.csv")
