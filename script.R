
# load packages

library(tidyverse)
library(DataExplorer)
library(skimr)
library(ggrain)
library(ggeasy)


# read data--------

penguins <- read_csv(here::here("messy_penguins.csv"))


# find missingness------------

plot_missing(penguins)



## look at the shape-----------

skim(penguins)

plot_histogram(penguins)

hist(penguins$flipper_length_mm)




# check for coding issues----------

plot_bar(penguins)


# check for impossible values--------------

penguins %>%
  ggplot(aes(x = species, y = flipper_length_mm)) +
  geom_boxplot() 


penguins %>%
  ggplot(aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2)


penguins %>%
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point() +
  labs(title= "A very heavy chinstrap and an Adelie with flippers that are too long")


## tell the story-----------

## make a raincloud plot --------------
penguins %>%
  ggplot(aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_rain() +
  theme_minimal() +
  guides(fill = "none") # removes the legend


## make it thesis worthy---------

penguins %>%
  ggplot(aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_rain(alpha = 0.5) +
  theme_minimal() +
  guides(fill = "none") + # remove the legend
  theme_classic() +
  labs(x = "Penguin Species", y = "Flipper length (mm)") +
  easy_x_axis_labels_size(size = 12) +
  easy_y_axis_labels_size(size = 12) +
  easy_all_text_size(size = 14) 


# export to png---------
ggsave("penguin_rain.png")


