
# load packages

library(tidyverse)
library(DataExplorer)
library(skimr)
library(ggrain)
library(ggeasy)


# read data--------

your_data <- read_csv(here::here("______.csv"))


# find missingness------------

plot_missing(your_data)



## look at the shape-----------

skim(your_data)

plot_histogram(your_data)

hist(your_data$______)


# check for coding issues----------

plot_bar(your_data)


# check for impossible values--------------

your_data %>%
  ggplot(aes(x = _____, y = _____)) +
  geom_boxplot() 


your_data %>%
  ggplot(aes(x = _____, y = _____)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2)


your_data %>%
  ggplot(aes(x = _____, y = ______, colour = _____)) +
  geom_point() +
  labs(title= "____________")


## tell the story-----------

## make a raincloud plot --------------
your_data %>%
  ggplot(aes(x = ______, y = _____, fill = _____)) +
  geom_rain() +
  theme_minimal() +
  guides(fill = "none") # removes the legend


## make it thesis worthy---------

your_data %>%
  ggplot(aes(x = _____, y = _____, fill = _____)) +
  geom_rain(alpha = 0.5) +
  theme_minimal() +
  guides(fill = "none") + # remove the legend
  theme_classic() +
  labs(x = "______", y = "______") +
  easy_x_axis_labels_size(size = 12) +
  easy_y_axis_labels_size(size = 12) +
  easy_all_text_size(size = 14) 


# export to png---------

ggsave("______.png")


