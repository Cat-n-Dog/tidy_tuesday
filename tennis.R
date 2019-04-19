library(tidyverse)
library(lubridate)
library(ggthemes)
library(cowplot)
library(gghighlight)

player_dob2 <- player_dob %>%
  filter(!is.na(grand_slam)) %>%
  mutate(calc_age = (date_of_first_title - date_of_birth))

player_dob2 %>% ggplot(aes(x = age)) +
  geom_dotplot(binwidth = 90) +
  theme_minimal()


player_dob2 %>% ggplot(aes(x = date_of_first_title, y = age)) +
  geom_point() +
  theme_minimal()


grand_slams %>%
  # filter(rolling_win_count == 1) %>%
  group_by(gender, year) %>%
  summarise(new_champs = sum(rolling_win_count <= 4)) %>%
  ggplot(aes(x = year, y = new_champs)) +
  geom_line() +
  facet_wrap(~gender) +
  theme_bw()


grand_slams %>%
  filter(gender == "Female") %>%
  ggplot() +
  geom_line(aes(x = year, y = rolling_win_count, group = name)) +
  geom_point(aes(x = year, y = rolling_win_count, group = name), size = 0.5) +
  theme_minimal() +
  ggtitle("Number of Grand Slam Titles by Female Player") +
  xlab("") +
  ylim(0, 25) + ylab("") +
  gghighlight::gghighlight(max(rolling_win_count) >= 10, use_group_by = TRUE) +
  ggsave("female_slams.png")


grand_slams %>%
  filter(gender == "Male") %>%
  ggplot() +
  geom_line(aes(x = year, y = rolling_win_count, group = name)) +
  geom_point(aes(x = year, y = rolling_win_count, group = name), size = 0.5) +
  theme_minimal() +
  ggtitle("Number of Grand Slam Titles by Male Player") +
  xlab("") +
  ylim(0, 25) + ylab("") +
  gghighlight::gghighlight(max(rolling_win_count) >= 10, use_group_by = TRUE) +
  ggsave("male_slams.png")
