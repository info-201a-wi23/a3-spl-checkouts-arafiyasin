library("tidyverse")
library("ggplot2")
library("dplyr")


small_22_23 <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv",stringsAsFactors = FALSE) %>% 
  filter(CheckoutYear == 2022) %>% 
  select(UsageClass,CheckoutType,MaterialType, CheckoutYear, CheckoutMonth, Checkouts, Title)

# the months for the books
checkout_Month <- small_22_23 %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  pull(CheckoutMonth)

# different trends from here 
uniq_mat_Type <- unique(small_22_23$MaterialType)

Hardcover_Books <- small_22_23 %>%
  filter(MaterialType == "BOOK") %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  pull(total_checkouts)

Audio_books <- small_22_23 %>%
  filter(MaterialType == "AUDIOBOOK") %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  pull(total_checkouts)

eBook <- small_22_23 %>%
  filter(MaterialType == "EBOOK") %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  pull(total_checkouts)

Sound_disc <- small_22_23 %>% 
  filter(MaterialType == "SOUNDDISC") %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  pull(total_checkouts)

#plot--------------------

plot1_df <- data.frame(x1 = checkout_Month, y1 = Hardcover_Books,y2 = Audio_books, y3 = eBook, y4= Sound_disc)

plot1 <- plot1_df %>%
  pivot_longer(-x1, names_to = "type", values_to = "count") %>%
  ggplot(aes(x = x1, y = count, color = type, group = type)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("red", "black", "green", "blue"),
                     labels = c("Hardcover Books", "Audio books", "eBook", "Sound disc")) +
  labs(title = "Different book type checkouts over time", x = "Month", y = "Total checkout")
plot1
