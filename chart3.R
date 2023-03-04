#working on specific books
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


max_checkouts_each_month <- small_22_23 %>%
  group_by(CheckoutMonth) %>%
  slice_max(Checkouts) %>% 
  pull(Checkouts)


#plot--------------------

plot3_df <- data.frame(x1 = checkout_Month, y1 = max_checkouts_each_month)

plot3 <- ggplot(plot3_df, aes(x=x1, y=y1)) +
  geom_line(linewidth = 1,color="red")+
  labs(title = "Most individual checkout checkouts over time", x = "Month", y = "Total checkout")
plot3