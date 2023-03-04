library("tidyverse")
library("ggplot2")
library("dplyr")


small_22_23 <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv",stringsAsFactors = FALSE) %>% 
  filter(CheckoutYear == 2022) %>% 
  select(UsageClass,CheckoutType,MaterialType, CheckoutYear, CheckoutMonth, Checkouts, Title)


total_checkout_each_month <- small_22_23 %>%
  filter(MaterialType == "BOOK") %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  pull(total_checkouts)

checkout_Month <- small_22_23 %>%
  filter(MaterialType == "BOOK") %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  pull(CheckoutMonth)


plot0_df <- data.frame(x1 = checkout_Month, y1 = total_checkout_each_month)
plot0 <- ggplot(data = plot1_df, aes(x=x1, y = y1)) + geom_col(fill="blue")+labs(title="Total book checkouts each month in 2022",
                                                                                 x ="Month", y = "Total checkout")
plot0
