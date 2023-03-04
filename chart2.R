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


max_checkouts_each_month_byusage <- small_22_23 %>%
  group_by(UsageClass, CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  select(UsageClass,CheckoutMonth,total_checkouts)

##plot----------------
plot2 <- ggplot(max_checkouts_each_month_byusage, aes(x=CheckoutMonth, y=total_checkouts, group=UsageClass, color=UsageClass)) +
  geom_line() +
  labs(title="Total checkouts by Usage Class each month in 2022",x ="Month", y = "Total checkout") +
  scale_color_manual(values=c("red", "blue"))
plot2