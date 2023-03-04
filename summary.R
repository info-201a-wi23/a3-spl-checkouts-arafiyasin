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

?ggplot

#
#
plot0_df <- data.frame(x1 = checkout_Month, y1 = total_checkout_each_month)
plot0 <- ggplot(data = plot0_df, aes(x=x1, y = y1)) + geom_col(fill="blue")+labs(title="Total book checkouts each month in 2022",
                                                                                 x ="Month", y = "Total checkout")
plot0






#Getting the summary for the titles--------
Ave_checkouts<- small_22_23 %>% 
  summarize(mean(Checkouts))

#--------------------------
#month max on my book - done
book_checkouts <- small_22_23 %>%
  filter(Title== "The Third Sister")%>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts ==  max(total_checkouts)) %>% 
  pull(CheckoutMonth)

unique(small_22_23$CheckoutType)

#month most checkout on ebooks
book_title <- str_trim("The Third Sister")
most_ebook_checkouts <- small_22_23 %>%
  filter(MaterialType == "EBOOK") %>%
  group_by(CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts)) %>% 
  filter(total_checkouts ==  max(total_checkouts)) %>% 
  pull(CheckoutMonth)
