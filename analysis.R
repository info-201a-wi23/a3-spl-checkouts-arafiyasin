library("tidyverse")
library("ggplot2")
library("dplyr")

# only using this data for this A3
SPL_17_23_df <- read.csv("2017-2023-10-Checkouts-SPL-Data.csv",stringsAsFactors = FALSE) %>% 
  filter(CheckoutYear > 2021) %>% 
  mutate(date = as.Date(paste(CheckoutYear, CheckoutMonth,"10",sep="/")))


#SPL_13_23_df <- read.csv("2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE) %>% 
 # filter(CheckoutYear > 2021) %>% 
  #mutate(date = as.Date(paste(CheckoutYear, CheckoutMonth,"10",sep="/")))
small_22_23 <- read.csv("2022-2023-All-Checkouts-SPL-Data.csv",stringsAsFactors = FALSE) %>% 
  filter(CheckoutYear == 2022) %>% 
  select(UsageClass,CheckoutType,MaterialType, CheckoutYear, CheckoutMonth, Checkouts, Title)


# creating small version of the dataframes
small_17_23 <- select(SPL_17_23_df,UsageClass,CheckoutType,MaterialType, CheckoutYear, CheckoutMonth, Checkouts, Title)
#small_22_23 <- select(SPL_22_23_df,UsageClass,CheckoutType,MaterialType, CheckoutYear, CheckoutMonth, Checkouts, Title)


uniq_mat_Type <- unique(small_22_23$MaterialType)
View(uniq_mat_Type)
#---------------------------


#Getting the summary fo the titles--------
Ave_checkouts<- small_22_23 %>% 
  group_by(Title) %>% 
  summarise(Ave_checkouts=mean(Checkouts))

Ave_checkouts<- small_22_23 %>% 
  summarize(mean(Checkouts))

#--------------------------
#month max on my book - done
book_title <- str_trim("The Third Sister")
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

#publication_year <-  SPL_13_23_df %>% select(PublicationYear)
#checkoutyear<- SPL_13_23_df %>% select(checkoutyear)




#-------------------------plots-----------------------------
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



plot1_df <- data.frame(x1 = checkout_Month, y1 = total_checkout_each_month)
plot1 <- ggplot(data = plot1_df, aes(x=x1, y = y1)) + geom_col(fill="blue")
plot1

#plot + geom_point(aes(colour = "black"))
#plot + scale_x_date(date_breaks = "2011", date_labels = "%W")


#par(mfrow=c(1,2))
plot2<- ggplot(data = SPL_17_23_df, aes(x=date, y = Checkouts)) +
  geom_col(colour = "red")
#geom_line(colour = "black")
plot2


#plot2 + geom_line(aes(colour = "black"))

plot3 <- ggplot(data = SPL_17_23_df, aes(x=Checkouts, y = Checkouts)) + geom_histogram(colour = "red")
plot3



ggplot(SPL_17_23_df, aes(x=PublicationYear, y = Checkouts)) +
  geom_col(aes(group=1))
scale_x_continuous(breaks=seq(min(PublicationYear), max(PublicationYear), 1000))




library(knitr)

