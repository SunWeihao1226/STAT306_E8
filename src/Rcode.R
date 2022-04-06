# R code for the final project report
library(tidyverse)
library(ggplot2)
library(GGally)

# Read the dataset
data <- read_csv("./data/food_hygiene_rating_data.csv", col_names=TRUE)

# Clean the dataset
data_eda <- data %>% 
  select(LocalAuthorityCode, RatingDate, BusinessType, RatingValue) %>%
  drop_na()
any(is.na(data_eda))


summary(data_eda)

png("results/LAC_hist.png",width=800, height=600)
LAC_hist <- hist(data_eda$LocalAuthorityCode, xlab="Local Authority Code", main="Histogram of Local Authority Code")
dev.off()

options(repr.plot.width = 15, repr.plot.height = 10)
ggplot(data=data_eda, aes(x=factor(BusinessType))) +
  geom_bar(stat="count") +
  xlab("Count") +
  ylab("Business Type")+
  ggtitle("Bar Plot of Business Type")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()
ggsave("./results/BT_bar.png")

ggplot(data=data_eda, aes(x=factor(RatingValue))) +
  geom_bar(stat="count") +
  xlab("Count") +
  ylab("Rating Value")+
  ggtitle("Bar Plot of Rating Value")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("./results/RV_bar.png")




