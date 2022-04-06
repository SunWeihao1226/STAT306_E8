# R code for the final project report
library(tidyverse)
library(ggplot2)
library(GGally)
library(rsample)
# Read the dataset
data <- read_csv("./data/food_hygiene_rating_data.csv", col_names=TRUE)

# Clean the dataset

data_eda <- data %>% 
  select(LocalAuthorityCode, RatingDate, BusinessType, RatingValue) %>%
  mutate(Year = format(RatingDate, "%Y") %>% as.numeric(),
         Month = format(RatingDate, "%m") %>% as.numeric(), 
         Day = format(RatingDate, "%d") %>% as.numeric(),
         BusinessType = BusinessType <- as.factor(BusinessType),
         RatingValue = RatingValue <- as.factor(RatingValue)) %>%
  select(-RatingDate) %>%
  drop_na()

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

data_split <- initial_split(data_eda, prop = 0.75, strata = RatingValue)
training_data <- training(data_split)
testing_data <- testing(data_split)

mod1 <- glm(RatingValue~LocalAuthorityCode+BusinessType+Year+Month+Day, data=training_data, family="binomial")


