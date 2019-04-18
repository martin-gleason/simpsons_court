#read and clean
library(tidyverse)
library(lubridate)
library(here)


kaggle_source <- "https://www.kaggle.com/usdpic/execution-database/version/1#database.csv"
death_penalty <- file.path("inputs/database.csv")
data <- read_csv(death_penalty)

Race_Sex_tbl <- table(data$Race, data$Sex)
data %>% filter(`Victim Count` == 168)

prop.table(Race_Sex_tbl)
margin.table(Race_Sex_tbl)

Race_Sex_V <- data %>%
  group_by(Race, Sex, `Victim Count`) %>%
  summarise(Total = n())

Race_Sex_V <- table(Race_Sex_V)

prop.table(Race_Sex_V)

data %>% 
  group_by(Race, Sex) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop = total/sum(total))

data %>% 
  group_by(Race, Sex, `Victim Count`) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  mutate(prop = total/sum(total))

data %>% 
  group_by(Race, Sex) %>%
  summarize(total = n()) %>%
  ggplot(aes(x = Race)) + geom_histogram(stat="count", position = "dodge",
                                            aes(fill = Sex))

simple_table <- table(data$Race, data$Sex)
