#************************************************
#Kaggle Titanic Project
#SYS 6018 hw1
#************************************************


library(readr) 
library(dplyr)

p1.train = read_csv("train.csv")
p1.test = read_csv("test.csv")


Age<-p1.train$Age
Gender<-p1.train$Sex
Embarked<-p1.train$Embarked

full <- p1.train %>%
  mutate(
    Age = ifelse(is.na(Age), mean(p1.train$Age, na.rm=TRUE), Age),
    `AgeGroup` = case_when(Age < 13 ~ 1, 
                            Age >= 13 & Age < 18 ~ 2,
                            Age >= 18 & Age < 60 ~ 3,
                            Age >= 60 ~ 4))


full1 <- full %>%
  mutate(
    `GenderGroup` = case_when(Gender=="male" ~ 0, 
                           Gender=="female" ~ 1))

full2 <- full1 %>%
  mutate(
    `EmbarkedGroup` = case_when(Embarked == 'S' ~ 1, 
                           Embarked == 'C' ~ 2,
                           Embarked == 'Q' ~ 3))

p1.lm <- glm(Survived ~ GenderGroup+EmbarkedGroup+Age+Pclass+Parch+SibSp, data=full2,family="binomial")


