#************************************************
#Kaggle Titanic Project
#SYS 6018 hw1
#************************************************


library(readr) 
library(dplyr)

p1.train1 = read_csv("train.csv")
p1.predict = read_csv("test.csv")

sub <- sample(1:891,size=446)
p1.train <- p1.train1[sub,]     # Select subset for cross-validation
p1.test <- p1.train1[-sub,]

Age<-p1.train$Age
Gender<-p1.train$Sex
Embarked<-p1.train$Embarked

full <- p1.train %>%
  mutate(
    Age = ifelse(is.na(Age), mean(p1.train$Age, na.rm=TRUE), Age),
    `Age` = case_when(Age < 13 ~ 1, 
                            Age >= 13 & Age < 18 ~ 2,
                            Age >= 18 & Age < 60 ~ 3,
                            Age >= 60 ~ 4))


full1 <- full %>%
  mutate(
    `Sex` = case_when(Gender=="male" ~ 0, 
                           Gender=="female" ~ 1))

full2 <- full1 %>%
  mutate(
    `Embarked` = case_when(Embarked == 'S' ~ 1, 
                           Embarked == 'C' ~ 2,
                           Embarked == 'Q' ~ 3))

p1.lm <- glm(Survived ~ Sex+Embarked+Age+Pclass+Parch+SibSp, data=full2,family="binomial")
summary(p1.lm)

p1.lm1 <- glm(Survived ~ Sex+Age+Pclass+SibSp, data=full2,family="binomial")
summary(p1.lm1)

Age<-p1.test$Age
Gender<-p1.test$Sex
Embarked<-p1.test$Embarked

full3 <- p1.test %>%
  mutate(
    Age = ifelse(is.na(Age), mean(p1.test$Age, na.rm=TRUE), Age),
    `Age` = case_when(Age < 13 ~ 1, 
                      Age >= 13 & Age < 18 ~ 2,
                      Age >= 18 & Age < 60 ~ 3,
                      Age >= 60 ~ 4))


full4 <- full3 %>%
  mutate(
    `Sex` = case_when(Gender=="male" ~ 0, 
                      Gender=="female" ~ 1))

full5 <- full4 %>%
  mutate(
    `Embarked` = case_when(Embarked == 'S' ~ 1, 
                           Embarked == 'C' ~ 2,
                           Embarked == 'Q' ~ 3))

probs<-as.vector(predict(p1.lm1,newdata=full5, type="response"))
preds <- rep(0,445) 
preds[probs>0.5] <- 1 
table(preds,full5$Survived) 

#The prediction result is relevantly satisfying.
#So we proceed with prediction

Age<-p1.predict$Age
Gender<-p1.predict$Sex
Embarked<-p1.predict$Embarked

full6 <- p1.predict %>%
  mutate(
    Age = ifelse(is.na(Age), mean(p1.test$Age, na.rm=TRUE), Age),
    `Age` = case_when(Age < 13 ~ 1, 
                      Age >= 13 & Age < 18 ~ 2,
                      Age >= 18 & Age < 60 ~ 3,
                      Age >= 60 ~ 4))


full7 <- full6 %>%
  mutate(
    `Sex` = case_when(Gender=="male" ~ 0, 
                      Gender=="female" ~ 1))

full8 <- full7 %>%
  mutate(
    `Embarked` = case_when(Embarked == 'S' ~ 1, 
                           Embarked == 'C' ~ 2,
                           Embarked == 'Q' ~ 3))

probs<-as.vector(predict(p1.lm1,newdata=full8, type="response"))
preds <- rep(0,418)  
preds[probs>0.5] <- 1
prediction<-cbind.data.frame(full8$PassengerId,preds)
colnames(prediction) <- c("PassengerId", "Survived")
write.csv(prediction,file="SL_submission.csv")

