library(stringr)
library(magrittr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(arm)
library(DAAG)
library(car)
library(caret)
library(tableone)
library(kableExtra)
library(MatchIt)


# load the date
setwd("/Users/amelia/Documents/mssp/MA678/MA678-Midterm-proposal")
df <- read.table("kc_house_data.txt", sep = ",", header = T)

df$date %<>% str_sub(1, 8)
df$year <- str_sub(df$date, 1, 4)
df$month <- str_sub(df$date, 5, 6)
df$day <- str_sub(df$date, 7, 8)
df$yr_latest <- ifelse(df$yr_built > df$yr_renovated, df$yr_built, df$yr_renovated)
df$year %<>% as.numeric()
df$yr_latest %<>% as.numeric()
df$age <- df$year - df$yr_latest
df$logprice <- log(df$price)
df %<>% na.omit()

df2 <- df %>% dplyr::select(logprice, bedrooms, 
              bathrooms, sqft_living, floors, waterfront, view, 
              condition, grade, zipcode, lat, long)

# EDA
## summary
# summary(df)

## I wonder the most common house
comhouse <- df %>% group_by(bedrooms, floors) %>% summarise(count = sum(price > 0))
p1 <- ggplot(comhouse, aes(x = floors, y = bedrooms, size = count)) + 
  geom_point() + 
  ylim(0, 13) + 
  ggtitle("The most common house") + 
  theme(plot.title = element_text(hjust = 0.5))
p1
# As we can see from the picture, the most common house is house with one floor and three bedrooms.

## houses are best sold in which month?
monhouse <- df %>% group_by(month) %>% summarise(count = sum(price > 0))
p2 <- ggplot(monhouse, aes(x = month, y = count, fill = factor(month))) + 
  geom_bar(stat = "identity") + 
  guides(fill = F)
p2
# houses are best sold in May. And the turnover is higher in summer than in winter.

## I wonder the location of these houses
df%>%leaflet()%>%addTiles()%>%
  addCircleMarkers(lng=~long,lat=~lat, radius = 0.05, fill = F)
## They are in Washington state, and in Seatle.

## I wonder what factors may influence the house price
### will the number of bedrooms influence the house price?
p2 <- ggplot(data = df, mapping = aes(x = factor(bedrooms), price)) + 
  geom_boxplot()

### will the number of bathrooms influence the house price?
p3 <- ggplot(data = df, mapping = aes(x = factor(bathrooms), price)) + 
  geom_boxplot()

### will the living area influence the house price?
p4 <- ggplot(data = df, mapping = aes(x = sqft_living, price)) + 
  geom_point() + geom_smooth()

### will the number of floors influence the house price?
p5 <- ggplot(data = df, mapping = aes(x = factor(floors), price)) + 
  geom_boxplot()

### will the waterfront influence the house price?
p6 <- ggplot(data = df, mapping = aes(x = factor(waterfront), price)) + 
  geom_boxplot()

### will the view influence the house price?
p7 <- ggplot(data = df, mapping = aes(x = factor(view), price)) + 
  geom_boxplot()

### will the waterfront influence the house price?
p8 <- ggplot(data = df, mapping = aes(x = factor(waterfront), price)) + 
  geom_boxplot()

### will the location influence the house price?
p9 <- ggplot(data = df, mapping = aes(x = factor(zipcode), price)) + 
  geom_boxplot() + 
  coord_flip()

### will the age influence the house price?
p10 <- ggplot(data = df, mapping = aes(x = age, price)) + 
  geom_point()

grid.arrange(p2, p5, p6, p7, p8, nrow=3)
grid.arrange(p3, p4, p9, p10, nrow=2)

## Model
# fit linear regession model
fit1 <- lm(logprice ~ bedrooms + bathrooms + sqft_living + factor(floors) + factor(waterfront) + factor(view) + factor(condition) + grade + factor(zipcode) + age, 
           data = df)
summary(fit1)
vif(fit1)
# all vifs are <5, which indicates the multicollinearity is small.

# residual plot
par(mfrow=c(2, 2))
plot(fit1)


# Propensity Score Matching
df$Group = as.logical(df$waterfront==1)

#Then we compare the distribution of bedrooms/bathrooms/sqft_living/floors/waterfront/view/condition/grade/zipcode/age/logprice
table1 <- CreateTableOne(vars = c('bedrooms', 'bathrooms', 'sqft_living', 'floors', 'view', 'condition', 'grade', 'zipcode', 'age', 'logprice'), 
                         data = df, 
                         factorVars = c('floors', 'view', 'condition', 'zipcode'), 
                         strata = 'waterfront')
table1 <- print(table1, 
                printToggle = FALSE, 
                noSpaces = TRUE)
kable(table1[,1:3],  
      align = 'c', 
      caption = 'Table 1: Comparison of unmatched samples')
# The level of logprice seems to have no significantly difference in the two groups.


#Propensity scores are from logistic regression, so let's look at how this goes.
scoremodel = glm(Group ~ bedrooms + bathrooms + sqft_living + factor(floors) + factor(view) + factor(condition) + grade + factor(zipcode) + age,data=df,family=binomial())
propenscores = data.frame(pr_score = predict(scoremodel,type="response"),
                          Group= scoremodel$model$Group)
head(propenscores)
scoremodel

propenscores %>% 
  mutate(Group == ifelse(Group==T, "onWaterfront","offWaterfront")) %>% 
  ggplot(aes(x = pr_score,fill=Group)) +
  geom_histogram(color="white") +
  facet_wrap(~Group) +
  xlab("Probability of getting treated") +
  theme_bw()

# Matching the sample
#The package MatchIt lets us use propensity score matching more easily and gives us a variety of options for matching algorithms
match.it = matchit(Group ~ bedrooms + bathrooms + sqft_living + factor(floors) + factor(view) + factor(condition) + grade + factor(zipcode) + age, 
                   data=df, method="nearest")
a <- summary(match.it)

#Here we can see our remaining data post-matching
df.match = match.data(match.it)
head(df.match)
#After matching the samples, the size of the population sample was reduced to the size of the onWaterfront sample (n=163).

#We can compare our groups again with our new matched dataset.
table2 <- CreateTableOne(vars = c('bedrooms', 'bathrooms', 'sqft_living', 'floors', 'view', 'condition', 'grade', 'zipcode', 'age', 'logprice'), 
                         data = df.match, 
                         factorVars = c('floors', 'view', 'condition', 'zipcode'), 
                         strata = 'waterfront')
table2 <- print(table2, 
                printToggle = FALSE, 
                noSpaces = TRUE)
kable(table2[,1:3],  
      align = 'c', 
      caption = 'Table 2: Comparison of matched samples')

kable(a$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

#We can now estimate treatment effects. We can use t-tests or other things, but here I'll use a linear regression.
model = lm(logprice ~ bedrooms + bathrooms + sqft_living + factor(floors) + waterfront + factor(view) + factor(condition) + grade + factor(zipcode) + age, data=df.match)
summary(model)

plot(match.it, type = 'jitter', interactive = FALSE)

# Validation
## create training and testing data
set.seed(1)
trainingrow <- sample(1:nrow(df), 0.8*nrow(df))
training <- df[trainingrow, ]
testing <- df[-trainingrow,]

## fit the testing data
predtest <- predict(fit1, testing)

## calculate the accuracy and error rate
actualpred <- data.frame(cbind(actuals = testing$logprice, predicteds = predtest))
correlationaccuracy <- cor(actualpred)
correlationaccuracy
data.frame( R2 = R2(predtest, testing$logprice), 
            RMSE = RMSE(predtest, testing$logprice), 
            MAE = MAE(predtest, testing$logprice))
#head(actualpred)


## k-fold Cross-Validation
par(mfrow=c(1, 1))
cv.lm(df, form.lm=formula(logprice ~ bedrooms + bathrooms + sqft_living + factor(floors) + factor(waterfront) + factor(view) + factor(condition) + grade + factor(zipcode) + age), 
      m=5, dots = FALSE, plotit = T, printit = T)

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=5)
# Fit Naive Bayes Model
model <- train(logprice ~ bedrooms + bathrooms + sqft_living + factor(floors) + factor(waterfront) + factor(view) + factor(condition) + grade + factor(zipcode) + age, 
               data=df, 
               trControl=train_control, 
               method="lm")
# Summarise Results
print(model)








