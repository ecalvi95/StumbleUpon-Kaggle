library(tidyverse)
library(ggplot2) #density plot
library(rpart) #class tree
library(rpart.plot) #class tree
library(randomForest) #rf
library(mice) #imputation
library(readr)

source("~/Babson/Fall/QTM6300/Data/BabsonAnalytics.R")
df <- read_tsv('~/Babson/Spring/MSB6300/Intro Project/train.tsv')

df$alchemy_category_score = as.numeric(df$alchemy_category_score) #converted from char to num
df$framebased = as.factor(df$framebased) #converted from num
df$hasDomainLink = as.factor(df$hasDomainLink) #converted from num
df$is_news[is.na(df$is_news) ] <- 0 # converts all ? to 0 -> making assumption all ? are 0 or FALSE
df$is_news = as.factor(df$is_news) 
df$lengthyLinkDomain = as.factor(df$lengthyLinkDomain) #converted from num
df$news_front_page = as.factor(df$news_front_page) #convert from char (made 3 levels, i think one for ?)
df$news_front_page[df$news_front_page == "?" ] <- NA #convert ? to NA

df$framebased = NULL
df$url = NULL # cannot use bc char
df$urlid = NULL # do not need
df$boilerplate = NULL #cannot use bc char

df$news_front_page = NULL #removed bc it was giving me troube when imputing 


df$label = as.logical(df$label) #convert from num

summary(df)

#using random forest to impute missing values in alchemy_category_score

idx = is.na(df$alchemy_category_score)
impute_rf = mice(df, method = 'rf')
df_rf = complete(impute_rf)
df_rf[idx, ]


df_rf$alchemy_category[is.na(df_rf$alchemy_category) ] <- 13 #makes missing values 0 
df_rf[df_rf$alchemy_category == "business", ]$alchemy_category = 1 #dummy variable for business = 1
df_rf[df_rf$alchemy_category == "arts_entertainment", ]$alchemy_category = 2
df_rf[df_rf$alchemy_category == "computer_internet", ]$alchemy_category = 3
df_rf[df_rf$alchemy_category == "culture_politics", ]$alchemy_category = 4
df_rf[df_rf$alchemy_category == "gaming", ]$alchemy_category = 5
df_rf[df_rf$alchemy_category == "health", ]$alchemy_category = 6
df_rf[df_rf$alchemy_category == "law_crime", ]$alchemy_category = 7
df_rf[df_rf$alchemy_category == "recreation", ]$alchemy_category = 8
df_rf[df_rf$alchemy_category == "religion", ]$alchemy_category = 9
df_rf[df_rf$alchemy_category == "science_technology", ]$alchemy_category = 10
df_rf[df_rf$alchemy_category == "sports", ]$alchemy_category = 11
df_rf[df_rf$alchemy_category == "weather", ]$alchemy_category = 12
df_rf[df_rf$alchemy_category == "unknown", ]$alchemy_category = 13

df_rf$alchemy_category = as.factor(df_rf$alchemy_category)


#partition the data

N = nrow(df_rf)
trainingSize =round(N*0.6)

set.seed(1234)
trainingCases = sample(N, trainingSize)
training = df_rf[trainingCases, ] 
test = df_rf[-trainingCases, ] 

#create the model

model=glm(label ~., data = training, family=binomial)
summary(model)
model = step(model)
summary(model)

pred = predict(model, test, type = "response")
predTF = pred > 0.5
obs = test$label


table(predTF,obs)

error_rate = sum(predTF != obs)/nrow(test) # 36%   #new 34%
error_bench = benchmarkErrorRate(training$label, test$label) # 49%

sensitivity = sum(predTF == TRUE & obs == TRUE)/sum(obs == TRUE)# new 70%  # 0.73 -> % of observed trues we predict correctly
specificity = sum(predTF == FALSE & obs == FALSE)/sum(obs == FALSE) # new 62% #0.55 -> % of observed falses we predicted correctly

ROCChart(obs, pred)
liftChart(obs,pred)

library(pROC)

auc(obs,pred)

AUC = 0.68

#new AUC with category factors is 0.72!