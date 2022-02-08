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
df$label = as.factor(df$label) #convert from num
df$framebased = NULL
df$url = NULL # cannot use bc char
df$urlid = NULL # do not need
df$boilerplate = NULL #cannot use bc char
df$alchemy_category[df$alchemy_category == "?"] <- NA
df$news_front_page = NULL #removed bc it was giving me troube when imputing

library(mice)

idx = is.na(df$alchemy_category_score) #indicates missing values
impute_rf = mice(df, method = 'rf')
#rf means random forest
df_rf = complete(impute_rf)
df_rf[idx, ]

summary(df_rf)
View(df_rf)

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

summary(df_rf$alchemy_category)


#partition the data

N = nrow(df_rf)
trainingSize =round(N*0.6)

set.seed(1234)
trainingCases = sample(N, trainingSize)
training = df_rf[trainingCases, ] 
test = df_rf[-trainingCases, ] 


#create the model
model_tree = rpart(label~., method = "class", data=training)
rpart.plot(model_tree) 
predictions_tree = predict(model_tree,test, type="class")
observations = test$label

# Evaluate
error_rate_btree = sum(predictions_tree != observations)/nrow(test)
error_bench = benchmarkErrorRate(training$label, test$label)

# NOTE/THOUGHT --> Do we need to control for overfitting or pruning?? - I did it because we usually do, but in terms of
# of telling a story about the data, overfitting doesn't really seem like a concern. Also it makes our tree so much harder to read. 

# Overfitting. 
stopping_rules = rpart.control(minsplit=1,minbucket=1,cp=0) #for easier view(minsplit=25,minbucket=25,cp=.0001)
model_tree_pruned = rpart(label ~., data=training, control=stopping_rules)
predictions_overfit = predict(model_tree_pruned,test, type="class")
error_overfit = sum(predictions_overfit != observations)/nrow(test)

#Prune tree. Take off the unnecessary branches
model_tree_prune1 = easyPrune(model_tree_pruned)
rpart.plot(model_tree_prune1) 


#Random Forest

model_rf = randomForest(label ~.,method = "class", data=training, ntree=500)
predictions_rf = predict(model_rf,test,type="class")
error_rf = sum(predictions_rf != observations) / nrow(test)
varImpPlot(model_rf) 
table(predictions_rf,observations)