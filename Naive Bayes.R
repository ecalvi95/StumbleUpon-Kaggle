library(readxl)
df <- read_excel("Bag of Words - zeros ones.xlsx")
source("~/Babson/Fall/QTM6300/Data/BabsonAnalytics.R")

everyColumn = colnames(df) #gives a name to collective columns
df[everyColumn] = lapply(df[everyColumn], as.factor) #changes all columns to factor

#partition the data
set.seed(1234)
N = nrow(df)
trainingSize = round(N*0.8)
trainingCases  = sample(N, trainingSize)
training = df[trainingCases, ]
test = df[-trainingCases, ]

library(e1071)
model = naiveBayes(label ~., data = training)

#predict
predictions = predict(model, test) 
observations = test$label

#evaluate
error_rate = sum(predictions != observations)/nrow(test)# 26% error rate
error_bench = benchmarkErrorRate(training$label, test$label) #49% bench error

numerator = sum(df$crust == "1" & df$label == "1") 
denominator = sum(df$crust == "1" & df$label  == "0")

odds= numerator/denominator 

#10.68 - a boilerplate that contains "recipe" is 10.68 times more likely to be labeled evergreen, then not. 
#6.13 - "chocolate" is 6.13 time more likely to be evergreen
#3.58 - "food" is 3.58 times more likely to be evergreen
#7.98 - "recipes"
#10.4 - "butter"
#7.37 - "sugar"
#8.47 - "cheese"
#1.08 - "health"
#2.50 - "healthy"
#12.01 - "baking"
#10.27 - "cheesecake"
#3.0 - "damascus"
#14.8 - "dough"
#9.88 - "pastry"
#17.54 - "skillet"
#15.3 - "creamy"
#17.25 - "nutella"
#9.25 - "brownies"
#6.77 - "pasta"
#6.14 - "chicken"













