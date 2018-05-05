library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)

########################_PART_1_###############################################
#defining df

df <- read.csv('student-mat.csv', sep = ';')

# Num only
num.cols <- sapply(df, is.numeric)

# Filter
cor.data <- cor(df[, num.cols])

# Creating a corrplot
print(corrplot(cor.data, method = 'color'))

# Creating a corrgram
corrgram(df,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

# Creating a ggplot
ggplot(df,aes(x=G3)) + geom_histogram(bins=20,alpha=0.5,fill='blue') + theme_minimal() 

#######################_PART_2_#################################################

#split Data into Train and Test Set
library(caTools)
# Set seed
set.seed(101)

# Split up sample
sample <- sample.split(df$G3, SplitRatio = 0.7)

# 70% of data -> train, the comparisson after the coma is the split rule
train <- subset(df, sample == TRUE)

# 30% will be test
test <- subset(df, sample == FALSE)

# Building a model in R + Running it
model <- lm(G3 ~., data = train)

# Getting the sammery of the model 
summary(model)

# Visualising the residuals

res <- residuals(model)

res <- as.data.frame(res)

ggplot(res, aes(res)) + geom_histogram(fill='blue', alpha = 0.5)

plot(model)

##############################_PART_3_########################################

# Getting the predictions for the test set
G3.predictions <- predict(model,test)

# Creating a data frame with the predictions and the actual results
results <- cbind(G3.predictions,test$G3) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)

#Creating a function for the grades that are negative (which cant exist) and applying it to the results array
to_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

results$pred <- sapply(results$pred, to_zero)


##############Avaluating the error of the mashin#############################

# Mean square error
mse <- mean((results$real-results$pred)^2)
print(mse)

# Root of MSE
print(mse^0.5)

# R squered value 
SSE = sum((results$pred - results$real)^2)
SST = sum( (mean(df$G3) - results$real)^2)

R2 = 1 - SSE/SST
print(R2)
