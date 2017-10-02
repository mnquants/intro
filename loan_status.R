# loan_status.R
# Logistic Regress in R
# Author: Yeshi Lhewa
# Date Updated: 10/1/17
# Summary: This file is meant to highlight 
#          the process for doing Logistic 
#          Regression in R

#Response Variable is Loan.Status with the value being either Fully Paid or Charged Off which means original 
#creditor has given up on being repaid according to the original terms of the loan.

#Explanatory variables are easily interpreted by the variable name so I won't explain too much.

#We have dataset called credit_train we read in and this will be our main data set we work with. 
credit_data <- read.csv("credit_train.csv")

#Now we need to take a look at our variables and do some data cleaning.
str(credit_data)

#We see from our preliminary look into the data set that there are 19 variables including the response variable. We really don't need the first two becuase 
#it will probably not lead to any meaningful predictions to whether the loan was fully paid or not.
credit_data <- credit_data[ ,!colnames(credit_data) %in% c("Loan.ID", "Customer.ID")]

#Question 1 - There are many other ways to subset the data so find another way to subset the data like the code above

#Now that we have gotten rid of the Loan.ID and Customer.ID variables now lets see if we can get rid of some NA values.
#Check first the percentage of NA values in each column and after that we can decide whether we want to just get rid of the NA values or replace them.
checkNA <- function(data){
  apply(data, 2, function(x) mean(is.na(x)))
}

checkNA(credit_data)

#A good rule of thumb is if there are less than 10% of missing data in a column its fine if you just erase those values but it is 
#honestly very conditional on how much data you have. In this case we have around 100,000 rows so we have a good amount of data where 
#taking out 10% of the data wouldn't do much harm to our predictive power. 

#We see only 2 columns that are over 10% NA values. These variables are Annual.Income with around 20% missing data and Months.since.last.delinquents being around 53%.
#When we see a huge number like 53% it is not worth our time keeping this variable so we will take it out.
credit_data <- credit_data[ ,!colnames(credit_data) %in% "Months.since.last.delinquent"]

#Next with the Annual.Income, it is around 20% missing values. In this case you can either replace the NA values with the mean Annual.Income or 
#you can make a personal judgement and just take out all the NA values if you don't think it will hurt the predictive power. For this lesson we will 
#replace the NA with the mean of the Annual.Income variabel.


credit_data$Annual.Income[which(is.na(credit_data$Annual.Income))] <- mean(credit_data$Annual.Income, na.rm = T)
credit_data_replace <- na.omit(credit_data)

checkNA(credit_data_replace)

#We have finally cleaned all the data now it is time to split up the data into a training, validation and test set.
#When we split up the data we are going to do random sampling the the data set and split it into these 
#percentages - Training: 60% Validation: 20% and Testing: 20%.
set.seed(123)
ind <- sample(3, nrow(credit_data_replace), replace = TRUE, prob = c(.6, .2, .2))
training <- credit_data_replace[ind == 1,]
validation <- credit_data_replace[ind == 2,]
testing <- credit_data_replace[ind == 3,]

#Question 2- What is a training, validation and testing set and why are they important in machine learning?

#Now we have everything in place so now we can finnaly do logistic regression on our training set.
modl1 <- glm(Loan.Status~., data = training, family = "binomial")
summary(modl1)

#We then predict the whether or not the loan was fully paid in the validation set and then we get probabilities as 
#our predictions. You then change the ones greater than 50% to fully paid and the rest into charged off.
pred <- predict(modl1, validation[ ,-1], type = "response")
modl1_pred_val <- rep("Charged Off",length(validation$Loan.Status))
modl1_pred_val[pred > .5] <- "Fully Paid"

table(modl1_pred_val, validation$Loan.Status)

#After looking at the table pick the two number that are misclassified and add them together then divide by the length
#of the Loan.Status variable and then you get you misclassification error for this model. You should get around 15%


#Lets see if we can make this a better model. Lets use the step function and choose which variables are most significant in the model
#based on the AIC criterion. This may takes a couple of minutes.
modl2 <- step(modl1)
summary(modl2)

#Question 3 - What are AIC and BIC and how do they help with choosing a model that fits the data.

#The AIC has only changed a miniscule amount so misclassification error might be similar.
pred2 <- predict(modl2, validation[ ,-1], type = "response")
modl2_pred_val <- rep("Charged Off",length(validation$Loan.Status))
modl2_pred_val[pred2 > .5] <- "Fully Paid"

table(modl2_pred_val, validation$Loan.Status)


#We find that this model was able to classify one more case correctly than in model 2 so we will use this when classifying our test set.
pred3 <- predict(modl2, testing[ ,-1], type = "response")
modl2_pred_test <- rep("Charged Off",length(testing$Loan.Status))
modl2_pred_test[pred3 > .5] <- "Fully Paid"

table(modl2_pred_test, testing$Loan.Status)

#Misclassifictation error was found to be around 16% for the test set

#Question 4 - What are some other things that you could have done to get a better model before you tested the model with the testing set?

#This wraps up my lesson on classification using logistic regression. My name is Yeshi and my email is lhewa001@umn.edu, please let me know if there
#are any typos or things you don't understand, or if you just have general question feel free to contact me.
#Heres is a great youtube clip of someone doing logistic regression in r: https://www.youtube.com/watch?v=xrAg3FLQ0ZI 
