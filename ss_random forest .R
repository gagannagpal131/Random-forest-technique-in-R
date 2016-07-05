
print("Use of random forest technique for Machine Learning")

df <- read.csv("sleepstudy.csv")

index <- sample(1:nrow(df),round(0.75*nrow(df)))

#dividing the data into two parts test and train
train <- df[index,]
test <-  df[-index,]

#using the library randomForest  
library("randomForest")
myFormula <- Reaction ~ Days + Subject 

#using randomForest Technique on the train data set 
rf <- randomForest(formula = myFormula ,data = train)
plot(rf)

#To implement machine learning,i.e. predictive analysis of data
pr_rf <- predict(rf,newdata = test)

#Display the actual values and the predicted values side by side 
finalOutput <- cbind(pr_rf,test$Reaction)
colnames(finalOutput) <- cbind("predicted","actual")
View(finalOutput)

#Graphically show the actual values and the predicted values
plot(pr_rf, type = "b", col = "blue",ylim = c(200,500),ylab ="Reaction time in ms",main = "Actual values (black) vs predicted values (blue)")
lines(test$Reaction,type = "b")

#mean absolute percentage error
mape_rf = (mean(abs(test$Reaction - pr_rf)/test$Reaction))*100

print(mape_rf)




















