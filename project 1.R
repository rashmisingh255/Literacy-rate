library(randomForest)
View(Literacy_Rate)

median(Literacy_Rate$`Literacy Rate (Persons) - Total - 2001`)
median(Literacy_Rate$`Literacy Rate (Persons) - Total - 2011`)
median(Literacy_Rate$`Literacy Rate (Persons) - Rural - 2001`)
median(Literacy_Rate$`Literacy Rate (Persons) - Rural - 2011`)
median(Literacy_Rate$`Literacy Rate (Persons) - Urban - 2001`)
median(Literacy_Rate$`Literacy Rate (Persons) - Rural - 2011`)
median(Literacy_Rate$`Literacy Rate (Persons) - Urban - 2001`)
median(Literacy_Rate$`Literacy Rate (Persons) - Urban - 2011`)


#mean
mean(Literacy_Rate$`Literacy Rate (Persons) - Total - 2001`)
mean(Literacy_Rate$`Literacy Rate (Persons) - Total - 2011`)
mean(Literacy_Rate$`Literacy Rate (Persons) - Rural - 2001`)
mean(Literacy_Rate$`Literacy Rate (Persons) - Rural - 2011`)
mean(Literacy_Rate$`Literacy Rate (Persons) - Urban - 2001`)
mean(Literacy_Rate$`Literacy Rate (Persons) - Urban - 2011`)

#median
#mode
mode(Literacy_Rate$`Literacy Rate (Persons) - Total - 2001`)
mode(Literacy_Rate$`Literacy Rate (Persons) - Total - 2011`)
mode(Literacy_Rate$`Literacy Rate (Persons) - Rural - 2001`)
mode(Literacy_Rate$`Literacy Rate (Persons) - Rural - 2011`)
mode(Literacy_Rate$`Literacy Rate (Persons) - Urban - 2001`)
mode(Literacy_Rate$`Literacy Rate (Persons) - Urban - 2011`)
mode(Literacy_Rate$Category)
mode(Literacy_Rate$`Country/ States/ Union Territories Name`)

#variance
var(Literacy_Rate$`Literacy Rate (Persons) - Total - 2001`,Literacy_Rate$`Literacy Rate (Persons) - Total - 2011`)
var(Literacy_Rate$`Literacy Rate (Persons) - Rural - 2001`,Literacy_Rate$`Literacy Rate (Persons) - Rural - 2011`)
var(Literacy_Rate$`Literacy Rate (Persons) - Urban - 2001`,Literacy_Rate$`Literacy Rate (Persons) - Urban - 2011`)

#standard deviation
sqrt(var(Literacy_Rate$`Literacy Rate (Persons) - Total - 2001`,Literacy_Rate$`Literacy Rate (Persons) - Total - 2011`))
sqrt(var(Literacy_Rate$`Literacy Rate (Persons) - Rural - 2001`,Literacy_Rate$`Literacy Rate (Persons) - Rural - 2011`))
sqrt(var(Literacy_Rate$`Literacy Rate (Persons) - Urban - 2001`,Literacy_Rate$`Literacy Rate (Persons) - Urban - 2011`))

#summary
summary(Literacy_Rate)

## Warning: package 'randomForest' was built under R version 3.6.1
library(tree)
library(ggplot2)
library(GGally)
library(dplyr)
Literacy_Rate %>% head()
Literacy_Rate %>% tail()
summary(Literacy_Rate)

         
#Testset, Trainset

index_row <- sample(2,  nrow(Literacy_Rate), replace = T,  prob = c(0.7, 0.3))
#assign values to the rows (1: Training, 2: Test)
train_data <- Literacy_Rate[index_row == 1,]
test_data <- Literacy_Rate[index_row == 2,]

#randomforest (training)

Literacy_Rate_classifier <- randomForest(Literacy_Rate$`Literacy Rate (Persons) - Total - 2001` ~ Literacy_Rate$`Literacy Rate (Persons) - Rural - 2001`,  data = train_data,  #train data set 
                                         importance = T)


Literacy_Rate_classifier   

plot(Literacy_Rate_classifier)
importance(Literacy_Rate_classifier) 
varImpPlot(Literacy_Rate_classifier)

qplot(Literacy_Rate$`Literacy Rate (Persons) - Rural - 2011`,Literacy_Rate$`Literacy Rate (Persons) - Urban - 2011`, data=Literacy_Rate, color =Literacy_Rate$`Literacy Rate (Persons) - Total - 2001` )

install.packages("party")
library(party)
tree <- ctree(`Literacy Rate (Persons) - Total - 2001`~.,data=Literacy_Rate)
plot(tree)
randomForest.rf <-randomforest(`Literacy Rate (Persons) - Total - 2001`~.,data=Literacy_Rate,ntree=1000,keep.Forest=FALSE,importance=FALSE)
plot(Literacy_Rate..if,log="y",tittle="")





install.packages("ggplot2")

library(ggplot2)

ggplot(data=Literacy_Rate,aes(x= Literacy_Rate$`Literacy Rate (Persons) - Total - 2001`, y=Literacy_Rate$`Literacy Rate (Persons) - Total - 2011` + geom_point() + theme_minimal()))
# Speaking of frequency... a best way to show it is to use his

ggplot(data=Literacy_Rate,aes(x=Literacy_Rate$`Literacy Rate (Persons) - Total - 2001`)) + geom_histogram(binwidth=0.8) +theme_minimal()
# we can a stacked histogram by adding another aesthetic 'fill'
ggplot(data=Literacy_Rate,aes(x=Literacy_Rate$`Literacy Rate (Persons) - Total - 2001`,fill=Category)) + geom_histogram() +theme_minimal()

#predicting (testing)
predicted_table <- predict(Literacy_Rate_classifier, test_data[,-8])
table(observed = test_data[,8], predicted = predicted_table)


                          







