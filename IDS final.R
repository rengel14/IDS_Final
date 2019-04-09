#Final Project Code IDS

#Question #1
cancer <- read.csv("FNA_cancer.csv", header = T) #reading in file
glimpse(cancer) #33 variables and 569 observations

#Question #2
summary(cancer)
cancer <- cancer %>% select(-id, -X)

ggplot(data = cancer, aes(x = diagnosis) ) + geom_histogram(stat = "count") #far more benign than malignant
table1 <- table(cancer$diagnosis)
prop.table(table1) #63% benign vs. 37# malignant

summary(cancer$radius_mean) #looking at radius mean variable, max is almost double the mean
ggplot(data = cancer, aes(x = radius_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) #graph shows a clear correlation between larger radius and larger chance of being malignant 

summary(cancer$texture_mean) #max is also double mean for this variable as well
ggplot(data = cancer, aes(x = texture_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) # not a clear correlation between texture and diagnosis

summary(cancer$area_mean) #Max is almost 5 times the mean
ggplot(data = cancer, aes(x = area_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) #also a clear correlation for area and diagnosis

#Now looking at the worst values for Area and Radius since there mean values had the greatist effect on diagnosis

summary(cancer$area_worst) # max is about 4 times the mean
ggplot(data = cancer, aes(x = area_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) #correlation is even more clear with worst values than with mean

summary(cancer$radius_worst) # max is about 2 times the mean
ggplot(data = cancer, aes(x = radius_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) #correlation is even more clear with worst values than with mean

#the worst values of radius and area have even a more clear impact on diagnosis than the mean values.





#Question 3

n <- nrow(cancer)
set.seed(1847)
test_index <- sample.int(n, size = round(.2*n))
train_cancer <- cancer[-test_index,]
test_cancer <- cancer[test_index, ] #divinding between training and test data
glimpse(train_cancer)
glimpse(test_cancer) #glimpse of the data

#Question 4 - decision tree

library(rpart)
library(partykit) #adding libraries needed
diagnosis_tree <- rpart(diagnosis ~ ., data=train_cancer) #creating tree with all variables - tree chose to split on concave points worst and area worst
diagnosis_tree$cptable #complexity parameter and relative error is minimized with splits on concave points worst and area worst
diagnosis_tree$variable.importance #concave points worst is the most important variable
plot(as.party(diagnosis_tree)) #tree chose two splits concave points worst and area worst
plotcp(diagnosis_tree) #big drop in relative error with relatively small CP increase


test_cancer$pred <- predict(diagnosis_tree,newdata = test_cancer,type="class") #creating prediction variable
confusion1 <- table(test_cancer$pred, test_cancer$diagnosis)
confusion1 #creating confusion table
sum(diag(confusion1)) / nrow(test_cancer) #tree has 92% accuracy 




#Part 6
library(class)

#creating rescaling function
rescale_xy <- function(x,y){(x-min(y))/(max(y)-min(y))}

#scaling training and testing data, except for response variable
trainSetScaled <- as.tibble(mapply(rescale_xy, train_cancer[-1], train_cancer[-1], SIMPLIFY = F))
testSetScaled <- as.tibble(mapply(rescale_xy, test_cancer[-1], train_cancer[-1], SIMPLIFY = F))

#Adding response variable back into training and testing sets
trainSetScaled <- trainSetScaled %>% mutate(diagnosis = train_cancer$diagnosis)
testSetScaled <- testSetScaled %>% mutate(diagnosis = test_cancer$diagnosis)

#Creating a KNN Classification for with all predictors
allPredictorsKNN <- knn(trainSetScaled[-31]
               ,test = testSetScaled[-31]
               ,cl=trainSetScaled$diagnosis
               ,k=21)

#Creating confusion matrix and calculating miscalculation rate
confusion <- table(allPredictorsKNN, testSetScaled$diagnosis, dnn = c("Model Prediction", "Actual Diagnosis"))
confusion
1-sum(diag(confusion))/sum(confusion)


#Filtering column names to those that only include 'worst' in them, and adding diagnosis response variable column name
worstNames <- colnames(trainSetScaled)[str_detect(colnames(trainSetScaled), pattern = "worst")]
worstNames <- c(worstNames, "diagnosis")
#Selecting only 'worst' variables to be predictors
trainFiltered <- trainSetScaled %>% select(worstNames)
testFiltered <- testSetScaled %>% select(worstNames)

#Creating a KNN Classification for with only 'worst' predictors
worstPredictorsKNN <- knn(trainFiltered[-11]
               ,test = testFiltered[-11]
               ,cl=trainSetScaled$diagnosis
               ,k=21)

#Creating confusion matrix and calculating miscalculation rate
confusion <- table(worstPredictorsKNN, testSetScaled$diagnosis, dnn = c("Model Prediction", "Actual Diagnosis"))
confusion
1-sum(diag(confusion))/sum(confusion)


#Filtering column names to all except for the standard error columns
seNames <- colnames(trainSetScaled)[str_detect(colnames(trainSetScaled), pattern = "_se")]
#Selecting all variables except the standard error ones
trainFiltered <- trainSetScaled %>% select(-seNames)
testFiltered <- testSetScaled %>% select(-seNames)

#Creating a KNN Classification for with 'mean' and 'worst' predictors
noSePredictorsKNN <- knn(trainFiltered[-21]
                          ,test = testFiltered[-21]
                          ,cl=trainSetScaled$diagnosis
                          ,k=21)

#Creating confusion matrix and calculating miscalculation rate
confusion <- table(noSePredictorsKNN, testSetScaled$diagnosis, dnn = c("Model Prediction", "Actual Diagnosis"))
confusion
1-sum(diag(confusion))/sum(confusion)

##----------------------------------------------------EDA Part----------------------------------------------------

library(ggplot2)
library(gridExtra)
p1 <- ggplot(data = cancer, aes(x = radius_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p2 <- ggplot(data = cancer, aes(x = radius_se, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p3 <- ggplot(data = cancer, aes(x = radius_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)



p1 <- ggplot(data = cancer, aes(x = texture_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p2 <- ggplot(data = cancer, aes(x = texture_se, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p3 <- ggplot(data = cancer, aes(x = texture_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)



p1 <- ggplot(data = cancer, aes(x = perimeter_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p2 <- ggplot(data = cancer, aes(x = perimeter_se, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p3 <- ggplot(data = cancer, aes(x = perimeter_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)



p1 <- ggplot(data = cancer, aes(x = area_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p2 <- ggplot(data = cancer, aes(x = area_se, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p3 <- ggplot(data = cancer, aes(x = area_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)



p1 <- ggplot(data = cancer, aes(x = smoothness_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p2 <- ggplot(data = cancer, aes(x = smoothness_se, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p3 <- ggplot(data = cancer, aes(x = smoothness_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)



p1 <- ggplot(data = cancer, aes(x = compactness_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p2 <- ggplot(data = cancer, aes(x = compactness_se, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p3 <- ggplot(data = cancer, aes(x = compactness_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)



p1 <- ggplot(data = cancer, aes(x = concavity_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p2 <- ggplot(data = cancer, aes(x = concavity_se, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p3 <- ggplot(data = cancer, aes(x = concavity_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)



p1 <- ggplot(data = cancer, aes(x = concave.points_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p2 <- ggplot(data = cancer, aes(x = concave.points_se, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p3 <- ggplot(data = cancer, aes(x = concave.points_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)



p1 <- ggplot(data = cancer, aes(x = symmetry_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p2 <- ggplot(data = cancer, aes(x = symmetry_se, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p3 <- ggplot(data = cancer, aes(x = symmetry_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)



p1 <- ggplot(data = cancer, aes(x = fractal_dimension_mean, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p2 <- ggplot(data = cancer, aes(x = fractal_dimension_se, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

p3 <- ggplot(data = cancer, aes(x = fractal_dimension_worst, fill = diagnosis)) +
  geom_histogram(bins = 50, position="identity", alpha=0.5) 

grid.arrange(p1, p2, p3, ncol = 3, nrow = 1)





