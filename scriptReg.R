data <- read.csv(file="audi.csv", header=T, sep=",")
s <- sample(10668, 1000)
data <- data[s, ]
summary(data)
dim(data)
p <- ncol(data)-1

library(caret)
set.seed(1)

#k-nn
ctrl <- trainControl(method="cv", number=5)
param <- expand.grid(k=seq(1,10))
fit_knn <- train(price~., data, method="knn", trControl=ctrl, tuneLength=100, tuneGrid=param)
plot(fit_knn)
print(fit_knn)

#Forêts aléatoires
ctrl <- trainControl(method="cv", number=5)
param <- data.frame(mtry=1:p)
fit_rf <- train(price~., data, method="rf", trControl=ctrl, tuneLength=100, tuneGrid=param)
plot(fit_rf)
print(fit_rf)
plot(varImp(fit_rf))

#Boosting
ctrl <- trainControl(method="cv", number=5)
param <- expand.grid(n.trees=seq(10,2000,10), interaction.depth=c(1,2), shrinkage=0.01, n.minobsinnode=10)
fit_gbm <- train(price~., data, method="gbm", trControl=ctrl, tuneLength=100, tuneGrid=param, verbose=F)
print(fit_gbm)
plot(fit_gbm, type="l")
print(fit_gbm$bestTune)
plot(fit_gbm$finalModel, i="mpg")
plot(fit_gbm$finalModel, i="year")
plot(fit_gbm$finalModel, i="mileage")
plot(fit_gbm$finalModel, i="engineSize")

#encore des arbres
library(randomForest)
library(rpart)
library(party)
library(partykit)
ctrl <- rpart.control(maxdepth=3)
fit_tree <- rpart(price~., data, method='anova', control=ctrl)
plot(as.party(fit_tree))

ctrl <- trainControl(method='cv', number=5)
fit_tree_tuned <- train(price~., data, method='rpart', trControl=ctrl, tuneLength=10)
fit_tree_best <- fit_tree_tuned$finalModel
plot(as.party(fit_tree_best))

#toujours des arbres
fit_bag <- randomForest(price~., data, ntree=1000, mtry=p, importance=T)
fit_bag
varImpPlot(fit_bag)

param <- data.frame(mtry=1:p)
fit_rf_tuned <- train(price~., data, method='rf', trControl=ctrl, tuneGrid=param)
plot(fit_rf_tuned)

fit_rf_best<-train(price~., data, method='rf', trControl=ctrl, tuneGrid=data.frame(mtry=3))
plot(varImp(fit_rf_best))

ctrl <- trainControl(method = 'cv', number=5)
param <- expand.grid(shrinkage=1/100, n.trees=seq(100,2000,20), interaction.depth=c(1,2,4), n.minobsinnode=10)
fit_gbm <- train(price~., data, method='gbm', trControl=ctrl, tuneGrid=param, verbose=F)
plot(fit_gbm)
fit_gbm$bestTune
