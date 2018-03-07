set.seed(101) 

head(original_data)


sample = sample.split(simulated_data$label, SplitRatio = .70)
train = subset(simulated_data, sample == TRUE)
test  = subset(simulated_data, sample == FALSE)


# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

attach(simulated_data)


set.seed(7)
fit.cart <- train(as.factor(label)~., data=train, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(as.factor(label)~., data=train, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(as.factor(label)~., data=train, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(as.factor(label)~., data=train, method="rf", metric=metric, trControl=control)

# select the best model

results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)


# summarize Best Model

print(fit.cart)
print(fit.knn)
print(fit.svm)
print(fit.rf)



pred_cart <- predict(fit.cart, test)
lda_cart<- confusionMatrix(pred_cart, test$label)
lda_cart

pred_knn <- predict(fit.knn, test)
lda_knn<- confusionMatrix(pred_knn, test$label)
lda_knn

pred_svm <- predict(fit.svm, test)
lda_svm<- confusionMatrix(pred_svm, test$label)
lda_svm

pred_rf <- predict(fit.rf, test)
lda_rf<- confusionMatrix(pred_rf, test$label)
lda_rf

