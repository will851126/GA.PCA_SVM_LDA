require(e1071)
library(GAparsimony)
library(caret)
library(lhs)
library(mlbench)
datasets<-read.csv('/Users/huangbowei/Desktop/coding/R/論文/datadets/drd.csv')
set.seed(1111)
inTraining <- createDataPartition(datasets$class, p=.7, list=FALSE)
data_train <- datasets[ inTraining,]
data_test <- datasets[-inTraining,]



fitness_SVM <- function(chromosome, ...)
{
  tuneGrid <- data.frame(C=chromosome[1],sigma=chromosome[2])
  selec_feat <- chromosome[3:length(chromosome)]>0.50
  if (sum(selec_feat)<1) return(c(kappa_val=-Inf,kappa_test=-Inf,complexity=Inf))
  data_train_model <- data_train[,c(selec_feat,TRUE)]
  data_test_model <- data_test[,c(selec_feat,TRUE)]
  train_control <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
  set.seed(1111)
  model <- train(class ~ ., data=data_train_model, trControl=train_control,
                 method="svmRadial",
                 tuneGrid=tuneGrid, verbose=FALSE)
  kappa_val <- model$results$Kappa
  kappa_test <- postResample(pred=predict(model, data_test_model),
                             obs=data_test_model[,ncol(data_test_model)])[2]
  complexity <- sum(selec_feat)*1E6+model$finalModel@nSV
  vect_errors <- c(kappa_val=kappa_val,kappa_test=kappa_test,complexity=complexity)
  return(vect_errors)
}




library(GAparsimony)
# Ranges of size and decay
min_param <- c(0.1, 0.1)
max_param <- c(100, 100)
names_param <- c("C","sigma")
rerank_error <- 0.001



GAparsimony_model <- ga_parsimony(fitness=fitness_SVM,
                                  min_param=min_param,
                                  max_param=max_param,
                                  names_param=names_param,
                                  nFeatures=ncol(data_train)-1,
                                  names_features=colnames(data_train)[-ncol(data_train)],
                                  keep_history = TRUE,
                                  rerank_error = rerank_error,
                                  popSize = 40,
                                  maxiter = 20,
                                  early_stop=10,
                                  feat_thres=0.90,
                                  feat_mut_thres=0.10,
                                  seed_ini = 1234)


print(paste0("Best Parsimonious SVM with C=",
             GAparsimony_model@bestsolution['C'],
             " sigma=",
             GAparsimony_model@bestsolution['sigma'],
             " -> ",
             " AccuracyVal=",
             round(GAparsimony_model@bestsolution['fitnessVal'],6),
             " AccuracyTest=",
             round(GAparsimony_model@bestsolution['fitnessTst'],6)
             ))

print(summary(GAparsimony_model))
print(parsimony_importance(GAparsimony_model))





hard_model = svm(formula = data_train$class ~.,  # 依變數(在這裡是Type)的資料形態要是Factor
                 data = data_train,
                 type = 'C-classification',
                 kernel='linear',
                 cost=c(22.85)
)
# 可以看到SVM預設的參數設定
print(hard_model)
summary(hard_model)

hard_train.pred = predict(hard_model, data_train)
hard_test.pred = predict(hard_model, data_test)


table(real=data_train$class,predict=hard_train.pred)
table(real=data_test$class, predict=hard_test.pred)

train.hard_confus.matrix= table(real=data_train$class, predict=hard_train.pred)
sum(train.hard_confus.matrix)

train.hard_accuracy=round(sum(diag(train.hard_confus.matrix))/sum(train.hard_confus.matrix),4)

train.hard_accuracy

test.hard_confus.matrix = table(real=data_test$class, predict=hard_test.pred)
sum(test.hard_confus.matrix)
test.hard_svm_accuracy=round(sum(diag(test.hard_confus.matrix))/sum(test.hard_confus.matrix),4)

test.hard_svm_accuracy


rbf_model = svm(formula = data_train$class ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
                data = data_train,
                type = 'C-classification',
                method="svmRadial",
                gamma=c(0.0276),
                cost= c(96.26)
                
)
print(rbf_model)
summary(rbf_model)

rbf_train.pred = predict(rbf_model, data_train)
rbf_test.pred = predict(rbf_model, data_test)



table(real=data_test$class, predict=rbf_test.pred)


rbf_confus.matrix = table(real=data_test$class, predict=rbf_test.pred)
sum(rbf_confus.matrix)
rbf_svm_accuracy=round(sum(diag(rbf_confus.matrix))/sum(rbf_confus.matrix),4)

rbf_svm_accuracy



polynomial_model = svm(formula = data_train$class ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
                       data = data_train,
                       type = 'C-classification',
                       kernel='polynomial',
                       cost=96.48
)



print(polynomial_model)
summary(polynomial_model)

polynomial_train.pred = predict(polynomial_model, data_train)
polynomial_test.pred = predict(polynomial_model, data_test)



table(real=data_test$class, predict=polynomial_test.pred)


polynomial_confus.matrix = table(real=data_test$class, predict=polynomial_test.pred)
sum(polynomial_confus.matrix)
polynomial_svm_accuracy=round(sum(diag(polynomial_confus.matrix))/sum(polynomial_confus.matrix),4)

polynomial_svm_accuracy

