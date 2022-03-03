require(e1071)
library(MASS)
set.seed(1111)
datasets=read.csv('/Users/huangbowei/Desktop/coding/R/論文/datadets/drd.csv')

table(datasets$class)


smp.size = floor(0.7*nrow(datasets)) 
train.ind = sample(seq_len(nrow(datasets)), smp.size)
train = datasets[train.ind, ]
test = datasets[-train.ind, ] 

# LDA 
train_Lda=train[,1:16]
train_label=train[,17]
test_Lda=test[,1:16]
test_label=test[,17]


lda<-lda(train_Lda,train_label)
lda_predict<-predict(lda, test_Lda)
lda_predict$class
lda_confusion.matrix<-table(test_label,lda_predict$class)
lda_confusion.matrix

sum(lda_confusion.matrix)
lda.accuracy <- round(sum(diag(lda_confusion.matrix))/sum(lda_confusion.matrix),4)

lda.accuracy





## Hard margin svm
hard_model = svm(formula = train$class ~.,  # 依變數(在這裡是Type)的資料形態要是Factor
            data = train,
            type = 'C-classification',
            kernel='linear',
            cost=100
            )
# 可以看到SVM預設的參數設定
print(hard_model)
summary(hard_model)

hard_train.pred = predict(hard_model, train)
hard_test.pred = predict(hard_model, test)


table(real=train$class,predict=hard_train.pred)
table(real=test$class, predict=hard_test.pred)

train.hard_confus.matrix= table(real=train$class, predict=hard_train.pred)
sum(train.hard_confus.matrix)

train.hard_accuracy=round(sum(diag(train.hard_confus.matrix))/sum(train.hard_confus.matrix),4)

train.hard_accuracy

test.hard_confus.matrix = table(real=test$class, predict=hard_test.pred)
sum(test.hard_confus.matrix)
test.hard_svm_accuracy=round(sum(diag(test.hard_confus.matrix))/sum(test.hard_confus.matrix),4)

test.hard_svm_accuracy



## soft margin svm

soft_model = svm(formula = train$class ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
                 data = train,
                 type = 'C-classification',
                 kernel='linear',
                 cost=c(0.01)
                
                 )
# 可以看到SVM預設的參數設定
summary(soft_model)
print(soft_model)

soft_train.pred = predict(soft_model, train)
soft_test.pred = predict(soft_model, test)


table(real=train$class,predict=soft_train.pred)
train.soft_confus.matrix = table(real=train$class, predict=soft_train.pred)
sum(train.soft_confus.matrix)
train.soft_svm_accuracy=sum(diag(train.soft_confus.matrix))/sum(train.soft_confus.matrix)

train.soft_svm_accuracy

table(real=test$class, predict=soft_test.pred)


test.soft_confus.matrix = table(real=test$class, predict=soft_test.pred)
sum(test.soft_confus.matrix)
test.soft_svm_accuracy=round(sum(diag(test.soft_confus.matrix))/sum(test.soft_confus.matrix),4)


test.soft_svm_accuracy



rbf_model = svm(formula = train$class ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
                 data = train,
                 type = 'C-classification',
                 method="svmRadial",
                 gamma=0.1,
                 cost=100
                 
)
print(rbf_model)
summary(rbf_model)

rbf_train.pred = predict(rbf_model, train)
rbf_test.pred = predict(rbf_model, test)



table(real=test$class, predict=rbf_test.pred)


rbf_confus.matrix = table(real=test$class, predict=rbf_test.pred)
sum(rbf_confus.matrix)
rbf_svm_accuracy=round(sum(diag(rbf_confus.matrix))/sum(rbf_confus.matrix),4)

rbf_svm_accuracy



polynomial_model = svm(formula = train$class ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
                data = train,
                type = 'C-classification',
                kernel='polynomial',
                cost=100
                
)



print(polynomial_model)
summary(polynomial_model)

polynomial_train.pred = predict(polynomial_model, train)
polynomial_test.pred = predict(polynomial_model, test)



table(real=test$class, predict=polynomial_test.pred)


polynomial_confus.matrix = table(real=test$class, predict=polynomial_test.pred)
sum(polynomial_confus.matrix)
polynomial_svm_accuracy=round(
    sum(diag(polynomial_confus.matrix))/sum(polynomial_confus.matrix),4)

polynomial_svm_accuracy

