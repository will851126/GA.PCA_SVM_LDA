require(e1071)
library(MASS)
library(GAparsimony)
library(caret)
library(lhs)
library(mlbench)
set.seed(1111)
datasets=read.csv('/Users/huangbowei/Desktop/coding/R/論文/datadets/drd.csv')
table(datasets$class)

feature_data=datasets[,1:16]
scale_df<-as.data.frame(scale(feature_data, center = TRUE, scale = TRUE))


require(caTools) 
sample = sample.split(datasets$class, SplitRatio=0.7)
train = subset(datasets, sample==TRUE)
train
test = subset(datasets, sample==FALSE)



Z<-scale(train[1:ncol(datasets)-1], center = TRUE, scale = TRUE)    #standardization

View(train[1:ncol(datasets)-1])
View(Z)
C=var(Z) 

eigen.decomp=eigen(C) #command for eigen decomposition
eigen.value=eigen.decomp$values
eigen.vector=eigen.decomp$vectors
Z.p=as.data.frame(Z %*% eigen.vector)
View(Z.p)
train$class
Z.p$class=train$class



Z.test=scale(test[1:ncol(datasets)-1], center = TRUE, scale = TRUE) #standardization
View(Z.test)
Z.test.p=as.data.frame(Z.test %*% eigen.vector)       #use training data's eigen vector to transform testing data
View(Z.test.p)
View(test$class)
Z.test.p$class <- test$class

View(Z.test.p)


R.sq.pc=rep(0,ncol(datasets)-1) #initialize

for (j in 1:ncol(datasets)-1){
  R.sq.pc[j]=cor(Z.p[,j], Z.p$class)^2 } 

R.sq.pc
selection.seq.pc=order(R.sq.pc, decreasing=T) #sort R.sq.pc

selection.seq.pc
R.sq.pc[selection.seq.pc]


# sort(R.sq.pc[selection.seq.pc],decreasing = TRUE)

for(x in 1:ncol(datasets)-1){
  DATA_R.sq_SUM=rep(0,x)
  for (i in 1:x){
    DATA_R.sq_SUM[i]=R.sq.pc[selection.seq.pc[i]]
  } 
  sum(DATA_R.sq_SUM)# sum of r square
  print(sum(DATA_R.sq_SUM))
}

for(x in 1:ncol(datasets)-1){
  DATA_var_R.sq_SUM=rep(0,x)
  for (i in 1:x){
    DATA_var_R.sq_SUM[i]=R.sq.pc[i]
  } 
  sum(DATA_var_R.sq_SUM)# sum of r square
  print(sum(DATA_var_R.sq_SUM))
}


data_train <- as.data.frame(Z.p[,selection.seq.pc[]])
data_test <- as.data.frame(Z.test.p[,selection.seq.pc[]])

data_train$class=train$class
data_test$class=test$class

library(GAparsimony)

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



min_param <- c(0.1, 0.1)
max_param <- c(100, 100)
names_param <- c("C","sigma")
rerank_error <- 0.001


names_features=colnames(data_train)
names_features
nFeatures=ncol(data_train)
nFeatures

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


hard_svm.train.sorted.accuracy=rep(0,ncol(datasets)-1)
hard_svm.test.sorted.accuracy=rep(0,ncol(datasets)-1)

new.train <- as.data.frame(Z.p[,selection.seq.pc[]])
new.test <- as.data.frame(Z.test.p[,selection.seq.pc[]])


for(y in 1:ncol(datasets)-1){
  
  
  f2 <- paste(names(datasets)[ncol(datasets)], "~", paste(names(new.train)[1:y], collapse=" + "))
  
  
  newdata3 <- as.data.frame(new.train[1:y])
  newdata3$Class <- train$class
  
  newdata4 <- as.data.frame(new.test[1:y])
  newdata4$Class <- test$class
  
  hard_model = svm(formula =newdata3$Class~ ., 
                   data = newdata3,
                   type = 'C-classification',
                   kernel='linear',
                   cost=2.545)
  
  print(hard_model)
  
  hard_train.pred = predict(hard_model, newdata3)
  hard_test.pred = predict(hard_model, newdata4)
  
  table(real=test$class,predict=hard_test.pred)
  
  
  hard_confus.matrix = table(real=test$class, predict=hard_test.pred)
  hard_confus.matrix
  
  
  hard_svm.test.sorted.accuracy[y] <- round(sum(diag(hard_confus.matrix))/sum(hard_confus.matrix),4)
  
  
  y <- y + 1
  
}




hard_svm.test.sorted.accuracy

max(hard_svm.test.sorted.accuracy)




hard_model = svm(formula =newdata3$Class~ ., 
                 data = newdata3[1:15],
                 type = 'C-classification',
                 kernel='linear',
                 cost=100)
print(hard_model)




soft_svm.test.sorted.accuracy=rep(0,ncol(datasets)-1)


for(y in 1:ncol(datasets)-1){
  
  
  f2 <- paste(names(datasets)[ncol(datasets)], "~", paste(names(new.train)[1:y], collapse=" + "))
  
  
  newdata3 <- as.data.frame(new.train[1:y])
  newdata3$Class <- train$class
  
  newdata4 <- as.data.frame(new.test[1:y])
  newdata4$Class <- test$class
  
  soft_model = svm(formula = newdata3$Class ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
                   data = newdata3,
                   type = 'C-classification',
                   kernel='linear',
                   cost=c(0.01))
  print(soft_model)
  
  soft_train.pred = predict(soft_model, newdata3)
  soft_test.pred = predict(soft_model, newdata4)
  
  table(real=test$class,predict=soft_test.pred)
  
  
  soft_confus.matrix = table(real=test$class, predict=soft_test.pred)
  soft_confus.matrix
  
  
  soft_svm.test.sorted.accuracy[y] <- round(sum(diag(soft_confus.matrix))/sum(soft_confus.matrix),4)
  
  y <- y + 1
  
}

soft_svm.test.sorted.accuracy

max(soft_svm.test.sorted.accuracy)






rbf_svm.test.sorted.accuracy=rep(0,ncol(datasets)-1)

for(y in 1:ncol(datasets)-1){
  
  
  f2 <- paste(names(datasets)[ncol(datasets)], "~", paste(names(new.train)[1:y], collapse=" + "))
  
  
  newdata3 <- as.data.frame(new.train[1:y])
  newdata3$Class <- train$class
  
  newdata4 <- as.data.frame(new.test[1:y])
  newdata4$Class <- test$class
  
  rbf_model = svm(formula = newdata3$Class ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
                  data = newdata3,
                  type = 'C-classification',
                  kernel='radial',
                  gamma=1.037,
                  cost=2.545)
  
  
  rbf_train.pred = predict(rbf_model, newdata3)
  rbf_test.pred = predict(rbf_model, newdata4)
  
  rbf_confus.matrix = table(real=test$class, predict=rbf_test.pred)
  rbf_svm.test.sorted.accuracy[y] <- round(sum(diag(rbf_confus.matrix))/sum(rbf_confus.matrix),4)
  
  y<-y+1
  
}

rbf_svm.test.sorted.accuracy
max(rbf_svm.test.sorted.accuracy)

rbf_model = svm(formula = newdata3$Class ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
                data = newdata3[1:4],
                type = 'C-classification',
                kernel='radial',
                gamma=0.1,
                cost=100)

print(rbf_model)




polynomial_svm.test.sorted.accuracy=rep(0,ncol(datasets)-1)

for(y in 1:ncol(datasets)-1){
  
  
  f2 <- paste(names(datasets)[ncol(datasets)], "~", paste(names(new.train)[1:y], collapse=" + "))
  
  
  newdata3 <- as.data.frame(new.train[1:y])
  newdata3$Class <- train$class
  
  newdata4 <- as.data.frame(new.test[1:y])
  newdata4$Class <- test$class
  
  
  polynomial_model = svm(formula = newdata3$Class ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
                         data = newdata3,
                         type = 'C-classification',
                         kernel='polynomial',
                         cost=2.545)
  print(polynomial_model)
  
  
  polynomial_train.pred = predict(polynomial_model, newdata3)
  polynomial_test.pred = predict(polynomial_model, newdata4)
  
  table(real=test$class, predict=polynomial_test.pred)
  
  
  
  polynomial_confus.matrix = table(real=test$class, predict=polynomial_test.pred)
  polynomial_svm.test.sorted.accuracy[y]<-round(sum(diag(polynomial_confus.matrix))/sum(polynomial_confus.matrix),4)
  
  
  y<-y+1
  
  
}

polynomial_svm.test.sorted.accuracy

max(polynomial_svm.test.sorted.accuracy)



## LDA


lda.test.sorted.accuracy=rep(0,ncol(datasets)-1)



for(y in 1:ncol(datasets)-1){
  
  
  f2 <- paste(names(datasets)[ncol(datasets)], "~", paste(names(new.train)[1:y], collapse=" + "))
  
  
  newdata3 <- as.data.frame(new.train[1:y])
  newdata3$Class <- train$class
  
  newdata4 <- as.data.frame(new.test[1:y])
  newdata4$Class <- test$class
  
  lda.train.label<-train$class
  lda.test.label<- test$class
  
  
  
  lda.sort<-lda(newdata3[1:y],lda.train.label)
  lda_predict<-predict(lda.sort, newdata4[1:y])
  lda_confusion.matrix<-table(test$class,lda_predict$class)
  
  lda.test.sorted.accuracy[y]<-round(sum(diag(lda_confusion.matrix))/sum(lda_confusion.matrix),4)
  
  y<-y+1
  
}

lda.test.sorted.accuracy
max(lda.test.sorted.accuracy)



hard_svm.test.sorted.accuracy
soft_svm.test.sorted.accuracy
rbf_svm.test.sorted.accuracy
polynomial_svm.test.sorted.accuracy
lda.test.sorted.accuracy



lda.svm.df <- data.frame(#Features=c(as.character(1:(ncol(df)-1))),#,"21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50"
  Features=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16"),
  Model=c(rep("Hard-SVM",ncol(datasets)-1), rep("Soft-SVM",ncol(datasets)-1),rep("Rbf-SVM",ncol(datasets)-1),
          rep("Poly-SVM",ncol(datasets)-1),rep("LDA",ncol(datasets)-1)),
  Testing_Accuracy=c(hard_svm.test.sorted.accuracy,soft_svm.test.sorted.accuracy,
                     rbf_svm.test.sorted.accuracy,polynomial_svm.test.sorted.accuracy,
                     lda.test.sorted.accuracy))

lda.svm.lineplot <- ggplot( lda.svm.df, aes(x=Features, y=Testing_Accuracy, group=Model, color=Model)) +
  geom_line()+
  geom_point(size=1)+
  theme_classic()+   #remove background
  theme(text = element_text(size = 12))+
  scale_color_manual(values=c("red", "green" ,"blue",'purple','orange'))+
  ggtitle("drd classified by LDA and svm")

lda.svm.lineplot