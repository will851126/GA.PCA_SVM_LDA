rm(list=ls())
library(MASS)
library(GA)
set.seed(1235)
options("scipen" =100)

############################## Data preprocessing ##############
data <- as.matrix(read.csv("/Users/huangbowei/Desktop/coding/R/論文/學弟LDA-GA/wdbc.csv",header=FALSE,sep=","))
n_row <- nrow(data)
n_col <- ncol(data)
index <- sample(n_row,n_row)
label <- data[,n_col]
pos <- floor(length(index)/2)
data <- data[,1:n_col-1]
#data <- apply(data,2,scale)
###############split data into training and testing###############
train <- data[index[1:pos],]
train_label <- as.matrix(label[index[1:pos]])
test <- data[index[(pos+1):(pos*2)],]
test_label <- as.matrix(label[index[(pos+1):(pos*2)]])
train.1 <- train[which(train_label==1),]
train.0 <- train[which(train_label==0),]
################################ initil coefficients from LDA ####
lda.train<-lda(train,train_label)
suggestions<-c(lda.train$scaling)
#plot(drdlda)
predict<-predict(lda.train, train)$class
lda.confusion.matrix <- table(c(train_label),predict)
lda.accuracy <- sum(diag(lda.confusion.matrix))/sum(lda.confusion.matrix)
print(paste0("lda.accuracy = ", round(lda.accuracy,2)))
suggestions
################################################################
############### Fitness function  ###############
accuracy = 0
fitness<-function(weight.coefficient){
  transformed.feature.male <- train.1 %*% (weight.coefficient)
  transformed.feature.female <- train.0 %*% (weight.coefficient)
  if(mean(transformed.feature.male) > mean(transformed.feature.female)) {
    transformed.feature <- sort(c(transformed.feature.female,transformed.feature.male))
    true.positive <- c()
    true.negative <- c()
    accuracy <- c()
    for(i in 1:length(transformed.feature)){
      #print(transformed.feature[i])
      true.positive[i]<-length(which(transformed.feature.male > transformed.feature[i]))
      true.negative[i]<-length(which(transformed.feature.female <= transformed.feature[i]))
      accuracy[i] <- round((true.positive[i]+true.negative[i])/nrow(train),2)
      
    }
    print(max(accuracy))
  }
  else
  {
    mean(transformed.feature.male) < mean(transformed.feature.female)
    transformed.feature <- sort(c(transformed.feature.male,transformed.feature.female))
    true.positive <- c()
    true.negative <- c()
    accuracy <- c()
    for(i in 1:length(transformed.feature)){
      #print(transformed.feature[i])
      true.positive[i]<-length(which(transformed.feature.male < transformed.feature[i]))
      true.negative[i]<-length(which(transformed.feature.female >= transformed.feature[i]))
      accuracy[i] <- round((true.positive[i]+true.negative[i])/nrow(train),2)
      #print(true.positive)
      #print(true.negative)
      #print(accuracy)
      #print(transformed.feature)
      #print(max(accuracy))
    }
    print(max(accuracy))
  }
}

############################## GA parameter ####################
popsize <- 30 #population size
maxiter <- 300 #maximum number of iterations
maxfit <- 0.999999 #upper bound on fitness function
#domain <- c(-1,1)
suggestions <- suggestions
domain <- c(1.05*min(suggestions),1.05*max(suggestions))#search domain
############################## Lunch GA  ##################
GA <- ga("real-valued",popSize=popsize,fitness=fitness,maxFitness=maxfit,suggestions = suggestions, maxiter=maxiter,monitor=TRUE,lower=replicate(ncol(train),domain[1]),upper=replicate(ncol(train),domain[2]))
plot(GA)
ga.output <- summary(GA)
ga.output$solution[1,]
GA.max.accuracy<-ga.output$fitness
print(paste0("GA.Max.Accuracy: ", GA.max.accuracy))
