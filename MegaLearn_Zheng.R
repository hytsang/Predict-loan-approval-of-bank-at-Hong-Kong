

MegaLearn_Zheng <- function(x,y,method_list){
  #method_list = c("svm","glmnet","rf","svmw","glmnetw","rfw","psvm","psvmw","PCA_LDA","CART","ADA","ANN")
  #method_list = c("svm","glmnet","rf")
  # # load related libraty and data preprocessing (assuming x is numeric and y is categorical)
  # library("glmnet")
  # library("penalizedSVM")
  # library("penalizedLDA")
  # library("e1071")
  # library("randomForest")
  # library(boostr)
  # library(cluster)
  # library(MASS)
  # library(rpart)
  # library(ada)
  # library(nnet)
  # library(cvTools)
  #install.packages("rpart")
  #install.packages("ada")

  # loan <- read.csv("loan.csv")
  # summary(loan)
  # set.seed(154000850)
  # loan_random=loan[sample(nrow(loan)),]
  # y = as.factor(loan_random$RESPONSE)
  # x = as.matrix(loan_random[,-c(1,10)])


  alpha=.25  # proportion of testing set
  cvfold=10  # folds of cross validation
  k = round(alpha*nrow(x))
  # 1. Compares the performance of these methods across training and testing sets
  ytrain = y[-(1:k)]
  ytest = y[(1:k)]
  xtrain = x[-(1:k),]
  xtest = x[(1:k),]
  w <- sqrt(as.matrix(apply(xtrain,2,var)))
  xtrain.w <- t(apply(xtrain,1,function(x)x/w))
  xtest.w <- t(apply(xtest,1,function(x)x/w))

  #svm
  svm.Pred <- function(xtrain,ytrain,xtest,ytest){
    svm.out = svm(ytrain~., data=xtrain,scale=F)
    svmpred = predict(svm.out, newdata=xtest)
    tt = table(svmpred,ytest)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=svm.out)
  }
  #glmnet
  glmnet.Pred <- function(xtrain,ytrain,xtest,ytest){
    cvglm = cv.glmnet(x=xtrain, y=ytrain,family="multinomial",alpha = 0.8,standardize =F)
    glmnet.out = glmnet(x=xtrain, y=ytrain,family="multinomial",alpha = 0.8,lambda=cvglm$lambda.1se,standardize =F)
    glmpred = predict(glmnet.out,newx=xtest,s=cvglm$lambda.1se,type="class")
    tt = table(glmpred,y=ytest)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=glmnet.out)
  }

  #random forest
  rf.Pred <- function(xtrain,ytrain,xtest,ytest){
    rf.out = randomForest(x=xtrain,y=ytrain)
    rfpred = predict(rf.out,newdata=xtest)
    tt=table(rfpred,y=ytest)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=rf.out)
  }

  ##weighted, penalized methods

  #weighted svm
  svmw.Pred <- function(xtrain,ytrain,xtest,ytest){
    svmw.out = svm(ytrain~., data=xtrain.w,scale=F)
    svm.w.pred = predict(svmw.out,newdata=xtest.w)
    tt=table(predict=svm.w.pred, y=ytest)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=svmw.out)
  }

  #weighted glmnet
  glmnetw.Pred <- function(xtrain,ytrain,xtest,ytest){
    cvglm.w = cv.glmnet(x=xtrain.w, y=ytrain,family="multinomial",alpha = 0.8,standardize =F)
    glmnetw.out = glmnet(x=xtrain.w, y=ytrain,family="multinomial",alpha = 0.8,lambda=cvglm.w$lambda.1se,standardize =F)
    glm.w.pred = predict(glmnetw.out,newx=xtest.w,s=cvglm.w$lambda.1se,type="class")
    tt = table(glm.w.pred,y=ytest)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=glmnetw.out)
  }

  #weighted random forest
  rfw.Pred <- function(xtrain,ytrain,xtest,ytest){
    rfw.out = randomForest(x=xtrain.w,y=ytrain)
    rf.w.pred = predict(rfw.out,newdata=xtest.w)
    tt=table(rf.w.pred,y=ytest)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=rfw.out)
  }

  ##Penalized SVM
  psvm.Pred <- function(xtrain,ytrain,xtest,ytest){
    ytrain.psvm = as.matrix((ytrain==1)*2-1)
    ytest.psvm = as.matrix((ytest==1)*2-1)
    cvglm = cv.glmnet(x=xtrain, y=ytrain,family="multinomial",alpha = 0.8,standardize =F)
    psvm.out = svm.fs(x=xtrain, y = ytrain.psvm, fs.method = "scad", scale = F,lambda1.set = cvglm$lambda.1se)
    pred.psvm = as.matrix(predict(psvm.out,newdata=xtest)$pred.class)
    pred.psvm = (pred.psvm==1)*1
    tt= table(pred.psvm,y=ytest)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=psvm.out)
  }
  ##weighted penalized SVM
  psvmw.Pred <- function(xtrain,ytrain,xtest,ytest){
    ytrain.psvm = as.matrix((ytrain==1)*2-1)
    ytest.psvm = as.matrix((ytest==1)*2-1)
    cvglm = cv.glmnet(x=xtrain.w, y=ytrain,family="multinomial",alpha = 0.8,standardize =F)
    psvmw.out = svm.fs(x=xtrain.w, y = ytrain.psvm, fs.method = "scad", scale = F,lambda1.set = cvglm$lambda.1se)
    pred.psvm.w = as.matrix(predict(psvmw.out,newdata=xtest.w)$pred.class)
    pred.psvm.w = (pred.psvm.w==1)*1
    tt=table(pred.psvm.w,y=ytest)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=psvmw.out)
  }

  #PCA-LDA
  PCA_LDA.Pred <- function(xtrain,ytrain,xtest,ytest){
    pca <- prcomp(xtrain,retx = TRUE,center = TRUE,scale. = FALSE)
    lda.out <- lda(ytrain~pca$x[,1:3])
    lda.pred <- predict(lda.out)$class
    tt=table(lda.pred,y=ytrain)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=lda.out)
  }

  # CART
  CART.Pred <- function(xtrain,ytrain,xtest,ytest){
    tree = rpart(ytrain ~ ., data = as.data.frame(xtrain), control = rpart.control(cp = 0.0001))
    #printcp(tree)
    bestcp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
    # Prune the tree using the best cp.
    CART.out = prune(tree, cp = bestcp)
    tt = table(ytrain, predict(CART.out,type="class"))
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=CART.out)
  }

  # ADA boosting
  ADA.Pred <- function(xtrain,ytrain,xtest,ytest){
    ADA.out = ada(ytrain~.,data=as.data.frame(xtrain),iter=20,nu=1,type="discrete")
    ##add testing data set
    ADA.pred = addtest(ADA.out,xtest,ytest)
    tt = table(ADA.pred$fit,y=ytrain)
    list(error = (sum(tt)-sum(diag(tt)))/sum(tt),obj=ADA.out)
  }

  # ANN
  ANN.Pred <- function(xtrain,ytrain,xtest,ytest){
    ANN.out = nnet(ytrain ~ ., data = xtrain,size = 2, rang = 0.1,
                    decay = 5e-4, maxit = 200)
    ANN.pred = predict(ANN.out,xtest,type = "class")
    tt = table(ANN.pred,y=ytest)
    list(error = (sum(tt)-sum(diag(tt)))/sum(tt),obj=ANN.out)
  }


  #call the function:
  errors <- list()
  for (i in 1:length(method_list)){
     errors[i]<- get(paste(method_list[i],"Pred",sep="."))(xtrain,ytrain,xtest,ytest)$error
  }

  best.method <- which.min(errors)
  best <- get(paste(method_list[best.method],"Pred",sep="."))(xtrain,ytrain,xtest,ytest)
  performance <- as.data.frame(cbind(method_list,as.matrix(errors)))
  colnames(performance) <- c("method","misclassifcation_rate")


  #**************************************************************************************************
  # 2. Compares the performance of these methods Using cross validation
  cvfold=10
  folds <- cvFolds(NROW(x), K=cvfold)
  y.cv = as.matrix(as.factor(loan_random$RESPONSE))
  x.cv = as.matrix(loan_random[,-c(1,10)])
  w <- sqrt(as.matrix(apply(x.cv,2,var)))
  x.cv.w <- t(apply(x.cv,1,function(x)x/w))

  #svm
  svm.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    svm.cv.out <- svm(factor(ytrain.cv)~.,data=xtrain.cv,scale=F) #Get your new linear model (just fit on the train data)
    svm.cv.pred <- predict(svm.cv.out,newdata=xtest.cv) #Get the predicitons for the validation set (from the model just fit on the train data)
    tt = table(svm.cv.pred,ytest.cv)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=svm.cv.out)
  }


  #glmnet
  glmnet.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    cvglm = cv.glmnet(x=xtrain.cv, y=ytrain.cv,family="multinomial",alpha = 0.8,standardize =F)
    glmnet.cv.out = glmnet(x=xtrain.cv, y=ytrain.cv,family="multinomial",alpha = 0.8,lambda=cvglm$lambda.1se,standardize =F)
    glmpred = predict(glmnet.cv.out,newx=xtest.cv,s=cvglm$lambda.1se,type="class")
    tt = table(glmpred,y=ytest.cv)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=glmnet.cv.out)
  }

  #random forest
  rf.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    rf.cv.out = randomForest(x=xtrain.cv,y=ytrain.cv)
    rfpred = predict(rf.cv.out,newdata=xtest.cv)
    tt=table(rfpred,y=ytest.cv)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=rf.cv.out)
  }

  ##weighted, penalized methods

  #weighted svm
  svmw.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    svmw.cv.out = svm(ytrain.cv~., data=xtrain.cv.w,scale=F)
    svm.w.pred = predict(svmw.cv.out,newdata=xtest.cv.w)
    tt=table(predict=svm.w.pred, y=ytest.cv)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=svmw.cv.out)
  }

  #weighted glmnet
  glmnetw.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    cvglm.w = cv.glmnet(x=xtrain.cv.w, y=ytrain.cv,family="multinomial",alpha = 0.8,standardize =F)
    glmnetw.cv.out = glmnet(x=xtrain.cv.w, y=ytrain.cv,family="multinomial",alpha = 0.8,lambda=cvglm.w$lambda.1se,standardize =F)
    glm.w.pred = predict(glmnetw.cv.out,newx=xtest.cv.w,s=cvglm.w$lambda.1se,type="class")
    tt = table(glm.w.pred,y=ytest.cv)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=glmnetw.cv.out)
  }

  #weighted random forest
  rfw.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    rfw.cv.out = randomForest(x=xtrain.cv.w,y=ytrain.cv)
    rf.w.pred = predict(rfw.cv.out,newdata=xtest.cv.w)
    tt=table(rf.w.pred,y=ytest.cv)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=rfw.cv.out)
  }

  ##Penalized SVM
  psvm.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    ytrain.psvm = as.matrix((ytrain.cv==1)*2-1)
    ytest.psvm = as.matrix((ytest.cv==1)*2-1)
    cvglm = cv.glmnet(x=xtrain.cv, y=ytrain.cv,family="multinomial",alpha = 0.8,standardize =F)
    psvm.out = svm.fs(x=xtrain.cv, y = ytrain.psvm, fs.method = "scad", scale = F,lambda1.set = cvglm$lambda.1se)
    pred.psvm = as.matrix(predict(psvm.out,newdata=xtest.cv)$pred.class)
    pred.psvm = (pred.psvm==1)*1
    tt= table(pred.psvm,y=ytest.cv)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=psvm.out)
  }

  ##weighted penalized SVM
  psvmw.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    ytrain.psvm = as.matrix((ytrain.cv==1)*2-1)
    ytest.psvm = as.matrix((ytest.cv==1)*2-1)
    cvglm = cv.glmnet(x=xtrain.cv.w, y=ytrain.cv,family="multinomial",alpha = 0.8,standardize =F)
    psvmw.out = svm.fs(x=xtrain.cv.w, y = ytrain.psvm, fs.method = "scad", scale = F,lambda1.set = cvglm$lambda.1se)
    pred.psvm.w = as.matrix(predict(psvmw.out,newdata=xtest.cv.w)$pred.class)
    pred.psvm.w = (pred.psvm.w==1)*1
    tt=table(pred.psvm.w,y=ytest.cv)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=psvmw.out)
  }

  #PCA-LDA
  PCA_LDA.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    pca <- prcomp(xtrain.cv,retx = TRUE,center = TRUE,scale. = FALSE)
    lda.out <- lda(ytrain.cv~pca$x[,1:3])
    lda.pred <- predict(lda.out)$class
    tt=table(lda.pred,y=ytrain.cv)
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=lda.out)
  }

  # CART
  CART.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    tree = rpart(ytrain.cv ~ ., data = as.data.frame(xtrain.cv), control = rpart.control(cp = 0.0001))
    #printcp(tree)
    bestcp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
    # Prune the tree using the best cp.
    CART.out = prune(tree, cp = bestcp)
    tt = table(ytrain.cv, predict(CART.out,type="class"))
    list(error=(sum(tt)-sum(diag(tt)))/sum(tt),obj=CART.out)
  }

  # ADA boosting
  ADA.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    ada.out = ada(ytrain.cv~.,data=as.data.frame(xtrain.cv),iter=20,nu=1,type="discrete")
    ##add testing data set
    ada.pred = addtest(ada.out,xtest.cv,ytest.cv)
    tt = table(ada.pred$fit,y=ytrain.cv)
    list(error = (sum(tt)-sum(diag(tt)))/sum(tt),obj=ada.out)
  }

  # ANN
  ANN.Pred.cv <- function(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv){
    ANN.out = nnet(ytrain.cv ~ ., data = xtrain.cv,size = 2, rang = 0.1,
                   decay = 5e-4, maxit = 200)
    ANN.pred = predict(ANN.out,xtest.cv,type = "class")
    tt = table(ANN.pred,y=ytest.cv)
    list(error = (sum(tt)-sum(diag(tt)))/sum(tt),obj=ANN.out)
  }


  errors.cv = matrix(nrow = cvfold, ncol = length(method_list))
  #method_list = c("svm","glmnet","rf","svmw","glmnetw","rfw","psvm","psvmw","PCA_LDA","CART","ADA","ANN")

  for (i in 1:cvfold){
    xtrain.cv <- x.cv[folds$subsets[folds$which != i], ]
    xtest.cv <- x.cv[folds$subsets[folds$which == i], ]
    ytrain.cv <- factor(y.cv[folds$subsets[folds$which != i], ] )
    ytest.cv <- factor(y.cv[folds$subsets[folds$which == i], ] )
    xtrain.cv.w <- x.cv.w[folds$subsets[folds$which != i], ]
    xtest.cv.w <- x.cv.w[folds$subsets[folds$which == i], ]

    for (j in 1:length(method_list)){
      errors.cv[i,j]<- get(paste(method_list[j],"Pred.cv",sep="."))(xtrain.cv,ytrain.cv,xtest.cv,ytest.cv)$error
    }
  }

  ave.errors.cv = colMeans(errors.cv)
  best.method.idx <- which.min(ave.errors.cv)
  best.method.cv <- method_list[best.method.idx]
  performance.cv <- as.matrix(ave.errors.cv)
  performance.cv <- as.data.frame(cbind(method_list,performance.cv))
  colnames(performance.cv) <- c("method","misclassifcation_rate")
  result <- list(performance_table_TT=performance,best_method=best,performance_table_CV=performance.cv,best.method.cv=best.method.cv)
  return(result)
}





