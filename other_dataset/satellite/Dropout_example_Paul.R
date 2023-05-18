library(pracma)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tidyverse)
library(matrixcalc)
library(truncnorm)
library(mvtnorm)
library(caret)
library(ggplot2)
library(reshape2)
library(pROC)
library(gstat)
library(tidyverse)
library(vegan)
library(TSA)
library(kernlab)
library(gridExtra)



## Prep data
google <- read_csv('trendsClean.csv')
dataMat <- as.matrix(google[,-c(1:2)])
tStep <- ncol(dataMat)

Validation <- Calibration <- SSVS_Cal <- data.frame(Drop_Rate=seq(0.02, 0.1, by=0.02), Acc=NA, AUC=NA)


Y <- as.numeric(google$Type == 'Actor')
compBspec <- array(NA, dim=c(tStep,tStep,nrow(dataMat)))
for(i in 1:nrow(dataMat)){
  compBspec[,,i] <- bispecd(as.numeric(dataMat[i,]), tStep, 3, tStep)$bspec ### Compute Bispectrum
  print(i)
}
compBspec <- log(abs(compBspec))
compBspec <- aperm(compBspec, dim=c(3,1,2))
compBspecNorm <- apply(compBspec, c(2,3), scale)


Xdat <- array(NA, dim=c(200,52,52,1))
Xdat[,,,1] <- compBspecNorm



tauTune <- 0.1 ## For SSVS
cTune <- 10
piTune <- 0.5

func1 <- function(x){
  periodogram(x,plot=FALSE)$spec
}
compSpec <- t(apply(dataMat,1,func1))
eofSpec <- EOF(compSpec)
EQXspec <- compSpec %*% eofSpec$e[,1:20]
EQXspec <- cbind(1,scale(EQXspec))
eqdatSpec <- data.frame(Y=Y, X=I(EQXspec))


library(keras)
use_session_with_seed(100)
for(d in 1:nrow(Validation)){
  reps <- 50
  K <- 20
  Out <- rep(NA,reps)
  truthV <- c()
  predsV <- c()
  truthC <- c()
  predsC <- c()
  predsCssvs <- c()
  for(i in 1:reps){
    Ind <- sample(200, K)
    
    ## Fit BCNN first
    drop1 <- layer_dropout(rate=Validation$Drop_Rate[d])
    
    
    inputs <- layer_input(shape=c(52, 52, 1)) 
    
    
    outputs <- inputs %>%
      layer_conv_2d(filters = 32, kernel_size = c(3, 3),  activation = "relu",
                    input_shape = c(52, 52, 1)) %>%
      layer_max_pooling_2d(pool_size = c(3, 3)) %>%
      layer_batch_normalization() %>%
      drop1(training=T) %>%
      layer_conv_2d(filters = 32, kernel_size = c(5, 5),  activation = "relu") %>%
      layer_max_pooling_2d(pool_size = c(4, 4)) %>%
      layer_batch_normalization() %>%
      layer_flatten() %>%
      layer_dense(units=8, activation = 'relu') %>%
      drop1(training=T) %>%
      layer_dense(units=1, activation = 'sigmoid')
    
    model <- keras_model(inputs, outputs)
    
    model %>% compile(
      optimizer_adam(lr=0.001),
      loss = "binary_crossentropy",
      metrics = c("accuracy")
    )
    model %>% fit(
      Xdat[-Ind,,,,drop=F], Y[-Ind],
      epochs = 10, batch_size=8
    )
    
    postPred <- matrix(NA, nrow=K, ncol=100)
    for(j in 1:100){
      postPred[,j] <- as.numeric(predict(model, Xdat[Ind,,,,drop=F]))
    }
    predsMod <- apply(postPred,1,mean)
    ansV <- Y[Ind[1:10]]
    ansC <- Y[Ind[-c(1:10)]]
    truthV <- c(truthV, ansV)
    truthC <- c(truthC, ansC)
    predsV <- c(predsV, predsMod[1:10])
    predsC <- c(predsC, predsMod[-c(1:10)])
    
    
    ## Fit SSVS
    train <- eqdatSpec[-Ind,]
    ssvsMod <- ssvs(eqdatSpec[-Ind,], 2000, predX=eqdatSpec[Ind,]$X,0)
    predsCssvs <- c(predsCssvs, apply(ssvsMod$pred[-c(1:1000), ], 2, mean)[-c(1:10)])
    
    print(paste(i, d))
    print(paste0("Val: ",mean((predsV > 0.5) == truthV), 
                 " Cal: ",mean((predsC > 0.5) == truthC), 
                 " SSVS: ", mean((predsCssvs > 0.5) == truthC) ))
  }
  Validation$Acc[d] <- mean((predsV > 0.5) == truthV)
  Validation$AUC[d] <- plot.roc(truthV, predsV)$auc
  Calibration$Acc[d] <- mean((predsC > 0.5) == truthC)
  Calibration$AUC[d] <- plot.roc(truthC, predsC)$auc
  SSVS_Cal$Acc[d] <- mean((predsCssvs > 0.5) == truthC)
  SSVS_Cal$AUC[d] <- plot.roc(truthC, predsCssvs)$auc
}

