# set things up
rm(list=ls(all=TRUE))
library(glmnet); library(ggplot2)
# making things repeatable
set.seed(1)
# construct the data set containing the function we want to estimate
fnConstructTrueFunction <- function(intNumPoints, fltSD) {
  # this constructs the function of "truth" Which is:
  # a polynomial is fitted to dfData. This model is used to predict a 
  # vector of values. Some noise is added to the data. It is fuzzed
  dfData <- data.frame(x = c(0, 20, 35, 50), y = c(11, 18, 8, 20))
  # fit a polynomial regression to the dataset above. Degree of 
  # 3 is the maximum possible given 4 data points.
  trueModel <- lm(y~poly(x,3), data = dfData)
  # construct a vector of xValues dfTrueValue contains 'continuous' data
  dfTrueModel <- data.frame(x  = seq(0, 50, length.out=intNumPoints))
  # this constructs a 'continuous' function
  dfTrueModel$yTrue <- predict.lm(trueModel, newdata = dfTrueModel)
  # fuzz up some values. It is from these that we will draw a sample
  dfTrueModel$yNoisy <- sapply(dfTrueModel$y, function(x) {rnorm(1, x, fltSD)})
  # return the data frame
  return(dfTrueModel)
}
# helper function to calculate MSE
fnMSE <- function(vct1, vct2) {
  # take two vectors and calcuate the MSE
  return(mean((vct1 - vct2)^2))
}
# using R's plot() function to show different curves for different lambdas
fnPlotUnderOverFit <- function(lclDfSample, lclDfTruth, lclRidgeModel, lclMatxAll) {
  # This plots sample data, truth function and 7 under / over fitted
  # models for different values of lambda. The 4th curve is the 
  # model which best fits the data.  The values of lambda (sVals)
  # are hard coded.
  # all plots on one page 
  par(mfrow=c(1,1)) 
  plot.new()
  # this clips viewing area.  Do not want to see weird polynomial tails
  clipX <- c(4, 45)
  yRange <-  range(lclDfSample[vctTrain, "yNoisy"])
  par(bg = 'black')
  # plot the Truth function.  "l' is for line
  plot(lclDfTruth$x, lclDfTruth$yTrue, ylim = yRange, xlim = clipX, col = "grey", 
       typ = 'l', xlab = "", ylab = ""); par(new=T)
  # plot the sample data
  plot(lclDfSample[vctTrain, "x"], lclDfSample[vctTrain, "yNoisy"] , ylim = yRange, xlim = clipX,  
       axes = F, col = "grey", xlab = "",  ylab = "",  pch=16) ; par(new=T)
  
  numPlots = 7
  # these are the lambda values
  sVals <- c(0, 0.5, 1, 1.335, 2, 3, 5)
  colVals <- c("#cb181d", "#fb6a4a", "#fcae91", "#ffffff", 
               "#bdc9e1", "#74a9cf", "#0570b0")
  lineWidth <- 3
  # the following plots a series of 7 curves with different values of lambda
  for (i in 1:numPlots) {
    label <- paste("ridgeFit", i, sep = "")
    # create predictions
    dfTruth$label <- as.vector(predict(lclRidgeModel, s=sVals[i], newx= lclMatxAll, exact = T))
    # plot predictions
    plot(dfTruth$x, dfTruth$label, ylim = yRange, xlim = clipX, col = colVals[i],
         axes = F, typ = 'l', xlab = "", ylab = "" , lwd = lineWidth)
    # do not erase plot
    par(new=T)   
  } # for
} # function
# GGPLOT to plot the test and training
fnPlotTestTrain <- function (lclRidgeModel) {
  # This functions constructs the data and then plots the results for 
  # test vs train plot.  Different values of lambda are along the x axis
  # and MSE for test and train are on the y axis
  
  # an odd number of observations such that it will contain a midpoint
  numObs <- 301
  # 3.14 (hard coded) is the smallest lambda for the test set
  lambdaVals <- seq(3.143261 * 2, 0, length = numObs)
  # construct a data frame containing the data. Add x (i.e. lambda)
  dfTestTraining <- data.frame(x = lambdaVals)
  # empty vectors
  vctTestMSE <- rep(NA, numObs)
  vctTrainMSE <- rep(NA, numObs)
  for (i in 1:numObs) {
    # Cycle through lambdas and create test and training predictions
    # calculate MSE for train and test and then add results to the vectors
    lambda <- dfTestTraining[i, "x"]
    print(lambda)
    # run the ridgeModel ( trained using training data ) on test data
    ridgePredictTest <- predict(lclRidgeModel, s=lambda, newx= matX[vctTest,], exact = T)
    ridgePredictTrain <- predict(lclRidgeModel, s=lambda, newx= matX[vctTrain,], exact = T)
    # calculate MSE and then add this to current vector position
    vctTestMSE[i] <- fnMSE(ridgePredictTest, dfSample[vctTest, "yNoisy"])
    vctTrainMSE[i] <- fnMSE(ridgePredictTrain, dfSample[vctTrain, "yNoisy"])
  }
  # add the vectors to the data frame
  dfTestTraining$testMSE <- vctTestMSE
  dfTestTraining$trainMSE <- vctTrainMSE
  # crank up GGPLOT
  plot <- ggplot(dfTestTraining, aes(x))
  plot <- plot + geom_line(aes(y = testMSE), colour = '#FF0000', size = 1/2)
  plot <- plot + geom_line(aes(y = trainMSE), colour = '#00CC00', size = 1/2)
  # change the order of the x axis. Want descending order
  plot <- plot + scale_x_reverse()
  # voila !!
  plot
}
#============== Set up data sets =======================
# sampleLength is taken from trueLength. sdNoise is the
# number of standard deviations to add some fuzz to the sample
trueLength <- 200; sdNoise <- 4; sampleLength <- 80
# quite a squiggly polynomial
polyDegree <- 18
# we take a sample of 'sampleLength' and then divide this
# into test and training sets
perCentTrain = 0.5
# create a data frame containing the true function
dfTruth <- fnConstructTrueFunction(trueLength, sdNoise)
# draw a sample from dfTruth
dfSample <- dfTruth[sample(1:trueLength, sampleLength),]
# we have a sample, now partition this into test and training 
vctTrain <- sample(1:sampleLength, sampleLength * perCentTrain)
vctTest <- (-vctTrain)

#============== Build the models with GLMNET============
# GLMNet does not use the R formula so a feature matrix is needed.
# We don't need to the intercept. get rid of it i.e. [,-1]
# nrow(dfSample) x polyDegree matrix
matX <- model.matrix(yNoisy ~ poly(x, polyDegree), dfSample)[,-1]
# nrow(dfTruth) x polyDegree matrix
matxAll <- model.matrix(yNoisy ~ poly(x, polyDegree), dfTruth)[,-1]
# train our model. alpha = 0 ==> ridge; alpha = 1 ==> lasso
ridgeModel <- glmnet(matX[vctTrain,] , dfSample[vctTrain, "yNoisy"], alpha = 0)
# these two are not really needed. Used in development to get best lambda values
# cv.glmnet uses 10 fold x validation by default.
cv.out <- cv.glmnet(matX[vctTrain,] , dfSample[vctTrain, "yNoisy"], alpha = 0)
bestLamda <- cv.out$lambda.min
# plot(cv.out) looks pretty.
#============== Run the visualisations===========
#fnPlotUnderOverFit(dfSample, dfTruth,ridgeModel, matxAll)
fnPlotTestTrain(ridgeModel)



