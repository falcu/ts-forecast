#Libraries
#install.packages('forecast')
#install.packages('urca')
#install.packages("D:/Guido/Master Finanzas/2018/Segundo Trimestre/SeriesDeTiempo/ecm_4.3.0.zip", repos = NULL, type = "source")
#install.packages("zoo")
#install.packages("xlsx")
#install.packages("tseries")
#install.packages("quantmod")
#install.packages("lubridate")
#install.packages("vars")
#install.packages("CADFtest")
#install.packages("tsDyn")
#install.packages("Metrics")
#install.packages("MLmetrics")


library(forecast)
library(ecm)
library(urca)
library(zoo)
library(xlsx)
library(tseries)
library(quantmod)
library(lubridate)
library(lmtest)
library(vars)
library(CADFtest)
library(tsDyn)
library(Metrics)
library(MLmetrics)

#Configurable Variables
DATA_FILE <- "D:/Guido/Master Finanzas/2018/Segundo Trimestre/SeriesDeTiempo/TP Final/Data/Data.xlsx"
data <- read.xlsx(DATA_FILE,1)
V_INTEREST <- data$GS2
V_INTEREST_NAME <- 'GS2'
V_INTEREST_TYPE <- 'IR'  #Use for plot on y axis
YLIM <- c(0,7)
LEGENDPOS <- c(2003,7.5)

#Primitive functions
toTs <- function(rawData,from,to,frequency){
  theTs <- ts( rawData, start=from, end=to,frequency = frequency)
  return(theTs)
}

datePlusMonthNumeric <- function( aDate, m ){
  aDate <- as.Date(aDate)
  month(aDate) <- month(aDate) + m
  return(toNumericYearMonth(aDate))
}

createTSDummy <- function(dateTS,fromDate,toDate,breakFrom,breakTo, frequency){
  #Use of window causes issues
  timeIndex <- as.Date(dateTS)
  beforeBreak <-  c(createBoolTSDummy(fromDate,breakFrom,frequency = frequency ))
  beforeBreak <-  beforeBreak[1:length(beforeBreak)-1]
  inBreak <- c(createBoolTSDummy(breakFrom,breakTo,booleanValue = TRUE,frequency = frequency))
  afterBreak <-  c(createBoolTSDummy(breakTo,toDate,frequency = frequency))
  afterBreak <-  afterBreak[2:length(afterBreak)]
  dummy <- ts(c(beforeBreak,inBreak,afterBreak),start=fromDate,end=toDate,frequency = frequency)
  
  dummy
}

createBoolTSDummy <- function(fromDate,toDate,size=10000,frequency=12,booleanValue=FALSE){
  return(ts(createDummyArray(size,booleanValue),start=fromDate,end=toDate,frequency = frequency))
}

createDummyArray<- function(dLength, booleanValue=FALSE){
  value <- as.integer(booleanValue)
  rep(c(value),dLength)
}

toNumericYearMonth <- function(aDate){
  return(c(year(aDate),month(aDate)))
}

#GLOBAL CONSTANTS. Dates/other variables of interest
fromInSampleStr <- data$Date[1]
toInSampleStr <- "2015-12-01"
fromInSample <- toNumericYearMonth(fromInSampleStr )
toInSample <- toNumericYearMonth(toInSampleStr)
fromOutOfSample <- datePlusMonthNumeric(toInSampleStr,1)
toOutOfSample <- toNumericYearMonth(data$Date[length(data$Date)])
outOfSamplePeriods <- 32
fromInSamplePlus1 <- datePlusMonthNumeric(fromInSampleStr,1)
fromInSamplePlus2 <- datePlusMonthNumeric(fromInSampleStr,2)
fromInSamplePlus3 <- datePlusMonthNumeric(fromInSampleStr,3)
toInSampleMinus1 <- datePlusMonthNumeric(toInSampleStr,-1)
toInSampleMinus2 <- datePlusMonthNumeric(toInSampleStr,-2)
toInSampleMinus3 <- datePlusMonthNumeric(toInSampleStr,-3)

crisisSubprimeFrom <- toNumericYearMonth("2008-01-01")
crisisSubprimeTo <- toNumericYearMonth("2009-12-01")

#GLOBAL CONSTANTS. Frequency
FREQ <- 12

#GLOBAL CONSTANTS. Variables of interest
DATES <- data$Date
FED <- toTs( data$FEDFUND, from=fromInSample, to=toOutOfSample,frequency = FREQ)
UNEMPLOY <- toTs( data$UNEMPLOY, from=fromInSample, to=toOutOfSample,frequency = FREQ)
CPI <- toTs( data$CPI, from=fromInSample, to=toOutOfSample,frequency = FREQ)
GSPC <- toTs( data$X.GSPC, from=fromInSample, to=toOutOfSample,frequency = FREQ)
GS2 <- toTs( data$GS2, from=fromInSample, to=toOutOfSample,frequency = FREQ)
GS10 <- toTs( data$GS10, from=fromInSample, to=toOutOfSample,frequency = FREQ)
V_INTEREST <- toTs( V_INTEREST, from=fromInSample, to=toOutOfSample,frequency = FREQ)
#ANALYSIS CONFIGURATION

#END ANALYSIS CONFIGURATION
Y <- cbind(FED,UNEMPLOY,CPI,GSPC,GS2,GS10)
plot(Y)

FED_IN <- window(FED, start=fromInSample, end=toInSample)
UNEMPLOY_IN <- window(UNEMPLOY, start=fromInSample, end=toInSample)
CPI_IN <- window(CPI, start=fromInSample, end=toInSample)
GSPC_IN <- window(GSPC, start=fromInSample, end=toInSample)
GS2_IN <- window(GS2, start=fromInSample, end=toInSample)
GS10_IN <- window(GS10, start=fromInSample, end=toInSample)
V_INTEREST_IN <- window(V_INTEREST, start=fromInSample, end=toInSample)

Y_IN <- cbind(FED_IN,UNEMPLOY_IN,CPI_IN,GSPC_IN,GS2_IN,GS10_IN)
plot(Y_IN)

FED_OUT <- window(FED, start=fromOutOfSample, end=toOutOfSample)
UNEMPLOY_OUT <- window(UNEMPLOY, start=fromOutOfSample, end=toOutOfSample)
CPI_OUT <- window(CPI, start=fromOutOfSample, end=toOutOfSample)
GSPC_OUT <- window(GSPC, start=fromOutOfSample, end=toOutOfSample)
GS2_OUT <- window(GS2, start=fromOutOfSample, end=toOutOfSample)
GS10_OUT <- window(GS10, start=fromOutOfSample, end=toOutOfSample)
V_INTEREST_OUT <- window(V_INTEREST, start=fromOutOfSample, end=toOutOfSample)

Y_OUT <- cbind(UNEMPLOY_OUT,CPI_OUT,GSPC_OUT,FED_OUT,GS2_OUT,GS10_OUT)
plot(Y_OUT)

par(mar=c(1,1,1,1), mfrow=c(1,1) )

#Subprime Crisis dummy
D2008Crisis <- createTSDummy(DATES, fromInSample, toOutOfSample, crisisSubprimeFrom, crisisSubprimeTo, FREQ)
D2008Crisis_IN <- createTSDummy(DATES, fromInSample, toInSample, crisisSubprimeFrom, crisisSubprimeTo, FREQ)
D2008Crisis_OUT <- createBoolTSDummy(fromOutOfSample, toOutOfSample, frequency = FREQ, booleanValue = FALSE )
D2008CrisisForDiff_IN <- createTSDummy(DATES, fromInSamplePlus1, toInSample, crisisSubprimeFrom, crisisSubprimeTo, FREQ)

#Helper Functions
findARMA <- function( theTS, ar=5, ma=5, boxTestLimit=0.22, boxTestLag=12, xreg=NULL ){
  
  armaModel <- NULL
  lastArmaFound <- arima(theTS,order=c(ar,0,ma))
  for(arComponent in ar:0){
    for(maComponent in ma:0){
      armaModel <- arima(theTS,order=c(arComponent,0,maComponent),xreg=xreg)
      passedBoxTest <- TRUE
      for(lag in 1:boxTestLag){
        bTest <- Box.test(armaModel$residuals,lag)
        if(bTest$p.value <= boxTestLimit){
          passedBoxTest <- FALSE
          break()
        }
      }
      aicValue <- AIC(armaModel)
      print(paste('AIC',AIC(armaModel),sep=':'))
      print(paste(c('P', 'Q', 'D'),c(arimaorder(armaModel)),sep=':'))
      if(passedBoxTest){
        print("Model has passed Box Test")
        #Arbitrary criteria for smaller model
        if( aicValue< AIC(lastArmaFound) )
        {
          print("Model has lowest AIC so far")
          lastArmaFound <- armaModel
        }
      }
      else{
        print("Model has NOT passed Box Test")
      }
      print("-------------------------------------------------")
    }
  }
  print('Selected')
  print(paste(c('P', 'Q', 'D'),c(arimaorder(lastArmaFound)),sep=':'))
  return(lastArmaFound)
}

DFTest <- function( matrix ){
  columnNames <- colnames(matrix)
  print("Dickey-Fullter test P-value output:")
  options(warn=-1)
  for(col in columnNames){
    testResult <- adf.test(na.omit(matrix[,col]))
    print(paste(c(col,"P value: ",testResult$p.value),collapse=" "))
  }
  options(warn=0)
}


doARMAForecastStatic <- function(order,inSampleTS,outOfSampleTS,fromInSample, toInSample, fromOutOfSample, toOutOfSample, frequency=12, tsName='', labels=c('',''), legendPos=c(0,0)
                                   ,ylim=c(0,100)){
  forecastedValues <- NULL
  lower95 <- NULL
  upper95 <- NULL
  outWindow <- length(outOfSampleTS)
  #model computed only once
  armaModel <- arima(inSampleTS,order)
  theTS <- c(inSampleTS)
  for (i in 1:outWindow){
    theTS <- c(inSampleTS,outOfSampleTS[0:(i-1)]) #add new known value
    fcastTS <- forecast(theTS, model=armaModel)
    forecastedValues <- c(forecastedValues,fcastTS$mean[1])
    lower95 <- c(lower95,fcastTS$lower[,'95%'][1])
    upper95 <- c(upper95,fcastTS$upper[,'95%'][1])
  }
  
  fullSeries <- c(inSampleTS,outOfSampleTS)
  observedTS <- toTs( fullSeries, from=fromInSample, to=toOutOfSample,frequency = frequency)
  forecastedTS <- toTs( forecastedValues, from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  lower95TS <- toTs( lower95, from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  upper95TS <- toTs( upper95, from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  
  obsAndForecasted <- cbind(observedTS,forecastedTS)
  colnames(obsAndForecasted)[1] <- paste( 'Actual', tsName )
  colnames(obsAndForecasted)[2] <- paste( 'Forecasted', tsName )
  ic <- cbind(lower95TS,upper95TS)
  colnames(ic)[1] <- 'Lower 95%'
  colnames(ic)[2] <- 'Upper 95%'
  
  main='ARMA(Static)'
  doShowMetric(outOfSampleTS,forecastedTS,title=main)
  doForecastPlot(obsAndForecasted,ic,labels=labels,ylim=ylim,legendPos=legendPos,main=main)
  
  return(forecastedTS)
}


doARMAForecastRecursive <- function(order,inSampleTS,outOfSampleTS,fromInSample, toInSample, fromOutOfSample, toOutOfSample, xregIn=NULL, xregOut=NULL, frequency=12, tsName='', labels=c('',''), legendPos=c(0,0)
                                  ,ylim=c(0,100)){
  forecastedValues <- NULL
  lower95 <- NULL
  upper95 <- NULL
  inWindow <- length(inSampleTS)
  outWindow <- length(outOfSampleTS)
  theTS <- c(inSampleTS)
  for (i in 1:outWindow){
    theTS <- c(inSampleTS[i:inWindow],outOfSampleTS[0:(i-1)]) #Roll 1 step
    dummyInFunc <- data.frame(xregIn)
    #Unfortunately forecast searches in global env
    assign("dummyInFunc",dummyInFunc, envir=.GlobalEnv)
    armaModel <- arima(theTS,order,xreg=dummyInFunc)
    dummyOutFunc <- data.frame(xregOut)
    assign("dummyOutFunc",dummyOutFunc, envir=.GlobalEnv)
    fcastTS <- forecast(armaModel, xreg=dummyOutFunc)
    forecastedValues <- c(forecastedValues,fcastTS$mean[1])
    lower95 <- c(lower95,fcastTS$lower[,'95%'][1])
    upper95 <- c(upper95,fcastTS$upper[,'95%'][1])
    xregIn  <- c(xregIn[2:inWindow],0)
  }
  
  fullSeries <- c(inSampleTS,outOfSampleTS)
  observedTS <- toTs( fullSeries, from=fromInSample, to=toOutOfSample,frequency = frequency)
  forecastedTS <- toTs( forecastedValues, from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  lower95TS <- toTs( lower95, from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  upper95TS <- toTs( upper95, from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  
  obsAndForecasted <- cbind(observedTS,forecastedTS)
  colnames(obsAndForecasted)[1] <- paste( 'Actual', tsName )
  colnames(obsAndForecasted)[2] <- paste( 'Forecasted', tsName )
  ic <- cbind(lower95TS,upper95TS)
  colnames(ic)[1] <- 'Lower 95%'
  colnames(ic)[2] <- 'Upper 95%'
  
  main='ARMA(Recursive)'
  doShowMetric(outOfSampleTS,forecastedTS,title=main)
  doForecastPlot(obsAndForecasted,ic,labels=labels,ylim=ylim,legendPos=legendPos,main=main)
  
  return(forecastedTS)
  
}

doVECForecastRecursive<- function(inSampleTS,outOfSampleTS, K, vName, fromOutOfSample, toOutOfSample, tsName=NULL, frequency=12, dummyIn=NULL,legendPos=c(0,0),
                               labels=c('',''), ylim=c(0,100)){
  
  if(missing(tsName)){
    tsName <- vName
  }
  
  forecastedValues <- NULL
  lower95 <- NULL
  upper95 <- NULL
  inWindow <- nrow(inSampleTS)
  outWindow <- nrow(outOfSampleTS)
  theTS <- inSampleTS
  fcstIndex <- paste(vName,'.fcst', sep='')
  lowerIndex<- paste(vName,'.lower', sep='')
  upperIndex<- paste(vName,'.upper', sep='')
  for (i in 1:outWindow){
    theTS <- rbind(inSampleTS,outOfSampleTS[0:(i-1),]) #Merge frames and roll 1
    #Compute model
    vec_ur <- ca.jo(theTS, K=K, ecdet = "trend", dumvar=dummyIn)
    #Convert to var to use predict
    var_ur <- vec2var(vec_ur)
    VECForecast <- predict(var_ur,n.ahead=1, dumvar=cbind(dummy=c(0)))
    newForecasted <- data.frame(VECForecast$fcst)[,fcstIndex][1]
    newLower95 <- data.frame(VECForecast$fcst)[,lowerIndex][1]
    newUpper95 <- data.frame(VECForecast$fcst)[,upperIndex][1]
    forecastedValues <- c(forecastedValues,newForecasted)
    lower95 <- c(lower95,newLower95)
    upper95 <- c(upper95,newUpper95)
    dummyIn <- cbind(dummy=c(dummyIn,0))
  }
  currentOutOfSampleTS <- outOfSampleTS[tsName][,1]
  observedTS <- toTs( c(inSampleTS[tsName][,1],currentOutOfSampleTS) , from=fromInSample, to=toOutOfSample,frequency = frequency)
  forecastedTS <- toTs( forecastedValues , from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  lower95TS <- toTs( lower95 , from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  upper95TS <- toTs( upper95 , from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  obsAndForecasted <- cbind(observedTS,forecastedTS)
  colnames(obsAndForecasted)[1] <- paste( 'Actual', tsName )
  colnames(obsAndForecasted)[2] <- paste( 'Forecasted', tsName )
  ic <- cbind(lower95TS,upper95TS)
  colnames(ic)[1] <- 'Lower 95%'
  colnames(ic)[2] <- 'Upper 95%'
  
  main='VEC(Recursive)'
  doForecastPlot(obsAndForecasted,ic,labels=labels,ylim=ylim,legendPos=legendPos, main=main)
  doShowMetric(currentOutOfSampleTS,forecastedValues, title=main)
  
  return(forecastedTS)
}

doRandomWalkForecast<- function(inSampleTS,outOfSampleTS,fromInSample, toInSample, fromOutOfSample, toOutOfSample, xregIn=NULL, xregOut=NULL, frequency=12, tsName='', labels=c('',''), legendPos=c(0,0)
                                  ,ylim=c(0,100)){
  
  inWindow <- length(inSampleTS)
  outWindow <- length(outOfSampleTS)
  forecastedValues <- c(inSampleTS[inWindow])
  for (i in 1:(outWindow-1)){
    rwSD <- sd(c(inSampleTS[1:(inWindow-1)],forecastedValues))
    newValue <- outOfSampleTS[i]
    forecastedValues <- c(forecastedValues,newValue)
  }
  
  observedTS <- toTs( c(inSampleTS,outOfSampleTS) , from=fromInSample, to=toOutOfSample,frequency = frequency)
  forecastedTS <- toTs( forecastedValues , from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  obsAndForecasted <- cbind(observedTS,forecastedTS)
  colnames(obsAndForecasted)[1] <- paste( 'Actual', tsName )
  colnames(obsAndForecasted)[2] <- paste( 'Forecasted', tsName )
  
  main='Random Walk'
  doForecastPlot(obsAndForecasted, labels=labels,ylim=ylim,legendPos=legendPos, main=main)
  
  doShowMetric(outOfSampleTS,forecastedTS, title=main)
  
  return(forecastedTS)
}

doForecastPlot <- function(obsAndForecasted, ic=NULL, labels, ylim=c(0,100), legendPos=c(0,0), main=''){
  
  par(mar= c(5, 4, 4, 2) + 0.1, mfrow=c(1,1))
  plot(obsAndForecasted[,1],type = "l",lwd=1, col="black", xlab=labels[1],ylab=labels[2], ylim=ylim, main=main)
  lines(obsAndForecasted[,2], lwd=2, col="red")
  if(is.null(ic))
  {
    legendText = c(colnames(obsAndForecasted)[1] , colnames(obsAndForecasted)[2] )
    col <- c("black", "red")
  }
  else{
    icCol <- c(colnames(ic)[1],colnames(ic)[2])
    lines(ic[,1], lwd=2, col="mediumseagreen")
    lines(ic[,2], lwd=2, col="mediumseagreen")
    legendText = c(colnames(obsAndForecasted)[1] , colnames(obsAndForecasted)[2],icCol[1],icCol[2])
    col <- c("black", "red","mediumseagreen","mediumseagreen")
  }
  legend(legendPos[1], legendPos[2], legend=legendText,
         col=col, lty=1,bty = "n")
}

doNicePlot <- function(variables,labels=c('',''),ylim=c(0,100), legendPos=c(0,0), main='', colors=NULL, legendCols=NULL, verticalLine=NULL){
  defaultColors = c('black','green','red','blue','yellow')
  if(is.null(colors)){
    colors = defaultColors
  }
  if(is.null(legendCols)){
    legendCols = colnames(variables)
  }
  par(mar= c(5, 4, 4, 2) + 0.1, mfrow=c(1,1))
  plot(variables[,1],type = "l",lwd=1, col=colors[1], xlab=labels[1],ylab=labels[2], ylim=ylim, main=main)
  for(i in 2:ncol(variables)){
    lines(variables[,i], lwd=2, col=colors[i])
  }
  legend(legendPos[1], legendPos[2], legend=legendCols,
         col=colors, lwd=1,bty = "n")
  
  if(!is.null(verticalLine)){
    abline(v=verticalLine)
  }
}


doShowMetric <- function(actual,predicted,title=''){
  mapeResult <- paste("MAPE",round(MAPE(predicted,actual)*100,2), sep=": ")
  rmseResult <- paste("RMSE",round(rmse(actual,predicted)*100,2), sep=": ")
  print(title)
  print(mapeResult)
  print(rmseResult)
  print('--------------------------------------------')
}

#ANALYSIS

#Stationarity
DFTest(Y)
summary(ur.df(UNEMPLOY,type="trend",selectlags="BIC"))
summary(ur.df(CPI,type="trend",selectlags="BIC"))
summary(ur.df(GSPC,type="trend",selectlags="BIC"))
summary(ur.df(FED,type="trend",selectlags="BIC"))
summary(ur.df(GS2,type="trend",selectlags="BIC"))
summary(ur.df(GS10,type="trend",selectlags="BIC"))

#Diff
DIFF_UNEMPLOY <- diff(UNEMPLOY)
DIFF_CPI <- diff(CPI)
DIFF_GSPC <- diff(GSPC)
DIFF_FED <- diff(FED)
DIFF_GS2 <- diff(GS2)
DIFF_GS10 <- diff(GS10)
DIFF_V_INTEREST <- diff(V_INTEREST)

DIFF_V_INTEREST_IN <- diff(V_INTEREST_IN)

Y_DIFF <- cbind(DIFF_UNEMPLOY,DIFF_CPI,DIFF_GSPC,DIFF_FED,DIFF_GS2,DIFF_GS10)
plot(Y_DIFF)
#Stationarity
DFTest(Y_DIFF)
summary(ur.df(diffUNEMPLOY,type="trend",selectlags="AIC"))
summary(ur.df(diffCPI,type="trend",selectlags="AIC"))
summary(ur.df(diffGSPC,type="trend",selectlags="AIC"))
summary(ur.df(diffFED,type="trend",selectlags="AIC"))
summary(ur.df(diffGS2,type="trend",selectlags="AIC"))
summary(ur.df(diffGS10,type="trend",selectlags="AIC"))


summary(ca.jo(Y, type="trace", ecdet="trend", K=3, dumvar=cbind(dummy=c(D2008Crisis))))


#Analyze Variable of interest
print(paste('Variable of Interest',V_INTEREST_NAME))
plot(V_INTEREST, main = V_INTEREST_NAME)
summary(ur.df(V_INTEREST,type="trend",selectlags="BIC"))
plot(DIFF_V_INTEREST, main = paste('Diff',V_INTEREST_NAME))
summary(ur.df(DIFF_V_INTEREST,type="trend",selectlags="BIC"))
acf(DIFF_V_INTEREST, lag.max=100)
pacf(DIFF_V_INTEREST,lag.max=100)

armaModelVInterest <- findARMA(DIFF_V_INTEREST_IN,xreg=DIFF_V_INTEREST_IN*D2008CrisisForDiff_IN)
plot(armaModelVInterest$residuals, main='ARMA Residuals')
Box.test(armaModelFED$residuals)
autoArimaVInterest <- auto.arima(V_INTEREST_IN, seasonal = FALSE, xreg=data.frame(V_INTEREST_IN*D2008Crisis_IN))
ARMA_ORDER <- arimaorder(autoArimaVInterest)
print(paste(c("P", "D", "Q"),ARMA_ORDER))
autoArimaVInterest
plot(autoArimaVInterest$residuals, main=paste('Auto model for',V_INTEREST_NAME))
Box.test(autoArimaVInterest$residuals)


#Co-integracion
summary(ca.jo(Y, type="trace", ecdet="trend", K=2, dumvar=cbind(dummy=c(D2008Crisis))))
summary(ca.jo(Y, type="trace", ecdet="trend", K=2))

#Manual ECM model with 2 lags

diffVInterest <- lag(window( diff(V_INTEREST_IN), start=fromInSamplePlus3),3)
#First lag
UNEMPLOY_1<- lag(window( UNEMPLOY_IN, start=fromInSamplePlus2 ,end=toInSampleMinus1),2)
CPI_1 <- lag(window( CPI_IN, start=fromInSamplePlus2 ,end=toInSampleMinus1),2)
GSPC_1 <- lag(window( GSPC_IN, start=fromInSamplePlus2 ,end =toInSampleMinus1),2)
FED_1 <- lag(window( FED_IN, start=fromInSamplePlus2 ,end =toInSampleMinus1),2)
GS2_1 <- lag(window( GS2_IN, start=fromInSamplePlus2 ,end =toInSampleMinus1),2)
GS10_1 <- lag(window( GS10_IN, start=fromInSamplePlus2 ,end =toInSampleMinus1),2)
#Second lag
UNEMPLOY_2<- lag(window( UNEMPLOY_IN, start=fromInSamplePlus1 ,end=toInSampleMinus2),1)
CPI_2 <- lag(window( CPI_IN, start=fromInSamplePlus1 ,end=toInSampleMinus2),1)
GSPC_2 <- lag(window( GSPC_IN, start=fromInSamplePlus1 ,end =toInSampleMinus2),1)
FED_2 <- lag(window( FED_IN, start=fromInSamplePlus1 ,end =toInSampleMinus2),1)
GS2_2 <- lag(window( GS2_IN, start=fromInSamplePlus1 ,end =toInSampleMinus2),1)
GS10_2 <- lag(window( GS10_IN, start=fromInSamplePlus1 ,end =toInSampleMinus2),1)
#First lag diff
diffUNEMPLOY_1 <- lag(window( diff(UNEMPLOY_IN), start=fromInSamplePlus2, end=toInSampleMinus1 ),2)
diffCPI_1 <- lag(window( diff(CPI_IN), start=fromInSamplePlus2, end=toInSampleMinus1 ),2)
diffGSPC_1 <- lag(window( diff(GSPC_IN), start=fromInSamplePlus2, end=toInSampleMinus1 ),2)
diffFED_1 <- lag(window( diff(FED_IN), start=fromInSamplePlus2, end=toInSampleMinus1 ),2)
diffGS2_1 <- lag(window( diff(GS2_IN), start=fromInSamplePlus2, end=toInSampleMinus1 ),2)
diffGS10_1 <- lag(window( diff(GS10_IN), start=fromInSamplePlus2, end=toInSampleMinus1 ),2)
#Second lag diff
diffUNEMPLOY_2 <- lag(window( diff(UNEMPLOY_IN), start=fromInSamplePlus1, end=toInSampleMinus2 ),1)
diffCPI_2 <- lag(window( diff(CPI_IN), start=fromInSamplePlus1, end=toInSampleMinus2 ),1)
diffGSPC_2 <- lag(window( diff(GSPC_IN), start=fromInSamplePlus1, end=toInSampleMinus2 ),1)
diffFED_2 <- lag(window( diff(FED_IN), start=fromInSamplePlus1, end=toInSampleMinus2 ),1)
diffGS2_2 <- lag(window( diff(GS2_IN), start=fromInSamplePlus1, end=toInSampleMinus2 ),1)
diffGS10_2 <- lag(window( diff(GS10_IN), start=fromInSamplePlus1, end=toInSampleMinus2 ),1)

#Dummy
D2008CrisisAdapted <- lag(window( D2008Crisis_IN, start=fromInSamplePlus3),3)

ecmModelVInterest <- lm( diffVInterest~ 
                  UNEMPLOY_1+CPI_1+GSPC_1+FED_1+GS2_1+GS10_1
                + UNEMPLOY_2+CPI_2+GSPC_2+FED_2+GS2_2+GS10_2
                + diffUNEMPLOY_1+diffCPI_1+diffGSPC_1+diffFED_1+diffGS2_1+diffGS10_1
                + diffUNEMPLOY_2+diffCPI_2+diffGSPC_2+diffFED_2+diffGS2_2+diffGS10_2
                + D2008CrisisAdapted 
                + D2008CrisisAdapted*UNEMPLOY_1 + D2008CrisisAdapted*CPI_1 + D2008CrisisAdapted*GSPC_1 + D2008CrisisAdapted*FED_1 + D2008CrisisAdapted*GS2_1+ D2008CrisisAdapted*GS10_1
                + D2008CrisisAdapted*UNEMPLOY_2 + D2008CrisisAdapted*CPI_2 + D2008CrisisAdapted*GSPC_2 + D2008CrisisAdapted*FED_2 + D2008CrisisAdapted*GS2_2+ D2008CrisisAdapted*GS10_2
                )

summary(ecmModelFED)
bgtest(ecmModelFED,1)

ecmModelVInterestReduced <- lm( diffVInterest~ 
                        UNEMPLOY_1+CPI_1+FED_1+GS2_1
                      + GSPC_2+FED_2+GS2_2
                      + diffGS2_2
                      + D2008CrisisAdapted 
                      + D2008CrisisAdapted*UNEMPLOY_1 + D2008CrisisAdapted*GSPC_1 + D2008CrisisAdapted*FED_1 + D2008CrisisAdapted*GS2_1 + D2008CrisisAdapted*GS10_1
                      + D2008CrisisAdapted*UNEMPLOY_2 + D2008CrisisAdapted*CPI_2  + D2008CrisisAdapted*GSPC_2 + D2008CrisisAdapted*GS2_2 + D2008CrisisAdapted*GS10_2
                      )
summary(ecmModelVInterestReduced)
bgtest(ecmModelVInterestReduced,3)

#ECM
xeq <- xtr <- data.frame(cbind(FED_IN,UNEMPLOY_IN,CPI_IN,GSPC_IN,GS2_IN,GS10_IN,D2008Crisis_IN*UNEMPLOY_IN,D2008Crisis_IN*FED_IN, D2008Crisis_IN*CPI_IN,D2008Crisis_IN*GSPC_IN,D2008Crisis_IN*GS2_IN, D2008Crisis_IN*GS10_IN))
ecmModelVInterest <- ecm(V_INTEREST_IN, xeq, xtr, includeIntercept=TRUE)
summary(ecmModelVInterest)
bgtest(ecmModelVInterest,1)

inSampleVariales <- data.frame(cbind(FED=FED_IN,UNEMPLOY=UNEMPLOY_IN,CPI=CPI_IN,GSPC=GSPC_IN,GS2=GS2_IN,GS10=GS10_IN))
outOfSampelVariables <- data.frame(cbind(FED=FED_OUT,UNEMPLOY=UNEMPLOY_OUT,CPI=CPI_OUT,GSPC=GSPC_OUT,GS2=GS2_OUT,GS10=GS10_OUT))

#VECM
K=4
vec_ur <- ca.jo(inSampleVariales, K=K, ecdet = "trend", dumvar=cbind(dummy=c(D2008Crisis_IN)))
var_ur <- vec2var(vec_ur)
summary(var_ur)
serial.test(var_ur)


#FORECAST, with MAPE and RMSE

fARMAStatic <- doARMAForecastStatic(ARMA_ORDER,V_INTEREST_IN, V_INTEREST_OUT, 
                     fromInSample, toInSample, fromOutOfSample, toOutOfSample, tsName = V_INTEREST_NAME, legendPos = LEGENDPOS,
                     ylim=YLIM, labels=c('time',V_INTEREST_TYPE))


fARMARecursive <- doARMAForecastRecursive(ARMA_ORDER,V_INTEREST_IN, V_INTEREST_OUT, 
                        fromInSample, toInSample, fromOutOfSample, toOutOfSample, tsName = V_INTEREST_NAME, xregIn  = D2008Crisis_IN, xregOut= D2008Crisis_OUT, legendPos = LEGENDPOS,
                        ylim=YLIM, labels=c('time',V_INTEREST_TYPE))

FVECRecursive <- doVECForecastRecursive(inSampleVariales,outOfSampelVariables,K,V_INTEREST_NAME,fromOutOfSample,toOutOfSample, tsName=V_INTEREST_NAME, frequency = FREQ,
                       dummyIn=cbind(dummy=c(D2008Crisis_IN)),legendPos = LEGENDPOS, ylim=YLIM, labels=c('time',V_INTEREST_TYPE))

FRW <- doRandomWalkForecast(V_INTEREST_IN, V_INTEREST_OUT, 
                     fromInSample, toInSample, fromOutOfSample, toOutOfSample, tsName = V_INTEREST_NAME, legendPos = LEGENDPOS,
                     ylim=YLIM, labels=c('time',V_INTEREST_TYPE))

main <- paste('Actual VS Forecasted(',V_INTEREST_NAME,')')
doNicePlot(cbind(V_INTEREST,fARMARecursive,FVECRecursive,FRW),main=main,labels=c('time',V_INTEREST_TYPE),ylim=YLIM,
           legendPos = LEGENDPOS, legendCols = c('Actual','Forecast - ARMA Recursive','Forecast - VEC Recursive','Forecast - RW'), verticalLine = fromOutOfSample)

doNicePlot(cbind(V_INTEREST,fARMAStatic,FVECRecursive,FRW),main=main,labels=c('time',V_INTEREST_TYPE),ylim=YLIM,
           legendPos = LEGENDPOS, legendCols = c('Actual','Forecast - ARMA Static','Forecast - VEC Recursive','Forecast - RW'),verticalLine = fromOutOfSample)
