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

#GLOBAL CONSTANTS
#GLOBAL CONSTANTS. Data File
DATA_FILE <- "D:/Guido/Master Finanzas/2018/Segundo Trimestre/SeriesDeTiempo/TP Final/Data/Data.xlsx"
data <- read.xlsx(DATA_FILE,1)

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

Y <- cbind(FED,UNEMPLOY,CPI,GSPC,GS2,GS10)
plot(Y)

FED_IN <- window(FED, start=fromInSample, end=toInSample)
UNEMPLOY_IN <- window(UNEMPLOY, start=fromInSample, end=toInSample)
CPI_IN <- window(CPI, start=fromInSample, end=toInSample)
GSPC_IN <- window(GSPC, start=fromInSample, end=toInSample)
GS2_IN <- window(GS2, start=fromInSample, end=toInSample)
GS10_IN <- window(GS10, start=fromInSample, end=toInSample)

Y_IN <- cbind(FED_IN,UNEMPLOY_IN,CPI_IN,GSPC_IN,GS2_IN,GS10_IN)
plot(Y_IN)

FED_OUT <- window(FED, start=fromOutOfSample, end=toOutOfSample)
UNEMPLOY_OUT <- window(UNEMPLOY, start=fromOutOfSample, end=toOutOfSample)
CPI_OUT <- window(CPI, start=fromOutOfSample, end=toOutOfSample)
GSPC_OUT <- window(GSPC, start=fromOutOfSample, end=toOutOfSample)
GS2_OUT <- window(GS2, start=fromOutOfSample, end=toOutOfSample)
GS10_OUT <- window(GS10, start=fromOutOfSample, end=toOutOfSample)

Y_OUT <- cbind(UNEMPLOY_OUT,CPI_OUT,GSPC_OUT,FED_OUT,GS2_OUT,GS10_OUT)
plot(Y_OUT)

par(mar=c(1,1,1,1), mfrow=c(1,1) )

#Subprime Crisis dummy
D2008Crisis <- createTSDummy(DATES, fromInSample, toOutOfSample, crisisSubprimeFrom, crisisSubprimeTo, FREQ)
D2008Crisis_IN <- createTSDummy(DATES, fromInSample, toInSample, crisisSubprimeFrom, crisisSubprimeTo, FREQ)
D2008Crisis_OUT <- createBoolTSDummy(fromOutOfSample, toOutOfSample, frequency = FREQ, booleanValue = FALSE )
D2008CrisisForDiff_IN <- createTSDummy(DATES, fromInSamplePlus1, toInSample, crisisSubprimeFrom, crisisSubprimeTo, FREQ)

#Functions


findARMA <- function( theTS, ar=5, ma=5, boxTestLimit=0.22, boxTestLag=12, xreg=NULL ){
  
  armaModel <- NULL
  lastArmaFound <- arima(theTS,order=c(ar,0,ma))
  for(arComponent in ar:0){
    for(maComponent in ma:0){
      armaModel <- arima(theTS,order=c(arComponent,0,maComponent),xreg=xreg)
      modelApplies <- TRUE
      print(armaModel$arma)
      for(lag in 1:boxTestLag){
        bTest <- Box.test(armaModel$residuals,lag)
        print(bTest$p.value)
        if(bTest$p.value <= boxTestLimit){
          modelApplies <- FALSE
          break()
        }
      }
      if(modelApplies){
        #Arbitrary criteria for smaller model
        if( (armaModel$arma[1]+armaModel$arma[2])< (lastArmaFound$arma[1]+lastArmaFound$arma[2]) )
        lastArmaFound <- armaModel
        print("Found:")
        print(lastArmaFound$arma)
      }
    }
  }
  print("Result:")
  print(lastArmaFound$arma)
  return(lastArmaFound)
}

DFTest <- function( matrix ){
  columnNames <- colnames(matrix)
  print("Dickey-Fullter test P-value output:")
  options(warn=-1)
  for(col in columnNames){
    testResult <- adf.test(matrix[,col])
    print(paste(c(col,"P value: ",testResult$p.value),collapse=" "))
  }
  options(warn=0)
}

doARMAForecastStatic <- function( armaModel, fullSeries, h, fromInSample, toInSample, fromOutOfSample, toOutOfSample, breakDummy=NULL, frequency=12, tsName='', legendPos=c(0,0),
                                  labels=c('',''), ylim=c(0,100) ){
  
  fcastTS <- forecast(armaModel,xreg=breakDummy)
  observedTS <- toTs( fullSeries, from=fromInSample, to=toOutOfSample,frequency = frequency)
  forecastedTS <- toTs( fcastTS$mean[1:h], from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  lower95TS <- toTs( fcastTS$lower[,'95%'][1:h], from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  upper95TS <- toTs( fcastTS$upper[,'95%'][1:h], from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  
  obsAndForecasted <- cbind(observedTS,forecastedTS)
  colnames(obsAndForecasted)[1] <- paste( 'Actual', tsName )
  colnames(obsAndForecasted)[2] <- paste( 'Forecasted', tsName )
  ic <- cbind(lower95TS,upper95TS)
  colnames(ic)[1] <- 'Lower 95%'
  colnames(ic)[2] <- 'Upper 95%'
  
  doForecastPlot(obsAndForecasted,ic,legendPos=legendPos,main='ARMA(Static)',labels=labels,ylim=ylim)
  
  return(fcastTS)
}


doARMAForecastStatic2 <- function(order,inSampleTS,outOfSampleTS,fromInSample, toInSample, fromOutOfSample, toOutOfSample, frequency=12, tsName='', labels=c('',''), legendPos=c(0,0)
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
  
  doForecastPlot(obsAndForecasted,ic,labels=labels,ylim=ylim,legendPos=legendPos,main='ARMA(Static)')
}


doARMAForecastRolling <- function(order,inSampleTS,outOfSampleTS,fromInSample, toInSample, fromOutOfSample, toOutOfSample, xregIn=NULL, xregOut=NULL, frequency=12, tsName='', labels=c('',''), legendPos=c(0,0)
                                  ,ylim=c(0,100)){
  forecastedValues <- NULL
  lower95 <- NULL
  upper95 <- NULL
  windowSize <- length(inSampleTS)
  theTS <- c(inSampleTS)
  for (i in 1:length(outOfSampleTS)){
    theTS <- c(inSampleTS[i:windowSize],forecastedValues) #Roll 1 step
    dummyInFunc <- cbind(dummy=c(xregIn))
    #Unfortunately forecast searches in global env
    assign("dummyInFunc",dummyInFunc, envir=.GlobalEnv)
    armaModel <- arima(theTS,order,xreg=dummyInFunc)
    dummyOutFunc <- cbind(dummy=c(xregOut))
    assign("dummyOutFunc",dummyOutFunc, envir=.GlobalEnv)
    fcastTS <- forecast(armaModel, xreg=dummyOutFunc)
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
  
  doForecastPlot(obsAndForecasted,ic,labels=labels,ylim=ylim,legendPos=legendPos,main='ARMA(Rolling)')
}

doARMAForecastRolling2 <- function(order,inSampleTS,outOfSampleTS,fromInSample, toInSample, fromOutOfSample, toOutOfSample, xregIn=NULL, xregOut=NULL, frequency=12, tsName='', labels=c('',''), legendPos=c(0,0)
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
  
  doForecastPlot(obsAndForecasted,ic,labels=labels,ylim=ylim,legendPos=legendPos,main='ARMA(Rolling)')
}

doVECForecastStatic<- function(varModel, observedTS, vName, h, fromOutOfSample, toOutOfSample, tsName=NULL, frequency=12, dummy=NULL, legendPos=c(0,0),
                               labels=c('',''), ylim=c(0,100)){
  if(missing(tsName)){
    tsName <- vName
  }
  VECForecast <- predict(varModel,n.ahead=h, dumvar=dummy)
  fcstIndex <- paste(vName,'.fcst', sep='')
  lowerIndex<- paste(vName,'.lower', sep='')
  upperIndex<- paste(vName,'.upper', sep='')
  forecastedTS <- toTs( data.frame(VECForecast$fcst)[,fcstIndex], from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  lower95 <- toTs(  data.frame(VECForecast$fcst)[,lowerIndex], from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  upper95 <- toTs(  data.frame(VECForecast$fcst)[,upperIndex], from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  obsAndForecasted <- cbind(observedTS,forecastedTS)
  colnames(obsAndForecasted)[1] <- paste( 'Actual', tsName )
  colnames(obsAndForecasted)[2] <- paste( 'Forecasted', tsName )
  ic <- cbind(lower95,upper95)
  colnames(ic)[1] <- 'Lower 95%'
  colnames(ic)[2] <- 'Upper 95%'
  
  doForecastPlot(obsAndForecasted,ic,labels=labels,ylim=ylim,legendPos=legendPos, main='VEC(Static)')
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
  observedTS <- toTs( c(inSampleTS[tsName][,1],outOfSampleTS[tsName][,1]) , from=fromInSample, to=toOutOfSample,frequency = frequency)
  forecastedTS <- toTs( forecastedValues , from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  lower95TS <- toTs( lower95 , from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  upper95TS <- toTs( upper95 , from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
  obsAndForecasted <- cbind(observedTS,forecastedTS)
  print(obsAndForecasted)
  colnames(obsAndForecasted)[1] <- paste( 'Actual', tsName )
  colnames(obsAndForecasted)[2] <- paste( 'Forecasted', tsName )
  ic <- cbind(lower95TS,upper95TS)
  colnames(ic)[1] <- 'Lower 95%'
  colnames(ic)[2] <- 'Upper 95%'
  doForecastPlot(obsAndForecasted,ic,labels=labels,ylim=ylim,legendPos=legendPos, main='VEC(Recursive)')
}

doVECForecastRolling<- function(varModel, observedTS, vName, nroll, fromOutOfSample, toOutOfSample, tsName=NULL, frequency=12, dummy=NULL, legendPos=c(0,0),
                               labels=c('',''), ylim=c(0,100))
  {
    if(missing(tsName)){
      tsName <- vName
    }
    VECForecast <- predict_rolling(varModel,n.ahead=1, nroll=nroll, dumvar=dummy)
    forecastedTS <- toTs( VECForecast$pred[vName], from=fromOutOfSample, to=toOutOfSample,frequency = frequency)
    obsAndForecasted <- cbind(observedTS,forecastedTS)
    colnames(obsAndForecasted)[1] <- paste( 'Actual', tsName )
    colnames(obsAndForecasted)[2] <- paste( 'Forecasted', tsName )

    doForecastPlot(obsAndForecasted,NULL,labels=labels,ylim=ylim,legendPos=legendPos, main='VEC(Rolling)')
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


#Stationarity
DFTest(Y)
summary(ur.df(UNEMPLOY,type="trend",selectlags="BIC"))
summary(ur.df(CPI,type="trend",selectlags="BIC"))
summary(ur.df(GSPC,type="trend",selectlags="BIC"))
summary(ur.df(FED,type="trend",selectlags="BIC"))
summary(ur.df(GS2,type="trend",selectlags="BIC"))
summary(ur.df(GS10,type="trend",selectlags="BIC"))

#Diff
diffUNEMPLOY <- diff(UNEMPLOY)
diffCPI <- diff(CPI)
diffGSPC <- diff(GSPC)
diffFED <- diff(FED)
diffGS2 <- diff(GS2)
diffGS10 <- diff(GS10)

Y_DIFF <- cbind(diffUNEMPLOY,diffCPI,diffGSPC,diffFED,diffGS2,diffGS10)
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


#Analizo FED
plot(FED, main = 'FED rate')
summary(ur.df(FED,type="trend",selectlags="BIC"))
diffFED <- diff(FED)
plot(diffFED, main = 'FED rate(1st Diff)')
summary(ur.df(diffFED,type="trend",selectlags="BIC"))
acf(diffFED)
pacf(diffFED)
diffFED_IN <- diff(FED_IN)
armaModelFED <- findARMA(diffFED_IN,xreg=diffFED_IN*D2008CrisisForDiff_IN)
plot(armaModelFED$residuals, main='ARMA Residulas')
Box.test(armaModelFED$residuals)
autoArimaFED <- auto.arima(FED_IN, seasonal = FALSE, xreg=data.frame(FED_IN*D2008Crisis_IN))
armaModelFED <- arima(diffFED_IN,c(1,1,2),xreg=FED_IN*D2008CrisisForDiff_IN)
#autoArimaFED <- auto.arima(FED, seasonal = FALSE)
autoArimaFED
plot(autoArimaFED$residuals, main='AUTO ARMA FOR FED')
Box.test(autoArimaFED$residuals)

test <- forecast(FED_OUT, model=autoArimaFED, xreg=D2008Crisis_OUT)
fitted(test)

#Forecast TODO To be moved
doARMAForecastStatic(autoArimaFED, FED, outOfSamplePeriods, 
               fromInSample, toInSample, fromOutOfSample, toOutOfSample, breakDummy = data.frame(D2008Crisis_OUT), tsName = 'FED', legendPos = c(2010,7), ylim=c(0,7), labels=c('time','IR'))

doARMAForecastStatic2(c(1,1,2),FED_IN, FED_OUT, 
                      fromInSample, toInSample, fromOutOfSample, toOutOfSample, tsName = 'FED', legendPos = c(2007,7),
                      ylim=c(0,7), labels=c('time','IR'))

doARMAForecastRolling(c(1,1,2),FED_IN, FED_OUT, 
                      fromInSample, toInSample, fromOutOfSample, toOutOfSample, tsName = 'FED', xregIn  = D2008Crisis_IN, xregOut= D2008Crisis_OUT, legendPos = c(2007,7),
                      ylim=c(0,7), labels=c('time','IR'))


doARMAForecastRolling2(c(1,1,2),FED_IN, FED_OUT, 
                      fromInSample, toInSample, fromOutOfSample, toOutOfSample, tsName = 'FED', xregIn  = D2008Crisis_IN, xregOut= D2008Crisis_OUT, legendPos = c(2007,7),
                      ylim=c(0,7), labels=c('time','IR'))


#Co-integracion
summary(ca.jo(Y, type="trace", ecdet="trend", K=2, dumvar=cbind(dummy=c(D2008Crisis))))
summary(ca.jo(Y, type="trace", ecdet="trend", K=2))

#modelo MCE FED

diffFED <- lag(window( diff(FED_IN), start=fromInSamplePlus3),3)
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

ecmModelFED <- lm( diffFED~ 
                  UNEMPLOY_1+CPI_1+GSPC_1+FED_1+GS2_1+GS10_1
                + UNEMPLOY_2+CPI_2+GSPC_2+FED_2+GS2_2+GS10_2
                #+ diffUNEMPLOY_1+diffCPI_1+diffGSPC_1+diffFED_1+diffGS2_1+diffGS10_1
                + diffUNEMPLOY_2+diffCPI_2+diffGSPC_2+diffFED_2+diffGS2_2+diffGS10_2
                + D2008CrisisAdapted 
                + D2008CrisisAdapted*UNEMPLOY_1 + D2008CrisisAdapted*CPI_1 + D2008CrisisAdapted*GSPC_1 + D2008CrisisAdapted*FED_1 + D2008CrisisAdapted*GS2_1+ D2008CrisisAdapted*GS10_1
                + D2008CrisisAdapted*UNEMPLOY_2 + D2008CrisisAdapted*CPI_2 + D2008CrisisAdapted*GSPC_2 + D2008CrisisAdapted*FED_2 + D2008CrisisAdapted*GS2_2+ D2008CrisisAdapted*GS10_2
                )

summary(ecmModelFED)
bgtest(ecmModelFED,1)

ecmModelFEDReduced <- lm( diffFED~ 
                        UNEMPLOY_1+CPI_1+FED_1+GS2_1
                      + GSPC_2+FED_2+GS2_2
                      + diffGS2_2
                      + D2008CrisisAdapted 
                      + D2008CrisisAdapted*UNEMPLOY_1 + D2008CrisisAdapted*GSPC_1 + D2008CrisisAdapted*FED_1 + D2008CrisisAdapted*GS2_1 + D2008CrisisAdapted*GS10_1
                      + D2008CrisisAdapted*UNEMPLOY_2 + D2008CrisisAdapted*CPI_2  + D2008CrisisAdapted*GSPC_2 + D2008CrisisAdapted*GS2_2 + D2008CrisisAdapted*GS10_2
                      )
summary(ecmModelFEDReduced)
bgtest(ecmModelFEDReduced,3)

#ECM
xeq <- xtr <- data.frame(cbind(FED_IN,UNEMPLOY_IN,CPI_IN,GSPC_IN,GS2_IN,GS10_IN,D2008Crisis_IN*UNEMPLOY_IN,D2008Crisis_IN*FED_IN, D2008Crisis_IN*CPI_IN,D2008Crisis_IN*GSPC_IN,D2008Crisis_IN*GS2_IN, D2008Crisis_IN*GS10_IN))
ecmModelFED <- ecm(FED_IN, xeq, xtr, includeIntercept=TRUE)
summary(ecmModelFED)
bgtest(ecmModelFED,1)

#VECM
inSampleVariales <- data.frame(cbind(FED=FED_IN,UNEMPLOY=UNEMPLOY_IN,CPI=CPI_IN,GSPC=GSPC_IN,GS2=GS2_IN,GS10=GS10_IN))
outOfSampelVariables <- data.frame(cbind(FED=FED_OUT,UNEMPLOY=UNEMPLOY_OUT,CPI=CPI_OUT,GSPC=GSPC_OUT,GS2=GS2_OUT,GS10=GS10_OUT))
vec_ur <- ca.jo(inSampleVariales, K=4, ecdet = "trend", dumvar=cbind(dummy=c(D2008Crisis_IN)))
var_ur <- vec2var(vec_ur)
summary(var_ur)
serial.test(var_ur)

doVECForecastRecursive(inSampleVariales,outOfSampelVariables,4,'FED',fromOutOfSample,toOutOfSample, tsName='FED', frequency = FREQ,
                    dummyIn=cbind(dummy=c(D2008Crisis_IN)),legendPos = c(2007,7), ylim=c(0,7), labels=c('time','IR'))

doVECForecastStatic(var_ur,FED,'FED_IN',outOfSamplePeriods,fromOutOfSample,toOutOfSample, tsName='FED', frequency = FREQ,
                    dummy=cbind(dummy=c(D2008Crisis_OUT)),legendPos = c(2007,7), ylim=c(0,7), labels=c('time','IR'))

VEC <- VECM(vecVariables,lag=4,r=1,estim=c("ML"),LRinclude=c("trend"),dumvar=cbind(dummy=c(D2008Crisis_IN)))

lineVar <- lineVar(vecVariables,lag=4,r=1,model='VECM', estim='ML')

doVECForecastRolling(lineVar,FED,'FED_IN',outOfSamplePeriods,fromOutOfSample,toOutOfSample, tsName='FED', frequency = FREQ,
                    dummy=cbind(dummy=c(D2008Crisis_OUT)),legendPos = c(2007,7), ylim=c(0,7), labels=c('time','IR'))

VECForecast <- predict_rolling(lineVar,n.ahead=1, nroll=outOfSamplePeriods, dumvar=D2008Crisis_OUT)

#VECM OTRO

cointest<-ca.jo(vecVariables, type="trace", ecdet="trend", K=2, dumvar=cbind(dummy=c(d2008Crisis)))
vecm<-cajorls(cointest)
predict(vecm)
forecast(vecm)

#FIN modelo MCE FED

y = cbind(UNEMPLOY,CPI,GSPC,FED,GS2,GS10)
VARselect(y, lag.max=5, type="none")


diffGS2 <- diff(GS2)
y <- cbind(lag(diffUNEMPLOY),lag(diffCPI),lag(diffGSPC),lag(diffFED),lag(diffGS2),lag(diffGS10))
y <- cbind(diffUNEMPLOY,diffCPI,diffGSPC,diffGS2,diffGS10)
y <- cbind(diffUNEMPLOY,diffCPI,diffGSPC,diffGS10)
y <- cbind(diffUNEMPLOY,diffCPI,diffGSPC,diffGS2,diffGS10)
plot(diffFED)
d2008CrisisVar <- window( d2008Crisis, end =preLastDate)
VARselect(y, lag.max=10, type="none", exogen=cbind(dummy=c(d2008CrisisVar)))


varModel <- VAR(y,p=3, exogen=cbind(dummy=c(d2008Crisis)))
summary(varModel)
serial.test(varModel, lags.pt=10, type="PT.asymptotic") # Malo que p value sea bajo
normality.test(varModel, multivariate.only=TRUE)



