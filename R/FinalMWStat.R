##MwStatistics
#' @title MwStatistics
#' @description This function allows you to calculate a variety of statistics in a time series using a moving window.
#' @author William Kopans - \email{wkopans123@@gmail.com}
#' @export MwStatistics
#' @param dat Name of dataset.
#' @param TimeVarCol Column number of
#' @param ColA Column number of first variable
#' @param ColB Column number of second variable
#' @param win Window Size
#' @param stat Calculated Statistic
#' @param overlap Overlap between windows
#' @param genplot If you want a plot to be generated
#' @param genDataFrame If you want a dataframe to be generated
#' @return Plot and/or Dataframe
#' @examples
#' MwStatistics(dat = data.frame(replicate(10,sample(0:10,1000,rep=TRUE))),TimeVarCol = 1, ColA = 2, ColB = 3, win = 2, stat = "Correlation", genplot = TRUE, genDataFrame = FALSE, overlap = 0.5)
#' @usage MwStatistics(dat = NULL, TimeVarCol = 1, ColA = 2, ColB = 3, win = NULL, stat = NULL, overlap = 0, genplot = TRUE, genDataFrame = FALSE)
#' @export
#' @import ggplot2
#' @import dplyr
#' @import lmodel2
utils::globalVariables(c("MwStatistics"))
library(dplyr)
library(ggplot2)
library(lmodel2)

MwStatistics <- function (dat = NULL,TimeVarCol = 1, ColA = 2, ColB = 3, win = NULL, stat = NULL, overlap = 0.0, genplot = TRUE, genDataFrame = FALSE)
{
  old <- Sys.time() #used to find how long the function takes to run

  if (overlap > win) {
    stop("Overlap is larger than window size.")
  }
  if (win == overlap) {
    stop("Overlap is equal to window size.")
  }

  data = dat %>% #Selecting the columns
    select(all_of(TimeVarCol),all_of(ColA),all_of(ColB))

  StatRec <- data.frame(Value = 1) #creating a dataframe to store the recorded statistics
  TimeVarColRec <- data.frame(Value = 1) #creating a dataframe to store the timeframe
  #It starts with the first cell being one, but that is later removed

  data %>% filter(!is.na(TimeVarCol)) #removing any NAs
  data %>% filter(!is.na(ColA))
  data %>% filter(!is.na(ColB))

  NewWinStart <- min(data[,1]) #creating the first window
  NewWinEnd <- NewWinStart + win

  TestCase <- data %>% #creating the dataframe TestCase which is where the window is stored
    filter(between(as.double(data[,1]),0,0)) #contains nothing at first

  while (NewWinEnd < max(data[,1])) { #will loop until the end of the dataset

    TestCase <- data %>% #sets up window
      filter(between(as.double(data[,1]),as.double(NewWinStart),as.double(NewWinEnd))) #selecting rows that the TimeVarCol falls within the window range

    if(genDataFrame == T){
      if (nrow(TestCase) == 0) {
        print(paste0("Window spanning from ",NewWinStart," to ", NewWinEnd, " contains no values"))
      } #added feature that will tell you when you have an empty window
    }
    #print(paste0("Window spans from ",NewWinStart," to ", NewWinEnd)) #this can be commented out but will say where your windows are
    #mostly used during testing

    if (stat == "Correlation"){
      StatRec[nrow(StatRec) + 1,] = c (cor(TestCase[2],TestCase[3]))
    }
    else if (stat == "P-Value") {
      StatRec[nrow(StatRec) + 1,] = c(t.test(TestCase[,2],TestCase[,3])[3])
    }
    else if (stat == "SDevA") {
      StatRec[nrow(StatRec) + 1,] = c (sd(unlist(TestCase[2])))
    }
    else if (stat == "SDevB") {
      StatRec[nrow(StatRec) + 1,] = c (sd(unlist(TestCase[3])))
    }
    else if (stat == "SErrorSlope") {
      StatRec[nrow(StatRec) + 1,] = c (summary(lm(TestCase[,2] ~ TestCase[,3]))$coef[[4]]) #Make sure your window size is relatively large or else you will get error messages.
    }
    else if (stat == "SErrorIntercept") { #Make sure your window size is relatively large or else you will get error messages.
      StatRec[nrow(StatRec) + 1,] = c (coef(summary(lm(TestCase[,2] ~ TestCase[,3])))[, "Std. Error"][1])
    }
    else if (stat == "Intercept") {
      StatRec[nrow(StatRec) + 1,] = c ((lm(formula = TestCase[,2] ~ TestCase[,3])$coefficients)[1])
    }
    else if (stat == "Slope") {
      StatRec[nrow(StatRec) + 1,] = c ((lm(formula = TestCase[,2] ~ TestCase[,3])$coefficients)[2])
    }
    else if (stat == "OLS") { # ordinary least squares (reduced major axis regression)


      #Regression Results (OLS)
      StatRec[nrow(StatRec) + 1,1] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[3]][1,][2]) #OLS intercept
      StatRec[nrow(StatRec),2] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[3]][1,][3]) #OLS Slope
      StatRec[nrow(StatRec),3] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[3]][1,][4]) # OLS Angle (Degrees)

      #Regression Results (MA):
      StatRec[nrow(StatRec),4] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[3]][2,2]) #MA Intercept
      StatRec[nrow(StatRec),5] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[3]][2,3]) #MA Slope
      StatRec[nrow(StatRec),6] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[3]][2,4]) #MA Angle (Degrees)
      StatRec[nrow(StatRec),7] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[3]][2,5]) #MA P-Perm (1-Tailed)

      #Confidence Intervals (OLS)
      StatRec[nrow(StatRec),8] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[4]][1,][2]) #OLS 2.5% intercept
      StatRec[nrow(StatRec),9] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[4]][1,][3]) #OLS 97.5% intercept
      StatRec[nrow(StatRec),10] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[4]][1,][4]) #OLS 2.5 Slope
      StatRec[nrow(StatRec),11] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[4]][1,][5]) #OLS 97.5 Slope

      #Confidence Intervals (MA) - Lmodel2 is not working for this - lmodel2(DummyData[,1] ~ DummyData[,2], data = DummyData, range.y=NULL, range.x=NULL, nperm=98)[[4]][2,]
      StatRec[nrow(StatRec),12] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[4]][2,][2]) #MA 2.5% intercept
      StatRec[nrow(StatRec),13] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[4]][2,][3]) #MA 97.5% intercept
      StatRec[nrow(StatRec),14] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[4]][2,][4]) #MA 2.5 Slope
      StatRec[nrow(StatRec),15] = c (lmodel2::lmodel2(TestCase[,2] ~ TestCase[,3], data = TestCase, range.y=NULL, range.x=NULL, nperm=98)[[4]][2,][5]) #MA 97.5 Slope



      #lmodel2(DummyData[,1] ~ DummyData[,2], data = DummyData, range.y=NULL, range.x=NULL, nperm=98)[[4]][1,][2]
      #MwStatistics(dat = DummyData, win = 2, TimeVarCol = 1, ColA = 2, ColB = 3, stat = "OLS", genplot = T, genDataFrame = T, overlap = 0.5)

    }
    else {
      message("Please choose a statistic")
      break
    }
    TimeVarColRec[nrow(TimeVarColRec) + 1,] = c (colMeans(TestCase[1]))
    if (overlap == 0) { #If overlap is 0
      NewWinStart <- win + NewWinStart
    } else{ #If overlap is anything else
      NewWinStart <- NewWinStart + (win - overlap) #Move a window but backtrack to retain part of previous window
    }
    NewWinEnd <- win + NewWinStart #Window start already dealt with overlap so just adding the window

  }


  RunTime <- Sys.time() - old # calculate difference between start and end times
  RunTime <- round(as.double(sub("", "", RunTime)), digits = 2) #rounds time spent
  message(paste0("Finished in ", RunTime, " Seconds.")) #prints how long the program took to run

  StatRec <- StatRec[-1,] #removing that first 1 value
  TimeVarColRec <- TimeVarColRec[-1,] #removing that first 1 value
  #print(summary(StatRec)) #printing a summary of the staristics calculated, can be commented out

  StatRec <- cbind(TimeVarColRec, StatRec) #Combines the recorded statistics and the window position.
  StatRecDF <- as.data.frame(StatRec) #makes it a dataframe

  if (stat == "OLS") {

    names(StatRecDF)[2] <- "OLS intercept"; names(StatRecDF)[3] <- "OLS Slope";
    names(StatRecDF)[4] <- "OLS Angle (Degrees)"; names(StatRecDF)[5] <- "MA Intercept";
    names(StatRecDF)[6] <- "MA Slope"; names(StatRecDF)[7] <- "MA Angle (Degrees)";
    names(StatRecDF)[8] <- "MA P-Perm (1-Tailed)"; names(StatRecDF)[9] <- "OLS 2.5% intercept";
    names(StatRecDF)[10] <- "OLS 97.5% intercept"; names(StatRecDF)[11] <- "OLS 2.5 Slope";
    names(StatRecDF)[12] <- "OLS 97.5 Slope"; names(StatRecDF)[13] <- "MA 2.5 intercept";
    names(StatRecDF)[14] <- "MA 97.5 interpcept"; names(StatRecDF)[15] <- "MA 2.5 slope";
    names(StatRecDF)[16] <- "MA 97.5 slope";

  }

  if (genDataFrame == T){ #If you want to view the dataframe here is your option!
    View(StatRecDF) #This is great if you don't want to rely solely on your eyes and the graph
    message("Remember to add .csv end to file")

    tryCatch({
      try(write.csv(StatRecDF,file = file.choose(new = T)), silent = TRUE)
    }, interrupt = function(x) {
      message("Something went wrong")
    })

  }

  if (genplot == T && stat != "OLS") { #Generates the plot
    ggplot2::ggplot(data=StatRecDF, mapping = ggplot2::aes(x=TimeVarColRec,y=StatRec))+
      ggplot2::geom_line(color = "#0e4142")+
      ggplot2::labs(y = stat,x=names(dat)[TimeVarCol],title = "Moving Window Analysis")
    #ggplot2::labs(y = stat,x="Time Variable",title = "Moving Window Analysis")
    #If you want to be able to share the plot directly,
    #I can have the type of time variable as an input
    #to the function which is filled in on the X-Axis rather than "Time Varaible"
  }

}

#Examples for each statistic:
#MwStatistics(dat = TotalPiecewiseInterp, win = 2, TimeVarCol = 1, ColA = 2, ColB = 3, stat = "Correlation", genplot = T, genDataFrame = F, overlap = 0.5)
#MwStatistics(dat = TotalPiecewiseInterp, win = 2, TimeVarCol = 1, ColA = 2, ColB = 3, stat = "SErrorIntercept", genplot = T, genDataFrame = F, overlap = 0.5)
#MwStatistics(dat = TotalPiecewiseInterp, win = 2, TimeVarCol = 1, ColA = 2, ColB = 3, stat = "SErrorSlope", genplot = T, genDataFrame = F, overlap = 0.5)
#MwStatistics(dat = TotalPiecewiseInterp, win = 2, TimeVarCol = 1, ColA = 2, ColB = 3, stat = "SDevA", genplot = T, genDataFrame = F, overlap = 0.5)
#MwStatistics(dat = TotalPiecewiseInterp, win = 2, TimeVarCol = 1, ColA = 2, ColB = 3, stat = "SDevB", genplot = T, genDataFrame = F, overlap = 0.5)
#MwStatistics(dat = TotalPiecewiseInterp, win = 2, TimeVarCol = 1, ColA = 2, ColB = 3, stat = "Intercept", genplot = T, genDataFrame = F, overlap = 0.5)
#MwStatistics(dat = TotalPiecewiseInterp, win = 2, TimeVarCol = 1, ColA = 2, ColB = 3, stat = "Slope", genplot = T, genDataFrame = F, overlap = 0.5)
#MwStatistics(dat = TotalPiecewiseInterp, win = 2, TimeVarCol = 1, ColA = 2, ColB = 3, stat = "P-Value", genplot = T, genDataFrame = F, overlap = 0.5)
