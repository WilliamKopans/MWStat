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
utils::globalVariables(c("MwStatistics"))

MwStatistics <- function (dat = NULL,TimeVarCol = 1, ColA = 2, ColB = 3, win = NULL, stat = NULL, overlap = 0.0, genplot = TRUE, genDataFrame = FALSE)
{
  old <- Sys.time() #used to find how long the function takes to run

  if (overlap > win) {
    stop("Overlap is larger than window size.")
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
    }else if (stat == "Slope") {
      StatRec[nrow(StatRec) + 1,] = c ((lm(formula = TestCase[,2] ~ TestCase[,3])$coefficients)[2])
    } else {
      print("Please choose a statistic")
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
  print(paste0("Finished in ", RunTime, " Seconds.")) #prints how long the program took to run

  StatRec <- StatRec[-1,] #removing that first 1 value
  TimeVarColRec <- TimeVarColRec[-1,] #removing that first 1 value
  #print(summary(StatRec)) #printing a summary of the staristics calculated, can be commented out


  StatRec <- cbind(TimeVarColRec, StatRec) #Combins the recorded statistics and the window position.
  StatRecDF <- as.data.frame(StatRec) #makes it a dataframe

  if (genDataFrame == T){ #If you want to view the dataframe here is your option!
    View(StatRecDF) #This is great if you don't want to rely solely on your eyes and the graph
  }

  if (genplot == T) { #Generates the plot
    ggplot2::ggplot(data=StatRecDF, mapping = ggplot2::aes(x=TimeVarColRec,y=StatRec))+
      ggplot2::geom_line(color = "#0e4142")+
      ggplot2::labs(y = stat,x="Time Variable",title = "Moving Window Analysis")
    #If you want to be able to share the plot directly,
    #I can have the type of time variable as an input
    #to the function which is filled in on the X-Axis rather than "Time Varaible"
  }

}
