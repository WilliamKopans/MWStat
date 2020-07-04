MwStatistics <- function (dat = NULL,TimeVarCol = 1, ColA = 2, ColB = 3, win = NULL, stat = NULL, overlap = 0.0, genplot = T, genDataFrame = F)
{
  old <- Sys.time() #used to find how long the function takes to run
  utils::globalVariables(c("%>%", add=FALSE))
  data = dat %>% #Selecting the columns
    dplyr::select(all_of(TimeVarCol),all_of(ColA),all_of(ColB))

  StatRec <- data.frame(Value = 1) #creating a dataframe to store the recorded statistics
  #It starts with the first cell being one, but that is later removed

  data %>% dplyr::filter(!is.na(TimeVarCol)) #removing any NAs
  data %>% dplyr::filter(!is.na(ColA))
  data %>% dplyr::filter(!is.na(ColB))

  NewWinStart <- min(data[,1]) #creating the first window
  NewWinEnd <- NewWinStart + win

  TestCase <- data %>% #creating the dataframe TestCase which is where the window is stored
    dplyr::filter(dplyr::between(as.double(data[,1]),0,0)) #contains nothing at first

  while (NewWinEnd < max(data[,1])) { #will loop until the end of the dataset

    TestCase <- data %>% #sets up window
      dplyr::filter(dplyr::between(as.double(data[,1]),as.double(NewWinStart),as.double(NewWinEnd))) #selecting rows that the TimeVarCol falls within the window range

    if(genDataFrame == T){
      if (nrow(TestCase) == 0) {
        print(paste0("Window spanning from ",NewWinStart," to ", NewWinEnd, " contains no values"))
      } #added feature that will tell you when you have an empty window
    }
    print(paste0("Window spans from ",NewWinStart," to ", NewWinEnd)) #this can be commented out but will say where your windows are
    #mostly used during testing

    if (stat == "Correlation"){
      StatRec[nrow(StatRec) + 1,] = c (cor(TestCase[2],TestCase[3]))
    }
    else if (stat == "P-Value") {
      StatRec[nrow(StatRec) + 1,] = c(t.test(TestCase[,2],TestCase[,3])[3])
    }
    else if (stat == "SDevX") {
      StatRec[nrow(StatRec) + 1,] = c (sd(unlist(TestCase[2])))
    }
    else if (stat == "SDevY") {
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

    if (overlap == 0) { #If overlap is 0
      NewWinStart <- win + NewWinStart
    } else{ #If overlap is anything else
      NewWinStart <- NewWinStart + (win - overlap) #Move a window but backtrack to retain part of previous window
    }
    NewWinEnd <- win + NewWinStart #Window start already dealt with overlap so just adding the window

  }
  RunTime <- Sys.time() - old # calculate difference dplyr::between start and end times
  RunTime <- round(as.double(sub("", "", RunTime)), digits = 2) #rounds time spent
  print(paste0("Finished in ", RunTime, " Seconds.")) #prints how long the program took to run

  StatRec <- StatRec[-1,] #removing that first 1 value
  print(summary(StatRec)) #printing a summary of the staristics calculated, can be commented out


  if (genDataFrame == T){ #If you want to view the dataframe here is your option!
    View(StatRec) #This is great if you don't want to rely solely on your eyes and the graph
  }

  WindowPos <- (1:NROW(StatRec)) #Says where the window is positioned in the dataset
  StatRec <- cbind(WindowPos, StatRec) #Combins the recorded statistics and the window position.
  StatRecDF <- as.data.frame(StatRec) #makes it a dataframe

  if (genplot == T) { #Generates the plot
    ggplot2::ggplot(data=StatRecDF, mapping = ggplot2::aes(x=WindowPos,y=StatRec))+
      ggplot2::geom_line(color = "#0e4142")+
      ggplot2::labs(y = stat,x="Postion of Window",title = "Moving Window Analysis")
  }
}
