# MwStatistics

MwStatistics allows you to easily calculate seven common statistics on time series data using a moving window.

## Getting Started

Submitting to CRAN shortly. Fingers crossed!

### Usage

General Usage.
```
MwStatistics(dat = NULL, TimeVarCol = 1, ColA = 2, ColB = 3, win = NULL,
stat = NULL, overlap = 0, genplot = T, genDataFrame = F)
```
Example
```
MwStatistics(dat = data.frame(replicate(10,sample(0:10,1000,rep=TRUE))),
TimeVarCol = 1, ColA = 2, ColB = 3, win = 2,
stat = "Correlation", genplot = T, genDataFrame = F, overlap = 0.5)

```
Availible statistics: 
- "Correlation" = Correlation

 - "P-Value" = P-Value

 - "SDevX" = Standard Deviation of variable A

 - "SDevY" = Standard Deviation of variable B

 - "SErrorSlope" = Standard Error of the slope

 - "SErrorIntercept" = Standard Error of the intercept

 - "Intercept" = Intercept (Linear regression)

 - "Slope" = Slope (Linear regression)

Arguments:

 - dat
    - Dataset name

 - TimeVarCol
    - Column number of time series variable

 - ColA
    - Column number of first variable

 - ColB
     - Column number of second variable

 - win
     - Window size

 - stat
     - Statistic to calculate

 - overlap
     - How much the windows overlap

 - genplot
     - Generate a plot of recorded statistic

 - genDataFrame
     - Generate dataframe with recorded statistic values

 

### Installing

Considering wether to submit to CRAN. For now, enter the following commands:
```
library(devtools)
install_github("WilliamKopans/MwStat")
```
You can check that it installed correctly using:
```
?MWStat::MwStatistics
```

## Running The Function

Fill in the correct information, replacing the sample information provided and run by selecting and clicking command enter.


## Contributing

If you find any improvements, please email me at wkopans123@gmail.com. 


## Authors

* **William Kopans** - [WilliamKopans](https://github.com/WilliamKopans)

See also the list of [contributors](https://github.com/WilliamKopans/MwStatistics/graphs/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE](https://github.com/WilliamKopans/MwStat/blob/master/LICENSE) file for details

## Acknowledgments
Inspiration from MwCor from Astrochron.
