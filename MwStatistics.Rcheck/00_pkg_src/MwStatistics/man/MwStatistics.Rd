\name{MwStatistics}
\alias{MwStatistics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moving Window Statistics
}
\description{
This function allows you to calculate a variety of statistics in a time series using a moving window.
}
\usage{
MwStatistics(dat = NULL, TimeVarCol = 1, ColA = 2, ColB = 3, win = NULL,
stat = NULL, overlap = 0, genplot = T, genDataFrame = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Dataset name
}
  \item{TimeVarCol}{
Column number of time series variable
}
  \item{ColA}{
Column number of first variable
}
  \item{ColB}{
Column number of second variable
}
  \item{win}{
Window size
}
  \item{stat}{
Statistic to calculate
}
  \item{overlap}{
How much the windows overlap
}
  \item{genplot}{
Generate a plot of recorded statistic
}
  \item{genDataFrame}{
Generate dataframe with recorded statistic values
}
}
\details{
N/A
}
\value{
Can generate a plot and/or a dataframe.
}
\references{
Based on mwCor() from astrochron
}
\author{
William Kopans
}
\note{
The statistic you want calculated MUST have quotation marks around it's name.

Here are the possible statistics:

 - "Correlation" = Correlation

 - "P-Value" = P-Value

 - "SDevX" = Standard Deviation of variable A

 - "SDevY" = Standard Deviation of variable B

 - "SErrorSlope" = Standard Error of the slope

 - "SErrorIntercept" = Standard Error of the intercept

 - "Intercept" = Intercept (Linear regression)

 - "Slope" = Slope (Linear regression)
}
\section{Warning }{
For standard error of slope and intercept, make sure to have a large window size to prevent errors.
}

\seealso{
N/A
}
\examples{
MwStatistics(dat = data.frame(replicate(10,sample(0:10,1000,rep=TRUE))),
TimeVarCol = 1, ColA = 2, ColB = 3, win = 2,
stat = "Correlation", genplot = TRUE, genDataFrame = FALSE, overlap = 0.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~datagen }
\keyword{ ~ts }
