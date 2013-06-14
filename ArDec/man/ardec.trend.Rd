\name{ardec.trend}
\alias{ardec.trend}
% \alias{ardec.trend.bayes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimation of the trend component from a monthly time series}
\description{
  Function ardec.trend extracts the trend component from the autoregressive decomposition of a monthly time series.

%  Function ardec.trend.bayes extracts the trend component from each autoregressive decomposition based on a simulated vector of autoregressive parameters.
}
\usage{
ardec.trend(x)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{time series}
%  \item{R}{size of sample to be simulated from posterior}

}

\value{
 
  A list with components: 

  \item{modulus }{damping factor for the annual component}
  \item{trend}{trend component }
%  \item{trendSim}{ matrix containing the simulated trend components as columns (for ardec.trend.bayes) }
}

\author{S. M. Barbosa}

\examples{
%# warning: running the next command can be time comsuming!

data(co2)
ardec.trend(co2)
% ardec.trend.bayes(co2,2)

}

\keyword{ ts }% at least one, from doc/KEYWORDS