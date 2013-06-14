\name{ardec.periodic}
\alias{ardec.periodic}
% \alias{ardec.periodic.bayes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extraction of individual periodic components from a monthly time series}
\description{

Function ardec.periodic extracts a periodic component from the autoregressive decomposition of a monthly time series.

% Function ardec.periodic.bayes extracts a periodic component from each autoregressive decomposition based on a simulated vector of autoregressive parameters.
}
\usage{
ardec.periodic(x, per, tol = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ time series}
  \item{per}{period of the component to be extracted}
  \item{tol}{tolerance for the period of the component}
%  \item{R}{size of sample to be simulated from posterior}
}

\value{
  A list with components: 
  
  \item{period }{period for the anual component}
  \item{modulus }{damping factor for the annual component}
  \item{component}{extracted component }
%  \item{compSim}{ matrix containing the simulated components as columns (for ardec.periodic.bayes) }

}


\author{S. M. Barbosa}

\examples{
% # warning: running the next command can be time comsuming!

data(tempEng)
ardec.periodic(tempEng,per=12)
% ardec.periodic.bayes(tempEng,per=12,R=2)

}
\keyword{ts}% at least one, from doc/KEYWORDS
