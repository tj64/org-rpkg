\name{ardec}
\alias{ardec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Time series autoregressive decomposition}
\description{
Decomposition of a time series into latent subseries from a fitted autoregressive model
}
\usage{
ardec(x, coef, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x }{  time series}
  \item{coef }{  autoregressive parameters of AR(p) model }
  \item{\dots }{  additional arguments for specific methods }
}
\details{

If an observed time series can be adequately described by an (eventually high order) autoregressive AR(p) process, a constructive result (West, 1997) yields a time series decomposition in terms of latent components following either AR(1) or AR(2) processes depending on the eigenvalues of the state evolution matrix.

Complex eigenvalues r exp(iw) correspond to pseudo-periodic oscillations as a damped sine wave with fixed period (2pi/w) and damping factor r. Real eigenvalues correspond to a first order autoregressive process with parameter r.

}
\value{
  A list with components: 

  \item{period }{periods of latent components}
  \item{modulus }{damping factors of latent components}
  \item{comps }{matrix of latent components}
}
\references{ 
West, M. (1997), Time series decomposition. Biometrika, 84, 489-494.

West, M. and Harrisson, P.J. (1997), Bayesian Forecasting and Dynamic Models, Springer-Verlag.
}

\author{ S. M. Barbosa }


\examples{

data(tempEng)
coef=ardec.lm(tempEng)$coefficients

# warning: running the next command can be time comsuming!

decomposition=ardec(tempEng,coef)

 }

\keyword{ts}% at least one, from doc/KEYWORDS
