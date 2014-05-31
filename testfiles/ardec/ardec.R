## * ardec
## ** meta
##   :PROPERTIES:
##   :title:    Time series autoregressive decomposition
##   :name:     ardec
##   :ppName:   ArDec
##   :alias:    ardec
##   :author:   S. M. Barbosa
##   :keyword:  ts
##   :END:

## ** man
## *** Description

## Decomposition of a time series into latent subseries from a fitted
## autoregressive model

## *** Usage

## #+begin_example
##  ardec(x, coef, ...)
## #+end_example

## *** Arguments

## - x ::  time series
## - coef :: autoregressive parameters of AR(p) modebl
## - ... :: additional arguments for specific methods

## *** Details

## If an observed time series can be adequately described by an (eventually high
## order) autoregressive AR(p) process, a constructive result (West, 1997) yields
## a time series decomposition in terms of latent components following either
## AR(1) or AR(2) processes depending on the eigenvalues of the state evolution
## matrix.

## Complex eigenvalues r exp(iw) correspond to pseudo-periodic oscillations as a
## damped sine wave with fixed period (2pi/w) and damping factor r. Real
## eigenvalues correspond to a first order autoregressive process with parameter
## r.

## *** Value

## A list with components: 

## - period :: periods of latent components
## - modulus :: damping factors of latent components
## - comps :: matrix of latent components

## *** References

## #+begin_latex
##   \cite{west1997time}
##   \linebreak{}
##   \cite{west1997bayesian}
## #+end_latex

## *** Examples

data(tempEng)
coef=ardec.lm(tempEng)$coefficients

## warning: running the next command can be time comsuming!
decomposition=ardec(tempEng,coef)

## ** R

ardec <- function(x,coef, ...) {

    dat=x-mean(x)
    p=length(coef)
    ndat=length(x)

    G=matrix(nrow=p,ncol=p)

    G[1,]=coef
    G[seq(2,p),seq(1,p-1)]=diag(1,(p-1))
    G[seq(2,p),p]=0

    modulus=Mod(eigen(G)[[1]])
    lambda=2*pi/Arg(eigen(G)[[1]])

    eigenvalues=eigen(G)
    A=diag(eigenvalues[[1]])
    E=eigenvalues[[2]]
    B=solve(E)
    F=rep(NA,p)
    F[1]=1
    F[seq(2,p)]=0
    a=t(E) %*% F
    d=diag(as.vector(a))
    H=d %*% B

    Z=matrix(nrow=p,ncol=ndat)
    Z[1,]=dat
    for (i in seq(1,p-1)){
        Z[i+1,]=as.vector(filter(dat,c(rep(0,i),1), method="convolution",
        sides=1))}

    g=matrix(nrow=p,ncol=ndat)
    for (j in seq(1,p)){
        for (t in seq(1,ndat)){
            g[j,t]=H[j, ] %*% Z[,t] }}


    return(list(period=lambda,modulus=modulus,comps=g))
}


## ** data

## * ardec.lm
## ** meta
##   :PROPERTIES:
##   :title:    Fit an autoregressive model as a linear regression
##   :name:     ardec.lm
##   :alias:    ardec.lm
##   :seealso:  ar lm
##   :END:

## ** man
## *** Description

## Function ardec.lm fits an autoregressive model of order p, AR(p) to a time
## series through a linear least squares regression.

## #+begin_comment
## Function ardec.lm.bayes provides a sample of autoregressive parameters from
## the multivariate normal posterior distribution for the coefficients assuming a
## (non-informative) reference prior.
## #+end_comment

## *** Usage

## #+begin_example
##  ardec.lm(x)
## #+end_example

## *** Arguments

## #+begin_example
##   x:: time series
##   R:: size of sample to be simulated from posterior
## #+end_example

## #+begin_comment
##   med:: logical, indicating if a median vector of autoregressive parameter
##      should be computed from the simulated sample}
## #+end_comment

## *** Value

## #+begin_example

##   For ardec.lm, an object of class "lm".

##   # For ardec.lm.bayes an Rxp matrix containing the samples of autoregressive
##   # coefficients as columns (if med=FALSE).

##   # If med=TRUE, ardec.lm.bayes returns a single column matrix containing the
##   # median vector of autoregressive parameters.

## #+end_example

## *** References

## \cite{west1995bayesian}

## *** Examples
## ** R

ardec.lm <- function(x) {

    require(stats)

    dat=x-mean(x)
    ndat=length(dat)
    ## p=order of autoregressive model from AIC (burg method)
    p=ar(dat,method="burg")[[1]]
    ## linear autoregressive model fit
    X=t(matrix(dat[rev(rep((1:p),ndat-p)+
    rep((0:(ndat-p-1)),rep(p,ndat-p)))],p,ndat-p))
    y=rev(dat[(p+1):ndat])
    fit=lm(y~-1+X, x=TRUE)

    return(fit)
}

## * ardec.periodic
## ** meta
##   :PROPERTIES:
##   :title:    Time series autoregressive decomposition
##   :name:     ardec
##   :ppName:   ArDec
##   :alias:    ardec
##   :author:   S. M. Barbosa
##   :keyword:  ts
##   :END:

## ** man
## *** Description

## Function ardec.periodic extracts a periodic component from the autoregressive
## decomposition of a monthly time series.

## #+begin_comment 
## Function ardec.periodic.bayes extracts a periodic component from each
## autoregressive decomposition based on a simulated vector of autoregressive
## parameters.
## #+end_comment

## *** Usage

## #+begin_example
##   ardec.periodic(x, per, tol = 0.95)
## #+end_example

## *** Arguments

## - x :: time series
## - per :: period of the component to be extracted
## - tol :: tolerance for the period of the component
## #+begin_comment
## - R :: size of sample to be simulated from posterior
## #+end_comment

## *** Value

## A list with components: 
  
## period :: period for the anual component
## modulus :: damping factor for the annual component
## component :: extracted component 
## #+begin_comment
## compSim :: matrix containing the simulated components as columns (for
## ardec.periodic.bayes)
## #+end_comment

## *** Examples

  ## warning: running the next command can be time comsuming!
  data(tempEng)
  ardec.periodic(tempEng,per=12)
  ## ardec.periodic.bayes(tempEng,per=12,R=2)

## ** R

ardec.periodic <- function(x,per,tol=0.95){

    ## if(frequency(x)!=12){stop("monthly time series required")}
    ## updated 29 Apr 2013

    fit=ardec.lm(x)

    comp=ardec(x,fit$coefficients)

    if(any(comp$period > (per-tol) & comp$period < (per+tol))) {
        candidates=which(comp$period > (per-tol) & comp$period < (per+tol))
        lper=candidates[which.max(comp$modulus[candidates])]
        l=comp$period[lper]
        m=comp$modulus[lper]
        gt=Re(comp$comps[lper,]+comp$comps[lper+1,])   }

    return(list(period=l,modulus=m,component=gt))

}

## * ardec.trend
## ** meta
##    :PROPERTIES:
##    :title:    Estimation of the trend component from a monthly time series
##    :name:     ardec.trend
##    :alias:    ardec.trend
##    :END:

## ** man
## *** Description

##     Function ardec.trend extracts the trend component from the autoregressive
##     decomposition of a monthly time series.

## #+begin_comment
##     Function ardec.trend.bayes extracts the trend component from each
##     autoregressive decomposition based on a simulated vector of autoregressive
##     parameters.
## #+end_comment

## *** Usage

## #+begin_example
##     ardec.trend(x)
## #+end_example

## *** Arguments

##     x :: time series
## #+begin_comment
##     R :: size of sample to be simulated from posterior
## #+end_comment

## *** Value

##     A list with components:

##     modulus :: damping factor for the annual component
##     trend:: trend component
## #+begin_comment
##     trendSim:: matrix containing the simulated trend components as columns
##     (for ardec.trend.bayes)
## #+end_comment

## *** Examples
  ## warning: running the next command can be time comsuming!
  data(co2)
  ardec.trend(co2)
  ## ardec.trend.bayes(co2,2)

## ** R

ardec.trend <- function(x){

    options(warn=-1)

    fit=ardec.lm(x)


    comp=ardec(x,fit$coefficients)

    if(any(comp$period==Inf)){warning("no trend component")}


    if(any(comp$period ==Inf)){
        l=comp$period[which(match(comp$period,Inf)==1)[1]]
        m=comp$modulus[which(match(comp$period,Inf)==1)[1]]
        gt=Re(comp$comps[which(match(comp$period,Inf)==1 )[1],])

    }

    return(list(modulus=m,trend=gt))

}

## * tempEng
## ** meta
##   :PROPERTIES:
##   :title:    Time series of monthly temperature values
##   :name:     tempEng
##   :alias:    tempEng
##   :keyword:  datasets
##   :END:

## ** man
## *** Description

##   Monthly temperature in Central England from 1723-1970

## *** Usage

## #+begin_example
##   data(tempEng)
## #+end_example

## *** Format

##  Time-Series [1:2976] from 1723 to 1971

## *** Examples

  data(tempEng)
  ## maybe str(tempEng) ; plot(tempEng) ...

## ** R

## \bibliographystyle{apalike}
## \bibliography{ardec}
