

#### Play with 1-D and 2-D copula

## Author: Nafis Sadat
## Updated: June 8, 2017


##### V1: Get two random distributions and try and induce rank correlation (1-dim copula)

rm(list=ls())

require(foreign)
require(readstata13)
require(data.table)
require(ggplot2)
require(mvtnorm)
require(MASS)

### Let's simulate a normal and a gamma distribution each.


set.seed(11)
N <- 10000
dt1 <- data.table(id = c(1:N), dist1 = rnorm(N), dist2 = rgamma(N, 1.5, 2))


### What is the current Spearman and Pearson corr values now?
cor(dt1[,2:3], method= "spearman"); cor(dt1[,2:3], method = "pearson");


## Since this is a 2-D dataset, let's create the required correlation matrix we want to induce
corr_mat <- matrix(c(1, 0.9, 0.9, 1), nrow=2)

## We will simulate a multivariate normal distribution with the specific correlation structure; 
### the distribution info is irrelevant


draw1Dcopula <- function(X, corr_mat, print=FALSE){
    mvdat <- t(mvrnorm(n=dim(X)[1], mu=0 * 1:dim(X)[2], Sigma=corr_mat))
    ranks <- t(apply(mvdat, 1, rank, ties.method="first"))
    sorted_X <- apply(X, 2, sort)
    sapply(1:dim(X)[2], function(x) sorted_X[,x][as.vector(ranks[x,])])
}


dt1_post_copula <- data.table(draw1Dcopula(dt1[,2:3], corr_mat))
colnames(dt1_post_copula) <- c("dist1_sorted", "dist2_sorted")

## Compare summary stats
summary(dt1)
summary(dt1_post_copula)

cor(dt1_post_copula, method = "spearman")
cor(dt1_post_copula, method = "pearson")




### Plot stuff

### Histogram of data before sorting

ggplot(dt1) +
	geom_histogram(   aes(x = dist1), bins = 100, fill = "steelblue", alpha = 0.4) +
	geom_histogram(   aes(x = dist2), bins = 100, fill = "red3", alpha = 0.4)  

### Histogram of data after sorting
ggplot(dt1_post_copula) +
	geom_histogram( aes(x = dist1_sorted), bins = 100, fill = "steelblue", alpha = 0.5) +
	geom_histogram( aes(x = dist2_sorted), bins = 100, fill = "red3", alpha = 0.5) 


## Scatters	of distributions before and after
ggplot() +
	geom_point(data = dt1, aes(x = dist1, y=dist2), color = "blue2", fill="steelblue", alpha = .5) +
	geom_point(data = dt1_post_copula, aes(x = dist1_sorted, y=dist2_sorted), color = "red3", fill="red", alpha = .5)





##### V2: Using a 2-D copula to coerce rank correlation in one dimensions, but keeping correlation in another specified one (e.g. time)


# Here's a function for creating AR draws each draw has an AR process for x years (and therefore specifying a time correlation)
simulate_time_series <- function(years, draws, corr, print=FALSE){
  X <- matrix(runif(draws*years), nrow=years, ncol=draws)
  corr_mat <- corr**abs(outer(0:(years-1), 0:(years-1), "-"))
  mvdat <- t(mvrnorm(n=draws, mu=0 * 1:years, Sigma=corr_mat, empirical=TRUE))
  ranks <- t(apply(mvdat, 1, rank, ties.method="first"))
  sorted_X <- t(apply(X, 1, sort))
  t(sapply(1:years, function(x) sorted_X[x,][ranks[x,]]))
}



draws <- 10000 # number of draws
years <- 25 # number of years
corr <- .98 # correlation over time we want
ages <- 10 # number of age groups we have
age_corr <- .75 # correlation we want between adjacent age groups



# simulate draws of AR time series for independent age groups
age_simulations <- lapply(1:ages, function(x) simulate_time_series(years, draws, corr))

# Restructure data so it is in 3D array with dims being c(time, age, draws)
Xtad <- aperm(array(c(sapply(age_simulations, function(x) x)), 
                    dim=c(years, draws,ages)), c(1, 3, 2))

# check out the correlation over time it looks good thanks to first function
sapply(1:ages, function(x) sapply(2:years, function(y)
  cor(Xtad[y,x,], Xtad[y-1,x,])))

# since ages were created independently however they are uncorrelated
sapply(2:ages, function(x) sapply(1:years, function(y)
  cor(Xtad[y,x,], Xtad[y,x-1,])))

# here I am going to devise a corr mat I want for ages but this is most likely going to be derived from data for other cases

## Using the same AR decaying matrix formula as in the time series function:
corr_mat <- age_corr**abs(outer(0:(ages-1), 0:(ages-1), "-"))



# this is the function that takes a 3D array and leaves the first dimension (in
# our test case that dimension is time) unchnaged while sorting the 
# 3rd dimension (this is probably always gonna be the draws dimension) 
# in order to get the desired correlation in the 2nd dimension (for us it is
# age but it could just as easily be country or cause or SDI component whatever)
draw2Dcopula <- function(X, cor_mat){
  L <- dim(X)[2]
  D <- dim(X)[3]
  Xsum <- apply(X, c(2, 3), sum)
  mvdat <- mvrnorm(n=D, mu=0 * 1:L, Sigma=cor_mat, empirical=TRUE)
  ranks <- apply(mvdat, 2, rank, ties.method="first")
  sortedXsim <- apply(Xsum, 1, function(x) sort(x, index.return=TRUE)$ix)
  sortedX <- Xtad
  for(i in 1:L){
    sortedX[,i,] <- Xtad[,i,sortedXsim[,i]]
  }
  Xcorr <- sortedX
  for(i in 1:L){
    Xcorr[,i,] <- sortedX[,i,ranks[,i]]
  }
  Xcorr
}

# lets 2D copulate the data now
Xcorr <- draw2Dcopula(Xtad, corr_mat)

# correlation over our first dimension (time) remains unchanged
sapply(1:ages, function(x) sapply(2:years, function(y)
  cor(Xcorr[y,x,], Xcorr[y-1,x,])))

# correlation over our second dimension (age) now approaches desired target somewhat
sapply(2:ages, function(x) sapply(1:years, function(y)
  cor(Xcorr[y,x,], Xcorr[y,x-1,])))








