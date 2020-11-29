# kNN
[![Build Status](https://www.travis-ci.org/Gongting811/kNN.svg?branch=master)](https://www.travis-ci.org/Gongting811/kNN)
[![codecov](https://codecov.io/gh/Gongting811/kNN/branch/master/graph/badge.svg?token=hPgaD0LL52)](https://codecov.io/gh/Gongting811/kNN)

## usage

### Install
```R=T
install.package("kNN")
```
### Example

#### kNN_R

kNN_R is kNN implemented purely in R. This is slow because R is slower than C++. 

```R=T
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
preds = kNN_R(train, test, cl, k = 10, prob=TRUE, use.all = T)
probs=attributes(preds)$prob
```

#### kNN

kNN is kNN implemented by R and Rcpp and accelerated by multi-thread. This method is about 10x faster than kNN_R.

```R=T
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
preds = kNN_R(train, test, cl, k = 10, prob=TRUE, use.all = T)
probs=attributes(preds)$prob
```
#### Correctness and Efficiency
```R=T
library(class)

Rcpp::compileAttributes()
devtools::document()
usethis::use_rcpp()
devtools::build()
devtools::install()

d = 2
m = d*2000
train <- matrix(runif(m*40, -1000, 1000), m, 40)
cl <- factor(c(rep(1,m/d), rep(2,m/d)))
test <- matrix(runif(m/10*40, -1000, 1000), m/10, 40)

#
# d = 2
# m = d*2000
# train <- matrix(rep(runif(40, -1000, 1000), m), m, 40)
# test <- matrix(rep(runif(40, -1000, 1000), m/10), m/10, 40)
# cl <- factor(c(rep(1,m/d), rep(2,m/d)))
#

start = Sys.time()
r1=knn(train, test, cl, k = 11, prob=TRUE, use.all = F)
end = Sys.time()
print(end-start) #Time difference of 0.06614304 secs

start = Sys.time()
r2=kNN_R(train, test, cl, k = 11, prob=TRUE, use.all = F)
end = Sys.time()
print(end-start) #Time difference of 5.547898 secs

start = Sys.time()
r=kNN(train, test, cl, k = 11, prob=TRUE, use.all = F)
end = Sys.time()
print(end-start) #Time difference of 0.02477503 secs

sum(r1 != r2)
sum(r1 != r)
sum(r != r2)
sum(attributes(r)$prob != attributes(r1)$prob)
sum(attributes(r)$prob != attributes(r2)$prob)
sum(attributes(r1)$prob != attributes(r2)$prob)

```
knn is the existing implementation from Package "Class". And the time it takes to run on the simulated data is 0.06614304 secs.

The kNN_R is the one implemented by R. So it is much slower and takes 5.547898 secs. It is about 80x slower than knn.

The kNN is the kNN implementation by Rcpp and accelerated by multi-thread in C++. It is even 2x-3x faster than existing implemented knn due to multi-thread acceleration, and it takes 0.02477503 secs. This is because I set 3 as the number of threads.

#### Manual
The instruction of kNN is at https://github.com/Gongting811/kNN/blob/master/man/kNN.Rd, or using
```R=T
help (kNN)
```

The instruction of kNN_R is at https://github.com/Gongting811/kNN/blob/master/man/kNN_R.Rd, or using
```R=T
help (kNN)
```
#### Further example

https://github.com/Gongting811/kNN/blob/master/vignettes/kNN.Rmd
