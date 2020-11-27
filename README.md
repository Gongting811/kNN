# kNN

## usage

### Install
```R=T
install.package("kNN")
```
### Example

#### kNN_R

kNN_R is kNN implemented purely in R.

```R=T
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
preds = kNN_R(train, test, cl, k = 10, prob=TRUE, use.all = T)
probs=attributes(preds)$prob
```

#### kNN

kNN is kNN implemented by R and Rcpp and accelerated by multi-thread.

```R=T
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
preds = kNN_R(train, test, cl, k = 10, prob=TRUE, use.all = T)
probs=attributes(preds)$prob
```
