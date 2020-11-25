#'@title kNN
#'
#'
#'@description Classfication Algorithm of K Nearest Neighbor (kNN)
#'@details Input a training dataset and its corresponding class labels, return the predicted classfications by taking majority votes of k-nearest neighbors' classes.
#'@param train training dataset with m data samples and d dimensions. A matrix of m by d.
#'@param test test dataset with n data samples and d dimensions. A matrix of n by d
#'@param cl training ground-true class labels for m data samples. A vector of length m.
#'@param k the number of nearest neighbors
#'@param l minimum vote to make decision for prediction, i.e. less than k-l dissenting votes are allowed, even if k may be larger than inputted k due to ties.
#'@param prob a boolean. If \code{TRUE}, prediction probability is returned as output's attribute. If \code{FALSE}, only prediction labels are returned.
#'@param use.all controls the way of handling ties. If \code{TRUE}, all distances equal to the kth largest are used. If \code{FALSE}, a random selection of distances equal to the k-th nearest distance is chosen to select exactly k neighbours.
#'@return the predicted classfication of test
#'
#'@examples
#'train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
#'test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
#'cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#'preds = kNN(train, test, cl, k = 10, prob=TRUE, use.all = T)
#'probs=attributes(preds)$prob
#'
#'@export
#'
kNN <- function(train, test, cl, k=1, l=0, prob=FALSE, use.all=TRUE) {
  if(is.null(dim(train)))
    stop("'train' is not a matrix!")
  train <- as.matrix(train)
  if(is.null(dim(test)))
    dim(test) <- c(1, length(test))
  test <- as.matrix(test)
  if(any(is.na(train)) || any(is.na(test)) || any(is.na(cl)))
    stop(gettextf("There is missing value which is not allowed!"))
  size <- dim(train)
  p <- size[2]
  ntr <- size[1]
  if(length(cl) != ntr)
    stop("The number of 'train' samples and the number of 'class' should match!")
  if(ntr < k) {
    warning(gettextf("k = %d exceeds number %d of 'train' samples!", k, ntr),
            domain = NA)
    k <- ntr
  }
  if (k < 1)
    stop(gettextf("k = %d cannot be less than 1!", k), domain = NA)
  if(ncol(test) != p)
    stop("The dimensions of 'test' and 'train' do not match!")
  cls <- as.factor(cl)
  nc <- max(unclass(cls))
  Z = .Call(`_kNN_C_kNN_multi_thread`, as.integer(k),
            as.integer(l),
            as.integer(ntr),
            as.integer(p),
            (train),
            as.integer(unclass(cls)),
            (test),
            as.integer(nc),
            as.integer(FALSE),
            as.integer(use.all)
  )

  res <- factor(unlist(Z[1]), levels=seq_along(levels(cls)),labels=levels(cls))
  if(prob) attr(res, "prob") <- unlist(Z[2])
  return (res)
}
