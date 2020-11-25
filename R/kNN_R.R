#'@title kNN_R
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
#'preds = knn(train, test, cl, k = 10, prob=TRUE, use.all = T)
#'probs=attributes(preds)$prob
#'
#'@export
#'
#'
R_helper <- function(k_in, l_in, ntrain, dim, train, cls, test, ncls, cv, use_all) {

  EPSILON = 1e-4

  UNIF_RAND = runif(1) # random generator
  MAX_TIES = 1000 # maximum of allowed ties for distance comparison
  NUM_THREAD = 3 # default number of threads
  MAX_VALUE = 0.99 * 1.79769e+308 # possiblt max value for distance computation and comparison

  ntest = dim(test)[1]
  div = ceiling(ntest / NUM_THREAD)
  res = rep(0, ntest)
  pr = rep(0, ntest)
  k_init = k_in

  pos = rep(0, MAX_TIES)
  nclass = rep(0, MAX_TIES)


  for (n_t in 1:ntest) {
    k_pt = k_init
    dists = rep(MAX_VALUE, MAX_TIES)

    for (j in 1:ntrain) {
      # compute square Euclidean distance between j-th train sample and n_t-th test sample
      dist = sum((test[n_t,] - train[j,])^2)

      # insert the distance into dists if it's currently within k smallest
      if (dist <= dists[k_init] * (1 + EPSILON)){
        for (k in 1:k_pt){
          if (dist < dists[k]) {
            dists[(k+1):(k_pt+1)] = dists[k:k_pt]
            pos[(k+1):(k_pt+1)] = pos[k:k_pt]

            dists[k] = dist;
            pos[k] = j;
            if (dists[k_pt+1] <= dists[k_init])
              k_pt = k_pt + 1
            if (k_pt == MAX_TIES - 1)
              stop("There are too many ties in k nearest neighbors!");
            break;
          }
        }
      }
      dists[k_pt+1] = MAX_VALUE
    }
    # k cloest samples have been selected. Then start counting votes

    # Initialize votes count vector
    votes = rep(0, ncls)

    # use_all is true, "all distances equal to the kth largest are included"
    if (use_all) {
      votes[cls[pos[1:k_init]]] = votes[cls[pos[1:k_init]]] + 1

      extras = 0;
      for (j in k_init:k_pt) {
        if (dists[j] <= dists[k_init] * (1 + EPSILON)){
          extras = extras + 1
          votes[cls[pos[j]]] = votes[cls[pos[j]]] + 1
        }
      }
    }
    # use_all is false, "a random selection of distances equal to the kth
    # is chosen to use exactly k neighbours."
    else {
      extras = 0
      for (j in 1:k_init) {
        if (dists[j] >= dists[k_init] * (1 - EPSILON))
          break
        votes[cls[pos[j]]] = votes[cls[pos[j]]] + 1
      }
      j1 = j
      if (j1 == k_init) { # no ties for largest
        votes[cls[pos[j1]]] = votes[cls[pos[j1]]] + 1
      }
      else {
        needed = k_init - j1 + 1
        nclass[1:needed] = cls[pos[j1 + 1:needed]]
        ti = needed;
        for (j in (k_init + 1):k_pt) {
          if (dists[j] > dists[k_init] * (1 + EPSILON))
            break;
          ti = ti + 1
          if (ti * UNIF_RAND < needed) {
            j2 = j1 + as.integer(UNIF_RAND * needed)
            nclass[j2] = cls[pos[j]]
          }
        }
        votes[nclass[1:needed]] = votes[nclass[1:needed]] + 1
      }
    }

    nties = 1;
    index = 0;
    if (l_in > 0)
      max_vote =  (l_in - 1 + extras)
    else
      max_vote = 0

    for (i in 1:ncls)
      if (votes[i] > max_vote) { #//select larger vote
        nties = 1
        index = i
        max_vote = votes[i]
      }
    # if there is tie, select later one
    else if ((votes[i] == max_vote && votes[i] >= l_in)) {
      nties = nties + 1
      if (nties * UNIF_RAND < 1.0)
        index = i
    }
    res[n_t] = index
    pr[n_t] = max_vote / (k_init + extras)
  }
  return (list(res, pr))
}


kNN_R <- function(train, test, cl, k=1, l=0, prob=FALSE, use.all=TRUE) {
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
  Z = R_helper(as.integer(k),
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
