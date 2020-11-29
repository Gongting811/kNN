#include <Rcpp.h>
#include <stdlib.h>
#include <R.h>
#include <assert.h>
#include <array>
#include <math.h>
#include <float.h>
#include <thread>
#include <mutex>


using namespace Rcpp;
using namespace std;

#define UNIF_RAND unif_rand() // random generator
#define MAX_TIES 1000 // maximum of allowed ties for distance comparison
#define NUM_THREAD 3 // default number of threads
#define MAX_VALUE DOUBLE_XMAX // possiblt max value for distance computation and comparison

mutex myMutex; // initialize mutex object for data protection in multi-thread

// [[Rcpp::export]]
void C_knn_search (int &k_in, int &l_in, int &ntrain, int &dim, NumericMatrix &train,
                   IntegerVector &cls, NumericMatrix &test, IntegerVector &res, NumericVector &pr,
                   int &ncls, int &use_all, int begin, int end) {
  int i, index, j, k, k1, k_init = k_in, k_pt, max_vote, n_t, nties, extra;
  int nclass[MAX_TIES];
  int j1, needed, ti;
  double dist, diff;
  array<pair<int, double>, MAX_TIES> neighbors;
  IntegerVector votes(ncls + 1);

  GetRNGstate();

  for (n_t = begin; n_t < end; n_t++) {
    k_pt = k_init;
    // Initialize dists
    for (k = 0; k < k_pt; k++){
      neighbors[k].second = MAX_VALUE;
      neighbors[k].first = 0;
    }

    for (j = 0; j < ntrain; j++) {
      // compute square Euclidean distance between j-th train sample and n_t-th test sample
      dist = 0.0;
      for (k = 0; k < dim; k++) {
        diff = test(n_t, k) - train(j, k);
        dist += diff * diff;
      }

      // insert the distance into dists if it's currently within k smallest
      if (dist <= neighbors[k_init - 1].second)
        for (k = 0; k <= k_pt; k++)
          if (dist < neighbors[k].second) {
            for (k1 = k_pt; k1 > k; k1--) {
              neighbors[k1] = neighbors[k1 - 1];
            }
            neighbors[k].second = dist;
            neighbors[k].first = j;
            if (neighbors[k_pt].second <= neighbors[k_init - 1].second)
              assert (++k_pt != MAX_TIES - 1);
            // throw std::range_error("There are too many ties in k nearest neighbors!");
            break;
          }
          neighbors[k_pt].second = MAX_VALUE;
    }
    // k cloest samples have been selected. Then start counting votes
    std::lock_guard<std::mutex> myLock(myMutex); // lock data to prevent disorder

    // Initialize votes count vector
    for (j = 0; j <= ncls; j++)
      votes[j] = 0;

    // use_all is true, "all distances equal to the kth largest are included"
    if (use_all) {
      for (j = 0; j < k_pt; j++) {
        votes[cls[neighbors[j].first]]++;
      }
      extra = k_pt - k_init;
    }
    /* use_all is false, "a random selection of distances equal to the kth
    is chosen to use exactly k neighbours." */
    else {
      extra = 0;
      j = 0;
      while (j < k_init) {
        if (neighbors[j].second >= neighbors[k_init - 1].second)
          break;
        votes[cls[neighbors[j].first]]++;
        j++;
      }
      j1 = j;

      if (j1 != k_init - 1) { // there is ties for largest
        needed = k_init - j1;
        for (j = 0; j < needed; j++)
          nclass[j] = cls[neighbors[j1 + j].first];
        ti = needed;

        for (j = 0; j < needed; j++)
          votes[nclass[j]]++;
      }
      else { // no ties for largest
        votes[cls[neighbors[j1].first]]++;
      }
    }

    nties = 1;
    index = 0;
    max_vote = (l_in > 0) ? (l_in - 1 + extra) : 0;

    for (i = 1; i <= ncls; i++)
      if (votes[i] > max_vote) { //select larger vote
        nties = 1;
        index = i;
        max_vote = votes[i];
      }
      // if there is tie, select later one
      else if ((votes[i] == max_vote && votes[i] >= l_in) && (++nties * UNIF_RAND < 1.0)) {
        index = i;
      }
      res[n_t] = index;
      pr[n_t] = (double) max_vote / (k_init + extra);
  }
  PutRNGstate();
}

// [[Rcpp::export]]
List C_kNN_multi_thread(int &k_in, int &l_in, int &ntrain, int &dim,
                        NumericMatrix &train, IntegerVector &cls, NumericMatrix &test,
                        int &ncls, int &use_all) {
  int ntest = test.nrow(), begin, end;
  int div = ceil(ntest / NUM_THREAD);
  IntegerVector res(ntest);
  NumericVector pr(ntest);

  array<thread, NUM_THREAD> threads;
  for (int thread_id = 0; thread_id < NUM_THREAD; thread_id++) {
    begin = thread_id*div;
    end = begin+div;
    if (thread_id == NUM_THREAD - 1)
      end = ntest;
    threads[thread_id] = thread(C_knn_search, ref(k_in), ref(l_in), ref(ntrain), ref(dim),
                                ref(train), ref(cls), ref(test), ref(res), ref(pr),
                                ref(ncls), ref(use_all), begin, end);
  }
  for( auto & t : threads ) {
    t.join();
  }

  return List::create( res, pr );
}
