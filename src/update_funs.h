#ifndef UPDATE_H
#define UPDATE_H

#include "model.h"

//' Update Cholesky factor
//'
//' Updates the Cholesky factor of the covariance matrix of the random effects
//' based on the current values of \code{theta}. Template is typically one of
//' \code{double}, \code{autodiff:dual1st}, or \code{autodiff::dual2nd}.
//'
//' @srrstats {G1.4a} Internal function documented.
//'
//' @param Lambda Lower Cholesky factor, an object of class
//'   \code{Eigen::SparseMatrix<T>}.
//' @param theta Vector of unique elements of the Cholesky factor, an object of
//'   class \code{Eigen::Matrix<T, Eigen::Dynamic, 1>}.
//' @param theta_mapping Integer vector mapping elements of \code{theta} to the
//'   positions in \code{Lambdat}.
//' @return No return value. \code{Lambdat} is modified in-place.
//'
//' @noRd
template <typename T>
void update_Lambdat(SpMdual<T>& Lambdat, Vdual<T> theta,
                    const std::vector<int>& theta_mapping
                      ){
  int lind_counter{};
  for (int k{}; k < Lambdat.outerSize(); ++k){
    for (typename SpMdual<T>::InnerIterator
           it(Lambdat, k); it; ++it)
    {
      int ind = theta_mapping[lind_counter];
      if(ind != -1){
        it.valueRef() = theta(ind);
      }
      lind_counter++;
    }
  }
};

//' Update fixed effect matrix
//'
//' Updates the fixed effect design matrix \code{X} based on the current values
//' of the factor loadings in \code{lambda}. Template is typically one of
//' \code{double}, \code{autodiff:dual1st}, or \code{autodiff::dual2nd}.
//'
//' @srrstats {G1.4a} Internal function documented.
//'
//' @param X Design matrix of class
//'   \code{Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>}.
//' @param lambda Vector of factor loadings, of class
//'   \code{Eigen::Matrix<T, Eigen::Dynamic, 1>}.
//' @param lambda_mapping_X Integer vector mapping elements of
//'   \code{lambda} to elements of \code{X}, in row-major order.
//' @return No return value. \code{X} is modified in-place.
//'
//' @noRd
template <typename T>
void update_X(Mdual<T>& X, const Vdual<T>& lambda,
              const Eigen::VectorXi& lambda_mapping_X){
  if(lambda_mapping_X.size() == 0) return;
  if(lambda_mapping_X.size() != X.size()) Rcpp::stop("Mismatch in lambda_mapping_X size.");

  for(size_t i{}; i < lambda_mapping_X.size(); i++){
    int newind = lambda_mapping_X[i];
    T loading{0};
    bool update{false};
    if(newind == -2) {
      loading = 0;
      update = true;
    } else if(newind >= 0) {
      loading += lambda(newind);
      update = true;
    }

    if(update){
      *(X.data() + i) *= loading;
    }


  }
};

//' Update random effect matrix
//'
//' Updates the random effect design matrix \code{X} based on the current values
//' of the factor loadings in \code{lambda}. Template is typically one of
//' \code{double}, \code{autodiff:dual1st}, or \code{autodiff::dual2nd}.
//'
//' @srrstats {G1.4a} Internal function documented.
//'
//' @param Zt Transpose of design matrix for random effects, of class
//'   \code{Eigen::SparseMatrix<T>}.
//' @param lambda Vector of factor loadings, of class
//'   \code{Eigen::Matrix<T, Eigen::Dynamic, 1>}.
//' @param lambda_mapping_Zt Vector of integer vectors mapping elements of
//'   \code{lambda} to non-zero elements of \code{Zt} assuming compressed
//'   sparse column format is used. If \code{lambda_mapping_Zt_covs} is of
//'   length zero, then each outer element in \code{lambda_mapping_Zt} should be
//'   of length one, and it will then be multiplied by the corresponding element
//'   of \code{Zt}.
//' @param lambda_mapping_Zt_covs Vectir of double precision vector. Must either
//'   be of length zero, or the same length as \code{lambda_mapping_Zt_covs}.
//'   Each element contains potential covariates that the elements of
//'   \code{lambda_mapping_Zt} should be multiplied with. If the vector is of
//'   length 0, all elements of \code{lambda_mapping_Zt} are implicitly
//'   multiplied by 1.
//' @return No return value. \code{Zt} is modified in-place.
//'
//' @noRd
template <typename T>
void update_Zt(SpMdual<T>& Zt, const Vdual<T>& lambda,
               const std::vector<std::vector<int>>& lambda_mapping_Zt,
               const std::vector<std::vector<double>>& lambda_mapping_Zt_covs = {}){
  if(lambda_mapping_Zt.empty()) return;
  int counter{0};
  for(size_t k{}; k < Zt.outerSize(); ++k){
    for(typename SpMdual<T>::InnerIterator it(Zt, k); it; ++it){
      std::vector<int> newinds = lambda_mapping_Zt[counter];
      T loading{0};
      bool update{false};
      int inner_counter{0};

      for(int newind : newinds){
        if(newind >= 0){
          double cov{1};
          if(!lambda_mapping_Zt_covs.empty()){
            cov = lambda_mapping_Zt_covs[counter][inner_counter];
          }

          loading += lambda(newind) * cov;
          update = true;
        }
        inner_counter++;
      }
      if(update){
        it.valueRef() = loading * it.value();
      }
      counter++;
    }
  }
};

//' Update weight matrix
//'
//' Updates the diagonal matrix \eqn{W} containing weights. Template is
//' typically one of \code{double}, \code{autodiff:dual1st}, or
//' \code{autodiff::dual2nd}.
//'
//' @srrstats {G1.4a} Internal function documented.
//'
//' @param WSqrt Diagonal matrix containing the square roots of the estimated
//'   weights on its diagonal, of type
//'   \code{Eigen::DiagonalMatrix<T, Eigen::Dynamic>}.
//' @param weights Vector with weights parameters.
//' @param weights_mapping Integer vector mapping the elements of \code{weights}
//'   to the rows of \code{X}, or equivalents, to the diagonal of \code{WSqrt}.
//' @return No return value. \code{WSqrt} is modified in-place.
//'
//' @noRd
template <typename T>
void update_WSqrt(Ddual<T>& WSqrt, const Vdual<T>& weights,
                  const std::vector<int>& weights_mapping){
  if(weights_mapping.size() == 0) return;
  for(size_t i{}; i < weights_mapping.size(); i++){
    int newind = weights_mapping[i];
    if(newind >= 0){
      WSqrt.diagonal()(i) = sqrt(weights(newind));
    }
  }
}

#endif
