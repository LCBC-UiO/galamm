#ifndef UPDATE_H
#define UPDATE_H

#include "model.h"

template <typename T>
void update_Lambdat(Eigen::SparseMatrix<T>& Lambdat,
                    Eigen::Matrix<T, Eigen::Dynamic, 1> theta,
                    const Eigen::VectorXi& theta_mapping
                      ){
  int lind_counter{};
  for (int k{}; k < Lambdat.outerSize(); ++k){
    for (typename Eigen::SparseMatrix<T>::InnerIterator
           it(Lambdat, k); it; ++it)
    {
      int ind = theta_mapping(lind_counter);
      if(ind != -1){
        it.valueRef() = theta(ind);
      }
      lind_counter++;
    }
  }
};

template <typename T>
void update_X(Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& X,
              Eigen::Matrix<T, Eigen::Dynamic, 1> lambda,
              const Eigen::VectorXi& lambda_mapping_X){
  if(lambda_mapping_X.size() == 0) return;
  for(int i = 0; i < X.size(); i++){
    int newind = lambda_mapping_X(i);
    if(newind != -1){
      *(X.data() + i) *= lambda(newind);
    }
  }
};

template <typename T>
void update_Zt(Eigen::SparseMatrix<T>& Zt,
               Eigen::Matrix<T, Eigen::Dynamic, 1> lambda,
               const Eigen::VectorXi& lambda_mapping_Zt){
  if(lambda_mapping_Zt.size() == 0) return;
  int counter{};
  for(int k{}; k < Zt.outerSize(); ++k){
    for(typename Eigen::SparseMatrix<T>::InnerIterator it(Zt, k); it; ++it){
      int newind = lambda_mapping_Zt(counter);
      if(newind != -1){
        it.valueRef() = lambda(newind) * it.value();
      }
      counter++;
    }
  }
};

#endif
