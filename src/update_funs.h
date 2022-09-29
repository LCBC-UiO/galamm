#ifndef UPDATE_H
#define UPDATE_H

#include "model.h"

template <typename T>
void update_Lambdat(SpMdual<T>& Lambdat, Vdual<T> theta,
                    const Eigen::VectorXi& theta_mapping
                      ){
  int lind_counter{};
  for (int k{}; k < Lambdat.outerSize(); ++k){
    for (typename SpMdual<T>::InnerIterator
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
void update_X(Mdual<T>& X, Vdual<T> lambda,
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
void update_Zt(SpMdual<T>& Zt, Vdual<T> lambda,
               const Eigen::VectorXi& lambda_mapping_Zt){
  if(lambda_mapping_Zt.size() == 0) return;
  int counter{};
  for(int k{}; k < Zt.outerSize(); ++k){
    for(typename SpMdual<T>::InnerIterator it(Zt, k); it; ++it){
      int newind = lambda_mapping_Zt(counter);
      if(newind != -1){
        it.valueRef() = lambda(newind) * it.value();
      }
      counter++;
    }
  }
};

template <typename T>
void update_WSqrt(Ddual<T>& WSqrt, Vdual<T> weights,
                  const Eigen::VectorXi& weights_mapping){
  if(weights_mapping.size() == 0) return;
  for(int i = 0; i < weights_mapping.size(); i++){
    int newind = weights_mapping(i);
    if(newind != -1){
      WSqrt.diagonal()(i) = sqrt(weights(newind));
    }
  }
}

#endif
