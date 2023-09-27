#ifndef UPDATE_H
#define UPDATE_H

#include "model.h"

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

template <typename T>
void update_X(Mdual<T>& X, const Vdual<T>& lambda,
              const std::vector<std::vector<int>>& lambda_mapping_X){
  if(lambda_mapping_X.size() == 0) return;
  for(int i = 0; i < X.size(); i++){
    std::vector<int> newinds = lambda_mapping_X[i];
    T loading{0};
    bool update{false};
    int j{0};

    for(int newind : newinds){
      if(newind != -1){
        loading += lambda(newind);
        update = true;
      }
      j++;
    }
    if(update){
      *(X.data() + i) *= loading;
    }

  }
};

template <typename T>
void update_Zt(SpMdual<T>& Zt, const Vdual<T>& lambda,
               const std::vector<std::vector<int>>& lambda_mapping_Zt,
               const std::vector<std::vector<double>>& lambda_mapping_Zt_covs = {}){
  if(lambda_mapping_Zt.empty()) return;
  int counter{0};
  for(int k{}; k < Zt.outerSize(); ++k){
    for(typename SpMdual<T>::InnerIterator it(Zt, k); it; ++it){
      std::vector<int> newinds = lambda_mapping_Zt[counter];
      T loading{0};
      bool update{false};
      int inner_counter{0};

      for(int newind : newinds){
        if(newind != -1){
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

template <typename T>
void update_WSqrt(Ddual<T>& WSqrt, const Vdual<T>& weights,
                  const std::vector<int>& weights_mapping){
  if(weights_mapping.size() == 0) return;
  for(int i = 0; i < weights_mapping.size(); i++){
    int newind = weights_mapping[i];
    if(newind != -1){
      WSqrt.diagonal()(i) = sqrt(weights(newind));
    }
  }
}

#endif
