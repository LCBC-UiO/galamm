#include "model.h"
using namespace autodiff;

dual1st GALAMM::Poisson::cumulant(){
    return get_linpred().array().exp().sum();
  }

dual1st GALAMM::Poisson::constfun(){
    return -(y.array() + 1).lgamma().sum();
  }

VectorXdual1st GALAMM::Poisson::meanfun() {
    return get_linpred().array().exp();
  }

void GALAMM::Poisson::update_V(){
  V.diagonal().array() = meanfun().array();
}

void GALAMM::Poisson::update_phi(){
  phi = 1;
}

void GALAMM::Poisson::update_linpred(){
  linpred = X * beta + get_Zt().transpose() * get_Lambdat().transpose() * u;
}
