#include "model.h"
using namespace autodiff;

dual1st GALAMM::Binomial::cumulant(){
    return ((1 + get_linpred().array().exp()).log() * trials.array()).sum();
  }

dual1st GALAMM::Binomial::constfun(){
    return (lgamma(trials.array() + 1) - lgamma(y.array() + 1) -
            lgamma(trials.array() - y.array() + 1)).sum();
  }

VectorXdual1st GALAMM::Binomial::meanfun() {
    return get_linpred().array().exp() / (1 + get_linpred().array().exp()) *
      trials.array();
  }

void GALAMM::Binomial::update_V(){
  V.diagonal().array() = meanfun().array() / trials.array() * (trials.array() - meanfun().array());
}

void GALAMM::Binomial::update_phi(){
  phi = 1;
}

void GALAMM::Binomial::update_linpred(){
  linpred = get_X() * beta + get_Zt().transpose() * get_Lambdat().transpose() * u;
}
