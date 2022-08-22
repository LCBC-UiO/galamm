#include "model.h"

using namespace autodiff;


GALAMM::Model::Model(
  const Eigen::VectorXd& y0,
  const Eigen::VectorXd& trials0,
  const Eigen::MatrixXd& X0,
  const Eigen::MappedSparseMatrix<double>& Zt0,
  const Eigen::MappedSparseMatrix<double>& Lambdat0,
  const Eigen::VectorXd& beta0,
  const Eigen::VectorXd& theta0,
  const Eigen::VectorXi& theta_mapping0,
  const Eigen::VectorXd& lambda0,
  const Eigen::VectorXi& lambda_mapping_X0,
  const Eigen::VectorXi& lambda_mapping_Zt0,
  const int maxit_conditional_modes0
) : y { y0 },
  trials { trials0 },
  X_init { X0.cast<dual1st>() },
  X { X0.cast<dual1st>() },
  Zt_init { Zt0.cast<dual1st>() },
  Zt { Zt0.cast<dual1st>() },
  Lambdat { Lambdat0.cast<dual1st>() },
  beta { beta0.cast<dual1st>() },
  theta { theta0.cast<dual1st>() },
  theta_mapping { theta_mapping0 },
  lambda { lambda0.cast<dual1st>() },
  lambda_mapping_X { lambda_mapping_X0 },
  lambda_mapping_Zt { lambda_mapping_Zt0 },
  maxit_conditional_modes { maxit_conditional_modes0 }
{
  n = X.rows();
  p = X.cols();
  q = Zt.rows();
  u = VectorXdual1st::Zero(q);
  V = Eigen::DiagonalMatrix<dual1st, Eigen::Dynamic>(n);
}

void GALAMM::Model::get_conditional_modes(
    Eigen::SimplicialLDLT<Eigen::SparseMatrix<dual1st> >& solver
  ){
  VectorXdual1st delta_u{};
  solver.factorize(get_inner_hessian());
  dual1st deviance_prev = -2 * (exponent_g() - solver.vectorD().array().log().sum() / 2);
  dual1st deviance_new;


  for(int i{}; i < maxit_conditional_modes; i++){
    delta_u = solver.solve((get_Lambdat() * get_Zt() * (y - meanfun()) - u));
    if(delta_u.squaredNorm() < 1e-10) break;

    double step = 1;
    for(int j{}; j < 10; j++){
      update_u(delta_u, step);
      solver.factorize(get_inner_hessian());
      deviance_new = -2 * (exponent_g() - solver.vectorD().array().log().sum() / 2);
      if(deviance_new < deviance_prev){
        break;
      }
      update_u(delta_u, -step);
      step /= 2;
    }

    deviance_prev = deviance_new;
  }
}

dual1st GALAMM::Model::exponent_g(){
  return (y.dot(get_linpred()) - cumulant()) / get_phi() + constfun() -
    u.squaredNorm() / 2 / get_phi();
}

void GALAMM::Model::update_inner_hessian(){
  inner_hessian = (1 / get_phi()) *
    get_Lambdat() * get_Zt() * get_V() *
    get_Zt().transpose() * get_Lambdat().transpose();
}

Eigen::SparseMatrix<dual1st>& GALAMM::Model::get_inner_hessian(){
  update_inner_hessian();
  return inner_hessian;
}

void GALAMM::Model::update_Lambdat(){
  int lind_counter{};
  for (int k{}; k < Lambdat.outerSize(); ++k){
    for (Eigen::SparseMatrix<dual1st>::InnerIterator
           it(Lambdat, k); it; ++it)
    {
      it.valueRef() = theta(theta_mapping(lind_counter));
      lind_counter++;
    }
  }
}

void GALAMM::Model::update_X(){
  if(lambda_mapping_X.size() == 0) return;
  X = X_init;
  if(lambda_mapping_X.size() == 0) return;
  for(int i = 0; i < X.size(); i++){
    int newind = lambda_mapping_X(i);
    if(newind != -1){
      *(X.data() + i) *= lambda(newind);
    }
  }
}

void GALAMM::Model::update_Zt(){
  if(lambda_mapping_Zt.size() == 0) return;
  Zt = Zt_init;
  int counter{};
  for(int k{}; k < Zt.outerSize(); ++k){
    for(Eigen::SparseMatrix<dual1st>::InnerIterator it(Zt, k); it; ++it){
      int newind = lambda_mapping_Zt(counter);
      if(newind != -1){
        it.valueRef() = lambda(newind) * it.value();
      }
      counter++;
    }
  }
}

Eigen::SparseMatrix<dual1st>& GALAMM::Model::get_Lambdat(){
  update_Lambdat();
  return Lambdat;
}

Eigen::SparseMatrix<autodiff::dual1st>& GALAMM::Model::get_Zt(){
  update_Zt();
  return Zt;
}

Eigen::MatrixXdual1st& GALAMM::Model::get_X(){
  update_X();
  return X;
}

Eigen::DiagonalMatrix<dual1st, Eigen::Dynamic>&
  GALAMM::Model::get_V(){
    update_V();
    return V;
  }

dual1st& GALAMM::Model::get_phi(){
  update_phi();
  return phi;
}

VectorXdual1st& GALAMM::Model::get_linpred(){
  update_linpred();
  return linpred;
}

void GALAMM::Model::update_u(const VectorXdual1st& delta_u,
                             double alpha_bar){
  u += alpha_bar * delta_u;
}

void GALAMM::Model::update_linpred(){
  linpred = get_X() * beta + get_Zt().transpose() * get_Lambdat().transpose() * u;
}