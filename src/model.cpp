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
  X { X0.cast<dual1st>() },
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
    Eigen::SimplicialLLT<Eigen::SparseMatrix<dual1st> >& solver
  ){
  VectorXdual1st delta_u{};

  for(int i{}; i < maxit_conditional_modes; i++){

    solver.factorize(get_inner_hessian());
    VectorXdual1st b1 = solver.permutationP() *
      (get_Lambdat() * get_Zt() * (y - meanfun()) - u);
    VectorXdual1st cu = solver.matrixL().solve(b1);
    delta_u = solver.permutationPinv() * solver.matrixU().solve(cu);

    if(delta_u.squaredNorm() < 1e-10) break;
    update_u(delta_u, 1);
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

void GALAMM::Model::update_Zt(){
  if(lambda_mapping_Zt.rows() == 0) return;
  int counter{};
  for(int k{}; k < Zt.outerSize(); ++k){
    for(Eigen::SparseMatrix<dual1st>::InnerIterator it(Zt, k); it; ++it){
      int newind = lambda_mapping_Zt(counter);
      if(newind != -1){
        it.valueRef() = lambda(newind);
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
