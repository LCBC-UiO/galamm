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

void GALAMM::Model::get_conditional_modes(Eigen::SimplicialLLT<Eigen::SparseMatrix<dual1st> >& solver){
  VectorXdual1st delta_u{};

  for(int i{}; i < maxit_conditional_modes; i++){

    solver.factorize(get_inner_hessian());
    VectorXdual1st b1 = solver.permutationP() *
      (get_Lambdat() * Zt * (y - meanfun()) - u);
    VectorXdual1st cu = solver.matrixL().solve(b1);
    delta_u = solver.permutationPinv() * solver.matrixU().solve(cu);

    update_u(delta_u, 1);
  }
}

dual1st GALAMM::Model::exponent_g(){
  return (y.dot(get_linpred()) - cumulant()) / get_phi() + constfun() -
    u.squaredNorm() / 2 / get_phi();
}

void GALAMM::Model::update_inner_hessian(){
  inner_hessian = (1 / get_phi()) *
    get_Lambdat() * Zt * get_V() *
    Zt.transpose() * get_Lambdat().transpose();
}

Eigen::SparseMatrix<dual1st>& GALAMM::Model::get_inner_hessian(){
  if(inner_hessian_needs_update){
    update_inner_hessian();
    inner_hessian_needs_update = false;
  }
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
  inner_hessian_needs_update = true;
  linpred_needs_update = true;

}

Eigen::SparseMatrix<dual1st>& GALAMM::Model::get_Lambdat(){
  if(Lambdat_needs_update){
    update_Lambdat();
    Lambdat_needs_update = false;
  }
  return Lambdat;
}

Eigen::DiagonalMatrix<dual1st, Eigen::Dynamic>&
  GALAMM::Model::get_V(){
    if(V_needs_update){
      update_V();
      V_needs_update = false;
    }
    return V;
  }

dual1st& GALAMM::Model::get_phi(){
  if(phi_needs_update){
    update_phi();
    phi_needs_update = false;
  }
  return phi;
}

VectorXdual1st& GALAMM::Model::get_linpred(){
  if(linpred_needs_update){
    update_linpred();
    linpred_needs_update = false;
  }
  return linpred;
}

void GALAMM::Model::update_u(const VectorXdual1st& delta_u,
                             double alpha_bar){
  u += alpha_bar * delta_u;
  linpred_needs_update = true;
}
