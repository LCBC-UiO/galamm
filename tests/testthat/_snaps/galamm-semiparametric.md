# GAMM with factor structures and random effects works

    Code
      summary(mod)
    Output
      Generalized additive latent and mixed model fit by maximum marginal likelihood.
      Formula: y ~ 0 + item + s(x, by = loading) + (0 + loading | id/timepoint)
         Data: dat
      
           AIC      BIC   logLik deviance df.resid 
        -777.1   -712.3    398.5   -797.1     4790 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       8.573 17.079 52.697 64.954 83.643 
      
      Lambda:
              loading       SE
      lambda1  1.0000        .
      lambda2  1.4004 0.003441
      lambda3  0.3003 0.002088
      
      Random effects:
       Groups       Name         Variance Std.Dev.
       timepoint:id loading      0.266010 0.51576 
       id           loading      0.877356 0.93667 
       Xr           s(x):loading 2.072054 1.43946 
       Residual                  0.009845 0.09922 
      Number of obs: 4800, groups:  timepoint:id, 1600; id, 200; Xr, 8
      
      Fixed effects:
                      Estimate Std. Error  t value  Pr(>|t|)
      item1            1.25075    0.06752 18.52360 1.332e-76
      item2            1.75502    0.09453 18.56640 6.010e-77
      item3            0.37909    0.02041 18.57146 5.470e-77
      s(x):loadingFx1  0.02038    0.20527  0.09927 9.209e-01

