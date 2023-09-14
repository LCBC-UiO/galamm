# galamm reproduces gamm4

    Code
      print(summary(mod), digits = 2)
    Output
      Generalized additive latent and mixed model fit by maximum marginal likelihood.
      Formula: y ~ s(x)
         Data: dat
      
           AIC      BIC   logLik deviance df.resid 
        4788.5   4810.1  -2390.3   4780.5     1596 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
       -2.69  -0.67  -0.02   0.66   3.16 
      
      Random effects:
       Groups   Name Variance Std.Dev.
       Xr       s(x) 2.8      1.7     
       Residual      1.1      1.1     
      Number of obs: 1600, groups:  Xr, 8
      
      Fixed effects:
                  Estimate Std. Error t value Pr(>|t|)
      (Intercept)      1.3      0.027   46.66     0.00
      s(x)Fx1         -0.1      0.318   -0.32     0.75
      
      Approximate significance of smooth terms:
           edf Ref.df   F p-value
      s(x) 5.4    5.4 103  <2e-16
      

# GAMM with factor structures and random effects works

    Code
      print(summary(mod), digits = 2)
    Output
      Generalized additive latent and mixed model fit by maximum marginal likelihood.
      Formula: y ~ 0 + item + s(x, by = loading) + (0 + loading | id/timepoint)
         Data: dat
      
           AIC      BIC   logLik deviance df.resid 
        -777.1   -712.3    398.5   -797.1     4790 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
         8.6   17.1   52.7   65.0   83.6 
      
      Lambda:
              loading     SE
      lambda1     1.0      .
      lambda2     1.4 0.0034
      lambda3     0.3 0.0021
      
      Random effects:
       Groups       Name         Variance Std.Dev.
       timepoint:id loading      0.2660   0.516   
       id           loading      0.8774   0.937   
       Xr           s(x):loading 2.0721   1.439   
       Residual                  0.0098   0.099   
      Number of obs: 4800, groups:  timepoint:id, 1600; id, 200; Xr, 8
      
      Fixed effects:
                      Estimate Std. Error t value Pr(>|t|)
      item1               1.25      0.068  18.524  1.3e-76
      item2               1.76      0.095  18.566  6.0e-77
      item3               0.38      0.020  18.571  5.5e-77
      s(x):loadingFx1     0.02      0.205   0.099  9.2e-01
      
      Approximate significance of smooth terms:
                   edf Ref.df    F p-value
      s(x):loading 8.7    8.7 4647  <2e-16
      

