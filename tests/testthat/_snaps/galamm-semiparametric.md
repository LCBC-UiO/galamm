# galamm reproduces gamm4

    Code
      print(summary(mod1), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: y ~ fac + s(x2, by = fac)
         Data: dat
      
           AIC      BIC   logLik deviance df.resid 
        1802.3   1842.2   -891.2   1782.3      390 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
      -2.806 -0.722  0.051  0.679  2.955 
      
      Random effects:
       Groups   Name       Variance Std.Dev.
       Xr       s(x2):fac1   4.6     2.1    
       Xr.0     s(x2):fac2   2.4     1.6    
       Xr.1     s(x2):fac3 596.6    24.4    
       Residual              4.6     2.2    
      Number of obs: 400, groups:  Xr, 8; Xr.0, 8; Xr.1, 8
      
      Fixed effects:
                    Estimate Std. Error t value Pr(>|t|)
      (Intercept)      1.272       0.19   6.634  3.3e-11
      fac2            -2.275       0.26  -8.609  7.4e-18
      fac3             2.199       0.27   8.120  4.7e-16
      s(x2):fac1Fx1    0.014       0.75   0.019  9.8e-01
      s(x2):fac2Fx1   -2.022       0.60  -3.387  7.1e-04
      s(x2):fac3Fx1   -2.771       3.23  -0.859  3.9e-01
      
      Approximate significance of smooth terms:
                 edf Ref.df    F p-value
      s(x2):fac1 2.4    2.4  4.6   0.008
      s(x2):fac2 9.0    9.0 13.6  <2e-16
      s(x2):fac3 9.0    9.0 25.6  <2e-16
      

