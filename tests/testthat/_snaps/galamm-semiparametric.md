# galamm reproduces gamm4

    Code
      print(summary(mod), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: y ~ t2(x)
         Data: dat
      
           AIC      BIC   logLik deviance df.resid 
        1052.4   1068.3   -522.2   1044.4      388 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
      -2.977 -0.668 -0.018  0.699  2.873 
      
      Random effects:
       Groups   Name  Variance Std.Dev.
       Xr       t2(x) 2.90     1.70    
       Residual       0.82     0.91    
      Number of obs: 392, groups:  Xr, 3
      
      Fixed effects:
                  Estimate Std. Error t value Pr(>|t|)
      (Intercept)    1.106      0.053    20.7  6.2e-95
      t2(x)Fx1       0.073      0.059     1.2  2.2e-01
      
      Approximate significance of smooth terms:
            edf Ref.df  F p-value
      t2(x) 3.7    3.7 31  <2e-16
      

---

    Code
      print(summary(mod), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: y ~ s(x, fx = TRUE) + (1 | id)
         Data: dat
      
           AIC      BIC   logLik deviance df.resid 
         736.4    784.1   -356.2    712.4      380 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
      -2.854 -0.636 -0.004  0.654  2.806 
      
      Random effects:
       Groups   Name        Variance Std.Dev.
       id       (Intercept) 0.58     0.76    
       Residual             0.25     0.50    
      Number of obs: 392, groups:  id, 49
      
      Fixed effects:
                  Estimate Std. Error t value Pr(>|t|)
      (Intercept)     1.39       0.11   12.44  1.6e-35
      s(x)Fx1         0.14       0.27    0.53  6.0e-01
      s(x)Fx2        -1.24       0.81   -1.52  1.3e-01
      s(x)Fx3        -0.32       0.19   -1.68  9.3e-02
      s(x)Fx4        -0.88       0.59   -1.51  1.3e-01
      s(x)Fx5        -0.19       0.22   -0.88  3.8e-01
      s(x)Fx6         1.30       0.64    2.03  4.3e-02
      s(x)Fx7         0.51       0.31    1.65  9.8e-02
      s(x)Fx8         3.59       1.29    2.79  5.3e-03
      s(x)Fx9         0.20       0.58    0.35  7.2e-01
      
      Approximate significance of smooth terms:
           edf Ref.df  F p-value
      s(x)   9      9 64  <2e-16
      

---

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
      s(x2):fac2 2.1    2.1 57.8  <2e-16
      s(x2):fac3 7.0    7.0 34.0  <2e-16
      

