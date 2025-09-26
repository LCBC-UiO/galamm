# Covariate measurement error model works

    Code
      anova(mod, mod0)
    Output
      Data: diet
      Models:
      mod0: formula0
      mod: formula
           npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
      mod0    9 2764.0 2805.5 -1373.0   2002.9                     
      mod    12 2768.3 2823.6 -1372.2   2002.9 1.7058  3     0.6357

# Mixed response and heteroscedastic error works

    Code
      print(summary(mod), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: y ~ x + (1 | id)
         Data: mresp_hsced
      
           AIC      BIC   logLik deviance df.resid 
       12356.9  12388.3  -6173.4  31426.4     3995 
      
      Random effects:
       Groups Name        Variance Std.Dev.
       id     (Intercept) 0        0       
      Number of obs: 4000, groups:  id, 1000
      
      Variance function:
          a     b 
      1.000 0.078 
      
      Fixed effects:
                  Estimate Std. Error z value Pr(>|z|)
      (Intercept)    0.015      0.063    0.24  8.1e-01
      x              0.996      0.112    8.93  4.4e-19
      
      

