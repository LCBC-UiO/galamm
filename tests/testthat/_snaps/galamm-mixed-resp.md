# Covariate measurement error model works

    Code
      summary(mod)
    Output
      Generalized additive latent and mixed model fit by maximum marginal likelihood.
      Formula: y ~ 0 + chd + (age * bus):chd + fiber + (age * bus):fiber + fiber2 +  
          (0 + loading | id)
         Data: diet
      
           AIC      BIC   logLik deviance df.resid 
        2768.3   2823.6  -1372.2   2002.9      730 
      
      Scaled residuals: 
          Min      1Q  Median      3Q     Max 
      -258547      -1       0       0      66 
      
      Lambda:
              loading      SE
      lambda1  1.0000       .
      lambda2  1.0000       .
      lambda3 -0.1339 0.05121
      
      Random effects:
       Groups Name    Variance Std.Dev.
       id     loading 23.64    4.862   
      Number of obs: 742, groups:  id, 333
      
      Fixed effects:
                    Estimate Std. Error  z value   Pr(>|z|)
      chd           -1.91520    0.27229 -7.03361  2.013e-12
      fiber         17.94834    0.48686 36.86566 1.641e-297
      fiber2         0.22406    0.41783  0.53624  5.918e-01
      chd:age        0.06613    0.05931  1.11515  2.648e-01
      chd:bus       -0.02900    0.34355 -0.08441  9.327e-01
      age:fiber     -0.21207    0.10090 -2.10172  3.558e-02
      bus:fiber     -1.68278    0.63721 -2.64084  8.270e-03
      chd:age:bus   -0.04998    0.06507 -0.76812  4.424e-01
      age:bus:fiber  0.16821    0.11223  1.49882  1.339e-01

---

    Code
      anova(mod, mod0)
    Output
      Data: diet
      Models:
      mod0: y ~ 0 + chd + fiber + (age * bus):fiber + fiber2 + (0 + loading | id)
      mod: y ~ 0 + chd + (age * bus):chd + fiber + (age * bus):fiber + fiber2 + (0 + loading | id)
           npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
      mod0    9 2764.0 2805.5 -1373.0   2746.0                     
      mod    12 2768.3 2823.6 -1372.2   2744.3 1.7058  3     0.6357

