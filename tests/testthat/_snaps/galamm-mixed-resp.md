# Covariate measurement error model works

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

