# galamm reproduces gamm4

    Code
      print(summary(mod$gam), digits = 2)
    Output
      
      Family: gaussian 
      Link function: identity 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)    1.385      0.046      30   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
           edf Ref.df  F p-value    
      s(x) 4.7    4.7 47  <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.359   
      lmer.REML =   Scale est. = 0.82148   n = 392

---

    Code
      print(summary(mod$gam), digits = 2)
    Output
      
      Family: gaussian 
      Link function: identity 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)    1.385      0.046      30   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
            edf Ref.df  F p-value    
      t2(x) 3.7    3.7 31  <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.359   
      lmer.REML =   Scale est. = 0.82097   n = 392

---

    Code
      print(summary(mod$gam), digits = 2)
    Output
      
      Family: gaussian 
      Link function: identity 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)     1.39       0.11      12   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
           edf Ref.df  F p-value    
      s(x)   9      9 64  <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.825   
      lmer.REML =   Scale est. = 0.24854   n = 392

---

    Code
      print(summary(mod1$gam), digits = 2)
    Output
      
      Family: gaussian 
      Link function: identity 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)     8.01       0.04     199   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
             edf Ref.df    F p-value    
      s(x1)  4.1    4.1  486  <2e-16 ***
      t2(x2) 4.0    4.0 1167  <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.941   
      lmer.REML =   Scale est. = 0.64969   n = 400

---

    Code
      print(summary(mod1$gam), digits = 2)
    Output
      
      Family: gaussian 
      Link function: identity 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)     1.27       0.19     6.6    1e-10 ***
      fac2           -2.27       0.26    -8.6   <2e-16 ***
      fac3            2.20       0.27     8.1    6e-15 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
                 edf Ref.df    F p-value    
      s(x2):fac1 2.4    2.4  4.6   0.008 ** 
      s(x2):fac2 2.1    2.1 57.8  <2e-16 ***
      s(x2):fac3 7.0    7.0 34.0  <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.618   
      lmer.REML =   Scale est. = 4.6267    n = 400

---

    Code
      print(summary(mod1$gam), digits = 2)
    Output
      
      Family: gaussian 
      Link function: identity 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)     1.23       0.33     3.7    2e-04 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
               edf Ref.df F p-value  
      s(x0):x2   2      2 3    0.05 .
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.00979   
      lmer.REML =   Scale est. = 12.03     n = 400

---

    Code
      print(summary(mod1$gam), digits = 2)
    Output
      
      Family: gaussian 
      Link function: identity 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)     1.22       0.33     3.7    2e-04 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
                edf Ref.df   F p-value  
      t2(x0):x2 3.1    3.1 3.4    0.02 *
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.0212   
      lmer.REML =   Scale est. = 11.897    n = 400

---

    Code
      print(summary(mod1$gam), digits = 2)
    Output
      
      Family: binomial 
      Link function: logit 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)     2.84       0.31       9   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
               edf Ref.df Chi.sq p-value    
      s(x0):x2   2      2     37  <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.145   
      glmer.ML =   Scale est. = 1         n = 400

---

    Code
      print(summary(mod1$gam), digits = 2)
    Output
      
      Family: binomial 
      Link function: logit 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)     2.84       0.31       9   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
                edf Ref.df Chi.sq p-value    
      t2(x0):x2   3      3     40  <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.158   
      glmer.ML =   Scale est. = 1         n = 400

---

    Code
      print(summary(mod1$gam), digits = 2)
    Output
      
      Family: poisson 
      Link function: log 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)    0.820      0.034      24   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
            edf Ref.df Chi.sq p-value    
      s(x2) 6.1    6.1     83  <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.137   
      glmer.ML =   Scale est. = 1         n = 400

---

    Code
      print(summary(mod1$gam), digits = 2)
    Output
      
      Family: poisson 
      Link function: log 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error z value Pr(>|z|)    
      (Intercept)    0.913      0.066      14   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
                edf Ref.df Chi.sq p-value    
      t2(x2):x0 4.8    4.8     57  <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  0.0879   
      glmer.ML =   Scale est. = 1         n = 400

# galamm with by variables and loadings works

    Code
      print(summary(mod), digits = 2)
    Output
      GALAMM fit by maximum marginal likelihood.
      Formula: y ~ domain + sl(x, k = 4, by = domain, load.var = c("ability1",  
          "ability3"))
         Data: dat
      Control: galamm_control(optim_control = list(maxit = 3))
      
           AIC      BIC   logLik deviance df.resid 
       51403.8  51491.7 -25689.9  51379.8    11188 
      
      Scaled residuals: 
         Min     1Q Median     3Q    Max 
         1.0    2.8    9.4   12.3   34.9 
      
      Lambda:
              ability1   SE ability3    SE
      lambda1     1.00    .        .     .
      lambda2     1.22 0.16        .     .
      lambda3     0.73 0.17        .     .
      lambda4        .    .      1.0     .
      lambda5        .    .      1.1 0.049
      lambda6        .    .      1.1 0.049
      lambda7        .    .      2.5 0.096
      
      Random effects:
       Groups   Name                  Variance Std.Dev.
       Xr       s(x):domain1:ability1 5.9      2.4     
       Xr.0     s(x):domain3:ability3 9.7      3.1     
       Residual                       5.7      2.4     
      Number of obs: 11200, groups:  Xr, 2; Xr.0, 2
      
      Fixed effects:
                               Estimate Std. Error t value Pr(>|t|)
      (Intercept)                  0.90      0.035   25.98 8.1e-149
      domain3                      3.72      0.046   80.32  0.0e+00
      s(x):domain1:ability1Fx1    -0.02      0.149   -0.13  8.9e-01
      s(x):domain3:ability3Fx1     1.35      0.097   13.95  3.2e-44
      
      Approximate significance of smooth terms:
                            edf Ref.df    F p-value
      s(x):domain1:ability1 2.8    2.8   97  <2e-16
      s(x):domain3:ability3 2.9    2.9 1380  <2e-16
      

