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
      print(summary(mod)$gam_summary, digits = 2)
    Output
      
      Family: gaussian 
      Link function: identity 
      
      Formula:
      NULL
      
      Parametric coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
      (Intercept)    0.904      0.035      26   <2e-16 ***
      domain3        3.720      0.046      81   <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Approximate significance of smooth terms:
                            edf Ref.df    F p-value    
      s(x):domain1:ability1 2.8    2.8   97  <2e-16 ***
      s(x):domain3:ability3 2.9    2.9 1380  <2e-16 ***
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      R-sq.(adj) =  -29.9   
      lmer.REML =   Scale est. = 5.7416    n = 11200

