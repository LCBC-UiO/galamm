devtools::load_all()
B <- matrix( c( 1, 2, 0, 2, 5, 0, 0, 0, 3  ), 3, 3, TRUE )
b <- c( 5, 1, 7 )
B %*% solve( B, b )
A <- as( B, 'dgCMatrix' )
B %*% sparse1(A, b)$beta

library(lme4)
fm1 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy, REML = FALSE)

formula <- Reaction ~ Days + (1 | Subject)
data <- sleepstudy
