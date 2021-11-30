

linkinv <- function(x) (1 + exp(-x))^(-1)

log_response_prob <- function(mean, y, trials = 1){
  y * log(mean) + (trials - y) * log(1 - mean)
}



