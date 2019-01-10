#The Loss Function
#Given a set of predictor variables X and some response y, we look for some function f(x) to make prediction of y from those input
#variables. We also need a function to penalize errors in prediction-a loss function, L(Y,f(x)). With chosen loss function, we then
#find the model which will minimize loss, generally speaking.
#Continous Outcomes
#Squared Error.

sqerrloss <- function(beta, X, y) {
  mu <- X %*% beta
  sum((y - mu)^2)
}

#Then set seed.
set.seed(123)
X <- cbind(1, rnorm(100), rnorm(100));x
y <- rowSums(X[, -1] + rnorm(100));y
out1 = optim(par = c(0, 0, 0), fn = sqerrloss, X = X, y = y);out1
out2 = lm(y ~ X[, 2] + X[, 3]);out2 # check with lm
rbind(c(out1$par, out1$value), c(coef(out2), sum(resid(out2)^2)))