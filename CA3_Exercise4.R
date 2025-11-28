## Exercise 3
## 1
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

## 2
nll_lm <- function(params, data){
  y <- data[,1]
  X <- as.matrix(data[,-1])
  X <- cbind(1, X)

  beta <- params[1:4]
  sigma <- params[5]
  
  mu <- X %*% beta
  eps <- y - mu
  
  -sum(dnorm(eps, mean = 0, sd = sigma, log = TRUE))
}

## 3
inits <- c(mean(df$y), 0, 0, 0, sd(df$y))
fit <- optim(inits, nll_lm, method = "L-BFGS-B", lower = -Inf, upper = Inf, data = df)
fit$value

## 4
## The optim function performs minimisation of the negative logLik by default

## 5
## To calculate betahat we do (XtX)^-1 * Xty
y <- df[,1]
X <- as.matrix(df[,-1])
X <- cbind(1, X)

beta_hat <- solve(crossprod(X), crossprod(X,y))

## Values seem relatively close
beta_hat; fit$par[1:4]

## 6
degreesOfFreedom <- 32

#MLE; biased
sqrt(crossprod(y - X %*% beta_hat)/degreesOfFreedom)
fit$par[5]

#unbiased estimator
sqrt(crossprod(y - X %*% beta_hat)/(degreesOfFreedom - ncol(X)))
temp <- summary(lm(y ~ X))
temp$sigma

## 7
## Optim uses a biased MLE whilst the matrix calculations are unbiased 

## 8
fit <- optim(inits, nll_lm, method = "L-BFGS-B", lower = -Inf, upper = Inf, data = df, hessian = TRUE)

regressionCoef <- sqrt(diag(solve(fit$hessian)))[1:4]
regressionCoef

##
mod <- lm(y ~X)
mod$coefficients

temp <- summary(lm(y ~ X))
temp$sigma

