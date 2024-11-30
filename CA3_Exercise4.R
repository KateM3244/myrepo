
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))


nll_lm <- function(data, par) {
  y <- data$y
  x1 <- data$x1
  x2 <- data$x2
  x3 <- data$x3
  beta0 <- par[1]
  beta1 <- par[2]
  beta2 <- par[3]
  beta3 <- par[4]
  
  yhat <- beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3
  e <- y - yhat
  sigma2 <- var(e)
  
  
  nll <- (length(y) / 2) * log(2 * pi * sigma2) + (1 / (2 * sigma2)) * sum(e^2)
  
  return(nll)
}


par <- c(0,1,1,1)

nll <- nll_lm(df,par)

nll

parameter <- c(mean(df$y), 0, 0, 0, sd(df$y)) 

nll_lm <- function(data, par) {
  y <- data$y
  x1 <- data$x1
  x2 <- data$x2
  x3 <- data$x3
  beta0 <- par[1]
  beta1 <- par[2]
  beta2 <- par[3]
  beta3 <- par[4]
  
  sigma <- par[5]
  
  yhat <- beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3
  e <- y - yhat
  
  
  
  nll <- (length(y) / 2) * log(2 * pi * sigma^2) + (1 / (2 * sigma^2)) * sum(e^2)
  
  return(nll)
}

fit <- optim(
  par = parameter,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  lower = c(-Inf, -Inf, -Inf, -Inf, 0.000001), 
  upper = c(Inf, Inf, Inf, Inf, Inf)
  
)
params <- fit$par

params


optim is designed to minimize functions, not maximize and minimizing the negative log-likelihood results in maximizing the log-likelihood.

X <- as.matrix(cbind(1, df$x1, df$x2, df$x3))
y <- df$y

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y


beta_hat


X <- as.matrix(cbind(1, df$x1, df$x2, df$x3))
y <- df$y

beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
residuals <- y - X %*% beta_hat

n<- length(y)
p<- ncol(X)


sigma_hat <- sqrt(sum(residuals^2) / (n-p))


sigma_hat


fit <- optim(par = parameter, fn = nll_lm, data = df, 
             method = "L-BFGS-B", hessian = TRUE)
matrix <- fit$hessian


params <- sqrt(diag(solve(matrix)))
params



fit <- optim(
  par = parameter,
  fn = nll_lm,
  data = df,
  method = "L-BFGS-B",
  lower = c(-Inf, -Inf, -Inf, -Inf, 0.000001), 
  upper = c(Inf, Inf, Inf, Inf, Inf),
  hessian = TRUE
  
)
params <- fit$par



matrix<-solve(fit$hessian)
matrix

se <- sqrt(diag(matrix))

params
se

lm_fit <- lm(y ~ x1 + x2 + x3, data = df)

beta_hat <- coef(lm_fit)


sigma_hat <- summary(lm_fit)$sigma

sigma hat
beta_hat