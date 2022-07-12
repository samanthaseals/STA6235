library(tidyverse)

# enter data

# predictor values
x <- c( 80,  30,  50,  90,  70,  60, 120,  80, 100,
        50,  40,  70,  90,  20, 110, 100,  30,  50,
        90, 110,  30,  90,  40,  80,  70)

# vector of 1's for design matrix
ones <- c(rep(1, length(x)))

# design matrix
X <- cbind(ones, x)

# vector of outcomes
Y <- c(399, 121, 221, 376, 361, 224, 546, 352, 353,
       157, 160, 252, 389, 113, 435, 420, 212, 268,
       377, 421, 273, 468, 244, 342, 323)

Y <- as_vector(Y)

# transpose is t(); 
# matrix multiplication is %*%;
# inverse of square matrix is solve()

# find X'X
XtX <- t(X) %*% X

# find X'Y
XtY <- t(X) %*% Y

# find b = inv(X'X) * X'Y
b <- solve(t(X)%*%X) %*% t(X)%*%Y
b <- solve(XtX) %*% XtY

# find y hat
pred <- X%*%b

# find error
e <- Y - pred

# find hat matrix
H = X%*%solve(XtX)%*%t(X)

# matrix of 1's
J <- matrix(1, nrow(X), nrow(X))

# identity matrix
I <- diag(nrow(X))

# find ANOVA components
SSR <- t(Y) %*% (H - (1/nrow(X)) * J) %*% Y
SSE <- t(Y) %*% (I - H) %*% Y
SST <- SSR + SSE
# SST <- t(Y) %*% (I - 1/nrow(X) * J) %*% Y

MSR <- SSR/(ncol(X)-1)
MSE <- SSE*(1/(nrow(X)-(ncol(X)-1)-1))

F <- MSR / MSE

# find variances of b estimates
s2_b <- as.numeric(MSE) * solve(XtX) 

# want to create vector of t-values
t <- c(b[1] / sqrt(s2_b[1,1]), b[2] / sqrt(s2_b[2,2]))

# find p-values corresponding to the t-values
p <- 2*pt(q = abs(t), df = nrow(X), lower.tail = FALSE)

# estimating 
Xh <- t(c(1, 65))

Yhat_h <- Xh %*% b

s2_Yhat_h <- MSE *  (Xh %*% solve(XtX) %*% t(Xh))
