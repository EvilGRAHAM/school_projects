# Packages ----------
library(glmnet, warn.conflicts = FALSE, quietly = TRUE)

a.cv.glmnet <- function(x, y, alpha = 1, gamma = 1, ...) {
    cv_ridge_reg <- cv.glmnet(x, y, alpha = 0, ...)
    alasso_w <- 1 / abs(matrix(coef(cv_ridge_reg, s=cv_ridge_reg$lambda.min)[, 1][2:(ncol(x)+1)]))^gamma
    alasso_w[alasso_w[,1] == Inf] <- 999999999 ## Replacing values estimated as Infinite for 999999999
    
    cv.glmnet(x, y, ..., penalty.factor = alasso_w, alpha = alpha)
}
