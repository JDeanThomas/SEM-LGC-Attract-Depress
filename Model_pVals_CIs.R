# Takes single input of name of OpenMx model estimate or model fit
# Retruns P Values, Paramater Estimates and Lower and Upper CIs

pValues <- function(model){
    sum.param <- summary(model)$parameters
    param <- sum.param[,5]
    SE <- sum.param[,6]
    pVals <- as.matrix((1-pnorm(abs(param)/SE))*2)
    resCI <- param + t(outer(c(-1, 0, 1), qnorm(.975)*SE))
    vals <- cbind(pVals,resCI)
    rownames(vals) <- sum.param[,1]
    colnames(vals) <- c('P Value', 'CI Lower', 'Estimate', 'CI Upper')
    vals
}


# confidenceIntervals <- function(model){
#     sum.param <- summary(model)$parameters
#     param <- sum.param[,5]
#     SE <- sum.param[,6]
#     res.ci <- param + t(outer(c(-1, 0, 1), qnorm(.975)*SE))
#     rownames(res.ci) <- sum.param[,1]
#     colnames(res.ci) <- c('lower', 'estimate', 'upper')
#     res.ci
# }
# 
# 
# P_Values <- function(model){
#     sum.param <- summary(model)$parameters
#     param <- sum.param[,5]
#     SE <- sum.param[,6]
#     PVals <- as.matrix((1-pnorm(abs(param)/SE))*2)
#     rownames(PVals) <- sum.param[,1]
#     colnames(PVals) <- 'P Values'
#     PVals
# }


