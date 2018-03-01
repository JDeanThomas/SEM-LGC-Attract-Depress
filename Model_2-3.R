################################################################################

### Model 2
# xtmixed cesd_sum_ attrct_ age age2 || aid:, mle
# estimates store m12, title(Model 2)

attrctLatLabs <- c(rep(c(paste0(("AttractI"), rep(12:28)), paste0(("AttractS"), 
                rep(12:28)), paste0(("AttractQ"), rep(12:28)))))
attrctLatLabs <- c(attrctLatLabs[seq(1, 51, 2)]))

observed <- c(cesdSum, attract)
numPaths <- length(observed)

dta      <- subset(data,select=observed)
latent   <- c("intercept","slope","quad")

pathData <- mxData(dta, type="raw")

## Model Estimation
# Model
lgcQModel <- mxModel("LGC",type="RAM", pathData, manifestVars=observed, 
                     latentVars=latent,
                     
                     ## Residual Variances
                     manResid <- mxPath(from=c(observed), arrows=2, 
                                        free=c(rep(T, numPaths)), 
                                        values=c(rep(1, numPaths)), 
                                        labels=c(paste0("R", rep(1:numPaths)))),
                     
                     # Latent Variances and Covariance
                     GroCov   <- mxPath(from=latent, arrows=2, connect="unique.pairs",
                                        free=T, values=.6 , 
                                        labels=c("II", "IS","IQ","SS", "SQ", "QQ")),
                                        
                     # The more parsimonious coding is to combine the below 3 mxPath
                     # calls into one. I cannot find a parsimonious way to creat lables
                     # like "Attract12I", "Attract12S", "Attract12Q"..."Attract28I", 					             # "Attract28S", "Attract28Q" within the function call due to
                     # limitations of OpenMx creating var with at top of script with:
                     # AttractCoef <- c(unlist(rep(c("AttractI", "AttractS", "AttractQ"), 17)))
                     # works fine and allows recolapse. Leaving for now for clarity.
                                                   
                     
                     # Attract   <- mxPath(from=observed[18:34], to=latent[1:3],
                     #                     arrows=1, free = T, values = 0,
                     #                     lables=c(rep(c("AttractI", "AttractS",
                     #                              "AttractQ")), 17)),
                     
                     AttractI   <- mxPath(from=observed[18:34], to=latent[1], 
                                         arrows=1, free = T, values = 0, 
                                         labels=c(paste0("AttractI", rep(12:28)))),
                     
                     AttractS   <- mxPath(from=observed[18:34], to=latent[2], 
                                          arrows=1, free = T, values = 0, 
                                          labels=c(paste0("AttractS", rep(12:28)))),
                     
                     AttractQ   <- mxPath(from=observed[18:34], to=latent[3], 
                                          arrows=1, free = T, values = 0, 
                                          labels=c(paste0("AttractQ", rep(12:28)))),
                     
                     ## Growth factor loadings
                     # Intercept Loadings
                     Int      <- mxPath(from="intercept", to=observed[1:17],
                                        arrows=1, free=FALSE, values=1), 
                     # Linear Slope Loadings
                     Lin      <- mxPath(from="slope", to=observed[1:17], 
                                        arrows=1, free=FALSE, 
                                        values=c(seq(0, 16, by=1))),
                     # Quadratic Slope Loadings
                     Quad     <- mxPath(from="quad", to=observed[1:17], arrows=1,
                                        free=FALSE, values=c(seq(0, 16, by=1)^2)),
                     
                     ## Means
                     # Manifest Means
                     ManMeans <- mxPath(from="one", to=observed, arrows=1,
                                        free=c(rep(T, 17), rep(F, 17)), values=0,
                                        labels = c(paste0("cesd_sum_", rep(12:28), "_Mean"),
                                                   paste0("attrct_", rep(12:28), "_Mean"))),
                     # Latent Means
                     LatMeans <- mxPath(from="one", to=latent, arrows=1, free=TRUE,
                                        values=c(1, .3, .1),
                                        labels=c("meanI", "meanS", "meanQ"))
)


# Output Model Diagram 
omxGraphviz(lgcQModel, "Model2.dot")
system("dot -Tpng Model2.dot > Model2.png")

mxOption(lgcQModel, "Number of Threads", 8)

Model2_est <- omxAssignFirstParameters(lgcQModel)

## Model Estimation and Fit
mxOption(NULL, "Default optimizer", "SLSQP")

# Estimate Model Parameters
Model2Fit   <- mxRun(lgcQModel)
# Fit Model
Model2Fit   <- mxRun(Model2Fit)

# Model Summary
summary(Model2Fit)
# Verbose Model Output
Model2Fit$output
