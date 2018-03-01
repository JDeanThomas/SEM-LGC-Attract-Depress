mxOption(lgcQModel, "Number of Threads", 8)

################################################################################

# Attact as latent 

### Model 4
#xtmixed cesd_sum_ attrct_ age age2 attrctXage attrctXage2 || aid: ,mle
#estimates store m14, title(Model 4)

observed <- c(cesdSum, attract)
numPaths <- length(observed)

dta      <- subset(data,select=observed)
latent   <- c("intercept","slope","quad", "attract")

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
                     
                     GroCov   <- mxPath(from=latent[1:3], arrows=2, connect="unique.pairs",
                                        free=T, values=.6,
                                        labels=c("II", "IS","IQ","SS", "SQ", "QQ")),
                     
                     atrCov     <- mxPath(from="attract", arrows=2, free=T, values=.6,
                                          labels="AA"),
                     
                     ## Latent loadings
                     # Attract Loadings
                     atr_lat     <- mxPath(from="attract", to=latent[1:3],
                                           arrows=1, free=T, values=.5,
                                           labels=c("AttractI", "AttractS", "AttractQ")),
                     
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
                                        free=c(rep(T, 17), rep(T, 17)), values=0,
                                        labels = c(paste0("cesd_sum_", rep(12:28), "_Means"),
                                        paste0("attrct_", rep(12:28), "_Means"))),
                     
                     # ManMeans <- mxPath(from="one", to=observed , arrows=1,free=
                     #                    c(rep(F, numPaths)), values=0, 
                     #                    labels = c(paste0("M", rep(1:numPaths)))),                    
                     
                     
                     # # Latent Means
                     LatMeans <- mxPath(from="one", to=latent, arrows=1, free=TRUE,
                                        values=c(1, .3, .1),
                                        labels=c("meanI", "meanS", "meanQ"))
)

## Model Estimation and Fit
# Estimate Model Parameters
Model4Fit   <- mxRun(lgcQModel)
# Fit Model
Model4Fit   <- mxRun(Model4Fit)

# Model Summary
summary(Model4Fit)
# Verbose Model Output
Model4Fit$output

# Output Model Diagram 
omxGraphviz(lgcQModel, "Model4_test.dot")
system("dot -Tpng Model4_test.dot > Model4_test.png")

################################################################################

# Dep Latent for each T (still in progress)

### Model 4

Depression <- c(paste0("Dep", rep(12:28)))

observed <- c(cesdSum, attract)
numPaths <- length(observed)

dta      <- subset(data,select=observed)
latent   <- c("intercept","slope","quad", Depression)

pathData <- mxData(dta, type="raw")

## Model Estimation
# Model
lgcQModel <- mxModel("LGC",type="RAM", pathData, manifestVars=observed, 
                     latentVars=latent,
                     
                     ## Residual Variances
                     manResid <- mxPath(from=c(observed), arrows=2, 
                                        free=c(rep(T, numPaths)), 
                                        values=c(rep(1, numPaths)), 
                                        labels=c(paste0("cesdRes ", rep(12:28)),
                                        labels=c(paste0("attrRes ", rep(12:28))))),
                     
                     ## Latent Variances and Covariance
                     # Growth Curve Covariences
                     GroCov   <- mxPath(from=latent[1:3], arrows=2, connect="unique.pairs",
                                        free=T, values=.6,
                                        labels=c("II", "IS","IQ","SS", "SQ", "QQ")),
                     
                     # Dep Growth Curve Variences (estimate Covs as well?)
                     #### How many Cov measures here? Single (default) or Unique Pars (Vars and Covs)?
                     DepCov     <- mxPath(from=latent[4:20], arrows=2, free=T, values=.6,
                                          labels=c(paste0("CovDep", rep(12:28)))), 
                     
                     ## Observed Paths loadings
                     Attract   <- mxPath(from=attract, to=latent[4:20], #Where are we going? 
                                         arrows=1, free = T, values = 0, 
                                         labels=c(paste0("AttractDep", rep(12:28)))),         #What are we called
                     
                     # Depress   <- mxPath(from=cesdSum, to=latent[1], #Where are we going? 
                     #                     arrows=1, free = T, values = 0, 
                     #                     labels="AttractI"),         #What are we called?
                     
                     ## Growth factor loadings
                     # Dep Growth Factor Loadings
                     Dep     <- mxPath(from=Depression, to=latent[1:3],        #Where are we going?
                                           arrows=1, free=T, values=.5,
                                           labels=c(paste0("Dep", rep(12:28)))),
                     
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
                     
                     # ManMeans <- mxPath(from="one", to=observed , arrows=1,free=
                     #                    c(rep(F, numPaths)), values=0, 
                     #                    labels = c(paste0("M", rep(1:numPaths)))),                    
                     
                     
                     # Latent Means
                     LatMeans <- mxPath(from="one", to=latent[1:3], arrows=1, free=TRUE,
                                        values=c(1, .3, .1),
                                        labels=c("meanI", "meanS", "meanQ")),
                     
                     # Dep Means
                     DepMeans <- mxPath(from="one", to=latent[4:20], arrows=1,
                                        free=c(rep(T, 17)), values=0,
                                        labels = c(paste0("MeanDep", rep(12:28))))
                                                   
)

## Model Estimation and Fit
# Estimate Model Parameters
Model4Fit   <- mxRun(lgcQModel)
# Fit Model
Model4Fit   <- mxRun(Model4Fit)

# Model Summary
summary(Model4Fit)
# Verbose Model Output
Model4Fit$output

# Output Model Diagram 
omxGraphviz(lgcQModel, "Model4_test.dot")
system("dot -Tpng Model4_test.dot > Model4_test.png")

c(paste0("DepI", rep(12:28)), paste0("DepS", rep(12:28)), paste"DepQ", rep(12:28))