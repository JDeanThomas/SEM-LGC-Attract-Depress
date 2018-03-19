require(foreign)
require(OpenMx)
require(MASS)

# Set Num Threads globally (local setting available. Requires compiled package) 
# mxOption(NULL, "Number of Threads", 8)

if(.Platform$OS.type == "windows") {
    wd = "C:\\Users\\deadkins\\Dropbox\\Attractiveness_Depress\\SSR_1st\\"
} else {
    wd =  "/Users/jasonthomas/Dropbox/SSR_1st/"  
}

data <- read.dta(paste0(wd,"attract_dep_all_wide_2016.dta"))
names(data)

#####
###   LGC Model with Predictors of the Growth Factors
#####
#### Establishing the Longitudinal Functional Form of Attract -> Depress

id <- names(data[1])
attract <- names(data[seq.int(2, 98, 6)])
cesdFs <- names(data[seq.int(5, 104, 6)])
cesd3Fs <- names(data[seq.int(4, 104, 6)])
cesdSum <- names(data[seq.int(6, 104, 6)])   
cesd3Sum <- names(data[seq.int(7, 104, 6)])
sle <- names(data[seq.int(3, 104, 6)])
support <- "support"
meanPEdu <- "mean_p_educ"
sex <- "female"
ethnicity <- names(data[112:117])

################################################################################

### Model 4
#xtmixed cesd_sum_ attrct_ age age2 attrctXage attrctXage2 || aid: ,mle
#estimates store m14, title(Model 4)

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
                     # like "Attract12I", "Attract12S", "Attract12Q"..."Attract28I",
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


#############################################


mxOption(NULL, "Number of Threads", 1)

### R BLAS, 1 Thread. 
### R compiled with Apple Clang with Accelerate BLAS
### OpenMx complied with GCC 4.9 C++98 

## Model Estimation and Fit
# Estimate Model Parameters
system.time(Model4Fit   <- mxRun(lgcQModel))
# Fit Model
system.time(Model4Fit   <- mxRun(Model4Fit))