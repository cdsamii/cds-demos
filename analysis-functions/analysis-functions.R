# ICW Index function

# Function to standardize columns of a matrix
# where you designate a standardization group
# (e.g., the control group in an experiment)
# with "sgroup", a logical vector.

matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j]))/sd(x[sgroup,j])
  }
  return(x)
}

# Function that takes in data in matrix format and returns
# (i) IC weights and (ii) ICW index.
# Weights can be incorporated using the "wgts" argument.
# The "revcols" argument takes a vector indicating which columns,
# if any, should have values reversed (that is, standardized 
# values are multiplied by -1) prior to construction of the index. 

icwIndex <- function(	xmat,
                      wgts=rep(1, nrow(xmat)),
                      revcols = NULL,
                      sgroup = rep(TRUE, nrow(xmat))){
  X <- matStand(xmat, sgroup)
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  Sx <- cov.wt(X, wt=wgts)[[1]]
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  return(list(weights = weights, index = index))
}

# For printing R screen output in HTML

print_output <- function(output, cex = 0.45, yrange = c(0,1)) {
  tmp <- capture.output(output)
  plot.new()
  plot.window(xlim=c(0,1), ylim=yrange)
  text(0, 1, paste(tmp, collapse='\n'), adj = c(0,1), family = 'mono', cex = cex)
  box()
}

# Properly formatted cross-tab
require(gdata)
crossTab <- function(rowvar, colvar, rowlab, collab){
  crosstab <- table(rowvar, colvar)
  colproptab <- crosstab/as.matrix(rep(1,
                             nrow(crosstab)))%x%t(as.matrix(apply(crosstab, 
                                                                  2, sum)))
  forPrint.sc <- rbind(c("",colnames(crosstab),""),
                       cbind(rbind(cbind(c(rbind(rownames(crosstab), 
                                                 rep("", nrow(crosstab)))),
                                         interleave(matrix(as.character(crosstab), 
                                                           nrow=nrow(crosstab)),
                                                    matrix(as.character(round(colproptab, 2)), 
                                                           nrow=nrow(colproptab)))),
                                   c("",apply(crosstab, 2, sum))),
                             c(rbind(apply(crosstab, 1, sum), 
                                     rep("",nrow(crosstab))),
                               sum(crosstab))))
  forPrint <- rbind(c("","",collab,rep("", ncol(forPrint.sc)-2)),
                    cbind(c("",rowlab,rep("", nrow(forPrint.sc)-2)), 
                          forPrint.sc))
  colnames(forPrint) <- rep("", ncol(forPrint))
  crossTabresult <- list(crosstab, forPrint)
  return(crossTabresult)
}

# Create a binary variable with no missingness

makeBinary <- function(varIn, data, yesValue=1){
  varOut <- as.numeric(data[varIn] == yesValue)
  varOut[is.na(varOut)] <- 0
  return(varOut)
}

# Reverse code and clean Likert scales

revCleanLik <- function(xIn, orignegend=4, origposend=1){
  xIn[xIn==99] <- (orignegend+origposend)/2
  xIn[xIn==100] <- (orignegend+origposend)/2
  xIn[is.na(xIn)] <- (orignegend+origposend)/2
  xOut <- orignegend - xIn
}

# For cleaning numeric variables with -99 and NAs and imputing mean values

cleanNeg99mean <- function(varUp){
  varOut <- HH[,varUp]
  varOut[HH[,varUp]==-99] <- mean(varOut[HH[,varUp]!=-99], na.rm=TRUE)
  varOut[is.na(HH[,varUp])] <- mean(varOut[!is.na(HH[,varUp])], na.rm=TRUE)
  return(varOut)
}  

# For cleaning numeric variables with 98, -99, and NAs and imputing modal values

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

cleanMissMode <- function(varUp, missVals){
  varOut <- HH[,varUp]
  varOut[HH[,varUp]%in%missVals] <- NA
  varOut[is.na(varOut)] <- getmode(varOut[!is.na(varOut)])
  return(varOut)
}  

# For cleaning numeric variables with -99 and NAs and imputing 0

cleanNeg99_to_0 <- function(varUp){
  varOut <- HH[,varUp]
  varOut[HH[,varUp]==-99] <- 0
  varOut[is.na(HH[,varUp])] <- 0
  return(varOut)
}  

cleanNeg99_to_0_gen <- function(varUp, dataUp){
  varOut <- dataUp[,varUp]
  varOut[dataUp[,varUp]==-99] <- 0
  varOut[is.na(dataUp[,varUp])] <- 0
  return(varOut)
}  

# For finding variables that start with a certain pattern

findNames <- function(x, dataIn){
	names(dataIn)[apply(as.matrix(names(dataIn)), 
			1, 
			function(a){substr(a, 1, nchar(x))})  == x]
}

# Getting CV-optimal RPart and useful output

rpartOp <- function(formulaIn=NULL,
                    methodIn=NULL,
                    dataIn=NULL,
                    xvalIn=NULL,
                    minbucketIn=NULL,
                    cp_incIN=NULL,
                    target=NULL,
                    treatVar=NULL){
  if(target == "po"){
  treeOut <- rpart(formulaIn,
                   method=methodIn,
                   data=dataIn,
                   control=rpart.control(xval=xvalIn, 
                                         minbucket=minbucketIn,
                                         cp=cp_incIN),
                   model=TRUE)
  }
  if(target == "fx"){
	treeOut <- causalTree(formulaIn, 
                      data=dataIn,
                      treatment=treatVar,
                      split.Rule="CT",
                      split.Honest=F,
                      cv.option="CT",
                      cv.Honest=F,
                      split.alpha=1,
                      model=TRUE,
                      control=rpart.control(xval= xvalIn, 
                                         minbucket= minbucketIn,
                                         cp= cp_incIN)) 
  }  
  opcp <- treeOut$cptable[,1][which.min(treeOut$cptable[,4])]
  optreeOut <- prune(treeOut, opcp)
  resList <- list(tree=optreeOut, 
                  varimp=100*(optreeOut$variable.importance/sum(optreeOut$variable.importance)))
  return(resList)
}

# Plotting variable importance

rpart_impplot <- function(treeIn=NULL,
                          mainIn="",
                          labelScale=NULL){
  plot( treeIn$varimp,
        length(treeIn$varimp):1,
        xlim=c(0,100),
        axes=F,
        xlab="Importance measure",
        ylab="",
        main=mainIn,
        pch=19)
  axis(1, seq(0, 100, by=20))
  axis(2, at=length(treeIn$varimp):1, 
       labels=names(treeIn$varimp),
       las=2,
       cex.axis=labelScale)
  abline(v=seq(0, 100, by=20), lty="dashed")
  box()
}

# Fitting a GRF to estimate potential outcome response surfaces

library(grf)

grfFit <- function( dataIn,
                    Y="Y",
                    T="T",
                    X=NULL,
                    num.treesArg = 2000,
                    set.seedArg = 111){
        Y_tr = dataIn[,Y]  # outcome
        D_tr = dataIn[,T]  # treatment 
        X_tr = dataIn[,X] # Covariates
        set.seed(set.seedArg)
        e_hat <- regression_forest(X_tr,
                                  D_tr, 
                                  honesty=FALSE,
                                  tune.parameters = TRUE,
                                  num.trees=num.treesArg,
                                  num.fit.trees = min(10, num.treesArg)) 
        set.seed(set.seedArg)
        mu_1 <-  regression_forest( X_tr[D_tr==1,],
                                      Y_tr[D_tr==1],
                                  honesty=FALSE,
                                  tune.parameters = TRUE,
                                  num.trees=num.treesArg,
                                  num.fit.trees = min(10, num.treesArg))
        set.seed(set.seedArg)
        mu_0 <-  regression_forest( X_tr[D_tr==0,],
                                      Y_tr[D_tr==0],
                                  honesty=FALSE,
                                  tune.parameters = TRUE,
                                  num.trees=num.treesArg,
                                  num.fit.trees = min(10, num.treesArg))
       return(list(e_hat=e_hat, 
                   mu_1 = mu_1, 
                   mu_0 = mu_0))
}



