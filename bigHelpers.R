require(reader)
require(NCmisc)

#' Tidier display function for big matrix objects
#'
#' This function prints the first and last columns and rows of a big matrix, and
#' a few more than this if desired. Allows previewing of a big.matrix without 
#' overloading the console. 
#'
#' @param bigMat the description file, big.matrix object, or big.matrix.descriptor object
#' @param dir the directory the big.matrix backing/description files are in
#' @param name print a name for the matrix
#' @param dat whether to actually print any of the matrix contents (overrides row/col)
#' @param descr name of the description file, which if not null will be displayed
#' @param bck name of the backing file, which if not null will be displayed
#' @param mem logical, whether to display the amount of memory used by the object
#' @param row number of rows to display
#' @param col number of columns to display
#' @param rcap caption to display for the rows
#' @param ccap caption to display for the columns
#' @seealso print.large
#' @export
#' @examples
#' library(bigmemory)
#' bM <- filebacked.big.matrix(20, 50,
#'        dimnames = list(paste("r",1:20,sep=""), paste("c",1:50,sep="")),
#'        backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
#' bM[1:20,] <- replicate(50,rnorm(20))
#' print.big.matrix(bM)
#' print.big.matrix(bM,row=10,col=4)
print.big.matrix <- function(bigMat,dir="",row=3,col=2,name=NULL,dat=T,descr=NULL,bck=NULL,mem=F,rcap="",ccap="",...) {
  # print summary of big matrix contents
  must.use.package("bigmemory")
  if(is.null(name) & is.character(bigMat)) { name <- basename(bigMat[1]) } # if it's a file name
  bigMat <- getBigMat(bigMat,dir=dir)
  if(!is.null(name)) { 
    cat("\nBig matrix; '",name,"', with: ",sep="") 
  } else { cat("Big matrix with: ") }
  nC <- ncol(bigMat); nR <- nrow(bigMat)
  bcap <- paste(" (",c(rcap[1],ccap[1]),")",sep="") 
  if(rcap!="") { rcap <- bcap[1] }; if(ccap!="") { ccap <- bcap[2] }
  cat(nR," rows",rcap,", ",nC," columns",ccap,"\n",sep="") 
  if(is.sub.big.matrix(bigMat)) { cat("[a sub.big.matrix object]\n")}
  cat(" - data type:",is(bigMat[1,1])[1],"\n")
  if(!is.null(descr)) { cat(" - descriptor file;",descr,"\n") }
  if(is.filebacked(bigMat)) { 
    if(!is.null(bck)) {
      cat(" - backing file;",bck,"\n") }
  } else {
    cat(" - not filebacked! [only recommended when RAM is high versus datasize]")
  }
  if(dat) {
    print.large(bigMat,row=row,col=col,...)
  } else {
    if(!is.null(colnames(bigMat))) {
      cat(" - columns:",paste(colnames(bigMat)[1:max(row,col)],collapse=", "),
          "...",colnames(bigMat)[nC],"\n")
    } else { cat(" - no column names\n") }
    if(!is.null(rownames(bigMat))) {
      cat(" -    rows:",paste(rownames(bigMat)[1:max(row,col)],collapse=", "),
          "...",rownames(bigMat)[nR],"\n")  
    } else { cat(" - no row names\n") }
  }
  if(mem) {
    total.datapoints <- nR*nC
    disk.est <- round(estimate.memory(bigMat))
    cat("Total of",total.datapoints,"data-points, using",disk.est,"GB estimated disk space\n")
  }
  cat("\n")
}


#' Tidy display function for matrix objects
#'
#' This function prints the first and last columns and rows of a matrix, and
#' a few more than this if desired. Allows previewing of a matrix without 
#' overloading the console. Mainly useful when data has row and column names.
#'
#' @param largeMat a matrix
#' @param row number of rows to display
#' @param col number of columns to display
#' @param digits number of digits to display for numeric data
#' @param rL row label to describe the row names/numbers, e.g, row number, ID, etc
#' @param rlab label to describe the data rows
#' @param clab label to describe the data columns
#' @param rownums logical, whether to display rownumbers or ignore them
#' @param ret logical, whether to return the result as a formatted object, or just print to console
#' @seealso print.big.matrix
#' @export
#' @examples
#' mat <- matrix(rnorm(1000),nrow=50)
#' rownames(mat) <- paste("ID",1:50,sep="")
#' colnames(mat) <- paste("Var",1:20,sep="")
#' print.large(mat)
#' print.large(mat,row=9,col=4,digits=1,rL="#",rlab="samples",clab="variables")
print.large <- function(largeMat,row=3,col=2,digits=4,rL="Row#",rlab="rownames",clab="colnames",rownums=T,ret=F) 
{
  # nicely print a large matrix without overloading the output space
  # can return result as lines of text instead of printing to screen (for printing to file)
  # allows customization of row and column labels
  # only worth using with data that has row/col names
  if(length(dim(largeMat))!=2) { stop("expected largeMat to have 2 dimensions") }
  nC <- ncol(largeMat); nR <- nrow(largeMat); 
  if(nC<2 | nR<3) { warning("print.large only works for matrices with dims >= c(3,2), passed to print()")
                    print(largeMat); return(NULL) }
  row <- min(max(1,row),nR); col <- min(max(1,col),nC)
  cN <- colnames(largeMat); rN <- rownames(largeMat)
  if(is.null(cN)) { cN <- paste(1:ncol(largeMat)); clab <- "col#" }
  if(is.null(rN)) { rN <- paste(1:nrow(largeMat)); rlab <- "row#"; rownums=F }
  rD <- spc(min(2,max(nchar(paste(nR)))),".")
  rnD <- spc(min(4,max(nchar(rN[c(1:row,nR)]))),".")
  linez <- vector("list",row+3) #row,col =number of rows,cols to print
  rown <- max(nchar(paste(nR)),nchar(rL))*as.numeric(rownums)
  hdr <- (nchar(cN[c(1:col,nC)]))
  if(is.numeric(largeMat[1,])) {
    ## assess what the largest numbers are likely to be to adjust header spacing if necessary
    long.nums <- max(max(abs(largeMat[1,]),na.rm=T),max(abs(largeMat[,1]),na.rm=T))
    max.before.dp <- nchar(round(long.nums))+3
  } else { max.before.dp <- 6 }
  hdr[hdr<7] <- 7; hdr[hdr<(digits+max.before.dp)] <- (digits+max.before.dp)
  idln <- max(nchar(rlab),nchar(rN[c(1:row,nR)]))
  pad <- function(X,L) { paste(spc(L-nchar(X)),X,sep="") }
  if(!ret) { cat("\n"); cat(spc(rown),spc(idln),clab,"\n") }
  dotz <- "  ...  "; dotzh <- " ..... "; dotzn <- "..."
  # make adjustments if matrix is small enough to display all rows/cols
  if(nC<=col) { dotz <- dotzh <- "" ; col <- col-1 }
  if(nR<=row) { lstln <- 1 } else {  lstln <- 3 }
  ## make adjustments if not displaying rownumbers
  if(!rownums) {
    lstR <- "" ; rD <- ""; jstr <- rep("",times=row); rL=""
  } else {
    lstR <- nR; jstr <- paste(1:row)
  }
  linez[[1]] <- c(pad(rL,rown),pad(rlab,idln),pad(cN[c(1:col)],hdr[1:col]),
                  dotzh,pad(cN[nC],tail(hdr,1)))
  for (j in 1:row) { 
    linez[[j+1]] <- c(pad(jstr[j],rown),pad(rN[j],idln),
                      pad(round(largeMat[j,1:col],digits),hdr[1:col]),dotz,
                      pad(round(largeMat[j,nC],digits),tail(hdr,1)))
  }
  linez[[row+2]] <- c(pad(rD,rown),pad(rnD,idln),pad(rep(dotzn,times=col),
                                                   hdr[1:col]),dotz,pad(dotzn,tail(hdr,1)))
  linez[[row+3]] <- c(pad(lstR,rown),pad(rN[nR],idln),
                     pad(round(largeMat[nR,1:col],digits),hdr[1:col]),
                     dotz,pad(round(largeMat[nR,nC],digits),tail(hdr,1)))
  if(!ret) {
    for (j in 1:(row+lstln)) {
      cat(paste(linez[[j]],collapse=" "),"\n")
    }
  } else {
    # remove last two lines if all rows are displayed
    if(lstln==1) { for(ii in 1:2) { linez[[length(linez)]] <- NULL }  }
    return(linez)
  }
}


#' Estimate the variance percentages for uncalculated eigenvalues
#'
#' If using a function like irlba' to calculate PCA, then you can choose (for speed) 
#' to only calculate a subset of the eigenvalues. So there is no exact % of variance explained 
#' the PCA, or by each component as you will get as output from other routines.
#' This code uses a linear or b*1/x model to estimate the AUC for the unknown eigenvalues, providing
#' a reasonable estimate of the variances accounted for by each unknown eigenvalue, and
#' the predicted eigenvalue sum of the unknown eigenvalues.
#'
#' @param eigenv the vector of eigenvalues actually calculated
#' @param min.dim the size of the smaller dimension of the matrix submitted to singular
#'  value decomposition, e.g, number of samples - i.e, the max number of possible eigenvalues,
#'  alternatively use 'M'.
#' @param M optional enter the original dataset 'M'; simply used to derive the dimensions,
#'  alternatively use 'min.dim'.
#' @param elbow the number of components which you think explain the important chunk
#'  of the variance of the dataset, so further components are modelled as reflecting
#'  noise or very subtle effects, e.g, often the number of components used is decided
#'  by the 'elbow' in  a scree plot (see 'pca.scree.plots')
#' @param linear whether to use a linear model to model the 'noise' eigenvalues; alternative
#'  is a 1/x model with no intercept.
#' @param print.est whether to output the estimate result to the console
#' @param print.coef whether to output the estimate regression coefficients to the console
#' @param add.fit.line logical, if there is an existing scree plot, adds the fit line from this estimate
#'  to the plot ('pca.scree.plots' can use this option using the parameter of the same name)
#' @param col colour for the fit line
#' @param ignore.warn ignore warnings when an estimate is not required (i.e, all eigenvalues present)
#' @seealso pca.scree.plots
#' @export
#' @examples
#' nsamp <- 200; nvar <- 500; subset.size <- 50; elbow <- 6
#' mat <- matrix(rnorm(nsamp*nvar),ncol=nsamp) 
#' # or use: # mat <- crimtab-rowMeans(crimtab) ; subset.size <- 10 # crimtab centred
#' print.large(mat)
#' pca <- svd(mat,nv=subset.size,nu=0) # calculates subset of V, but all D
#' pca2 <- irlba(mat,nv=subset.size,nu=0) # calculates subset of V & D
#' pca3 <- princomp(mat,cor=T) # calculates all
#' # number of eigenvalues for svd is the smaller dimension of the matrix
#' eig.varpc <- estimate.eig.vpcs(pca$d^2,M=mat)$variance.pcs
#' cat("sum of all eigenvalue-variances=",sum(eig.varpc),"\n")
#' print(eig.varpc[1:elbow])
#' # number of eigenvalues for irlba is the size of the subset if < min(dim(M))
#' eig.varpc <- estimate.eig.vpcs((pca2$d^2)[1:subset.size],M=mat)$variance.pcs
#' print(eig.varpc[1:elbow])  ## using 1/x model, underestimates total variance
#' eig.varpc <- estimate.eig.vpcs((pca2$d^2)[1:subset.size],M=mat,linear=T)$variance.pcs
#' print(eig.varpc[1:elbow])  ## using linear model, closer to exact answer
#' eig.varpc <- estimate.eig.vpcs((pca3$sdev^2),M=mat)$variance.pcs
#' print(eig.varpc[1:elbow])  ## different analysis, but fairly similar var.pcs
estimate.eig.vpcs <- function(eigenv=NULL,min.dim=length(eigenv),M=NULL,elbow=NA,linear=T,
                              print.est=T,print.coef=F,add.fit.line=F,col="blue",ignore.warn=F) {
  ## if matrix is optionally inputted, calculate the minimum dim automatically
  if(!is.null(M)) { if(!is.null(dim(M))) { min.dim <- min(dim(M),na.rm=T) } }
  n.comp <- length(eigenv) # max(c(min.dim,length(eigenv)),na.rm=T)
  if(all(is.na(elbow))) { 
    if(n.comp==min.dim) {
      elbow <- quick.elbow(eigenv)
    } else {
      elbow <- 3 
    }
  }
  elbow <- round(min(n.comp,elbow,na.rm=T)) # make sure not > n.comp
  if(!is.numeric(eigenv)) { warning("eigenv not numeric"); return(NULL) }
  #catdb(c("elbow","min.dim","n.comp","eigenv"))
  if(is.na(min.dim) | ((min.dim-n.comp)<2) | ((n.comp-elbow)<(min.dim/20)) ) {
    # if most/all eigenvalues already present this is not needed, or if parameters insufficient
    # then don't try to calculate the AUC of the remaining eigenvalues
    if(n.comp==min.dim) {
      if(!ignore.warn) { cat("All eigenvalues present, estimate not required\n") }
    } else {
      warning("didn't attempt to estimate eigenvalues as there were",
        " very few unknowns compared to the number of samples,",
        " or not enough eigenvalues between the elbow and 'min.dim'")
    }
    var.pcs <- eigenv[1:n.comp]/(sum(eigenv)); tail.var <- 0
  } else {
    # estimate combined variance of eigenvalues not calculated by irlba using 1/x model
    if(!linear) {
      xx <- 1/(1:length(eigenv))
      ab <- lm(eigenv[elbow:n.comp]~0+xx[elbow:n.comp])$coef
      tail.var <- ((log(min.dim)-log(n.comp))*ab[1]) # integral evaluated
      mod.txt <- "[b/x, no intercept]"
      predy <- ab[1]*(1/c((elbow+1):min.dim))
    } else {
      xx <- 1:length(eigenv)
      ab <- lm(eigenv[elbow:n.comp]~xx[elbow:n.comp])$coef
      zeropoint <- round(ab[1]/abs(ab[2])); zeropoint <- min(c(min.dim,zeropoint),na.rm=T)
      tail.var <- (zeropoint-n.comp)*(ab[1]+((n.comp)*ab[2]))*.5
     # tail.var <- ((ab[1]*(zeropoint-n.comp))+((((n.comp-elbow)^2)-((zeropoint-elbow)^2))*ab[2]*.5)) # integral evaluated
      mod.txt <- "[a + bx]"
      predy <- ab[1]+(ab[2]*c((elbow+1):min.dim))
      predy[(zeropoint-elbow):(min.dim-elbow)] <- 0
    }
    # intercept ignored as the asymptote should theoretically be zero so assumption
    # values > this reflect noise variance that might dissipate as x--> n.samp
    if(print.est) {
      not.calc <- min.dim-length(eigenv)
      cat(" estimate of eigenvalue sum of",not.calc,"uncalculated eigenvalues:",(as.numeric(tail.var)),"\n")
    }
    if(print.coef) {
      cat(" slope",mod.txt,":",as.numeric(tail(ab,1)),"\n")
    }
    if(add.fit.line) {
      # add fitted line to scree plot if one exists
      predx <- c((elbow+1):min.dim); print(predy)
      #catdb(c("predx","predy"))
      print(length(predx)); print(length(predy))
      try(lines(predx,predy,col=col),T)
    }
    var.pcs <- eigenv[1:n.comp]/(sum(eigenv)+tail.var)
  }
  out <- list(var.pcs,tail.var)
  names(out) <- c("variance.pcs","tail.auc")
  return(out)
}


#' Make scree plots for any PCA
#'
#' Make a scree plot using eigenvalues from princomp, prcomp, svd, irlba, big.pca, etc.
#' Furthermore, if using a function like irlba' to calculate PCA, then you can choose (for speed) 
#' to only calculate a subset of the eigenvalues. So there is no exact % of variance explained 
#' the PCA, or by each component as you will get as output from other routines.
#' This code uses a linear or b*1/x model to estimate the AUC for the unknown eigenvalues, providing
#' a reasonable estimate of the variances accounted for by each unknown eigenvalue, using
#' the predicted eigenvalue sum of the unknown eigenvalues. This can be visualised
#' by adding the fitline of the estimate to the scree plot.
#'
#' @param eigenv the vector of eigenvalues actually calculated
#' @param elbow the number of components which you think explain the important chunk
#'  of the variance of the dataset, so further components are modelled as reflecting
#'  noise or very subtle effects, e.g, often the number of components used is decided
#'  by the 'elbow' in  a scree plot (see 'pca.scree.plots')
#' @param min.dim the size of the smaller dimension of the matrix submitted to singular
#'  value decomposition, e.g, number of samples - i.e, the max number of possible eigenvalues,
#'  alternatively use 'M'.
#' @param M optional enter the original dataset 'M'; simply used to derive the dimensions,
#'  alternatively use 'min.dim'.
#' @param linear whether to use a linear model to model the 'noise' eigenvalues; alternative
#'  is a 1/x model with no intercept.
#' @param printvar logical, whether to print summary of variance calculations
#' @param add.fit.line logical, if there is an existing scree plot, adds the fit line from this estimate
#'  to the plot ('pca.scree.plots' can use this option using the parameter of the same name)
#' @param n.xax number of components to include on the x-axis
#' @param ... further arguments to the plot function
#' @seealso pca.scree.plots
#' @export
# @examples
#' nsamp <- 200; nvar <- 500; elbow <- 6; subset.size <- 50
# this gives the full solution
#'pca <- svd(mat,nv=subset.size,nu=0)
# test with larger and smaller subset, larger gives 1/x better fit, smaller, x
#'pca2 <- irlba(mat,nv=subset.size,nu=0)
# show alternate fits for linear versus 1/x fit
#'pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=200,add.fit.line=T,
#'               min.dim=min(dim(mat)),linear=T, elbow=6, ylim=c(0,1400))
#'pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=200,add.fit.line=T,
#'               min.dim=min(dim(mat)),linear=F, elbow=6, ylim=c(0,1400))
#'subset.size <- 150
#'pca2 <- irlba(mat,nv=subset.size,nu=0)
#'pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=200,add.fit.line=T,
#'               min.dim=min(dim(mat)),linear=T, elbow=6, ylim=c(0,1400))
#'pca.scree.plot((pca2$d^2)[1:subset.size],n.xax=200,add.fit.line=T,
#'               min.dim=min(dim(mat)),linear=F, elbow=6, ylim=c(0,1400))
pca.scree.plot <- function(eigenv,elbow=NA,printvar=T,min.dim=NA,M=NULL,add.fit.line=F,n.xax=max(30,length(eigenv)),linear=T,...) 
{
  # do SCREE PLOTS AND calculate EIGENVALUE VARIANCE after a PCA
  if(!is.null(M)) { if(!is.null(dim(M))) { min.dim <- min(dim(M),na.rm=T) } }
  n.comp <- length(eigenv)
  if(all(is.na(elbow))) { 
    if(n.comp==min.dim) {
      elbow <- quick.elbow(eigenv)
    } else {
      elbow <- 3 
    }
  }
  elbow <- round(min(n.comp,elbow,na.rm=T))
  plot(eigenv[1:n.xax],bty="l",xlab="number of principle components",
       ylab="eigenvalues",bg="green",pch=21,...)
  abline(v=(elbow+.5),lty="dashed")
  legend("topright",legend=c("Principle components","scree plot 'elbow' cutoff"),
         pt.bg=c("green",NA),pch=c(21,NA),lty=c(NA,"dashed"),bty="n")
  scree.calc <- estimate.eig.vpcs(eigenv=eigenv,min.dim=min.dim,elbow=elbow,
                  print.est=T,print.coef=T,add.fit.line=add.fit.line,col="blue",linear=linear)
  if(printvar) {
    cat(" sum of eigen-variance:",round(sum(eigenv)+scree.calc$tail.auc,2),"\n")
    cat(" variance % estimates: \n ",round(scree.calc$variance.pcs,2),"\n")
  }
  return(scree.calc$variance.pcs)
}


# quickly choose an elbow for a PC. 
# at variance below 5% per component, choose the largest % drop
# designed for variance percentages, but will also work given a full set of Evalues
quick.elbow <- function(varpc,low=.04) {
  ee <- varpc/sum(varpc) # ensure sums to 1
  while(low>=max(ee)) { low <- low/2 } # when no big components, then adjust 'low'
  low.ones <- which(ee<low)
  others <- length(which(ee>=low))
  if(length(low.ones)>0) {
    if(length(low.ones)==1) {
      elbow <- low.ones 
    } else {
      set <- ee[low.ones]
      pc.drops <- abs(diff(set))/(set[1:(length(set)-1)])
      elbow <- which(pc.drops==max(pc.drops,na.rm=T))[1]+others
    }
  } else { 
    # if somehow there are no small eigenvalues, just choose the elbow as the second last
    elbow <- length(eigenv)-1 
  }
  if(elbow<1) {
    warning("elbow calculation failed, return zero")
    return(0)
  }
  return(elbow)
}



### add to NCmisc
## equivalent to base:::substitute but can do any length of arguments
# won't work if variable is NULL
Substitute <- function(x=NULL,...) {
  varlist <- list(...); out <- character(1)
  if(length(varlist)>0) { 
    extr <- Substitute(...)
  } else {
    extr <- NULL
  }
  if(!is.null(x)) { out[1] <- paste(substitute(x)) }
  out <- c(out,extr)
  return(out[out!=""])
}


## same as cat.db but no labels command, and input is without quotes
catdb <- function(...,counts=NULL) {
  varlist <- Substitute(...)
  return(cat.db(varlist,labels=NULL,counts=counts))
}

#' Output variable states within functions during testing/debugging
#'
#' # to put into NCmisc
#' By listing variables to track as character(), provides 'cat()' output 
#' of compact and informative variable state information, e.g, variable name, value,
#' datatype and dimension. Can also specify array or list elements, or custom labels.
#' @param varlist character vector, the list of variable(s) to report, which will trigger
#'  automatic labelling of the variable name, otherwise if entered as the variable value (ie.
#'  without quotes, then will by default be displayed as 'unknown variable')
#' @param labels, will label 'unknown variables' (see above) if entered as variables without quotes
#' @param counts a list of array index values; so if calling during a counting loop, the
#'  value can be reported each iteration, also printing the count index; if the list is
#'  named the name will also appear, e.g, variable[count=1]. This list must be the same
#'  length as varlist (and labels if not NULL), and each element [[i]] must contain as many values
#'  as the original corresponding varlist[i] has dimensions
#' @seealso Dim
# @export
# @examples
#' # create variables of different types to show output styles #
#' testvar1 <- 193
#' testvar2 <- "Atol"
#' testvar3 <- c(1:10)
#' testvar4 <- matrix(rnorm(100),nrow=25)
#' testvar5 <- list(first="test",second=testvar4,third=100:110)
#' cat.db("testvar1")
#' cat.db("testvar4")
#' cat.db(paste("testvar",1:5,sep=""))
#' cat.db(testvar1,"myvarname")
#' cat.db(testvar1)
#' # examples with loops and multiple dimensions / lists
#' for (cc in 1:4) {
#'  for (dd in 1:4) { cat.db("testvar4",counts=list(cc,dd)) }}
#'
#' for (dd in 1:3) { cat.db("testvar5",counts=list(dd=dd)) }
cat.db <- function(varlist,labels=NULL,counts=NULL) {
  ## for debugging, simplify code to print vars you are checking
  lab <- varlist
  # test whether 'counts' sublists are all of the same length as varlist, else ignore 'counts'
  if(is.list(counts)) {  if(!all(sapply(counts,length)==length(varlist))) { 
    counts <- NULL } } else { if(length(counts)==length(varlist)) { counts <- list(counts) } else { counts <- NULL } }
  #val <- vector("list",length(lab))
  display.var <- function(val,label,cnts=NULL) {
    if(is(cnts)[1]=="list") {
      ## if vars to debug have a counter, update the value and label with count(s)
      if(is(val)[1]=="list") { 
        for (dd in 1:length(cnts)) {
          val <- val[[ cnts[[dd]] ]] 
          if(!is.null(names(cnts))) { 
            label <- paste(label,"[[",names(cnts)[dd],"=",cnts[[dd]],"]]",sep="") 
          } else {
            label <- paste(label,"[[",cnts[[dd]],"]]",sep="")
          }
        }
      } else {
        #val <- val[cnts[[dd]] ]
        #cat.db(c("val","cnts"))
        if(length(Dim(val))!=length(cnts)) {
          val <- val ; warning("counts did not match dimensions")
        } else {
          arg.list <- vector("list",1+length(cnts)); arg.list[[1]] <- val
          arg.list[2:(1+length(cnts))] <- cnts
          val <- do.call("[",args=arg.list)
          if(!is.null(names(cnts))) { 
            label <- paste(label,"[",
                           paste(paste(names(cnts),"=",cnts,sep=""),collapse=","),"]",sep="") 
          } else {
            label <- paste(label,"[",paste(cnts,collapse=","),"]",sep="")
          }
        }
      }
    } else {
      #counts not a list
    }
    ## display appropriately according to datatype ##
    typ <- is(val)[1]
    if(length(unlist(val))==1) {
      cat(label,": ",val," (",typ,", ",Dim(val),")",sep=""); return(invisible())
    } 
    if(is(val)[1]=="list") {
      cat(label," (",typ,", ",Dim(val),")\n",sep=""); print(headl(val)); return(invisible())
    }
    if(typ=="big.matrix") {
      print.big.matrix(val,name=label); return(invisible())
    } else {
      if(!is.null(dim(val))) {
        cat(label," (",typ,", ",Dim(val),")\n",sep="");
        print.large(val)
        return(invisible())
      } else {
        cat(label," (",typ,", ",Dim(val),") [head]:\n",sep="")
        print(head(val))
        return(invisible())
      }
    }
  }
  ## if data not entered as
  if(!is.character(varlist) | !is.null(labels)) {
    if(is.null(labels) | ((length(labels)!=1) & (length(varlist)!=length(labels)))) {
      display.var(varlist,"unknown variable"); cat("\n")
    } else { 
      for(cc in 1:length(labels)){
        if(is.list(counts)) { cnts <- lapply(counts,"[",cc) } else { cnts <- NULL }
        if(is.list(varlist)) {
          display.var(varlist[[cc]],labels[cc],cnts=cnts)
        } else {
          display.var(varlist[cc],labels[cc],cnts=cnts)
        }
        cat("\n") 
      }
      return(invisible())
    }
    return(invisible())
  } 
  ENVIR <- parent.frame()
  for(cc in 1:length(lab)) {
    label <- lab[cc]
    #print(sys.parent())
    #print(sys.nframe())
    #print(sys.frame(-1))
    val <- get(lab[cc],envir=ENVIR)
    if(is.list(counts)) { cnts <- lapply(counts,"[",cc) } else { cnts <- NULL }
    display.var(val,label,cnts=cnts)
    cat("\n") 
  }
  return(invisible())
}


#' A more general 'dim()' function
#'
#' # to put into NCmisc
#' A more general 'dim' function. For arrays simply calls the dim() function, but for other data types, tries to
#' provide an equivalent, for instance will call length(x) for vectors, and will
#' recursively report dims for lists, and will attempt something sensible for other datatypes.
#' @param x the object to find the dimension for
#' @param cat.lists logical, for lists, TRUE will concatenate the dimesions to a single string,
#'  or FALSE will return the sizes as a list of the same structure as the original.
#' @seealso catdb, cat.db
#' @export
#' @examples
#' # create variables of different types to show output styles #
#' Dim(193)
#' Dim(1:10)
#' Dim(matrix(rnorm(100),nrow=25))
#' Dim(list(first="test",second=testvar4,third=100:110))
#' Dim(list(first="test",second=testvar4,third=100:110),F)
Dim <- function(x,cat.lists=T) {
  rez <- NULL
  try(rez <- dim(x))
  if(!is.null(rez)) { return(dim(x)) }
  if(is(x)[1]=="list") { 
    out <- lapply(x,Dim) 
    if(cat.lists) {
      out <- paste(out,collapse="; ")
    }
  } else { out <- length(x) }
  return(out)  
}


# replaced by bmcapply
# multi.fn.on.big.split <- function(bigMat,func,dir=NULL,bycol=T,by=200,n.cores=1,chunkwise=F,split.arg=NULL,...) {
#   # multicore way of calculating a function (e.g, dlrs) for a big.matrix,
#   # when chunkwise=F, a function that could be done with apply(); when chunkwise=T, one that
#   # is best done in chunks rather than row by row or col by col
#   # can do it column wise (bycol=T, eg for all samples), or row-wise, eg for all snps
#   # 'by' is number of rows/cols to process in each chunk
#   must.use.package("multicore")
#   if(bycol) { tot.main <- ncol(bigMat); d2 <- nrow(bigMat) } else { tot.main <- nrow(bigMat); d2 <- ncol(bigMat) }
#   if(!is.null(split.arg)) { 
#     if(!is.null(dim(split.arg))) {
#       if(bycol) { 
#         if(ncol(split.arg)!=tot.main) { 
#           split.arg <- NULL ; warning("split.arg should have same number of columns as bigMat")
#       } } else {
#         if(nrow(split.arg)!=tot.main) { 
#           split.arg <- NULL ; warning("split.arg should have same number of rows as bigMat")
#       } }
#     } else {
#       if(is.vector(split.arg) & length(split.arg)!=tot.main) { 
#         split.arg <- NULL; warning("split arg needs to be the same length as the key bigMat dimension (",tot.main,")") 
#     } }
#   }
#   if(!is.null(split.arg)) { 
#     n.args.func <- length(formals(func))
#     if(n.args.func<2) { split.arg <- NULL ; warning("split arg ignored as function only has 1 parameter") }
#     if(n.args.func>2) { warning("split.arg must refer to the second argument of 'func' or results may be unpredictable") }
#   }
#   stepz <- round(seq(from=1,to=(tot.main+1),by=by))
#   if((tail(stepz,1)) != tot.main+1) { stepz <- c(stepz,tot.main+1) }
#   split.to <- length(stepz)-1
#   result <- numeric(tot.main)
#   ## define the function
#   big.fnc <- function(dd,func,stepz,bigMat,dir,bycol=T,split.arg=NULL,...)
#   {
#     big.type <- is.big.matrix(bigMat)
#     x1 <- stepz[dd]; x2 <- stepz[dd+1]-1 #subset row selection
#     if(bycol) {
#       if(big.type) {
#         next.block <- sub.big.matrix(bigMat, firstCol=x1, lastCol=x2, backingpath=dir )
#       } else {
#         next.block <- bigMat[,x1:x2]
#       }
#       dm <- 2
#     } else {
#       if(big.type) {
#         next.block <- sub.big.matrix(bigMat, firstRow=x1, lastRow=x2, backingpath=dir )
#       } else {
#         next.block <- bigMat[x1:x2,]
#       }
#       dm <- 1
#     }
#     if(!is.null(split.arg)) {
#       if(is.null(dim(split.arg))) { split.arg <- split.arg[x1:x2] } else {
#         if(bycol) { split.arg <- split.arg[,x1:x2] } else { split.arg <- split.arg[x1:x2,] }
#       }
#       print(length(split.arg))
#       # if a valid split.arg, then enter it as the second parameter of the function
#       if(chunkwise) { out <- func(next.block,split.arg,...)  } else {  out <- apply(next.block,dm,func,split.arg,...) }
#     } else {
#       if(chunkwise) { out <- func(next.block,...)  } else {  out <- apply(next.block,dm,func,...) }
#     }
#     rm(next.block) ; gc() # remove the sub-matrix pointer each iteration  
#     return(out)
#   }
#   ## run function as mclapply()
#   if(split.to>=1) {
#     result.list <- multicore::mclapply(1:split.to, big.fnc, func=func, stepz=stepz, 
#                                        bigMat=bigMat, dir=dir, bycol=bycol, mc.cores=n.cores,split.arg=split.arg,...)
#     if(!chunkwise) { result <- unlist(result.list) } # if apply-type, then recombine result, else leave as list
#   } else {
#     result <- NULL; warning("matrix had insufficient columns returning NULL")
#   }
#   return(result)  
# }




#' A multicore 'apply' function for big.matrix objects
#'
#' # to put into NCmisc
#' Multicore method to run a function for a big.matrix that could be run using 'apply'
#' on a regular matrix (when parameter use.apply=T [default]). Otherwise for a
#' function that might be more efficient done in done in chunks (e.g, utilising vectorised 
#'  functions) use.apply=F can be set so that processing is done on larger submatrices, rather
#' than 1 row/column at a time. Input to specify whether to perform the function
#' row or columnwise is equivalent to 'apply' syntax, 1=by-rows, 2=by-columns.
#' This function is useful for big.matrix processing even without multiple cores, particulary
#' when MARGIN=1 (row-wise). While native colmean, colmin and colsd functions for 
#' big.matrix objects are very fast (and will probably outperform bmcapply even with 1
#'  core versus many), these are only natively implemented for column-wise operations and 
#' the equivalent operations if needing to be row-wise should be faster with bmcapply for
#' matrices larger than available RAM.
#' Can also be used for regular matrices although there is unlikely to be a speed advantage.
#' @seealso getBigMat
#' @param bigMat the big.matrix object to apply the function upon, can enter as a filename,
#'  description object or any other valid parameter to getBigMat(). Can also use with a standard matrix
#' @param MARGIN 1=row-wise, 2=column-wise, see same argument for base:::apply()
#' @param FUN the function to apply, should return a result with 1 dimension that has the
#'  same length as dim(bigMat)[MARGIN]=L; i.e, a vector length L, matrix (L,x) or (x,L) or list[[L]].
#'  Note that using a custom 'combine.fn' parameter might allow exceptions to this.
#' @param dir directory argument for getBigMat(), ie. the location of the bigMat backing file if 
#'  not in the current working directory.
#' @param by integer, the number of rows/columns to process at once. The default should work in most
#'  situations however, if the dimension not specified by MARGIN is very large, this might need
#'  to be smaller, or if the function being applied is much more efficient performed 
#'  on a large matrix than several smaller ones then this 'by' parameter should be increased
#'  within memory contraints. You should make sure 'estimate.memory(c(by,dim(bigMat)[-MARGIN]))'
#'  doesn't exceed available RAM.
#' @param n.cores integer, the number of parallel cores to utilise; note that sometimes if a machine
#'  has only a few cores this can result in slower performance by tying up resources
#'  which should be available to perform background and system operations.
#' @param use.apply logical, if TRUE then use the 'apply' function to apply FUN to
#'  each submatrix, or if FALSE, then directly apply FUN to submatrices, which
#'  means that FUN must return results with at least 1 dimension the same as the input, 
#'  or you can use a custom 'combine.fn' parameter to recombine results from submatrices.
#' @param convert only need to change this parameter when use.apply=FALSE. If use are using a 
#'  function that can natively run on big.matrix objectsthen you can increase speed 
#'  by setting convert=FALSE. Most functions will expect a regular matrix
#'   and may fail with a big.matrix, so default convert=TRUE behaviour
#'  will convert submatrices to a regular matrix just before processing.
#' @param combine.fn a custom function to recombine input from sub.matrix processing. Default
#'  combine functions are list(), cbind() and rbind(); so a custom function should
#'  expect the same input as these; ie., a list of unspecified length, which will be
#'  the list of results from parallel calls on submatrices of bigMat, usually of size by*X.
#' @param ... if use.apply=TRUE, then additional arguments for apply(); else additional arguments
#'  for FUN.
#' @export
#' @examples
#' library(bigmemory); library(biganalytics)
#' # set up a toy example of a big.matrix (functions most relevant when matrix is huge)
#' bM <- filebacked.big.matrix(20, 50,
#'        dimnames = list(paste("r",1:20,sep=""), paste("c",1:50,sep="")),
#'        backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
#' bM[1:20,] <- replicate(50,rnorm(20))
#' print.big.matrix(bM)
#' # compare native bigmemory column-wise function to multicore [native probably faster]
#' v1 <- colsd(bM) # native bigmemory function
#' v2 <- bmcapply(bM,2,sd,n.cores=10) # use up to 10 cores if available
#' print(all.equal(v1,v2))
#' # compare row-means approaches
#' v1 <- rowMeans(as.matrix(bM))
#' v2 <- bmcapply(bM,1,mean,n.cores=10)
#' v3 <- bmcapply(bM,1,rowMeans,use.apply=F)
#' print(all.equal(v1,v2)); print(all.equal(v2,v3))
#' # example using a custom combine function; taking the mean of column means
#' weight.means.to.scalar <- function(...) { X <- list(...); mean(unlist(X)) }
#' v1 <- bmcapply(bM, 2, sd, combine.fn=weight.means.to.scalar)
#' v2 <- mean(colsd(bM))
#' print(all.equal(v1,v2))
#' ## note that this function works with normal matrices, however, multicore
#' # operation is only likely to benefit speed when operations take more than 10 seconds
#' # so this function will mainly help using large matrices or intensive functions
#' test.size <- 5 # try increasing this number, or use more intensive function than sd()
#' to test relative speed for larger matrices
#' M <- matrix(runif(10^test.size),ncol=10^(test.size-2)) # normal matrix
#' system.time(bmcapply(M,2,sd,n.cores=10)) # use up to 10 cores if available
#' system.time(apply(M,2,sd)) # 
bmcapply <- function(bigMat,MARGIN,FUN,dir=NULL,by=200,n.cores=1,use.apply=T,convert=!use.apply,combine.fn=NULL,...) {
  # multicore way of calculating a function (e.g, dlrs) for a big.matrix,
  # when use.apply=T, a function that could be done with apply(); when use.apply=F, one that
  # is best done in chunks rather than row by row or col by col
  # can do it column wise (bycol=T, eg for all samples), or row-wise, eg for all snps
  # 'by' is number of rows/cols to process in each chunk
  if(is.null(dir)) { dir <- getwd() } else { if(!file.exists(dir)) { dir <- getwd() } }
  if(!as.numeric(MARGIN)[1] %in% c(1,2)) { stop("MARGIN must be 1=rows, or 2=columns") }
  if(!is.function(FUN)) { stop("FUN must be a function") }
  if(!is.numeric(by)) { by <- 200 } else { by <- round(by) }
  bycol <- as.logical(as.numeric(MARGIN[1])-1)
  must.use.package("multicore")
  if(is.null(dim(bigMat))) { stop("this function only works on matrix objects") }
  if(length(dim(bigMat))!=2) { stop("this function only works on matrix objects") }
  if(bycol) { tot.main <- ncol(bigMat) } else { tot.main <- nrow(bigMat)}
  stepz <- round(seq(from=1,to=(tot.main+1),by=by))
  if((tail(stepz,1)) != (tot.main+1)) { stepz <- c(stepz,(tot.main+1)) }
  split.to <- length(stepz)-1
  result <- numeric(tot.main)
  ## define the function
  big.fnc <- function(dd,func,stepz,bigMat,dir,bycol=T,...)
  {
    big.type <- is.big.matrix(bigMat)
    x1 <- stepz[dd]; x2 <- stepz[dd+1]-1 #subset row selection
    if(bycol) {
      if(big.type) {
        next.block <- sub.big.matrix(bigMat, firstCol=x1, lastCol=x2, backingpath=dir )
      } else {
        next.block <- bigMat[,x1:x2]
      }
      dm <- 2
    } else {
      if(big.type) {
        next.block <- sub.big.matrix(bigMat, firstRow=x1, lastRow=x2, backingpath=dir )
      } else {
        next.block <- bigMat[x1:x2,]
      }
      dm <- 1
    }
    if(convert | is.null(dim(next.block))) { next.block <- as.matrix(next.block) } # conv to standard matrix if req'd
    if(!use.apply) { out <- func(next.block,...)  } else {  out <- apply(next.block,dm,func,...) }
    rm(next.block) ; gc() # remove the sub-matrix pointer each iteration  
    return(out)
  }
  ## run function as mclapply()
  if(split.to>=1) {
    result.list <- multicore::mclapply(1:split.to, FUN=big.fnc, func=FUN, stepz=stepz, 
                                       bigMat=bigMat, dir=dir, bycol=bycol, mc.cores=n.cores,...)
    if(is.function(combine.fn)) {
      result <- do.call(combine.fn,args=result.list)
    } else {
      comb.fn <- choose.comb.fn(result.list,stepz) # automatically decide best combining function based on largest and most common result
      result <- do.call(comb.fn,args=result.list)
      #result <- unlist(result.list,recursive=unlist.results) 
    }
  } else {
    result <- NULL; warning("matrix had insufficient columns returning NULL")
  }
  return(result)  
}


## internal function
choose.comb.fn <- function(result.list,stepz) {
  ### NB: when usefulBox is a package this should require(usefulBox)
  # choose the best function to combine multicore results from bigmcapply
  ## DEFINE function to choose best based on 1 example result:
  sub.comb.fn <- function(dm,ls,ll) {
    # evaluate output based on dimensions of an example result
    if(ls==ll) { comb.fn <- "c" } else {
      if(!is.null(dm)){
        if(dm[1]==dm[2] & dm[1]==ls) {
          warning("looks like results have dim[1]==dim[2], suggest using 'combine.fn' parameter to obtain desired result")
          comb.fn <- c("list","cbind","rbind")
        } else {
          if(dm[1]==ls | dm[2]==ls) {
            if(dm[2]==ls) {
              comb.fn <- "cbind"
            } else {
              comb.fn <- "rbind"
            }
          } else {
            warning("mapping from function results was not obvious, suggest using 'combine.fn' parameter")
            comb.fn <- "list"
          }
        }
      } else {
        comb.fn <- "list"
      }
    }
    return(comb.fn)
  }
  ## process max and mode results using function defined above
  rawlens <- sapply(result.list,function(x) { length(unlist(x)) })
  #print(is(rawlens)); print(dim(rawlens)); print(head(rawlens))
  lL <- length(rawlens); if(length(rawlens)>1) { rawlens <- rawlens[-lL] }
  max.res <- which(rawlens==max(rawlens,na.rm=T))[1] # choose the largest result to define structure
  mode.res <- which(rawlens==Mode(rawlens,multi=T))[1] # choose the largest result to define structure
  dm <- dim(result.list[[max.res]]); ll <- length(result.list[[max.res]]); ls <- length(stepz[max.res]:stepz[max.res+1])-1
  fn.max <- sub.comb.fn(dm,ls,ll)
  dm <- dim(result.list[[mode.res]]); ll <- length(result.list[[mode.res]]); ls <- length(stepz[mode.res]:stepz[max.res+1])-1
 # print(dm); print(ls); print(ll); print(mode.res); print(max.res)
  fn.mode <- sub.comb.fn(dm,ls,ll)
  if(fn.max[1]!=fn.mode[1]) {
    # resolve any conflict between results based on max or mode
    allf <- c(fn.max,fn.mode); 
    if(length(allf)==4) { 
      fn.max <- Mode(allf) # by chance one had equal dimensions, but this should pick the best by convergence
    } else {
      warning("differential testing of result output from multiple cores gave different structures, suggest using 'combine.fn' to ensure correct output")
    }
  }
  return(fn.max[1])
}


#' Attempt to install the bigalgebra package using SVN
#'
#' The bigalgebra package for efficient algebraic operations on big.matrix objects
#' is not currently on CRAN, and fails a check on dependencies. Changing the 
#' description file to add the dependency, and linking 'BH' allows the package to work.
#' This function attempts to check-out the latest version of bigalgebra from SVN
#' version management system and corrects the description file then installs.
#' Note you must also have 'BLAS' installed on your system to utilise this package
#' effectively. PCA functions in the present package are better with bigalgebra installed,
#' but will still run without it. For more information on installation alternatives, 
#' type big.algebra.install.help().
#' Returns TRUE if bigalgebra is already installed.
#' @seealso big.algebra.install.help
#' @param verbose whether to report on installation progress/steps
#' @export
#' @examples
#' # not run # svn.bigalgebra.install(TRUE)
svn.bigalgebra.install <- function(verbose=F) {
  # this is a major hack to install bigalgebra from SVN,
  # manually modifying the DESCRIPTION file to depend and link to 'BH'
  cur.dir <- getwd()
  cat("\nAttempting to install the bigalgebra package using SVN")
  if(verbose) { cat("\n") } else { cat(" .") }
  #my.fn <- file("bigalgebra.install.log",open="w")
  #sink(file=my.fn,type="message")
  if(!check.linux.install("svn")) { return(F) }
  nons <- cat.path(getwd(),"tempfdsg345t")
  dir.create(nons)
  setwd(nons)
  system("svn checkout svn://scm.r-forge.r-project.org/svnroot/bigmemory",
         intern=!verbose, ignore.stderr=!verbose)
  cat(".")
  setwd("./bigmemory/pkg")
  a.mod <- F
  des.fn <- "./bigalgebra/DESCRIPTION"
  if(is.file(des.fn,dir=getwd())) {
    DES <- readLines(des.fn); cat(".")
    l1 <- grep("Depends: bigmemory",DES)
    l2 <- grep("LinkingTo: bigmemory",DES)
    if(length(l1)==1 & length(l2)==1) {
      if(length(grep("BH",l1))==0) {
        if(verbose) { cat("modifying bigalgebra DESCRIPTION file to depend on BH\n") }
        DES[l1] <- gsub("Depends: bigmemory","Depends: BH, bigmemory",DES[l1])
        a.mod <- T; cat(".")
      }
      if(length(grep("BH",l2))==0) {
        if(verbose) { cat("modifying bigalgebra DESCRIPTION file to link to BH\n") }
        DES[l2] <- gsub("LinkingTo: bigmemory","LinkingTo: BH, bigmemory",DES[l2])
        a.mod <- T  ; cat(".")
      }
    }
    if(a.mod) {
      writeLines(DES,con=des.fn); cat(".")
    }
    system("REFBLAS=1 R CMD INSTALL bigalgebra",intern=!verbose, ignore.stderr=!verbose)
    cat(". done\n")
    suc <- T
  } else {
    warning("bigalgebra DESCRIPTION file not found, installation failed")
    suc <- F
  }
  setwd(nons)
  system("rm -rf bigmemory", intern=!verbose, ignore.stderr=!verbose)
  setwd(cur.dir)
  unlink(nons)
  return(suc)
}


#' Attempt to install the bigalgebra package
#'
#' Will attempt to see whether bigalgebra is installed, then check CRAN in case it
#' has been updated, then check RForge. Failing that, it will attempt to install
#' using svn.bigalgebra.install(). Returns TRUE if already installed.
#' The bigalgebra package for efficient algebraic operations on big.matrix objects
#' is not currently on CRAN, and fails a check on dependencies. Changing the 
#' description file to add the dependency, and linking 'BH' allows the package to work.
#' This function attempts to check-out the latest version of bigalgebra from SVN
#' version management system and corrects the description file then installs.
#' Note you must also have 'BLAS' installed on your system to utilise this package
#' effectively. PCA functions in the present package are better with bigalgebra installed,
#' but will still run without it. For more information on installation alternatives, 
#' type big.algebra.install.help().
#' @seealso svn.bigalgebra.install
#' @param verbose whether to report on installation progress/steps
#' @export
#' @examples
#' # not run # big.algebra.install.help(TRUE)
big.algebra.install.help <- function(verbose=F) {
  ## bigalgebra package doesn't install easily using the regular R way of installing packages
  # here try a simple way that might work, and if not, provide links and instructions to 
  # guide a manual installation
  try({ if(require(bigalgebra)) { return(T) } })
  cat("\nbigalgebra installation not found, will attempt to install now, but it can be tricky\n")
  if("bigalgebra" %in% search.cran("big")[[1]]) { must.use.package("bigalgebra",T) }
  do.call("install.packages",args=list("bigalgebra", repos="http://R-Forge.R-project.org"))
  if(require(bigalgebra)) {
    cat("bigalgebra seems to have installed successfully\n")
    return(T)
  } else {
    tt <- svn.bigalgebra.install(verbose=verbose)
    if(!tt) {
      cat("standard bigalgebra installation has failed\n")
      cat("go to: http://connectir.projects.nitrc.org/download/\n")
      cat("for installation tips\n")
      cat("can use a command line like: ‘REFBLAS=1 R CMD INSTALL bigalgebra’\n")
      cat("where ‘bigalgebra’ is the source package (for example, ‘bigalgebra_0.8.1.tar.gz’)\n")
      cat("downloaded from https://r-forge.r-project.org/R/?group_id=556\n")
      cat("Your system may also be missing a BLAS installation, in which case you might try\n")
      cat("installing OpenBLAS; see instructions at http://xianyi.github.com/OpenBLAS/\n")
      return(F)
    } else {
      return(T)
    }
  }
}


#' Retrieve a big.matrix object
#'
#' This function can load a big.matrix object using a big.matrix.descriptor object, the
#' name of a description file, the name of a binary file containing a big.matrix.descriptor
#' or if passed a big.matrix object, it will just return that object. Only the object or
#' file name plus the directory containing the backing file are required.
#' @param fn the name of a description file, the name of a binary file containing a
#'  big.matrix.descriptor, a big.matrix object or a big.matrix.descriptor object.
#' @param dir directory containing the backing file (if not the working directory)
#' @param verbose whether to display information on method being used, or minor warnings
#' @export
#' @examples
#' library(bigmemory); 
#' # set up a toy example of a big.matrix 
#' bM <- filebacked.big.matrix(20, 50,
#'        dimnames = list(paste("r",1:20,sep=""), paste("c",1:50,sep="")),
#'        backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
#' bM[1:20,] <- replicate(50,rnorm(20))
#' # Now have a big matrix which can be retrieved using this function in 4 ways:
#' d.bM <- describe(bM)
#' save(d.bM,file="fn.RData")
#' bM1 <- getBigMat("test.dsc")
#' bM2 <- getBigMat(d.bM)
#' bM3 <- getBigMat("fn.RData")
#' bM4 <- getBigMat(bM)
#' print.big.matrix(bM)
#' print.big.matrix(bM1)
#' print.big.matrix(bM2)
#' print.big.matrix(bM3)
#' print.big.matrix(bM4)
getBigMat <- function(fn,dir="",verbose=F)
{
  # loads a big.matrix either using an big.matrix description object
  # , or this object in a binary file or text file, or points to a bigmatrix or matrix
  if(all(dir=="")) { dir <- getwd() }
  if(exists("validate.dir.for",mode="function")) {
    ## plumbCNV specific code ##
    dir <- do.call("validate.dir.for",list(dir=dir,elements="big",warn=F))  
    dir.big <- dir$big
  } else {
    # otherwise
    dir.big <- dir
    if(is.list(dir)) { if(!is.null(dir[["big"]])) { dir.big <- dir$big } }
  }
  if(is(fn)[1]=="big.matrix.descriptor")
  {
    bigMat2 <- attach.big.matrix(fn,path=dir.big)
  } else {
    if(is(fn)[1]=="big.matrix" | is(fn)[1]=="matrix")
    {
      if(is(fn)[1]=="matrix") {
        bigMat2 <- as.big.matrix(fn,descriptorfile="TEMPBIG",backingpath=dir.big)
      } else { bigMat2 <- fn }
    } else {
      lastchar <- substr(dir.big,nchar(dir.big),nchar(dir.big))
      if (length(grep(".RData",fn))==0) {
        fn <- basename(fn)
        if(!fn %in% list.files(dir.big)) { 
          stop(paste("Error: big.matrix file '",fn,"' not in 'dir.big'",sep=""))
        }
        if(verbose) { cat(" loading big matrix using text description\n") }
        if(lastchar=="/") { dir.big <- substr(dir.big,1,nchar(dir.big)-1) }
        bigMat2 <- attach.big.matrix(fn,path=dir.big)
      } else {
        if(verbose) { cat(" loading big matrix using RData description\n") }
        if(lastchar!="/") { dir.big <- paste(dir.big,"/",sep="") }
        filenm <- cat.path(dir.big,fn,must.exist=T)
        dscnm <- paste(load(filenm))
        big.fn <- NULL
        for (ii in 1:length(dscnm)) {
          if("big.matrix.descriptor" %in% is(get(dscnm[ii])))
          { big.fn <- dscnm[ii] } 
        }
        if(!is.null(big.fn)) {
          descr <- get(big.fn) 
        } else {
          stop(paste("Error: didn't find bigmatrix descriptor in file",fn))
        }
        bigMat2 <- attach.big.matrix(descr,path=dir.big) 
      }
    }
  }
  return(bigMat2)
}





# internal? function to quickly extract whether a delimited matrix
# has row or column names, and what the column names are
quick.mat.format <- function(fn) {
  temp.fn <- "twmpwerw123.txt"
  txtx <- readLines(fn,n=11) # change this to like 5 when reader is updated to save memory
  writeLines(txtx,con=temp.fn)
  first2 <- reader(temp.fn)
  if(is.null(dim(first2))) {
    ## assuming no header row #
    return(list(rownames=F,colnames=F,ncol=1,cnames=NULL))
  }
  rn <- rownames(first2); if(rn[1]=="1") { rn <- NULL }
  if(!is.null(rn)) { ern <- T } else { ern <- F }
  cn <- colnames(first2); if(cn[1]=="V1") { cn <- NULL }
  if(!is.null(cn)) { ecn <- T } else { ecn <- F }
  ncl <- ncol(first2)
  unlink(temp.fn)
  return(list(rownames=ern,colnames=ecn,ncol=ncl,cnames=cn))
}


## internal function for import.big.matrix
check.text.matrix.format <- function(fn,ncol=NA,header=NULL,row.names=NULL,sep="\t") 
{
  # read the first two lines of a text matrix file and determine
  # whether it has row and or column names, return T/F indices for this
  # plus a further 
  # assumes tab as separator
  fn <- fn[1]
  if(!is.character(fn)) { warning("file name 'fn' should be a character()") }
  if(!file.exists(fn)) { stop(paste("Error: file",fn,"not found")) } 
  dat.file <- file(fn); headrow <- NULL
  rnames.in.file <- F; first.is.head <- F; name.lookup <- F
  # read first two lines of matrix format datafile
  open(con=dat.file,open="r")
  next.line <- readLines(dat.file,n=1)
  line1 <- strsplit(next.line,sep,fixed=T)[[1]]
  next.line <- readLines(dat.file,n=1)
  line2 <- strsplit(next.line,sep,fixed=T)[[1]]
  close(con=dat.file)
  ## check whether first row is header or just plain data
  frst <- length(line1)
  scnd <- length(line2)
  if(is.na(ncol)) {
    if((scnd-frst)==1) { rnames.in.file <- T; first.is.head <- T; name.lookup <- F;
                         headrow <- line1 }
  } else {
   # catdb(c("frst","ncol"))
    if (frst!=ncol & frst!=ncol+1) {
      #cat.db(c("frst","ncol"))
      stop("dimensions of import file do not match id lists specified, exiting")
      break; break; 
    } else {
      if(length(which(paste(line1[1:10]) %in% paste(header)))>8) {
        # first line seems to be the header
        if(!all(header==paste(line1[(frst-ncol+1):frst]))) {
          stop("Error: ID list did not match file header. Please check the files (bash$ head -1 <filename>) and fix this")
        } else {
          # will need to go to next line to avoid reading header as data
          first.is.head <- T
        }
      } else {
        first.is.head <- F
      }
    }
    ## check whether second row starts with a rowname, or just plain data
    if(length(line2)==ncol+1) {
      rnames.in.file <- T
    } else {
      # seem to be no rownames in file
      rnames.in.file <- F
    }
  }
  # if colnames row found + header specified, check they are the same
  if(first.is.head & !is.null(header)) {
    if(!all(paste(line1)==paste(header))) {
      if(!paste(line2[1]) %in% paste(header)) {
        warning("there seems to be a header of column labels in the raw file but not the header specified\n",
                "proceeding with these, but please check this mismatch is expected (e.g, might be col numbers)")
        name.lookup <- F
      } else {
        warning("order of header (column labels) in data file does not match order of inputted 'header' list\n",
                "proceeding, but using name lookup method will make import very slow and possibly unstable\n",
                "If your source file has an inconsistent order this is the only supported import method.\n",
                "Otherwise, recommend to cancel (ctrl-C), amend this discrepancy and run again")
        name.lookup <- T
      }
    }
  }
  # if rownames row found + rownames specified, check they are the same
  if(rnames.in.file & !is.null(row.names)) {
   # catdb(c("line2","row.names"),counts=list(1,1))
    if(!paste(line2[1]) %in% paste(row.names[1:2])) {
      if(!paste(line2[1]) %in% paste(row.names)) {
        warning("there seem to be row labels in the raw file but not the rownames specified\n",
                "proceeding with these, but please check this mismatch is expected (e.g, might be row numbers)")
        name.lookup <- F
      } else {
        warning("order of row labels in data file does not match order of inputted rowname list\n",
                "proceeding, but using name lookup method will make import very slow and possibly unstable\n",
                "If your source file has an inconsistent order this is the only supported import method.\n",
                "Otherwise, recommend to cancel (ctrl-C), amend this discrepancy and run again")
        name.lookup <- T
      }
    }
  }
  out <- list(first.is.head,rnames.in.file,name.lookup,headrow)
  names(out) <- c("header","rnames","match","colnames")
  return(out)
}



# patch some reader functions that cause trouble for long format files :(
# fix problem in reader:::get.delim :(
get.delim <- function(...,delims=c("\t"," ","\t| +",";",",")) {
  return(reader:::get.delim(...,delims=delims))
}
# fix problem in reader:::file.ncol :(
file.ncol <- function(fn,...) { reader:::file.ncol(fn,del=get.delim(fn),...) }


# to add to NCmisc
#' Monitor CPU, RAM and Processes
#' 
#' This function runs the unix 'top' command and returns the overall CPU and RAM usage,
#' and optionally the table of processes and resource use for each. Works only with
#' unix-based systems such as Mac OS X and Linux, where 'top' is installed. Default
#' is to return CPU and RAM overall stats, to get detailed stats instead, set Table=TRUE.
#'
#' @param CPU logical, whether to return overall CPU usage information
#' @param RAM logical, whether to return overall RAM usage information
#' @param Table logical, whether to return system information for separate processes. This
#'  is returned as table with all of the same columns as a command line 'top' command. If
#'  'Table=TRUE' is set, then the default becomes not to return the overall CPU/RAM usage stats.
#'  The dataframe returned will have been sorted by descending memory usage.
#' @param procs integer, if Table=TRUE, then the maximum number of processes to return (default 20)
#' @param mem.key character, default for Linux is 'mem' and for Mac OS X, 'physmem', but if the 'top'
#'  command on your system displays memory usage using a different label, then enter it here
#'  (case insensitive) to override defaults.
#' @param cpu.key character, default for Linux and Mac OS X is 'cpu', but if the top
#'  command on your system displays CPU usage using a different label, then enter it here.
#' @export
#' @author Nicholas Cooper
#' @examples
#' top()
#' top(Table=T,proc=5)
top <- function(CPU=!Table,RAM=!Table,Table=F,procs=20,mem.key=NULL,cpu.key=NULL) {
  if(!RAM & !CPU & !Table) { warning("Deselected all options, null will be returned"); return(NULL) }
  if(!check.linux.install("top")) {
    warning("'top' command only works on Mac OS X and Linux")
    return(NULL)
  }
  if(toupper(Sys.info()["sysname"])=="DARWIN") { macos <- T } else { macos <- F }
  if(macos) {
    # MAC OS X
    txt <- tryCatch(system("top -l 1 -n 1",intern=T), error = function(e) e)
    if(length(txt)==0) { warning("command failed"); return(NULL) }
    dtt <- divide.top.txt(txt)
    parz <- dtt$table; headr <- dtt$header
    if(!is.character(mem.key)) { mem.key <- "physmem" }
    if(RAM) { ram.gb.list <- suck.mem(headr,key=mem.key) }
  }
  if(!macos) {
    ## LINUX
    txt <- tryCatch(system("top -n 1 -b",intern=T), error = function(e) e)
    if(length(txt)==0) { warning("command failed"); return(NULL) }
    dtt <- divide.top.txt(txt)
    parz <- dtt$table; headr <- dtt$header
    if(!is.character(mem.key)) { mem.key <- "mem" }
    if(RAM) { ram.gb.list <- suck.mem(headr,key=mem.key) }
  }
  if(!is.character(cpu.key)) { cpu.key <- "cpu" }
  if(CPU) { cpu.pc.list <- suck.cpu(headr,key=cpu.key) }
  if(Table) {
    tab <- make.top.tab(parz)
    mem.col <- grep("mem",colnames(tab),ignore.case=T)[1]
    if(is.na(mem.col)) { mem.col <- grep("RSIZE",colnames(tab),ignore.case=T)[1] }
    cpu.col <- grep("cpu",colnames(tab),ignore.case=T)[1]
    tab <- tab[rev(order(tab[,mem.col])),]
    tab <- tab[rev(order(tab[,cpu.col])),];     tab <- tab[rev(order(tab[,mem.col])),]
    if(is.na(as.numeric(procs))) { procs <- nrow(tab) } else { procs <- round(procs) }
    procs <- min(c(procs,nrow(tab)),na.rm=T)
  }
  outlist <- NULL; outnms <- NULL
  if(CPU) { outlist <- c(outlist,list(cpu.pc.list)); outnms <- c(outnms,"CPU") }
  if(RAM) { outlist <- c(outlist,list(ram.gb.list)); outnms <- c(outnms,"RAM") }
  if(Table) { outlist <- c(outlist,list(tab[1:procs,])); outnms <- c(outnms,"Table") }
  names(outlist) <- outnms
  return(outlist)
}


# internal function to support top() function
make.top.tab <- function(parz) {
  cnts <- sapply(parz,length)
  exp.lines <- Mode(cnts)
  shortz <- which(cnts<exp.lines)
  longz <- which(cnts>exp.lines)
  parz[longz] <- lapply(parz[longz],function(X) { X[1:exp.lines] })
  if(length(shortz)>0) { parz <- parz[-shortz] }
  df <- as.data.frame(matrix(ncol=length(parz[[1]]),nrow=length(parz)))
  for(cc in 1:length(parz[[1]])) { df[,cc] <- sapply(parz,"[",cc) }
  tab <- df[-1,]; colnames(tab) <- df[1,]; rownames(tab) <- NULL
  return(tab)
}

# internal function to support top() function
divide.top.txt <- function(txt) {
  parz <- strsplit(txt," +|\t")
  parz <- lapply(parz,function(X) { X <- X[!is.na(X)] ; X[X!=""] } ) 
  headline <- which(sapply(parz,function(X) { all(c("PID","USER") %in% toupper(X)) }))
  parz <- parz[headline:length(parz)]
  headr <- txt[1:(headline-1)]
  return(list(header=headr,table=parz))
}

# internal function to support top() function
suck.num.from.txt <- function(txt) {
  splt <- strsplit(txt,"")
  nmall <- numeric()
  anm <- function(X) { suppressWarnings(as.numeric(X)) }
  for(cc in 1:length(splt)) {
    nm <- sapply(splt[[cc]],function(X) {
      if(!is.na(anm(X))) { anm(X) } else { if(X==".") { X } else { NA } } } )
    nmall[cc] <- anm(paste(narm(nm),collapse="",sep=""))
  }
  return(nmall)
}

# internal function to support top() function
suck.cpu <- function(headr,key="cpu") {
  cpz <- grep(key,headr,ignore.case=T)
  if(length(cpz)>0) {
    cpuline <- headr[cpz[1]]
    ms <- strsplit(cpuline,",")[[1]]
    ms <- gsub("cpu","",ms,ignore.case=T)
    user <- ms[grep("us",ms,ignore.case=T)]
    sys <- ms[grep("sy",ms,ignore.case=T)]
    idle <- ms[grep("id",ms,ignore.case=T)]
    if(length(user)>0) {
      user1 <- rmv.spc(gsub("us","",gsub("user","",user,ignore.case=T)))
      user.gb <- suck.num.from.txt(user1)
    } else { user.gb <- NA }
    if(length(sys)>0) {
      sys1 <- rmv.spc(gsub("sy","",gsub("sys","",sys,ignore.case=T)))
      sys.gb <- suck.num.from.txt(sys1)
    } else { sys.gb <- NA }
    if(length(idle)>0) {
      idle1 <- rmv.spc(gsub("id","",gsub("idle","",idle,ignore.case=T)))
      idle.gb <- suck.num.from.txt(idle1)
    } else { idle.gb <- NA }
    if(is.na(idle.gb) & !is.na(sys.gb) & !is.na(user.gb)) { idle.gb <- 100-user.gb-sys.gb }
    if(is.na(sys.gb) & !is.na(idle.gb) & !is.na(user.gb)) { sys.gb <- 100-user.gb-idle.gb }
    if(is.na(user.gb) & !is.na(sys.gb) & !is.na(idle.gb)) { user.gb <- 100-idle.gb-sys.gb }
  } else { 
    cat("no CPU usage information found\n")
    return(NULL)
  }
  return(list(total=user.gb,idle=idle.gb,sys=sys.gb,unit="%"))
}
  
# internal function to support top() function
suck.mem <- function(headr,key="Mem") {
  memz <- grep(key,headr,ignore.case=T)
  if(length(memz)>0) {
    memline <- headr[memz[1]]
    ms <- strsplit(memline,",")[[1]]
    ms <- gsub("mem","",ms,ignore.case=T)
    tot <- ms[grep("total",ms,ignore.case=T)]
    free <- ms[grep("free",ms,ignore.case=T)]
    used <- ms[grep("used",ms,ignore.case=T)]
    if(length(tot)>0) {
      tot1 <- rmv.spc(gsub("total","",tot,ignore.case=T))
      tot.gb <- suck.bytes(tot1)
    } else { tot.gb <- NA }
    if(length(free)>0) {
      free1 <- rmv.spc(gsub("free","",free,ignore.case=T))
      free.gb <- suck.bytes(free1)
    } else { free.gb <- NA }
    if(length(used)>0) {
      used1 <- rmv.spc(gsub("used","",used,ignore.case=T))
      used.gb <- suck.bytes(used1)
    } else { used.gb <- NA }
    if(is.na(used.gb) & !is.na(free.gb) & !is.na(tot.gb)) { used.gb <- tot.gb-free.gb }
    if(is.na(free.gb) & !is.na(used.gb) & !is.na(tot.gb)) { free.gb <- tot.gb-used.gb }
    if(is.na(tot.gb) & !is.na(free.gb) & !is.na(used.gb)) { tot.gb <- used.gb+free.gb }
  } else { 
    cat("no RAM usage information found\n")
    return(NULL)
  }
  return(list(total=tot.gb,used=used.gb,free=free.gb,unit="Gb"))
}

# internal function to support top() function  
suck.bytes <- function(tot1,GB=T) {
  if(length(grep("k",tot1,ignore.case=T))>0) { mult <- 1000 }
  if(length(grep("m",tot1,ignore.case=T))>0) { mult <- 10^6 }
  if(length(grep("g",tot1,ignore.case=T))>0) { mult <- 10^9 }
  lst <- c("kb","gb","mb","b","g","m","k")
  tot1 <- suck.num.from.txt(tot1)
  tot2 <- (as.numeric(tot1)*mult)/10^9 ; 
  if(!GB) { tot2 <- tot2/10^3 }
  return(tot2)
}


  
## bit of a mess  - give it a  goo!!
# useful for package prep, but a pain with 'quotes'
rox.args <- function(txt,PRE=T,POST=T,author="Nicholas Cooper") {
  # must change (")s to (')
  tspl <- strsplit(txt,",",fixed=T)  
  tspl2 <- sapply(tspl,strsplit,split="=",fixed=T)
  pars <- sapply(tspl2,"[",1)
  pars <- rmv.spc(pars)
  pars <- paste("#' @param ",pars,"\n",sep="")
  pre <- paste("#' title\n#' \n#' description ...\n#' ...\n#' ...\n")
  post <- paste("#' @export\n#' @seealso ...\n#' @author",author,"\n#' @examples\n#' ...\n#' ...\n")
  if(PRE) { cat(pre) }
  cat(pars,sep="")
  if(POST) { cat(post) }
}

#' Load a text file into a big.matrix object
#'
#' This provides a faster way to import text data
#' into a big.matrix object than bigmemory:::read.big.matrix(). The
#' method allows import of a data matrix with size exceeding RAM limits.
#' Can import from a matrix delimited file with or without row/column names,
#' or from a long format dataset with no row/columns names (these should be
#' specified as separate lists).
#' @param input.fn character, or list, either a single file name of the data, or
#'  a list of multiple file name if the data is stored as multiple files. If multiple,
#'  then the corresponding list of row or column names that is unique between files
#'  should be a list of the same length.
#' @param dir character, the directory containing all files. Or, if files are split between
#'  directories, then either include the directories explicitly in the filenames, or
#'  multiple directories can be entered as a list, with names 'big', 'ano' and 'col', where
#'  big is the location for big.matrix objects to file-back to, 'ano' is the location
#'  of row and column names, and 'col' is the location of the raw text datafiles.
#' @param long logical, if TRUE, then the data is assumed to be in long format, where
#'  each datapoint is on a new line, and the file is structured so that the data for
#'  each case/sample/id is consecutive and ordered consistently between samples. If using
#'  long format the file should contain no row or column names, these should be specified
#'  in either rows.fn/cols.fn file name arguments, or row.names/col.names vector arguments.
#'  If long=FALSE, then the dimensions of the file will be automatically detected; including
#'  if the file is in long format, however, if you know the data is in long format, specifying
#'  this explicitly will be quicker and guarantees the correct import method.
#' @param rows.fn character, with the name of a text file containing the list of row labels
#'  for the dataset. Unnecessary if importing from a matrix with row/column names in the file,
#'  or if using the row.names parameter. Must be a list of filenames if row names are split
#'  across multiple input.fn files.
#' @param cols.fn character, with the name of a text file containing the list of column labels
#'  for the dataset. Unnecessary if importing from a matrix with row/column names in the file,
#'  or if using the col.names parameter. Must be a list of filenames if column names are split
#'  across multiple input.fn files.
#' @param pref character, optional prefix to use in naming the big.matrix files (description/backing files)
#' @param delete.existing logical, if a big.matrix already exists with the same name as implied
#'  by the current 'pref' and 'dir' arguments, then default behaviour (FALSE) is to return an error.
#'  to overwrite any existing big.matrix file(s) of the same name(s), set this parameter to TRUE.
#' @param ret.obj logical, whether to return a big.matrix.descriptor object (TRUE), or
#'  just the file name of the big.matrix description file of the imported dataset.
#' @param verbose logical, whether to display extra information about import progress and
#'  notifications.
#' @param row.names character vector, optional alternative to specifying rows.fn file name(s),
#'  directly specify row names as a single vector, or a list of vectors if multiple input files
#'  with differing row names are being imported.
#' @param col.names character vector, optional alternative to specifying cols.fn file name(s),
#'  directly specify oclumn names as a single vector, or a list of vectors if multiple input files
#'  with differing column names are being imported.
#' @param dat.type character, data type being imported, default is "double", but can specify any type
#'  supported by a filebacked.big.matrix(), namely, "integer","char","short"; note these
#'  are C-style data types; double=numeric, char=character, integer=integer, short=numeric (although
#'  will be stored with less precision in the C-based big.matrix object).
#' @param ram.gb numeric, the number of gigabytes of free RAM that it is ok for the import
#'  to use. The higher this amount, the quicker the import will be, as flushing RAM contents
#'  to the hard drive more regularly slows down the process. Setting this lower
#'  will reduce the RAM footprint of the import. Note that if you set it too high, it can't
#'  be guaranteed, but usually R and bigmemory will do a reasonable job of managing the memory,
#'  and it shouldn't crash your computer.
#' @param hd.gb numeric, the amount of free space on your hard disk; if you set this
#'  parameter accurately the function will stop if it believes there is insufficient
#'  disk space to import the object you have specified. By default this is set to 
#'  1 terabyte, so if importing an object larger than that, you will have to increase
#'  this parameter to make it work.
#' @export
#' @examples
#' # all file names to use in this example #
#' all.fn <- c("rownames.txt","colnames.txt","functestdn.txt","funclongcol.txt","functest.txt",
#'  paste("rn",1:3,".txt",sep=""),paste("cn",1:3,".txt",sep=""),
#'  paste("split",1:3,".txt",sep=""),
#'  paste("splitmatCd",1:3,".txt",sep=""),paste("splitmatRd",1:3,".txt",sep=""),
#'  paste("splitmatC",1:3,".txt",sep=""), paste("splitmatR",1:3,".txt",sep=""))
#' any.already <- file.exists(all.fn)
#' if(any(any.already)) { 
#'  warning("files already exist in the working directory with the same names as some example files") }
#' # SETUP a test matrix 
#' test.size <- 4 # try increasing this number for larger matrices
#' M <- matrix(runif(10^test.size),ncol=10^(test.size-2)) # normal matrix
#' write.table(M,sep="\t",col.names=F,row.names=F,file="functest.txt",quote=F) # no dimnames
#' rown <- paste("rs",sample(10:99,nrow(M),replace=T),sample(10000:99999,nrow(M)),sep="")
#' coln <- paste("ID",sample(1:9,ncol(M),replace=T),sample(10000:99999,ncol(M)),sep="")
#' r.fn <- "rownames.txt"; c.fn <- "colnames.txt"
#' Mdn <- M; colnames(Mdn) <- coln; rownames(Mdn) <- rown
#' write.table(Mdn,sep="\t",col.names=T,row.names=T,file="functestdn.txt",quote=F) # with dimnames
#' print.large(Mdn)
#' writeLines(paste(as.vector(M)),con="funclongcol.txt")
#' in.fn <- "functest.txt"
#' ### IMPORTING SIMPLE 1 FILE MATRIX ##
#' writeLines(rown,r.fn); writeLines(coln,c.fn)
#' #1. import without specifying row/column names
#' ii <- import.big.data(in.fn); print.big.matrix(ii) # SLOWER without dimnames!
#' #2. import using row/col names from file
#' ii <- import.big.data(in.fn,cols.fn="colnames.txt",rows.fn="rownames.txt")
#' print.big.matrix(ii)
#' #3. import by passing colnames/rownames as objects
#' ii <- import.big.data(in.fn, col.names=coln,row.names=rown)
#' print.big.matrix(ii)
#' ### IMPORTING SIMPLE 1 FILE MATRIX ALREADY WITH DIMNAMES ##
#' #1. import without specifying row/column names, but they ARE in the file
#' in.fn <- "functestdn.txt"
#' ii <- import.big.data(in.fn); print.big.matrix(ii)
#' ### IMPORTING SIMPLE 1 FILE MATRIX ALREADY WITH MISORDERED rownames ##
#' rown2 <- rown; rown <- sample(rown);
#' # re-run test3 using in.fn with dimnames
#' ii <- import.big.data(in.fn, col.names=coln,row.names=rown)
#' print.big.matrix(ii)
#' # restore rownames: 
#' rown <- rown2
#' ### IMPORTING SIMPLE 1 FILE LONG FORMAT by columns ##
#' in.fn <- "funclongcol.txt"; #rerun test 2 # 
#' ii <- import.big.data(in.fn,cols.fn="colnames.txt",rows.fn="rownames.txt")
#' print.big.matrix(ii)
#' ### IMPORTING multifile LONG by cols ##
#' splF <- factor(rep(c(1:3),ncol(M)*c(.1,.5,.4)))
#' colnL <- split(coln,splF); MM <- as.data.frame(t(M))
#' Ms2 <- split(MM,splF)
#' Ms2 <- lapply(Ms2,
#'    function(X) { X <- t(X); dim(X) <- c(nrow(M),length(X)/nrow(M)); X } )
#' # preview Ms2 - not run # lapply(Ms2,print.large)
#' colfs <- paste("cn",1:length(colnL),".txt",sep="")
#' infs <- paste("split",1:length(colnL),".txt",sep="")
#' # create multiple column name files and input files
#' for(cc in 1:length(colnL)) { writeLines(colnL[[cc]],con=colfs[cc]) }
#' for(cc in 1:length(infs)) { 
#'   writeLines(paste(as.vector((Ms2[[cc]]))),con=infs[cc]) }
#' # test using colnames and rownames lists
#' ii <- import.big.data(infs, col.names=colnL,row.names=rown)
#' print.big.matrix(ii)
#' ### IMPORTING multifile MATRIX by rows ##
#' splF <- factor(rep(c(1,2,3),nrow(M)*c(.1,.5,.4)))
#' rownL <- split(rown,splF)
#' Ms <- split(M,splF)
#' Ms <- lapply(Ms,function(X) { dim(X) <- c(length(X)/ncol(M),ncol(M)); X } )
#' # preview Ms - not run # lapply(Ms,print.large)
#' # create multiple row name files and input files
#' rowfs <- paste("rn",1:length(rownL),".txt",sep="")
#' for(cc in 1:length(rownL)) { writeLines(rownL[[cc]],con=rowfs[cc]) }
#' infs <- paste("splitmatR",1:length(colnL),".txt",sep="")
#' for(cc in 1:length(infs)) { 
#'  write.table(Ms[[cc]],sep="\t",col.names=F,row.names=F,file=infs[cc],quote=F) }
#' # test using colnames and rownames files
#' ii <- import.big.data(infs, col.names="colnames.txt",rows.fn=rowfs)
#' print.big.matrix(ii)
#' # DELETE ALL FILES ##
#' unlink(all.fn[!any.already]) # only delete files not initially present (prevent deleting user's files)
import.big.data <- function(input.fn=NULL, dir=getwd(), long=F, rows.fn=NULL, cols.fn=NULL, dat.file.suf=".dat",
                              pref="", delete.existing=T, ret.obj=F, verbose=T, row.names=NULL, col.names=NULL,
                              dat.type="double", ram.gb=2, hd.gb=1000)
{
  # import from a text (hopefully long format) datafile to a big.matrix
  # return bigmatrix description 'object' or name of description file according to 'ret.obj'
  ### CALIBRATE OPTIONS AND PERFORM CHECKS ###
  dir.force.slash <- reader:::dir.force.slash # use internal function from 'reader'
  if(all(dir=="")) { dir <- getwd() }
  ## for compatibility with plumbCNV directory object
  if(exists("validate.dir.for",mode="function")) {
    ## plumbCNV specific code ##
    dir <- do.call("validate.dir.for",list(dir=dir,elements=c("big","ano","col"),warn=F))  
  } else {
    # otherwise
    dir <- list(big=dir,ano=dir,col=dir)
    if(is.list(dir)) { if(!is.null(dir[["big"]])) { dir.big <- dir$big } }
  }
  dat.type <- tolower(dat.type)
  if(!dat.type %in% c("double","short","character","integer","char","numeric")) {
    dat.type <- options()$bigmemory.default.type; as.type <- dat.type
  } else {
    as.type <- dat.type
    if(dat.type=="short") { as.type <- "numeric" }
    if(dat.type=="char") { as.type <- "character" }
    if(dat.type=="character") { dat.type <- "char" }
    if(dat.type=="numeric") { dat.type <- "double" }
  }
  ## Define data types for big.matrix
    # label
  input.fn <- cat.path(dir$col,input.fn)
  file.rn <- character()
  spec.rn <- spec.cn <- T; miswarn <- F
  #### GET THE SHAPED INPUT FILE NAME(S) ####
  # print(input.fn)
  if(length(input.fn)>1 & ((is.null(rows.fn)&is.null(row.names)) | (is.null(cols.fn)&is.null(col.names)))) {
    stop(paste("When using multiple input files, you must specify row and column names using",
               "row.names + col.names, or rows.fn + cols.fn.",
               "If the input files split the dataset by columns, then the column file names need",
               "to be entered as a list corresponding to unique column names in each input.fn entry,",
               "OR equivalently, a equal length list of row names if the data is split by rows"))
  }
  #### GET THE CORRESPONDING column LIST(S) ####
  if((length(col.names)>length(input.fn)) | is.list(col.names)) {
    if(is.list(col.names))  {
      if(all(sapply(col.names,is)[1,]=="character")) {
        ID.list <- col.names
      }
    } else {
      ID.list <- list(col.names)
    }
  } else {
    if(all(!is.null(cols.fn))) {
      cols.fn <- find.file(cols.fn,dir$ids)  # column id file name
      cat("Reading column and row names...\n")
      cat(paste(" reading column names from",cols.fn,"\n"))
      ID.list <- lapply(cols.fn,readLines)  #readLines(cols.fn)
    } else {
      cat("no column names specified\n"); spec.cn <- F
      ll <- max(1,length(cols.fn))
      ID.list <- vector("list",ll)
      qmf <- quick.mat.format(input.fn[1])
      if(qmf$ncol==1 | long)  { stop("If reading a long format file, must provide row and column names directly, or via filenames") }
      ecn <- qmf$colnames
      ern <- qmf$rownames; #catdb("ern")
      if(ecn) {
        for (cc in 1:ll) { ID.list[[cc]] <- quick.mat.format(input.fn[cc])$cnames }
      } else {
        for (cc in 1:ll) { ID.list[[cc]] <- paste("col",1:file.ncol(input.fn[cc],excl.rn=ern),sep="") }
      }
    }
  }
  ##print(headl(ID.list))
  cmb.ID.list <- paste(unlist(do.call("c",ID.list)))
  if(anyDuplicated(cmb.ID.list)) { stop("Cannot have duplicated column names") }
  ##print(length(ID.list[[1]]))
  num.c.fls <- length(ID.list)
  ### GET THE ORDERED row LIST ###
  if((length(row.names)>length(input.fn)) | is.list(row.names)) {
    if(is.list(row.names))  {
      if(all(sapply(row.names,is)[1,]=="character")) {
        rows.list <- row.names
      }
    } else {
      rows.list <- list(row.names)
    }
  } else {
    if(all(!is.null(rows.fn))) {
      rows.fn <- find.file(rows.fn,dir$ano,dir)  # row annotation file name
      cat(paste(" reading row names from",rows.fn,"\n"))
      rows.list <- lapply(rows.fn,readLines)  #readLines(cols.fn)
    } else {
      cat("no row names specified\n"); spec.rn <- F
      ll <- max(1,length(cols.fn))
      rows.list <- vector("list",ll)
      ecn <- as.numeric(quick.mat.format(input.fn[1])$colnames)
      for (cc in 1:ll) { 
        nr <- (file.nrow(input.fn[cc])-ecn)
        rows.list[[cc]] <- paste("row",1:nr,sep="") 
      }
    }
  }
  cmb.row.list <- paste(unlist(do.call("c",rows.list)))
  if(anyDuplicated(cmb.row.list)) { stop("Cannot have duplicated row names") }
  if(length(rows.list)>1 & length(ID.list)>1) {
    warning("cannot enter both multiple row and column file names")
    return(NULL)
  }
  num.r.fls <- length(rows.list)
  numfls <- num.r.fls*num.c.fls # one of these should be 1
  if(length(rows.list)>1) {
    multi.mode <- T; col.mode <- F
  } else {
    if(length(ID.list)>1) {
      multi.mode <- T; col.mode <- T
    } else {
      multi.mode <- F; col.mode <- T
    }
  }
  # determine what set of input files have been specified
  if(length(input.fn)>1) {
    if(length(input.fn)==numfls) {
      if(verbose) {
        if(col.mode) {
          cat(paste("reading a single cohort from",numfls,"source files.\n"))
        } else {
          cat(paste("reading a single varset from",numfls,"source files.\n"))
        }
      }
    } else {
      stop("Error: when reading from multiple source files, need an equal number of row or col id files")
    }
  } else { if(numfls!=1) { warning(paste("length of ID list was",numfls,"but only 1 input file")) } } 
  #### DETERMINE FILE DIMENSIONS ####
  num.sub <- length(cmb.ID.list) #ID.list)
  smp.szs <- sapply(ID.list,length)
  num.row <- length(cmb.row.list)
  row.szs <- sapply(rows.list,length)
  #catdb(c("num.sub","num.row","cmb.ID.list","cmb.row.list"))
  if(col.mode) {
    fil.ofs <- c(0,cumsum(smp.szs)) #note last element is the end of the last file
  } else {
    fil.ofs <- c(0,cumsum(row.szs))
  }
  cat(paste(" found",num.sub,"column names and",num.row,"marker names\n"))
  cls <- num.sub; rws <- num.row
  #cells.per.gb <- 2^27  # size of double() resulting in ~1GB of memory use by R 2.15
  #memory.estimate <- as.double((as.double(rws)*as.double(cls))/cells.per.gb)
  em <- estimate.memory(c(rws,cls))
  if (em > hd.gb) {
    stop(paste("Insufficient disk space availability (hd.gb) expected for this import method\n",
     "Please free up some disk space and try again",sep=""))
  } else {
    divs <- em/(ram.gb/2) # aim to flush once memory reaches half of RAM
    if(divs<1) { do.fl <- F } else { do.fl <- T }
    divs <- round(divs)
  }
  # use 'pref' as the name of the big.matrix backing files for this cohort
  bck.fn <- paste(pref,"bckfile",sep="")
  des.fn <- paste(pref,"descrFile",sep="")
  ### DELETE EXISTING FILE IF HAS SAME NAME ###
  if ((!des.fn %in% list.files(dir$big)) | delete.existing )
  {
    if(delete.existing & (des.fn %in% list.files(dir$big)))
    {
      dfn <- cat.path(dir$big,des.fn)
      cat("\n deleting",dfn,"\n")
      unlink(dfn)
    } else {
      #all clear, no files already exist with same name
    }
  } else {
    cat(paste("\nWarning: Big matrix description file",des.fn,"already exists in",dir$big,"\n"))
    cat("You may wish to delete, rename or move this file, or use option 'delete.existing'=T, before re-running this script\n")
    #stop()
  }
  ### MAIN IMPORT LOOP ###
  cat("\nCreating big matrix object to store group data")
  cat("\n predicted disk use: ",round(em,1),"GB\n")
  if(is.character(dir.force.slash(dir$big))) { if(dir$big=="") { dir$big <- getwd() } }
  bigVar <- big.matrix(nrow=rws,ncol=cls, backingfile=bck.fn, dimnames=list(cmb.row.list,cmb.ID.list),
                       type=dat.type, backingpath=dir.force.slash(dir$big),
                       descriptorfile=des.fn)
  for(ff in 1:numfls) {
    ifn <- cat.path(dir$col,input.fn[ff],must.exist=T)
    if(col.mode) { ffc <- ff; ffr <- 1 } else { ffc <- 1; ffr <- ff }
    # previously: create file name depending on whether in baf or lrr directory
    #test file type if matrix
    #cat.db(c("ff","numfls","ifn","col.mode","ffr","ffc"))
    if(long) { input.is.vec <- T } else {
      if(file.ncol(ifn)>1) { input.is.vec <- F } else { input.is.vec <- T }
    }
    nxt.rng <- (fil.ofs[ff]+1):(fil.ofs[ff+1])
    #cat.db("nxt.rng")
    if(col.mode) { cls1 <- length(nxt.rng); rws1 <- rws } else { cls1 <- cls; rws1 <- length(nxt.rng) }
    #cat.db(c("cls1","rws1","cls","rws"))
    if(!input.is.vec) {
      if(spec.rn & spec.cn) {
        frm <- check.text.matrix.format(fn=ifn,ncol=cls1,header=ID.list[[ffc]],row.names=rows.list[[ffr]])
        #catdb(c("cls1","rws1","cls","frm"))
      } else {
        frm <- check.text.matrix.format(fn=ifn)
        if(!is.null(frm$colnames)) { ID.list[[ff]][1:length(frm$colnames)] <- frm$colnames }
      }
      if(frm$rname & !frm$match) { file.rn <- character(rws) } # recording rownames as we go
      if(frm$match & !col.mode) { stop("cannot use matching method with separate files by rows") }
    }
    dat.file <- file(ifn)
    open(con=dat.file,open="r")
    cat(paste(" opening connection to ",c("matrix","long")[1+input.is.vec],
              " format datafile (",ff,"/",numfls,"): ",basename(ifn),"\n",sep=""))
    cat("\nLoading text data into big matrix object:\n")
    if(!input.is.vec)
    {
      ## read from matrix format tab file
      twty.pc <- round(rws1/divs) # flush data every 'n' rows
      for (cc in 1:rws1) {
        if (do.fl & (cc %% twty.pc)==0)  { fl.suc <- flush(bigVar) ; if(!fl.suc) { cat("flush failed\n") } }
        loop.tracker(cc,rws1)
        next.line <- readLines(dat.file,n=1)
        next.row <- strsplit(next.line,"\t",fixed=T)[[1]]
        if (cc==1 & frm$header) { 
          # need to go to next line to avoid reading header as data
          next.line <- readLines(dat.file,n=1); next.row <- strsplit(next.line,"\t",fixed=T)[[1]]
        }
        if (frm$rnames) {
          if(frm$match) {
            lbl <- next.row[1]; 
            row.sel <- match(lbl,cmb.row.list)
            bigVar[row.sel,nxt.rng] <- next.row[-1]
            file.rn[row.sel] <- lbl
          } else {
            if(col.mode) {
              #catdb("bigVar",counts=list(cc=cc,nxt.rng=nxt.rng[1]))
              #catdb(c("next.row","nxt.rng"))
              file.rn[cc] <- next.row[1]; bigVar[cc,nxt.rng] <- next.row[-1]
            } else {
              selc <- 1:(length(next.row)-1)
              file.rn[nxt.rng[cc]] <- next.row[1]; bigVar[nxt.rng[cc],selc] <- next.row[-1]
            }
          }
        } else {
          if(col.mode) {
            bigVar[cc,nxt.rng] <- next.row
          } else {
            bigVar[nxt.rng[cc],] <- next.row
          }
        }
      }
    } else {
      ## read from (long) vector format tab file
      if(col.mode) {
        ## col-wise file splits
        twty.pc <- round(cls1/divs) # flush data every 'n' cols
        for (cc in 1:cls1) {
          loop.tracker(cc,cls1)
          if (do.fl & (cc %% twty.pc)==0)  { fl.suc <- flush(bigVar) ; if(!fl.suc) { cat("flush failed\n") } }
          bigVar[,(cc+fil.ofs[ff])] <- as(readLines(dat.file,n=rws),as.type)
        }
      } else {
        ## row-wise file splits
        twty.pc <- round(rws1/divs)
        for (cc in 1:cls) {
          loop.tracker(cc,cls)
          if (do.fl &(cc %% twty.pc)==0)  { fl.suc <- flush(bigVar) ; if(!fl.suc) { cat("flush failed\n") } }
          bigVar[nxt.rng,cc] <- as(readLines(dat.file,n=rws1),as.type)
        }
      }
    }
    close(dat.file)
  }
  ### FINISH UP, RETURN BIGMATRIX DESCRIPTION ###
  # in the case of different rownames found in matrix, then show following warning text:
  if(!input.is.vec) {
    if(frm$rname & !frm$match) {
      if(!spec.rn) {
        if(nrow(bigVar)==length(file.rn)) {
          options(bigmemory.allow.dimnames=TRUE)
          rownames(bigVar) <- file.rn; cat("updated big.matrix rownames from names in file(s)\n")
          flush(bigVar)
          big.des <- describe(bigVar)
          des.fn <- cat.path(fn=des.fn,ext="RData")
          save(big.des,file=des.fn)
          warning("Had to change description file to a binary file to update rownames. This can be read in with getBigMat() [and should be faster to load]")
        } else {
          #catdb(c("file.rn"))
          miswarn <- T
        }
      } else {
        #print("gere")
      }
    } else {
      if(frm$match) {
        miswarn <- T
      } else {
        #print("hereg")
      }
    }
  }
  if(miswarn) {
    ofn <- cat.path(dir$ano,pref=pref,"_file_rowname_list_check_this.txt")
    if(exists("file.rn")) { 
      warning("rownames didn't match what was in filecheck the list in the file at:\n ",ofn) 
      writeLines(paste(file.rn),con=ofn) ; cat("\n file preview:\n"); print(head(file.rn,10)); cat("\n")
    } else {
      warning(paste("rownames didn't match what was in file"))
    }
  }
  cat("\n")
  cat(paste(" created big.matrix description file:",des.fn,"\n"))
  cat(paste(" created big.matrix backing file:",bck.fn,"\n"))
  if(ret.obj) {
    return(describe(bigVar))
  } else {
    return(des.fn)
  }
  cat("...complete!\n")
}


# mean replacement code not used at this stage
row.rep <- function(X) { X[is.na(X)] <- mean(X,na.rm=T); X }


#internal, analog from plumbCNV

select.col.row.custom <- function(bigMat,row,col)
{
  # based on files/vectors of row-ids and column-ids create selection
  # vectors to select only the ids in these lists for a matrix
  cat(" calculating selections for rows\n")
  # try to detect whether a vector of IDs, or file names
  row.ref <- rownames(bigMat)  ; col.ref <- colnames(bigMat) 
  byname <- T
  if (length(row)==1 & length(col)==1 & is.character(row) & is.character(col))
  {
    cat(" [assuming 'col' and 'row' are file names containing column and row ids]")
    if(file.exists(row)) {
      row.sel <- readLines(row)
    } else {
      if(row=="") {
        cat(c(" row subset file was empty, selecting all\n"))
        row.sel <- row.ref
      } else {
        stop("Error: argument 'row' should be a vector of rows names length>1 or a filename with a list of rows (no header)")
      }
    }
    if(file.exists(col)) {
      column.sel <- readLines(col)
    } else {
      if(col=="") {
        cat(c(" column subset file was empty, selecting all\n"))
        column.sel <- col.ref
      } else {
        stop("Error: argument 'col' should be a vector of column names length>1 or a filename with a list of rows (no header)")
      }
    }
  } else { 
    if(is.character(row) | is.character(col)) {
      #cat("[assuming 'col' and 'row' are vectors of column and row ids]")
      # if blank then assign all ids
      if(all(row=="")) {
        row.sel <- row.ref
      } else {
        row.sel <- row
      }
      if(all(col=="")) {
        column.sel <- col.ref
      } else { 
        column.sel <- col  
      }
    } else {
      byname <- F
      # assume numeric or logical selection
      if(is.logical(row)) { row <- which(row) }
      if(is.logical(col)) { col <- which(col) }
      to.order.r <- row[row<=nrow(bigMat)]
      to.order.c <- col[col<=ncol(bigMat)]
    }
  }
  # use sort/exclusion lists to get reordering vectors
  row.sel <- row.sel ; col.sel <- column.sel
  
  #print(head(row.sel));print(head(col.sel))
  if(byname) {
    # selection is based on row/col names
    to.order.r <- narm(match(row.sel,rownames(bigMat)))
    to.order.c <- narm(match(col.sel,colnames(bigMat)))
  } else {
    # selection is based on row/col numbers or logical
  }
  if (!(length(to.order.r[!is.na(to.order.r)])>0 & length(to.order.c[!is.na(to.order.c)])>0))
  { warning("selection of rows and/or columns has resulted in an empty dataset",
            "\ncheck rownames, column names and selection lists for errors") }
  
  out.list <- list(to.order.r,to.order.c,row.sel,column.sel)
  names(out.list) <- c("to.order.r","to.order.c","row.list","column.list")
  return(out.list)
}



#' basically a big wrapper for deep copy, making sure you do it all right and
#' managing the file names
#' @param select.rows can be numbers, logical, names, or a file with names
big.select <- function(des.fn, select.rows=NULL, select.cols=NULL, dir=getwd(), 
                       deepC=T, pref="thin", verbose=T )
{
  # sort and exclude snps/samples from a big.matrix
  if(exists("validate.dir.for",mode="function")) {
    ## plumbCNV specific code ##
    dir <- do.call("validate.dir.for",list(dir=dir,elements=c("big","pc"),warn=F))  
  } else {
    # otherwise
    dir <- list(big=dir,pc=dir)
  }
  # bigmatrix file names for re-ordered filtered matrix (which is the final output of this script)
  if(is.character(des.fn) & length(des.fn)==1) { Fn <- gsub("descrFile","",des.fn) } else { Fn <- "" }
  bck.fn.o <- cat.path(fn=Fn,pref=pref,suf="bckfile")
  des.fn.o <- cat.path(fn=Fn,pref=pref,suf="descrFile")
  R.descr <- cat.path(dir$big,des.fn.o,ext=".RData")
  
  bigMat <- getBigMat(des.fn,dir)
  if(verbose) { cat(paste(" attached matrix with dims:",paste(dim(bigMat),collapse=","),"\n")) }
  # get list of deleting/reordering vectors using annotation files
  trans.list <- select.col.row.custom(bigMat,row=select.rows,col=select.cols)
  if(verbose) { cat(paste(" selected",length(trans.list[[4]]),"listed samples and",length(trans.list[[3]]),"variables\n")) }
  wrn <- "trans.list was already attached, detaching now..\n"
  while("trans.list" %in% search()) { detach(trans.list); warning(wrn) }
  attach(trans.list)
  if(verbose) {
    cat("\nReordering Variables and Samples...\n")
    cat("\nINDEXES SUMMARY\n")
    cat(paste(length(to.order.r),"row indexes range is from",min(to.order.r),"to",max(to.order.r),"\n"))
    cat("-->",head(to.order.r),sep=", "); cat ("\n")
    cat(paste(length(to.order.c),"col indexes range is from",min(to.order.c),"to",max(to.order.c),"\n"))
    cat("-->",head(to.order.c),sep=", ")
    cat("\n\n raw big.matrix summary before selection/ordering:\n\n")
    print.big.matrix(bigMat,"bigMat")
  }
  if(!deepC)
  {
    # this is fast with available RAM (like 20 secs)
    if(verbose) { cat(" running reorder in system memory\n") }
    bigMat1 <- bigMat[to.order.r,to.order.c]
    if(verbose) {
      cat(" adding colnames\n") ; colnames(bigMat1) <- colnames(bigMat)[to.order.c] 
      cat(" adding rownames\n") ; rownames(bigMat1) <- rownames(bigMat)[to.order.r] 
      cat(" converting matrix to big.matrix\n") 
    }
    bigMat2 <- as.big.matrix(bigMat1, backingfile=bck.fn.o,
                             backingpath=dir$big, descriptorfile=des.fn.o)
    if(verbose) { cat(paste(" matrix descr saved as standard description file:",des.fn.o,"\n")) }
    descr <- describe(bigMat2)
  } else {
    #this is slow but creates backing file and will speed up ops later
    if(verbose) { cat(" starting deep copy...") }
    bigMat2 <- deepcopy(bigMat, cols = to.order.c, rows = to.order.r,
                        backingfile=bck.fn.o,backingpath=dir$big, descriptorfile=des.fn.o )
    cat("done\n")
    if(verbose) { cat("\nAdding names\n") }
    options(bigmemory.allow.dimnames=TRUE)
    colnames(bigMat2) <- colnames(bigMat)[to.order.c]
    if(verbose) { cat(" added colnames\n") }
    rownames(bigMat2) <- rownames(bigMat)[to.order.r]  
    if(verbose) { cat(" added rownames\n") }
    descr <- describe(bigMat2)
    flush(bigMat2) # hopefully this will ensure the row/colnames are added to the file backing
    if(verbose) { cat(paste(" due to use of deep copy option, recommend only to use descr saved as rbinary description file\n")) }
  }
  save(descr,file=R.descr)
  if(verbose) {
    cat(paste(" created big.matrix description file:",des.fn.o,"\n"))
    cat(paste(" created big.matrix backing file:",bck.fn.o,"\n"))
    cat(paste(" created big.matrix binary description file:",basename(R.descr),"\n"))
  } 
  while("trans.list" %in% search()) { detach(trans.list) }
  #return(descr) 
  return(R.descr)
}

#big.select (des.fn=, select.rows=NULL, select.cols=NULL, dir=getwd(), 
#                       deepC=T, pref="thin", verbose=T )
## returns a set of rows/cols most representative of the PCs for a subset
# HERE does it really work on rows=F?????
# mini.pca.select(t(mat),.4,rows=T)
# mini.pca.select(t(mat),.4,rows=F)
# mini.pca.select(mat,.4,rows=F)
# mini.pca.select(mat,.4,rows=T)
mini.pca.select <- function(bigMat,keep=.05,rows=TRUE,dir=getwd(),random=TRUE,ram.gb=0.1,...) {  
  # select an exactly evenly spaced subset (reproduceable)
  if(rows) { N <- nrow(bigMat) } else { N <- ncol(bigMat) }
  if(keep>2) {
    new.n <- round(keep)
  } else {
    new.n <- round(max(0,min(1,keep))*N)
  }
  min.other <- min(c(dim(bigMat)/2,20)) # e.g, minimum of 20 sample subset to test with very large variable set
  max.other <- min(c(dim(bigMat)/2,200)) # a good amount to test on
  if(estimate.memory(c(N,min.other))>ram.gb) {
    ## too big to do this
    warning("The matrix selected has too many variables to do a mini-PCA on a meaningful subset. Suggest choosing an alternative subset method")
    return(NULL)
  } 
  if(estimate.memory(c(N,max.other))<ram.gb) {
    # go with max
    keeper <- max.other
  } else {
    # go with best we can up to that memory point
    mult <- estimate.memory(c(N,max.other))/ram.gb
    keeper <- (max.other/mult)
  }
  rc <- uniform.select(bigMat,keep=keeper,rows=!rows,dir=dir,random=random)
  ## do it here
  sub.mat <- bigMat[rc[[1]],rc[[2]]]
  print(dim(sub.mat))
  quick.pc <- big.PCA(sub.mat,...)
  el <- quick.elbow(quick.pc$Evalues)
  varpcs <- estimate.eig.vpcs(eigenv=quick.pc$Evalues,M=sub.mat,elbow=el,print.est=F)$variance.pcs
  el <- quick.elbow(varpcs)
  pc <- quick.pc$PCs[,1:el]; 
  if(ncol(sub.mat)==nrow(pc)) {
    cm <- abs(cor(pc,t(sub.mat)))
  } else {
    cm <- cor(pc,sub.mat)
  }
  selected <- logical(ncol(cm)) # number of variables long logical
  new.set <- NULL
  vpc <- varpcs[1:el]/sum(varpcs[1:el])
  per.pc <- round(new.n*vpc)
  # make sure total number to choose will be exactly right
  while(sum(per.pc)>new.n) { rr <- sample(1:length(per.pc),1); per.pc[rr] <- max(0,per.pc[rr]-1) }
  while(sum(per.pc)<new.n) { rr <- sample(1:length(per.pc),1); per.pc[rr] <- max(0,per.pc[rr]+1) }
  for (cc in 1:nrow(cm)) {
    nxt.row <- cm[cc,]; indc <- 1:ncol(cm)
    nxt.row <- nxt.row[!selected]; indc <- indc[!selected]
    if(per.pc[cc]>0) {
      chc <- (indc[rev(order(nxt.row))])[1:per.pc[cc]]
      #print((nxt.row[rev(order(nxt.row))])[1:per.pc[cc]])
      new.set <- c(new.set,chc)
      selected[new.set] <- T
    }
  }  
  return(sort(new.set))
}  

## tailor this to make a random uniform selection
uniform.select <- function(bigMat,keep=.05,rows=TRUE,dir,random=TRUE,ram.gb=0.1) {  
  # select an exactly evenly spaced subset (reproduceable)
  if(rows) { N <- nrow(bigMat) } else { N <- ncol(bigMat) }
  if(keep>2) {
    new.n <- round(keep)
  } else {
    new.n <- round(min(0,max(1,keep))*N)
  }
  if(!random) {
    X <- cut.fac(N,new.n)
    indx <- tapply(1:N,X,function(x) { round(median(x)) })
    indx <- as.integer(indx)
    # select a randomly uniform subset
  } else {
    indx <- sort(sample(N,new.n))
  }
  if(rows) { 
    outlist <- list(order.r=indx, order.c=1:ncol(bigMat)) 
  } else {
    outlist <- list(order.r=1:nrow(bigMat), order.c=indx)
  }
  return(outlist)
}


mat <- matrix(rnorm(10*40000),ncol=200)
system.time(big.PCA(mat))
## other selection methods ##
# mini pca, and take those most correlated with PCs (weighted by var.pc)
# do PCA on max data size that will be quick
# choose quick elbow and take thos components as starting point
# choose 'n' for each components corresponding to % of variance
# select n top/bottom loading variables (remove each at a time so no double ups) from prelim pcs to
#  make up desired number of vars
# correlation matrix, those most correlated with other measures on mini version
# those most correlated with a difference in phenotypes on mini version, get from corina meth
# those least correlated with a difference in phenotypes on mini version, get from corina meth





cut.fac <- function(N,n.grps,start.zero=F,factor=T) {
  X <- findInterval(((1:N)/(N/n.grps)), 1:n.grps, all.inside=F, rightmost.closed=T)+as.numeric(!start.zero)
  #X <- cut(1:N,n.grps,labels=F)
  if(factor) { X <- as.factor(X) }
  return(X)
}



### to wrap ##
#### HERE !!!

#' PCA/Singular Value Decomposition for big.matrix
#' 
#' description ...
#' ...
#' ...
#' @param bigMat a big.matrix object, or any argument accepted by getBigMat(), which includes
#'  paths to description files or even a standard matrix object.
#' @param dir directory containing the filebacked.big.matrix, and also where the output
#'  file will be saved by default if the save.pcs option is set TRUE. 
#' @param pcs.to.keep integer, number of principle components to keep. Singular Value Decomposition
#'  methods are able to run faster if they don't need to calculate every single PC for a large
#'  matrix. Default is to calculate only the first 50; in practice even fewer than this are generally
#'  used directly. Apart from reducing processing time, this can also reduce storage/RAM burden for 
#'  the resulting matrix.
#' @param thin decimal, percentage of the original number of rows you want to thin the matrix to.
#'  If wanting to thin by columns, then use ... arguments for thin.big.mat. 
#'  Even though this PCA function uses mainly 'big.matrix' native methods, there is a step where the
#'  matrix must be stored fully in memory, so this limits the size of what matrix can be processed,
#'  depending on RAM limits. If you want to conduct PCA/SVD on a matrix larger than RAM you can thin
#'  the matrix to a percentage of the original size. Usually such a large matrix will contain correlated
#'  measures and so the exclusion of some data-rows (variables) will have only a small impact on the
#'  resulting principle components. In some applications tested using this function, using only 5% 
#'  of 200,000 variables a PCA gave extremely similar results to using the full dataset.
#' @param SVD logical, whether to use a Singular Value Decomposition method or a PCA method. The 
#'  eigenvalues and eigenvectors of each alternative will be highly correlated so for most applications,
#'  such as PC-correction, this shouldn't make much difference to the result. However, using SVD=TRUE
#'  can provide significant advantages in speed, or even allow a solution on a matrix that would be
#'  to large to practically compute full PCA. Note that only in SVD mode, and with the bigalgebra
#'  package installed will the full speed advantage of this function be utilised.
#' @param LAP logical, whether to use La.svd() instead of svd() when SVD=TRUE, see base:svd for more info.
#' @param center whether to 'centre' the matrix rows by subtracting rowMeans() before conducting the PCA. This is usually
#'  advisable, although you may wish to skip this if the matrix is already centred to save extra processing.
#'  unlike prcomp there is no option to standardize or use the correlation matrix, if this is desired, please
#'  standardize the bigMat object before running this function.
#' @param save.pcs whether to save the principle component matrix and eigenvalues to a binary file with name pcs.fn
#' @param pcs.fn name of the binary when save.pcs=TRUE
#' @param verbose whether to display detailed progress of the PCA
#' @export
#' @seealso getBigMat, LRR.PCA.correct
#' @author Nicholas Cooper
#' @examples
## Demonstrate PCA versus SVD ##
#' min.dim <- 200; nvar <- 500; subset.size <- 50
#' mat <- matrix(rnorm(min.dim*nvar),ncol=min.dim) # mat <- t(crimtab)
#' t.mat <- t(mat)
#' MMs <- t.mat %*% mat
#' MsM <- mat %*% t.mat
#' print.large(mat)
#' pca <- svd(mat) #,nv=subset.size,nu=0)
#' D <- pca$d
#' sig <- mat-mat; sig <- t.mat-t.mat; diag(sig) <- D
#' V <- pca$v
#' U <- pca$u
#' MMs2 <- V %*% (t(sig) %*% sig) %*% t(V)
#' MsM2 <- U %*% (sig %*% t(sig)) %*% t(U)
#' pr <- princomp(mat) # PCA using eigendecomposition of cov matrix
#' L <- matrix(rep(0,40000),ncol=200); diag(L) <- pr[[1]]^2 # eigenvalues as diag
#' mat2 <- (pr[[2]]) %*% L %*%  solve(pr[[2]]) # eigenvecs * eigenvals * inv(eigenvecs)
#' print.large(cov(mat)); print.large(mat2) #  == COVmat
#' median(abs(diag(cor(V,pr[["loadings"]])))); median(abs(diag(cor(U,pr[["scores"]]))))
#' cor(pr$sdev,D)
#' bmat <- as.big.matrix(mat)
#' result <- big.PCA(bmat,verbose=T)
#' pca.scree.plot(result$Evalues,M=bmat,add.fit.line=T,elbow=40,linear=F,ylim=c(0,1400),n.xax=200)
#' pca.scree.plot(result$Evalues,M=bmat,add.fit.line=T,elbow=13)
big.PCA <- function(bigMat,dir=getwd(),pcs.to.keep=50,thin=F,SVD=T,LAP=F,center=T,save.pcs=F,pcs.fn="PCsEVsFromPCA.RData",verbose=F) 
{
  # run principle components analysis on the SNP subset of the LRR snp x sample matrix
  # various methods to choose from with pro/cons of speed/memory, etc.
  #  must use SNP-subset to avoid LD, destroying main effects, +avoid huge memory requirements
  #dir <- validate.dir.for(dir,c("big","pc"))
  if(exists("validate.dir.for",mode="function")) {
    ## plumbCNV specific code ##
    dir <- do.call("validate.dir.for",list(dir=dir,elements=c("big","pc"),warn=F))  
  } else {
    # otherwise
    dir <- list(big=dir,pc=dir)
  }
  #must.use.package(c("irlba"),T)
  pcaMat <- getBigMat(bigMat,dir)
  if(verbose) { print.big.matrix(pcaMat,name="pcaMat") }
  est.mem <- estimate.memory(pcaMat)
  if(verbose | est.mem>1.5) {
    cat(" estimated memory required for",nrow(pcaMat),"x",ncol(pcaMat),"matrix:",round(est.mem,2),
      "GB. If this exceeds available,\n  then expect PCA to take a long time or fail!\n")
  }
  subMat <- as.matrix(pcaMat) # must convert bigmatrix to plain matrix here, no pca yet takes a bigmatrix
  rm(pcaMat)
  # center using row means
  if(center) {
    if(verbose) { cat(" centering data by row means...") }
    subMat <- subMat - rowMeans(subMat)  #matrix(rep(rowMeans(subMat),times=ncol(subMat)),ncol=ncol(subMat))
    subMat[is.na(subMat)] <- 0 # replace missing with the mean
    #cat(" means for first 10 snps:\n")
    #print(round(head(rowMeans(subMat),10))) # show that centering has worked
  } else {
    subMat <- apply(subMat,1,row.rep)
  }
  if(verbose) { cat(" replaced missing data with mean (PCA cannot handle missing data)\n") }
  #subMat <- t(subMat) # transpose
  dimz <- dim(subMat)
  if(pcs.to.keep > min(dimz)) { 
    # ensure not trying to extract too many pcs
    warning(paste("selected too many PCs to keep [",pcs.to.keep,"], changing to ",min(dimz),"\n",sep="")) 
    pcs.to.keep <- min(dimz)
  } 
  if(!SVD & (dimz[2]>dimz[1])) {
    if(verbose) { cat(" PCA using 'princomp' (only for datasets with more samples than markers)\n") }
    print(system.time(result <- princomp(t(subMat))))
    PCs <- result$scores[,1:pcs.to.keep]
    Evalues <- result$sdev^2 # sds are sqrt of eigenvalues
  } else {
    if(!SVD) {
      if(verbose) {
        cat(" PCA by crossproduct and solving eigenvectors\n")
        cat(" obtaining crossproduct of the matrix and transpose XtX...")
      }
      uu <-(system.time(xtx <- crossprod(subMat)))
      if(verbose) { 
        cat("took",round(uu[3]/60,1),"minutes\n")
        cat(" obtaining eigen vectors of the crossproduct XtX...")
      }
      uu <-(system.time(result <- eigen((xtx/nrow(subMat)),symmetric=T)))
      if(verbose) {  cat("took",round(uu[3]/60,1),"minutes\n") }
      PCs <- result$vectors[,1:pcs.to.keep]
      Evalues <- result$values
    } else {
      pt <- "package:"; pkgset <- gsub(pt,"",search()[grep(pt,search())])
      do.fast <- (!LAP & (all(c("irlba","bigalgebra") %in% pkgset)))
      if(verbose) {
        cat(" PCA by singular value decomposition...") # La.svd gives result with reversed dims. (faster?)
      }
      if(!LAP) {
        if(do.fast) {
          uu <-(system.time(result <- irlba(subMat,nv=pcs.to.keep,nu=0,matmul=matmul))) 
        } else {
          warning("[without 'bigalgebra' package, PCA runs slowly for large datasets,",
              "see 'big.algebra.install.help()']\n")
          uu <-(system.time(result <- svd(subMat,nv=pcs.to.keep,nu=0)))
        }
        if(verbose) { cat("took",round(uu[3]/60,1),"minutes\n") }
        PCs <- result$v[,1:pcs.to.keep]
        Evalues <- result$d^2 # singular values are the sqrt of eigenvalues
      } else {
        if(verbose) { cat("\n [using LAPACK alternative with La.svd]") }
        uu <- (system.time(result <- La.svd(subMat,nv=pcs.to.keep,nu=0)))
        if(verbose) { cat("took",round(uu[3]/60,1),"minutes\n") }
        PCs <- t(result$vt)[,1:pcs.to.keep]  ##?
        Evalues <- result$d^2 # singular values are the sqrt of eigenvalues
      }
    }
  }
  rownames(PCs) <- colnames(subMat)
  colnames(PCs) <- paste("PC",1:pcs.to.keep,sep="")
  if(save.pcs) {
    ofn <- cat.path(dir$pc,pcs.fn)
    cat(paste("~wrote PC data to file:",ofn,"\n"))
    save(PCs,Evalues,file=ofn) }
  out.dat <- list(PCs,Evalues)
  names(out.dat) <- c("PCs","Evalues")
  return(out.dat)
}


# XHMM follows the empirical rule of thumb
# by calculating the relative variance of each component and
# removing the K components with a value of 0.7 / n or higher,28
# where n is the number of components (in this case, number of
#                                      samples) and 0.7 is a user-tunable XHMM parameter. 


LRR.PCA.correct <- function(pca.result,descr.fn,dir,num.pcs=9,n.cores=1,pref="corrected",
                            big.cor.fn=NULL,write=F,sample.info=NULL,correct.sex=F,add.int=F)
{
  ## using results of a PCA analysis, run correction for 'num.pcs' PCs on a dataset
  # uncorrected matrix
  #dir <- validate.dir.for(dir,c("big","pc"))
  if(exists("validate.dir.for",mode="function")) {
    ## plumbCNV specific code ##
    dir <- do.call("validate.dir.for",list(dir=dir,elements=c("big","pc"),warn=F))  
  } else {
    # otherwise
    dir <- list(big=dir,pc=dir)
  }
  origMat <- getBigMat(descr.fn,dir)
  cat("\nRunning Principle Components correction (PC-correction), using LRR-dataset:\n")
  bigMatSummary(origMat,name="origMat")
  if(n.cores>1) { multi <- T } else { multi <- F }
  # get filenames now to add to result later
  rN <- rownames(origMat); cN <- colnames(origMat)
  # run pca.correction using ectors (PCs) and alues from LRR.PCA
  if(!is.list(pca.result)) {
    if(is.character(pca.result)) {
      ofn <- cat.path(dir$pc,pca.result) 
      if(file.exists(ofn))
      {
        pca.file <- get(load(ofn))
        cat(" loaded PCA values and vectors\n")
        PCs <- pca.file$PCs
      } else {
        stop("Error: file",ofn,"does not exist\n")
      }
    } else {
      if(ncol(origMat) %in% dim(pca.result))
      {
        #given dimensions = number of samples, assume PCs entered as matrix
        PCs <- pca.result
      } else {
        stop("Error: expecting file name or PC matrix: pca.result\n")
      }
    } 
  } else {
    PCs <- pca.result$PCs
  }
  # create new matrix same size, ready for corrected values
  nR <- nrow(origMat); nC <- ncol(origMat)
  cat(" creating new file backed big.matrix to store corrected data...")
  pcCorMat <- filebacked.big.matrix(nR,nC, backingfile=paste(pref,"Bck",sep=""),
                                    backingpath=dir$big, descriptorfile=paste(pref,"Descr",sep=""))
  cat("done\n")
  if(!is.filebacked(pcCorMat) | !is.filebacked(origMat)) {
    warning("at least one of the big.matrices is not filebacked, memory problems may be encountered")
  }
  # in result$vectors, PCs are the columns, only need first 10 or so
  # rows are subjects / samples
  col.sel <- 1:ncol(origMat)
  nPCs <- PCs[,1:num.pcs]; sex.txt <- ""
  if(correct.sex) { 
    ## add a column to the PC matrix which will allow to covary for sex too
    coln <- which(tolower(colnames(sample.info)) %in% c("gender","sex")) 
    if(length(coln)>0) { 
      indx <- match(colnames(origMat),rownames(sample.info))
      if(all(!is.na(indx))) {
        sex.vec <- sample.info[indx,coln[1]] 
        sex.vec[is.na(sex.vec)] <- mean(sex.vec,na.rm=T) # replace missing sex with mean 
        nPCs <- cbind(sex.vec,nPCs) ; sex.txt <- " (covarying for sex)"
      } else { warning("could not correct for sex as sample.info was missing some IDs") }
    }
  }
  nPCs <- cbind(rep(1,nrow(nPCs)),nPCs) # this adds intercept term for lm.fit() [remove if using lm() ]
  cat(" correcting by principle components",sex.txt,", taking the LRR lm-residual for each SNP\n",sep="")
  jj <- proc.time()
  nsamples <- ncol(origMat)
  num.snps <- nrow(origMat); sampz <- 1:nsamples
  snps.per.proc <- 400;   flush.freq <- 20
  if(nsamples>10000) { snps.per.proc <- 300 }; if(nsamples>20000) { snps.per.proc <- 150 }
  if(nsamples>40000) { snps.per.proc <- 50 }; if(nsamples>80000) { snps.per.proc <- 10 }
  ## assume if we have lots of cores, we'd also have lots of RAM too
  if(n.cores>5) { snps.per.proc <- snps.per.proc*2; flush.freq <- flush.freq*2 } 
  if(n.cores>15) { snps.per.proc <- snps.per.proc*2 }
  snps.per.proc <- max(snps.per.proc,n.cores) # at least 1 snp per core as a minimum
  stepz <- round(seq(from=1,to=num.snps+1,by=snps.per.proc))
  if((tail(stepz,1)) != num.snps+1) { stepz <- c(stepz,num.snps+1) }
  split.to <- length(stepz)-1
  big.extras <- T # flush memory every 'n' iterations.
  
  # this simple way works (instead of big for-loop) but hogs memory and is no faster
  # [NB: requires transpose of target corrected big matrix dimensions]
  ### pcCorMat <- apply(origMat,1,PC.fn,nPCs=nPCs,col.sel=sampz)
  for (dd in 1:split.to)
  {
    x1 <- stepz[dd]; x2 <- stepz[dd+1]-1 #subset row selection
    # use of this 'sub.big.matrix' structure, stops the memory leak behaviour which spirals
    # the memory relating to 'origMat' out of control. 
    next.rows <- sub.big.matrix(origMat, firstRow=x1, lastRow=x2, backingpath=dir$big )
    # next.rows is now a pointer to a matrix subset, must use 'as.matrix' to coerce to a regular R object 
    if(multi) {
      pcCorMat[x1:x2,] <- PC.fn.mat.multi(as.matrix(next.rows),nPCs,mc.cores=n.cores,add.int=add.int)
    } else {
      pcCorMat[x1:x2,] <- PC.fn.mat.apply(as.matrix(next.rows),nPCs,add.int=add.int)
    }
    loop.tracker(dd,split.to)
    ## Every 'flush.freq' iterations clean up the memory, remove the 
    ##  big.matrix object 'pcCorMat' and re-attach it 
    if(dd %% flush.freq == 0) {    
      fl.suc <- flush(pcCorMat) & flush(next.rows)
      if(!fl.suc) { cat("flush failed\n") } 
      gc()  # garbage collection
      if(big.extras) {
        RR <- describe(pcCorMat)
        rm(pcCorMat)
        pcCorMat <- attach.big.matrix(RR,path=dir$big)
      }
    }
    rm(next.rows) # remove the sub-matrix pointer each iteration or this memory builds up 
  }
  
  options(bigmemory.allow.dimnames=TRUE)
  rownames(pcCorMat) <- rN;  colnames(pcCorMat) <- cN 
  ll <- proc.time()
  cat(paste(" LRR PC-Correction took",round((ll-jj)[3]/3600,3),"hours\n"))
  flush(pcCorMat) # should allow names to take  
  cat("\nPC-corrected dataset produced:\n")
  bigMatSummary(pcCorMat,name="pcCorMat")
  
  mat.ref <- describe(pcCorMat)
  if(write) {
    if(is.null(big.cor.fn) | !is.character(big.cor.fn)) {
      big.fn <- paste("describePCcorrect",num.pcs,".RData",sep="")
    } else {
      big.fn <- big.cor.fn[1]
    }
    ofn <- cat.path(dir$big,big.fn)
    save(mat.ref,file=ofn)
    cat(paste("~wrote PC-corrected data description file to:\n ",ofn,"\n"))
    return(big.fn)
  } else {
    return(mat.ref)
  }
}



#' Transpose function for big.matrix objects
#'
#' At the time of writing, there is no transpose method for big.matrix()
#' This function returns a new filebacked big.matrix which is the transpose of the input
#' big.matrix. max.gb allows periodic manual flushing of the memory to be conducted in case
#' the builtin memory management of R/bigmemory is not working as desired.
#' This method is a non-native (not using the raw C objects from the package but merely
#' standard R accessors and operations) algorithm to transpose a big matrix efficiently
#' for memory usage and speed. A blank matrix is created on disk and the data is
#' block-wise transposed and buffered into the new matrix.
#'
#' @param bigMat default, a big.matrix(), although if 'file.ok' is set TRUE, then
#'  this can be a big.matrix descriptor, or a file location
#' @param dir the directory for the matrix backing file (preferably for both the original
#'  and the proposed transposed matrix). If this is left NULL and bigMat contains a path,
#'  this path (via dirname(bigMat)) will be used; if it doesn't contain a path the current
#'  working directory will be used
#' @param name the basename of the new transposed matrix
#' @param R.descr the name of a binary file that will store the big.matrix.descriptor
#'  for the transposed matrix. If "" then the descriptor won't be saved. If NULL, then
#'  it will be <name>.RData
#' @param max.gb the maximum number of GB of data to process before flushing the big.matrix
#' @param verbose whether to print messages about each stage of the process
#' @param prog whether to use a progress bar. NA means it will only be used if the matrix
#'  in question is larger than 1GB.
#' @param file.ok whether to accept big.matrix.descriptors or filenames as input for 
#'  'bigMat'; if T, then anything that works with getBigMat(bigMat,dir) is acceptable
#' @export
#' @examples
#' library(bigmemory)
#' bM <- filebacked.big.matrix(200, 500,
#'        dimnames = list(paste("r",1:200,sep=""), paste("c",1:500,sep="")),
#'        backingfile = "test.bck",  backingpath = getwd(), descriptorfile = "test.dsc")
#' bM[1:200,] <- replicate(500,rnorm(200))
#' print.big.matrix(bM)
#' tbM <- t.big(bM,dir=getwd(),verbose=T)
#' print.big.matrix(tbM)
t.big <- function(bigMat,dir=NULL,name="t.bigMat",R.descr=NULL,max.gb=NA,
                  verbose=F,prog=NA,file.ok=T) {
  #this can be slow!
  if(is.null(R.descr)) { R.descr <- cat.path(basename(name),name,ext="RData") }
  if(!is.big.matrix(bigMat)) {
    if(is.matrix(bigMat) | is.data.frame(bigMat)) {
      warning("just a regular matrix, used t()") ; return(t(bigMat)) 
    } else {
      if(file.ok) { 
        # bigMat can be a file path
        if(is.null(dir) & file.exists(bigMat)) {
          if(dirname(bigMat)!=".") { dir <- dirname(bigMat) }
        }
        try(bigMat <- getBigMat(bigMat,dir)) 
      }
      if(!is.big.matrix(bigMat)) {
        stop("invalid object for big.matrix transposition")    
      }
    }
  } else {
    if(is.null(dir)) { dir <- getwd() }
  }
  slow.is <- 1 #GB  [ above this if prog = NA, then do a progress bar ]
  nR <- nrow(bigMat); nC <- ncol(bigMat)
  if(is.na(prog)) { if(estimate.memory(c(nR,nC))>slow.is) { prog <- T } else { prog <- F } }
  cN <- colnames(bigMat); rN <- rownames(bigMat)
  if(verbose) { cat(" creating",nC,"x",nR,"target matrix,",name,"...") }
  des <- paste(name,"descrFile",sep="_")
  bck <- paste(name,"bckFile",sep="_")
  bigTrans <- big.matrix(nrow=nC,ncol=nR, backingfile=bck,
                         backingpath=dir, descriptorfile=des)
  if(verbose) { cat("done\n"); cat("\nAdding names\n") }
  options(bigmemory.allow.dimnames=TRUE)
  colnames(bigTrans) <- rN
  if(verbose) { cat(" added colnames\n") }
  rownames(bigTrans) <- cN
  if(verbose) { cat(" added rownames\n") }
  d2 <- d1 <- 0
  #try({
  split.to <- 10*round(estimate.memory(bigMat)) # split into .1GB chunks, save RAM without creating groups too small to process
  #if(n.cores>4) { split.to <- split.to * 4 } # divide more if using multicores
  stepz <- round(seq(from=1,to=nC+1,length.out=round((split.to+1))))
  if((tail(stepz,1)) != nC+1) { stepz <- c(stepz,nC+1) }
  split.to <- length(stepz)-1
  if(verbose) { cat(" transposing 'bigMat' into new big.matrix object:\n") }
  if(is.na(max.gb)) { max.gb <- split.to + 10 }
  for (cc in 1:split.to)
  {
    # within submatrix cols
    c1 <- stepz[cc]; c2 <- stepz[cc+1]-1  # check this in FN!
    # do the copying
    lilColRange <- c(c1:c2)
    if(prog) {      loop.tracker(cc,split.to) }
    if(is.finite(sum(lilColRange))) {
      #cat(range(lilColRange)); cat(dim(bigTrans)); cat(dim(bigMat))
      bigTrans[lilColRange,1:nR] <- t(bigMat[1:nR,lilColRange])
    } else {
      cat(" Warning: empty interval ignored\n")
    }
    if(cc %% (max.gb*10) == 0) {
      # reset memory after every 'max.gb' 1GB chunks to prevent skyrocketing RAM use #
      fl.suc <- flush(bigTrans) ;  if(!fl.suc) { cat("flush failed\n") } ; gc()  
      if(T) {
        RR <- describe(bigTrans); rm(bigTrans); bigTrans <- attach.big.matrix(RR,path=dir)
      }
    }
  }
  #})
  if(verbose) { cat(" combining complete, converting result to big matrix\n") }
  descr <- describe(bigTrans)
  flush(bigTrans) # hopefully this will ensure the row/colnames are added to the file backing
  
  if(verbose) {
    cat(paste(" created big.matrix description file:",des,"\n"))
    cat(paste(" created big.matrix backing file:",bck,"\n"))
    if(length(R.descr)>0 & all(R.descr!="")) { 
      save(descr,file=cat.path(dir,R.descr)) 
      cat(paste(" created big.matrix binary description file:",basename(R.descr),"\n"))
    }
  }
  return(bigTrans)
}


### INTERNAL FUNCTIONS ###
#' Internal
PC.fn.mat <- function(next.rows,nPCs,add.int=F)
{
  # matrix version of PC.fn (used to PC-correct one SNP at a time)
  col.sel <- 1:ncol(next.rows)
  for (dd in 1:nrow(next.rows)) {
    # compiled PC.fn should speed up these ops a little
    next.rows[dd,] <- PC.fn(next.rows[dd,],nPCs,col.sel,add.int=add.int) 
  }  
  return(next.rows)
}


#' Internal
PC.fn.mat.apply <- function(nextrows,nPCs,add.int=F)
{
  # matrix version of PC.fn (used to PC-correct one SNP at a time), vectorized version
  # testing shows the for-loop (non-vectorized) to be slightly faster, maybe because of t()
  # when using PC.fn.2 must pass in vec of 1's if you want the intecept
  col.sel <- 1:ncol(nextrows)
  nextrows <- t(apply(nextrows,1,PC.fn.2,nPCs=nPCs,col.sel=col.sel,add.int=add.int))
  return(nextrows)
}


#' Internal
PC.fn.mat.multi <- function(nextrows,nPCs,mc.cores=1,add.int=F)
{
  # matrix version of PC.fn (used to PC-correct one SNP at a time), vectorized version
  # testing shows the for-loop (non-vectorized) to be slightly faster, maybe because of t()
  # when using PC.fn.2 must pass in vec of 1's if you want the intecept
  col.sel <- 1:ncol(nextrows)
  nextrows <- lapply(seq_len(nrow(nextrows)), function(i) nextrows[i,]) # multi slows this down
  #nextrows <- multicore::mclapply(nextrows,PC.fn,nPCs=nPCs,col.sel=col.sel,mc.cores=mc.cores)
  nextrows <- multicore::mclapply(nextrows,PC.fn.2,nPCs=nPCs,col.sel=col.sel,mc.cores=mc.cores, add.int=add.int)
  nextrows <- do.call("rbind",nextrows)
  return(nextrows)
}


#' Internal
PC.fn <- function(next.row,nPCs,col.sel,add.int=F)
{
  # apply PC correction for a single SNP, allowing for missing data.
  bad1 <- which(is.na(next.row))
  if(length(bad1)>0) { sel <- -bad1 } else { sel <- col.sel }
  if(add.int) { int <- mean(next.row[sel]) } else { int <- 0 }
  next.row[sel] <- lm(next.row ~ nPCs,na.action="na.exclude")$residuals + int
  return(next.row)
}


#' Internal
PC.fn.2 <- function(next.row,nPCs,col.sel, add.int=F)
{
  # apply PC correction for a single SNP, allowing for missing data.
  # when using PC.fn.2 must pass in vec of 1's if you want the intecept
  bad1 <- which(is.na(next.row))
  if(length(bad1)>0) { sel <- -bad1 } else { sel <- col.sel }
  if(add.int) { int <- mean(next.row[sel]) } else { int <- 0 }
  next.row[sel] <- lm.fit(x=nPCs[sel,],y=next.row[sel])$residuals + int
  return(next.row)
}


#' Internal
matmul <- function(A, x, transpose=FALSE)
{
  # bigalgebra friendly version of matrix multiplier function
  if(transpose) {
    return(t( t(x) %*% A)) 
  } else {
    return (A %*% x) 
  }
}




# 
# t.big2 <- function(bigMat,dir,name="t.bigMat",R.descr="t.bigMat.RData",n.cores=1,max.gb=4,verbose=T) {
#   #this is slow!
#   if(!is.big.matrix(bigMat)) {
#     if(is.matrix(bigMat) | is.data.frame(bigMat)) {
#       warning("just a regular matrix, used t()") ; return(t(bigMat)) 
#     } else {
#       stop("invalid object for big.matrix transposition")    
#     }
#   }
#   nR <- nrow(bigMat); nC <- ncol(bigMat)
#   cN <- colnames(bigMat); rN <- rownames(bigMat)
#   cat(" creating",nC,"x",nR,"target matrix,",name,"...")
#   des <- paste(name,"descrFile",sep="_")
#   bck <- paste(name,"bckFile",sep="_")
#   bigTrans <- big.matrix(nrow=nC,ncol=nR, backingfile=bck,
#                          backingpath=dir, descriptorfile=des)
#   cat("done\n")
#   if(verbose) { cat("\nAdding names\n") }
#   options(bigmemory.allow.dimnames=TRUE)
#   colnames(bigTrans) <- rN
#   if(verbose) { cat(" added colnames\n") }
#   rownames(bigTrans) <- cN
#   if(verbose) { cat(" added rownames\n") }
#   d2 <- d1 <- 0
#   #try({
#     split.to <- 10*round(estimate.memory(bigMat)) # split into .1GB chunks, save RAM without creating groups too small to process
#     #if(n.cores>4) { split.to <- split.to * 4 } # divide more if using multicores
#     stepz <- round(seq(from=1,to=nC+1,length.out=round((split.to+1))))
#     if((tail(stepz,1)) != nC+1) { stepz <- c(stepz,nC+1) }
#     split.to <- length(stepz)-1
#     cat(" transposing 'bigMat' into new big.matrix object:\n")
#     big.fnc <- function(cc,func,stepz,bigMat,bigTrans,dir,nR)
#     {
#       c1 <- stepz[cc]; c2 <- stepz[cc+1]-1  # check this in FN!
#       # do the copying
#       lilColRange <- c(c1:c2)
#       if(is.finite(sum(lilColRange))) {
#         #cat(range(lilColRange)); cat(dim(bigTrans)); cat(dim(bigMat))
#         bigTrans[lilColRange,1:nR] <- t(bigMat[1:nR,lilColRange])
#         flush(bigTrans)
#       #  rm(next.block) ; gc() # remove the sub-matrix pointer each iteration  
#        # return(out)
#       }
#       loop.tracker(cc,split.to)
#       return(c(NULL))
#     }
#     ## run function as mclapply()
#     if(split.to>=1) {
#       result.list <- lapply(1:split.to, big.fnc, func=func, stepz=stepz, 
#                            bigMat=bigMat, bigTrans=bigTrans, dir=dir,nR=nR)
#       #,mc.cores=n.cores would be really easy to add mclapply, but actually slower!
#     }
#   flush(bigTrans)
#   return(getBigMat(describe(bigTrans),dir))
#     
#   #})
# }

