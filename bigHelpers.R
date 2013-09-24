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


#' Function to estimate the variance percentages for uncalculated eigenvalues
#'
#' If using a function like irlba' to calculate PCA, then you can choose (for speed) 
#' to only calculate a subset of the eigenvalues. So there is no exact % of variance explained 
#' the PCA, or by each component as you will get as output from other routines.
#' This code uses a b*1/x model to estimate the AUC for the unknown eigenvalues, providing
#' a reasonable estimate of the variances accounted for by each unknown eigenvalue, and
#' the predicted eigenvalue sum of the unknown eigenvalues.
#'
#' @param eigenv the vector of eigenvalues actually calculated
#' @param min.dim the size of the smaller dimension of the matrix submitted to singular
#'  value decomposition, e.g, number of samples - i.e, the max number of possible eigenvalues
#' @param elbow the number of components for which you want to estimate the variance
#'   explained, e.g, often the number of components used is decided by the 'elbow' in
#'   a scree plot (see 'pca.scree.plots')
#' @param print.est whether to output the estimate result to the console
#' @param whether to output the estimate regression coefficients to the console
#' @param if you have an existing scree plot, add the fit line from this estimate
#'  to the plot (see 'pca.scree.plots')
#' @param col colour for the fit line
#' @seealso pca.scree.plots
#' @export
#' @examples
#' nsamp <- 200; nvar <- 500; subset.size <- 50; elbow <- 6
#' mat <- matrix(rnorm(min.dim*nvar),ncol=min.dim) # mat <- crimtab
#' print.large(mat)
#' pca <- svd(mat,nv=subset.size,nu=0) # calculates subset of V, but all D
#' pca2 <- irlba(mat,nv=subset.size,nu=0) # calculates subset of V & D
#' pca3 <- princomp(mat) # calculates all
#' Evalues <- pca$d^2 # number always relates to the smaller dimension of the matrix
#' eig.varpc <- estimate.eig.vpcs(Evalues,M=mat)$variance.pcs
#' cat("sum of all eigenvalue-variances=",sum(eig.varpc),"\n")
#' print(eig.varpc[1:elbow])
#' Evalues2 <- pca2$d^2
#' eig.varpc <- estimate.eig.vpcs(Evalues2[1:10],M=mat)$variance.pcs
#' print(eig.varpc[1:elbow])  ## why dramatically underestimating????
#' eig.varpc <- estimate.eig.vpcs(pca3$sdev^2,M=mat)$variance.pcs
#' print(eig.varpc[1:elbow])
#' sum(sqrt(Evalues)[51:200])
#' pca.scree.plot(Evalues[1:10],n.comp=40,add.fit.line=T,min.dim=22)
#' print.large(mat,row=9,col=4,digits=1,rL="#",rlab="samples",clab="variables")
estimate.eig.vpcs <- function(eigenv=NULL,min.dim=length(eigenv),M=NULL,elbow=NA,
                              print.est=T,print.coef=F,add.fit.line=F,col="blue") {
  if(all(is.na(elbow))) { elbow <- 3 } 
  ## if matrix is optionally inputted, calculate the minimum dim automatically
  if(!is.null(M)) { if(!is.null(dim(M))) { min.dim <- min(dim(M),na.rm=T) } }
  n.comp <- length(eigenv) # max(c(min.dim,length(eigenv)),na.rm=T)
  elbow <- round(min(n.comp,elbow,na.rm=T)) # make sure not > n.comp
  if(!is.numeric(eigenv)) { warning("eigenv not numeric"); return(NULL) }
  if(is.na(min.dim) | ((min.dim-n.comp)<2) | ((n.comp-elbow)<(min.dim/20)) ) {
    # if most/all eigenvalues already present this is not needed, or if parameters insufficient
    # then don't try to calculate the AUC of the remaining eigenvalues
    if(n.comp==min.dim) {
      cat("All eigenvalues present, estimate not required\n")
    } else {
      warning("didn't attempt to estimate eigenvalues as there were",
        " very few unknowns compared to the number of samples,",
        " or not enough eigenvalues between the elbow and 'min.dim'")
    }
    var.pcs <- eigenv[1:n.comp]/(sum(eigenv)); tail.var <- 0
  } else {
    # estimate combined variance of eigenvalues not calculated by irlba using 1/x model
    xx <- 1/(1:length(eigenv)) ; ab <- lm(eigenv[elbow:n.comp]~0+xx[elbow:n.comp])$coef
    # intercept ignored as the asymptote should theoretically be zero so assumption
    # values > this reflect noise variance that might dissipate as x--> n.samp
    tail.var <- ((log(min.dim-elbow)-log(length(eigenv)-elbow))*ab[1]) # integral evaluated
    if(print.est) {
      not.calc <- min.dim-length(eigenv)
      cat(" estimate of eigenvalue sum of",not.calc,"uncalculated eigenvalues:",(as.numeric(tail.var)),"\n")
    }
    if(print.coef) {
      cat(" slope [1/x, no intercept]:",as.numeric(ab[1]),"\n")
    }
    if(add.fit.line) {
      # add fitted line to scree plot if one exists
      try(lines(c((elbow+1):min.dim),ab[1]*(1/c((elbow+1):min.dim)),col=col),T)
    }
    var.pcs <- eigenv[1:n.comp]/(sum(eigenv)+tail.var)
  }
  out <- list(var.pcs,tail.var)
  names(out) <- c("variance.pcs","tail.auc")
  return(out)
}


pca.scree.plot <- function(eigenv,elbow=9,n.comp=30,printvar=T,nsamp=NA,add.fit.line=F,...) 
{
  # do SCREE PLOTS AND calculate EIGENVALUE VARIANCE after a PCA
  plot(eigenv[1:n.comp],bty="l",xlab="number of principle components",ylab="eigenvalues",bg="green",pch=21,...)
  abline(v=(elbow+.5),lty="dashed")
  legend("topright",legend=c("Principle components","scree plot 'elbow' cutoff"),
         pt.bg=c("green",NA),pch=c(21,NA),lty=c(NA,"dashed"),bty="n")
  scree.calc <- estimate.eig.vpcs(eigenv=eigenv,nsamp=nsamp,elbow=elbow,
                  print.est=T,print.coef=T,add.fit.line=add.fit.line,col="blue")
  if(printvar) {
    cat(" sum of eigen-variance:",round(sum(eigenv)+scree.calc$tail.auc,2),"\n")
    cat(" variance % estimates: \n ",round(scree.calc$variance.pcs,2),"\n")
  }
  return(scree.calc$variance.pcs)
}


multi.fn.on.big.split <- function(bigMat,func,dir=NULL,bycol=T,by=200,n.cores=1,chunkwise=F,split.arg=NULL,...) {
  # multicore way of calculating a function (e.g, dlrs) for a big.matrix,
  # when chunkwise=F, a function that could be done with apply(); when chunkwise=T, one that
  # is best done in chunks rather than row by row or col by col
  # can do it column wise (bycol=T, eg for all samples), or row-wise, eg for all snps
  # 'by' is number of rows/cols to process in each chunk
  must.use.package("multicore")
  if(bycol) { tot.main <- ncol(bigMat); d2 <- nrow(bigMat) } else { tot.main <- nrow(bigMat); d2 <- ncol(bigMat) }
  if(!is.null(split.arg)) { 
    if(!is.null(dim(split.arg))) {
      if(bycol) { 
        if(ncol(split.arg)!=tot.main) { 
          split.arg <- NULL ; warning("split.arg should have same number of columns as bigMat")
      } } else {
        if(nrow(split.arg)!=tot.main) { 
          split.arg <- NULL ; warning("split.arg should have same number of rows as bigMat")
      } }
    } else {
      if(is.vector(split.arg) & length(split.arg)!=tot.main) { 
        split.arg <- NULL; warning("split arg needs to be the same length as the key bigMat dimension (",tot.main,")") 
    } }
  }
  if(!is.null(split.arg)) { 
    n.args.func <- length(formals(func))
    if(n.args.func<2) { split.arg <- NULL ; warning("split arg ignored as function only has 1 parameter") }
    if(n.args.func>2) { warning("split.arg must refer to the second argument of 'func' or results may be unpredictable") }
  }
  stepz <- round(seq(from=1,to=(tot.main+1),by=by))
  if((tail(stepz,1)) != tot.main+1) { stepz <- c(stepz,tot.main+1) }
  split.to <- length(stepz)-1
  result <- numeric(tot.main)
  ## define the function
  big.fnc <- function(dd,func,stepz,bigMat,dir,bycol=T,split.arg=NULL,...)
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
    if(!is.null(split.arg)) {
      if(is.null(dim(split.arg))) { split.arg <- split.arg[x1:x2] } else {
        if(bycol) { split.arg <- split.arg[,x1:x2] } else { split.arg <- split.arg[x1:x2,] }
      }
      print(length(split.arg))
      # if a valid split.arg, then enter it as the second parameter of the function
      if(chunkwise) { out <- func(next.block,split.arg,...)  } else {  out <- apply(next.block,dm,func,split.arg,...) }
    } else {
      if(chunkwise) { out <- func(next.block,...)  } else {  out <- apply(next.block,dm,func,...) }
    }
    rm(next.block) ; gc() # remove the sub-matrix pointer each iteration  
    return(out)
  }
  ## run function as mclapply()
  if(split.to>=1) {
    result.list <- multicore::mclapply(1:split.to, big.fnc, func=func, stepz=stepz, 
                                       bigMat=bigMat, dir=dir, bycol=bycol, mc.cores=n.cores,split.arg=split.arg,...)
    if(!chunkwise) { result <- unlist(result.list) } # if apply-type, then recombine result, else leave as list
  } else {
    result <- NULL; warning("matrix had insufficient columns returning NULL")
  }
  return(result)  
}


bigmcapply <- function(bigMat,MARGIN,FUN,dir=NULL,by=200,n.cores=1,use.apply=T,combine.fn=NULL,...) {
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



big.algebra.install.help <- function(verbose=F) {
  ## bigalgebra package doesn't install easily using the regular R way of installing packages
  # here try a simple way that might work, and if not, provide links and instructions to 
  # guide a manual installation
  try({ if(require(bigalgebra)) { return(T) } })
  cat("\nbigalgebra installation not found, will attempt to install now, but it can be tricky\n")
  install.packages("bigalgebra", repos="http://R-Forge.R-project.org")
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


load.data.to.bigmat <- function(dat.fn,inputType="TXT",bck,des,dir,Hopt=F,RNopt=T)
{
  # get data stored in a text or binary file and get it into a bigmatrix object format
  dir <- validate.dir.for(dir,c("ano","big"),warn=F)
  if(inputType=="RDATA")
  {
    # attach datafile bigmemory object
    cat("\nLoading RData file...")
    this.mat <- load(dat.fn)
    if (this.mat!="m.by.s.matrix") { 
      m.by.s.matrix <- get(this.mat)
      rm(this.mat) 
    }
    cat("complete\n")
    if(is.null(colnames(m.by.s.matrix)))
    {
      snp.fn <- cat.path(dir$ano,"snpNames.txt")
      snp.list <- readLines(snp.fn)
      if (ncol(m.by.s.matrix)==length(snp.list))
      {
        colnames(m.by.s.matrix) <- snp.list
        cat(paste("*warning: added column names to matrix from",snp.fn,"\n"))
        cat("if replacement with these names is undesired please check/modify the .R source code\n")
      } else {
        cat("Error: matrix has no column names and default file doesn't\n")
        cat("match number of columns, please check/modify the .R source code\n")
        stop()
      }
    }
    cat(" saving datafile as big matrix\n")
    bigMat <- as.big.matrix(m.by.s.matrix, backingfile=bck,
                            backingpath=dir$big, descriptorfile=des)
  } else {
    # assume inputType=="TXT"
    cat("\nLoading TAB file...(this will probably be slow)...")
    read.big.matrix(dat.fn, sep = '\t', header = Hopt,
                    has.row.names=RNopt, ignore.row.names=FALSE,
                    backingfile = bck, backingpath = dir$big,
                    descriptorfile = des, extraCols = NULL)
    cat("complete\n")
  }
}


### to wrap ##


get.PCA.subset <- function(dir,pc.to.keep=.13,assoc=F,autosomes=T,big.fn="combinedBigMat.RData",
                           snp.sub.fn="pca.snp.subset.txt",use.current=F,pref="PCAMatrix",n.cores=1,
                           descr.fn="pcaSubMat.RData",nprev=0,snp.info=NULL,sample.info=NULL) 
{
  ## extract LRR matrix with subset of SNPs, ready for PCA analysis
  dir <- validate.dir.for(dir,c("ano","big"))
  load.all.libs() # load all main libraries used by plumbCNV
  #if(add.pheno) {
  #  sample.info <- read.sample.info(dir)
  #  phenotype <- read.table(file=cat.path(dir$ano,"pheno.lookup.txt"))
  #  sample.info <- add.to.sample.info(sample.info,phenotype,"phenotype")
  #  write.sample.info(sample.info,dir)
  #}
  if(!is.data.frame(sample.info)) { sample.info <- read.sample.info(dir,nprev=nprev) }
  if(is(snp.info)[1]!="RangedData") { snp.info <- read.snp.info(dir,nprev=nprev) }
  #sample.info <- validate.samp.info(sample.info,QC.update=F,verbose=F) #this done done later anyway
  #samp.fn <- "combined.samples.txt"
  if(use.current & is.file(snp.sub.fn,dir$ano,dir)) {
    snps.to.keep <- get.vec.multi.type(snp.sub.fn,dir=dir)
  } else {
    snps.to.keep <- extract.snp.subset(snp.info,sample.info,pc.to.keep=pc.to.keep,assoc=assoc,autosomes=autosomes,
                                       writeResultsToFile=T,big.fn=big.fn,out.fn=snp.sub.fn,dir=dir, n.cores=n.cores)
  }
  ###bigMat <- getBigMat(big.fn,dir)
  if(length(snps.to.keep)>100) {
    ##writeLines(colnames(bigMat),paste(dir$ano,samp.fn,sep=""))
    subset.descr <- big.exclude.sort(big.fn,dir=dir,T,tranMode=1,pref=pref,f.snp=snps.to.keep,verbose=F)
  } else {
    stop("Error: list of snps to keep is too small - trying running again with a higher pc.to.keep\n")
  }
  #if(descr.fn!="") {
  #  save(subset.descr,file=cat.path(dir$big,descr.fn))
  #} else { warning("submatrix description returned but not saved\n")}
  print(subset.descr)
  return(subset.descr)
}


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


big.PCA <- function(subDescr,dir,pcs.to.keep=50,SVD=T,LAP=F,save.pcs=T,pcs.fn="PCsEVsFromPCA.RData") 
{
  # run principle components analysis on the SNP subset of the LRR snp x sample matrix
  # various methods to choose from with pro/cons of speed/memory, etc.
  #  must use SNP-subset to avoid LD, destroying main effects, +avoid huge memory requirements
  dir <- validate.dir.for(dir,c("big","pc"))
  #must.use.package(c("irlba"),T)
  pcaMat <- getBigMat(subDescr,dir)
  cat("\nRunning Principle Components Analysis (PCA), using LRR-data subset:\n\n")
  bigMatSummary(pcaMat,name="pcaMat")
  est.mem <- estimate.memory(pcaMat)
  cat(" estimated memory required for",nrow(pcaMat),"x",ncol(pcaMat),"matrix:",round(est.mem,2),
      "GB. If this exceeds available,\n  then expect PCA to take a long time or fail!\n")
  subMat <- as.matrix(pcaMat) # must convert bigmatrix to plain matrix here, no pca yet takes a bigmatrix
  rm(pcaMat)
  # center using row means
  cat(" centering data by row means...")
  subMat <- subMat - rowMeans(subMat)  #matrix(rep(rowMeans(subMat),times=ncol(subMat)),ncol=ncol(subMat))
  cat(" means for first 10 snps:\n")
  print(round(head(rowMeans(subMat),10))) # show that centering has worked
  subMat[is.na(subMat)] <- 0 # replace missing with the mean
  cat(" replaced missing data with mean (PCA cannot handle missing data)\n")
  #subMat <- t(subMat) # transpose
  dimz <- dim(subMat)
  if(pcs.to.keep > min(dimz)) { 
    # ensure not trying to extract too many pcs
    warning(paste("selected too many PCs to keep [",pcs.to.keep,"], changing to ",min(dimz),"\n",sep="")) 
    pcs.to.keep <- min(dimz)
  } 
  if(!SVD & (dimz[2]>dimz[1])) {
    cat(" PCA using 'princomp' (only for datasets with more samples than markers)\n")
    print(system.time(result <- princomp(t(subMat))))
    PCs <- result$scores[,1:pcs.to.keep]
    Evalues <- result$sdev^2 # sds are sqrt of eigenvalues
  } else {
    if(!SVD) {
      cat(" PCA by crossproduct and solving eigenvectors\n")
      cat(" obtaining crossproduct of the matrix and transpose XtX...")
      uu <-(system.time(xtx <- crossprod(subMat)))
      cat("took",round(uu[3]/60,1),"minutes\n")
      cat(" obtaining eigen vectors of the crossproduct XtX...")
      uu <-(system.time(result <- eigen((xtx/nrow(subMat)),symmetric=T)))
      cat("took",round(uu[3]/60,1),"minutes\n")
      PCs <- result$vectors[,1:pcs.to.keep]
      Evalues <- result$values
    } else {
      do.fast <- (!LAP & ((require(irlba) & require(bigalgebra))))
      cat(" PCA by singular value decomposition...") # La.svd gives result with reversed dims. (faster?)
      if(!LAP) {
        if(do.fast) {
          uu <-(system.time(result <- irlba(subMat,nv=pcs.to.keep,nu=0,matmul=matmul))) 
        } else {
          cat("[without 'bigalgebra' package, slow for large datasets]\n")
          uu <-(system.time(result <- svd(subMat,nv=pcs.to.keep,nu=0)))
        }
        cat("took",round(uu[3]/60,1),"minutes\n")
        PCs <- result$v[,1:pcs.to.keep]
        Evalues <- result$d^2 # singular values are the sqrt of eigenvalues
      } else {
        cat("\n [using LAPACK alternative with La.svd]")
        uu <- (system.time(result<- La.svd(subMat,nv=pcs.to.keep,nu=0)))
        cat("took",round(uu[3]/60,1),"minutes\n")
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
  dir <- validate.dir.for(dir,c("big","pc"))
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

