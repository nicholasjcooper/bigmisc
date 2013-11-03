require(tools)

#' Tidy display function for matrix objects
#'
#' This function prints the first and last columns and rows of a matrix, and
#' more, if desired. Allows previewing of a matrix without 
#' overloading the console. Most useful when data has row and column names.
#'
#' @param largeMat a matrix
#' @param rows number of rows to display
#' @param cols number of columns to display
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
#' print.large(mat,rows=9,cols=4,digits=1,rL="#",rlab="samples",clab="variables")
print.large <- function(largeMat,rows=3,cols=2,digits=4,rL="Row#",rlab="rownames",clab="colnames",rownums=T,ret=F) 
{
  # nicely print a large matrix without overloading the output space
  # can return result as lines of text instead of printing to screen (for printing to file)
  # allows customization of row and column labels
  # only worth using with data that has row/col names
  
  if(length(dim(largeMat))!=2) { stop("expected largeMat to have 2 dimensions") }
  nC <- ncol(largeMat); nR <- nrow(largeMat); 
  if(nC<2 | nR<3) { warning("print.large only works for matrices with dims >= c(3,2), passed to print(head())")
                    print(head(largeMat,rows+1)); return(NULL) }
  rows <- min(max(1,rows),nR); cols <- min(max(1,cols),nC)
  cN <- colnames(largeMat); rN <- rownames(largeMat)
  if(is.null(cN)) { cN <- paste(1:ncol(largeMat)); clab <- "col#" }
  if(is.null(rN)) { rN <- paste(1:nrow(largeMat)); rlab <- "row#"; rownums=F }
  rD <- spc(min(2,max(nchar(paste(nR)))),".")
  rnD <- spc(min(4,max(nchar(rN[c(1:rows,nR)]))),".")
  linez <- vector("list",rows+3) #row,col =number of rows,cols to print
  rown <- max(nchar(paste(nR)),nchar(rL))*as.numeric(rownums)
  hdr <- (nchar(cN[c(1:cols,nC)]))
  if(is.numeric(largeMat[1,])) {
    ## assess what the largest numbers are likely to be to adjust header spacing if necessary
    long.nums <- max(max(abs(largeMat[1,]),na.rm=T),max(abs(largeMat[,1]),na.rm=T))
    max.before.dp <- nchar(round(long.nums))+3
  } else { max.before.dp <- 6 }
  hdr[hdr<7] <- 7; hdr[hdr<(digits+max.before.dp)] <- (digits+max.before.dp)
  idln <- max(nchar(rlab),nchar(rN[c(1:rows,nR)]))
  pad <- function(X,L) { if(is.character(X)) { paste(spc(L-nchar(X)),X,sep="") } else { stop(X) } }
  RND <- function(X,...) { if (is.numeric(X)) { round(X,...) } else { X }}
  if(!ret) { cat("\n"); cat(spc(rown),spc(idln),clab,"\n") }
  dotz <- "  ...  "; dotzh <- " ..... "; dotzn <- "..."
  # make adjustments if matrix is small enough to display all rows/cols
  if(nC<=cols) { dotz <- dotzh <- "" ; cols <- cols-1 }
  if(nR<=rows) { lstln <- 1 } else {  lstln <- 3 }
  ## make adjustments if not displaying rownumbers
  if(!rownums) {
    lstR <- "" ; rD <- ""; jstr <- rep("",times=rows); rL=""
  } else {
    lstR <- nR; jstr <- paste(1:rows)
  }
  linez[[1]] <- c(pad(rL,rown),pad(rlab,idln),pad(cN[c(1:cols)],hdr[1:cols]),
                  dotzh,pad(cN[nC],tail(hdr,1)))
  for (j in 1:rows) { 
    catdb(j,cols)
    xy2 <- RND(largeMat[j,1:cols],digits)
    xy3 <- hdr[1:cols]
    xy4 <- largeMat[j,1:cols]
    catdb(xy2,xy3,xy4)
    linez[[j+1]] <- c(pad(jstr[j],rown),pad(rN[j],idln),
                      pad(RND(largeMat[j,1:cols],digits),hdr[1:cols]),dotz,
                      pad(RND(largeMat[j,nC],digits),tail(hdr,1)))
  }
  linez[[rows+2]] <- c(pad(rD,rown),pad(rnD,idln),pad(rep(dotzn,times=cols),
                                                     hdr[1:cols]),dotz,pad(dotzn,tail(hdr,1)))
  linez[[rows+3]] <- c(pad(lstR,rown),pad(rN[nR],idln),
                      pad(RND(largeMat[nR,1:cols],digits),hdr[1:cols]),
                      dotz,pad(RND(largeMat[nR,nC],digits),tail(hdr,1)))
  if(!ret) {
    for (j in 1:(rows+lstln)) {
      cat(paste(linez[[j]],collapse=" "),"\n")
    }
  } else {
    # remove last two lines if all rows are displayed
    if(lstln==1) { for(ii in 1:2) { linez[[length(linez)]] <- NULL }  }
    return(linez)
  }
}




#' Convert objects as arguments to object names
#' 
#' Equivalent to the base function substitute() but can do any length of arguments instead
#' of just one. Converts the objects in parentheses into text arguments as if they
#' had been entered with double quote strings. The objects must exist and be accessible in
#' the environment the function is called from for the function to work (same as for substitute()).
#' One application for this is to be able to create functions where object arguments can be
#' entered without quotation marks (simpler), or where you want to use the name of the object
#' as well as the data in the object.
#'
#' @param x compulsory, simply the first object in the list, no difference to any further objects
#' @param ... any further objects to return string names for.
#' @export
#' @seealso catdb, cat.db
#' @author Nicholas Cooper 
#' @examples
#' myvar <- list(test=c(1,2,3)); var2 <- "testme"; var3 <- 10:14
#' print(myvar)
#' # single variable case, equivalent to base::substitute()
#' print(substitute(myvar))
#' print(Substitute(myvar))
#' # multi variable case, substitute won't work
#' Substitute(myvar,var2,var3)
#' # catdb() is a wrapper for cat.db() allowing arguments without parentheses
#' # which is achieved internally by passing the arguments to Substitute()
#' cat.db(c("myvar","var2","var3"))
#' catdb(myvar,var2,var3)
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

# shared documentation with cat.db
## same as cat.db but no labels command, and input is without quotes
# and must be plain variable names of existing variables (no indices, args, etc)
# can be used inside functions and interactively
catdb <- function(...,counts=NULL) {
  varlist <- Substitute(...)
  return(cat.db(varlist,labels=NULL,counts=counts))
}

#' Output variable states within functions during testing/debugging
#'
#' A versatile function to compactly display most common R objects. Will
#' return the object name, type, dimension, and a compact representation of
#' object contents, for instance using print.large() to display matrices,
#' so as to not overload the console for large objects. Useful for debugging,
#' can be placed inside loops and functions to track values, dimensions, and data types.
#' Particularly when debugging complex code, the automatic display of the variable name
#' prevents confusion versus using regular print statements.
#' By listing variables to track as character(), provides 'cat()' output 
#' of compact and informative variable state information, e.g, variable name, value,
#' datatype and dimension. Can also specify array or list elements, or custom labels.
#' catdb() is the same as cat.db() except it can take objects without using double quotes
#' and has no 'labels' command (and doesn't need one).
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
    if(is.function(val)) {
      cat(label,": function",sep=""); return(invisible())
    }
    if(packages.loaded("bigmemory")) {
      if(typ=="big.matrix") {
        if(exists("print.big.matrix",mode="function")) {
          print.big.matrix(val,name=label); return(invisible())
        } else {
          warning("cat.db() needs the package bigmisc to display a big.matrix object")
        }
      }
    }
    if(length(unlist(val))==1) {
      cat(label,": ",val," (",typ,", ",paste(Dim(val),collapse="*"),")",sep=""); return(invisible())
    } 
    
    if(is(val)[1]=="list") {
      cat(label," (",typ,", ",paste(Dim(val),collapse="*"),")\n",sep=""); print(headl(val)); return(invisible())
    } else {
      #print(Dim(val))
      if(!is.null(dim(val))) {
        cat(label," (",typ,", ",paste(Dim(val),collapse="*"),")\n",sep="");
        if(length(dim(val))==2) {
          if(ncol(val)>=2 & nrow(val)>=3) {
            print.large(val)
          } else {
            print(head(val))
            cat(if(!is.null(rownames(val))) { "  ...    " } else { "" },rep("  ..  ",ncol(val)),"\n")
          }
        } else {
          print(c(head(val),if(length(val)>6) { (" ...") } else { NULL }))  # e.g, for a table
        }
        return(invisible())
      } else {
        cat(label," (",typ,", ",paste(Dim(val),collapse="*"),") [head]:\n",sep="")
        print(head(val))
        return(invisible())
      }
    }
  }
  ## if data not entered with a label, or as a string (not including catdb() converted calls)
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
    #print(sys.frame(-1))#
    mymode <- "any"
    if(exists(label,mode="function")) { if(exists.not.function(label)) { 
      mymode <- exists.not.function(label,T) } } # if object is also a function, what type is the other type?
    #if(mymode=="") { mymode <- "any" }
    val <- NULL
    try(val <- get(label,envir=ENVIR, mode=mymode),silent=T)
    sf <- sys.frames(); cc <- 1
    while(is.null(val) & cc<=length(sf)) { (try(val <- get(label,envir=sf[[cc]],mode=mymode),silent=T)); cc <- cc + 1 }
    if(!is.null(val)) {
      if(is.list(counts)) { cnts <- lapply(counts,"[",cc) } else { cnts <- NULL }
      display.var(val,label,cnts=cnts)
      cat("\n") 
    } else {
      cat("cat.db() couldn't find variable '",label,"'\n",sep="")
    }
  }
  return(invisible())
}





## update existing headl?

head2 <- function(X,...) { if(length(dim(X))==2) { print.large(X,...) } else { print(utils::head(X,...)) } }

headl <- function (x, n = 6, skip = 20, skip2 = 10, ind = "", ind2 = "  ") 
{
  if (!is.list(x)) {
    warning("not a list")
    return(NULL)
  }
  ll <- length(x)
  if (ll > 0) {
    ind.new <- paste(ind, ind2, sep = "")
    if (ll > skip) {
      ll <- skip
      skipped <- T
    }
    else {
      skipped <- F
    }
    for (cc in 1:ll) {
      if (!is.null(names(x))) {
        cat(ind, "$", names(x)[cc], ":\n", sep = "")
      }
      else {
        (cat("[[", cc, "]]\n", sep = ""))
      }
      if (is(x[[cc]], "list")) {
        headl(x[[cc]], n, ind = ind.new, ind2 = ind2, 
              skip = skip2, skip2 = skip2)
      }
      else {
        cat(ind, sep = "")
        head2(x[[cc]], n)
      }
    }
    if (skipped) {
      cat(ind, "... skipped ", length(x) - skip, " ...\n", 
          sep = "")
    }
  }
  else {
    cat(ind, "<empty>", "\n", sep = "")
  }
}


#' Simulate a dataset with correlated measures
#'
#' Simulate a dataset with correlated measures (normal simulation with e.g, rnorm() usually
#'  only gives small randomly distributed correlations between variables). This is a quick
#'  and unsophisticated method, but should be able to provide a dataset with slightly more
#'  realistic structure than simple rnorm() type functions. Varying the last three parameters
#'  gives some control on the way the data is generated. It starts with a seed random variable,
#'  then creates 'k' random variables with an expected correlation of r=genr() with that seed 
#'  variable. Then after this, one of the variables in the set (including the seed) is randomly
#'  selected to run through the same process of generating 'k' new variables; this is repeated
#'  until columns are full up. 'mix.order' then randomizes the column order destroying the
#'  relationship between column number and correlation structure, although in some cases,
#'  such relationships might be desired as representative of some real life datasets. 
#' @param nrow integer, number of rows to simulate
#' @param ncol integer, number of columns to simulate
#' @param genx the generating function for data, e.g rnorm(), runif(), etc
#' @param genr the generating function for desired correlation, e.g, runif()
#' @param k number of steps generating from the same seed before choosing a new seed
#' @param mix.order whether to randomize the column order after simulating
#' @export
#' @seealso 
#' @author Nicholas Cooper 
#' @examples
#' X <- rnorm(10,100,14)
#' cor.with(X,r=.5) # create a variable correlated .5 with X
#' cor(X,cor.with(X)$Z) # check the actual correlation
#' # some variability in the actual correlation, so run 1000 times:
#' print(mean(replicate(1000,{cor(X,cor.with(X)$Z)})))
#' cor.with(X,preserve=T) # preserve original mean and standard deviation
#' X[c(4,10)] <- NA # works fine with NAs, but new var will have same missing
#' cor.with(X,mn=50,st=2) # specify new mean and standard deviation
sim.cor <- function(nrow=100,ncol=100,genx=rnorm,genr=runif,k=3,mix.order=T) {
  ncol <- 100
  nrow <- 100
  new.mat <- matrix(numeric(ncol*nrow),nrow=nrow)
  X <- genx(nrow)
  new.mat[,1] <- X
  cnt <- 0
  for (cc in 2:ncol) {
    dd <- cor.with(X,r=genr(1))
    new.mat[,cc] <- dd
    cnt <- cnt+1
    if(cnt>=k) { X <- new.mat[,sample(cc,1)]; cnt <- 0 }
  }
  if(mix.order) {
    new.mat <- new.mat[,sample(ncol(new.mat))]
  }
  return(new.mat)
}

#' Simulate a correlated variable
#'
#' Simulate a variable correlated at level 'r' with cector x (of the same length). Can
#' either 'preserve' the mean and standard-deviation, leave standardizeed, 
#' or select new mean 'mn' and standard deviation 'st'.
#' @param x
#' @param r
#' @param preserve
#' @param mn
#' @param st
#' @export
#' @seealso 
#' @author Nicholas Cooper 
#' @examples
#' X <- rnorm(10,100,14)
#' cor.with(X,r=.5) # create a variable correlated .5 with X
#' cor(X,cor.with(X)$Z) # check the actual correlation
#' # some variability in the actual correlation, so run 1000 times:
#' print(mean(replicate(1000,{cor(X,cor.with(X)$Z)})))
#' cor.with(X,preserve=T) # preserve original mean and standard deviation
#' X[c(4,10)] <- NA # works fine with NAs, but new var will have same missing
#' cor.with(X,mn=50,st=2) # specify new mean and standard deviation
cor.with <- function(x,r=.5,preserve=F,mn=NA,st=NA) {
  # inspired by David C. Howell
  # http://www.uvm.edu/~dhowell/StatPages/More_Stuff/CorrGen.html
  X <- standardize(x)
  L <- length(X)
  y <- rnorm(L)
  a <- r/(sqrt(1-(r^2)))
  Z = a*X + y
  z <- standardize(Z)
  if(preserve) {
    mn <- mean(x,na.rm=T)
    st <- sd(x,na.rm=T)
  }
  if(preserve | (!is.na(mn) & !is.na(st))) {
    z <- (z*st)+mn
  }
  return(z)
}


#' Summarise the dimensions and type of available R example datasets
#' 
#' This function will parse the current workspace to see what R datasets
#' are available. Using the toHTML function from the tools package to interpret
#' the data() call, each dataset is examined in turn for type and dimensionality.
#' Can also use a filter for dataset types, to only show, for instance, matrix 
#' datasets. Also you can specify whether to only look for base datasets, or to
#' search for datasets in all available packages. Result is a printout to the
#' console of the available datasets and their characteristics.
#'
#' @param filter logical, whether to filter datasets by 'types'
#' @param types if filter=TRUE, which data types to include in the result
#' @param all logical, if all=TRUE, look for datasets in all available packages, else just base
#' @param ... if all is false, further arguments to the data() function to search datasets
#' @export
#' @author Nicholas Cooper 
#' @examples
#' summarise.r.datasets()
#' summarise.r.datasets(filter=T,"matrix")
## create a summary of R datasets you could use
summarise.r.datasets <- function(filter=F,types=c("data.frame","matrix"),all=F,...) { 
  # eg., package = .packages(all.available = TRUE)
  if(all) {
    ll <- unlist(strsplit((toHTML(data(package = .packages(all.available = TRUE)))),"\n"))
  } else {
    ll <- unlist(strsplit((toHTML(data(...))),"\n"))
  }
  ll <- ll[-grep("<",ll,fixed=T)]
  datasets <- ll[-grep(" ",ll,fixed=T)]
  
  for (cc in 1:length(datasets)) { 
    if(exists(datasets[cc])) {
      dd <- NULL
      try(dd <- get(datasets[cc]))
      if(is.null(dd)) { ddd <- ""; iz <- "" } else { ddd <- Dim(dd); iz <- is(dd)[1] }
      if(filter) { if(any(types %in% is(dd))) { disp <- T } else { disp <- F } } else { disp <- T }
      if(disp) {
        cat(paste(datasets[cc])," [",paste(ddd,collapse=","),"] (",iz,")\n",sep="")
      }
    }
  }
}


#for NCmisc
# internal, whether an object exists (ignoring function types)
#' Does object exist ignoring functions
#' 
#' The exists() function can tell you whether an object exists
#' at all, or whether an object exists with a certain type, but
#' it can be useful to know whether an object exists as genuine 
#' data (and not a function) which can be important when a variable
#' or object is accidently or intentionally given the same name as
#' a function. This function usually returns a logical value as to
#' the existence of the object (ignoring functions) but can also
#' be set to return the non-function type if the object exists.
#' @param x the object name to search for
#' @param ret.type logical, if TRUE then will return the objects' type (if it exists) rather
#' than TRUE or FALSE. If the object doesn't exist the empty string will be returned as the type.
#' @export
#' @author Nicholas Cooper 
#' @examples
#' x <- "test"
#' # the standard exists function, for all modes, correct mode, and other modes:
#' exists("x")
#' exists("x",mode="character")
#' exists("x",mode="numeric")
#' # standard case for a non-function variable
#' exists.not.function("x",T)
#' # compare results for a non-existent variable
#' exists("aVarNotSeen")
#' exists.not.function("aVarNotSeen")
#' # compare results for variable that is a function
#' exists("mean")
#' exists.not.function("mean")
#' # define a variable with same name as a function
#' mean <- 1.4
#' # exists.not.function returns the type of the variable ignoring the function of the same name
#' exists.not.function("mean",TRUE)
#' exists("mean",mode="function")
#' exists("mean",mode="numeric")
exists.not.function <- function(x,ret.type=F) {
  if(!is.character(x)) {
    stop("x should be the name of an object [as character type]")
  }
  other.modes <- c("logical", "integer", "list", "double", "character", "raw", "complex", "NULL")
  ex <- F; type <- ""
  for(cc in 1:length(other.modes)) {
    if(exists(x,mode=other.modes[cc])) { ex <- T ; type <- other.modes[cc] }
  }
  if(ret.type) {
    return(type)
  } else {
    return(ex)
  }
}

#' A more general 'dim()' function
#'
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
    txt <- tryCatch(system("top -l 1",intern=T), error = function(e) e)
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




#' Check whether a set of packages has been loaded
#' 
#' Returns TRUE if the whole set of packages entered has been loaded, or FALSE
#' otherwise. This can be useful when developing a package where there is optional
#' functionality depending if another package is in use (but the other package is
#' not part of 'depends' because it is not essential). Because 'require' cannot
#' be used within functions submitted as part of a CRAN package.
#' @param pcks character, a package name, or vector of names
#' @param ... further package names as character (same as entering via pcks, 
#'  but avoids need for c() in pcks)
#' @param cran.check logical, in the case at least one package is not found, whether
#'  to search CRAN and see whether the package(s) even exist on CRAN.
#' @export
#' @author Nicholas Cooper 
#' @examples
#' packages.loaded("NCmisc","reader")
#' packages.loaded(c("bigmisc","nonsenseFailTxt"))
#' packages.loaded(c("bigmisc","nonsenseFailTxt"),cran.check=F)
packages.loaded <- function(pcks,...,cran.check=T) {
  more <- c(...); if(length(more)>0) { pcks <- c(pcks,paste(more)) }
  if(!is.character(pcks)) { stop("must enter package names as character strings") }
  pt <- "package:"; pkgset <- gsub(pt,"",search()[grep(pt,search())])
  answer <- (all(pcks %in% pkgset))
  if(!answer & cran.check) {
    check.exist <- search.cran(pcks)
    for(cc in 1:length(check.exist)) {
      if(!pcks[cc] %in% check.exist[[cc]]) { cat("NB: package",pcks[cc],"is not on CRAN\n") }
    }
  }
  return(answer)
}


