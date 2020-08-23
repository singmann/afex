#'  Expected values of mean squares for factorial designs
#'  
#'  Implements the Cornfield-Tukey algorithm for deriving the expected values of the mean squares for factorial designs.
#'  
#' @param design A \code{formula} object specifying the factors in the design (except residual error, which is always implicitly included). The left hand side of the \code{~} is the symbol that will be used to denote the number of replications per lowest-level factor combination (I usually use "r" or "n"). The right hand side should include all fixed and random factors separated by \code{*}. Factor names should be single letters.
#' @param nested A \code{character} vector, where each element is of the form \code{"A/B"}, indicating that the levels of factor B are nested under the levels of factor A.
#' @param random A \code{character} string indicating, without spaces or any separating characters, which of the factors specified in the design are random.
#' 
#' @return The returned value is a formatted table where the rows represent the mean squares, the columns represent the variance components that comprise the various mean squares, and the entries in each cell represent the terms that are multiplied and summed to form the expectation of the mean square for that row. Each term is either the lower-case version of one of the experimental factors, which indicates the number of levels for that factor, or a "1", which means the variance component for that column is contributes to the mean square but is not multiplied by anything else.
#' 
#' @note Names for factors or parameters should only be of length 1 as they are simply concatenated in the returned table.
#' 
#' @author Jake Westfall
#' 
#' @seealso A detailed description with explanation of the example can be found \href{http://www.talkstats.com/threads/share-your-functions-code.18603/page-9#post-82050}{elsewhere} (note that the \code{design} argument of the function described at the link behaves slightly different).
#' 
#' Example applications of this function can be found here: \url{https://stats.stackexchange.com/a/122662/442}.
#' 
#' 
#' @example examples/examples.ems.R
#' @export


ems <- function(design, nested=NULL, random=""){
  # modify design formula based on nested factors specified
  if(!is.null(nested)){
    terms <- attr(terms(design), "term.labels")
    # for each nested, get indices of all terms not involving their interaction 
    keeps <- lapply(strsplit(nested, "/"), function(x){
      which(apply(sapply(x, grepl, terms), 1, function(x) !all(x)))
    })
    terms <- terms[Reduce(intersect, keeps)]
    formula <- paste(c(as.character(design)[2:1], paste(terms, collapse="+")), collapse="")
    design <- eval(parse(text=formula))
  }
  
  # build two-way table
  mat <- t(attr(terms(design), "factors"))
  terms <- tolower(as.character(attr(terms(design), "variables"))[-1])
  
  # resolve fixed/random dummies
  if (!is.null(random)){
    random <- unlist(strsplit(random,split=""))
    mat[,which(colnames(mat) %in% random)][mat[,
                                               which(colnames(mat) %in% random)]==1] <- ""
    mat[,which(!colnames(mat) %in% random)][mat[,
                                                which(!colnames(mat) %in% random)]==1] <- "fix"
  }
  
  # insert 1 in nested rows
  subs <- strsplit(rownames(mat), split=":")
  if(!is.null(nested)){
    nested <- strsplit(nested, split="/")
    for(term in nested){
      rows <- unlist(lapply(subs, function(x) term[2] %in% x))
      cols <- colnames(mat)==term[1]
      mat[rows,cols] <- "1"
    }
  }
  mat <- rbind(mat, e=rep("1", ncol(mat)))
  
  # insert numbers of levels for remaining cells
  for(row in seq(nrow(mat))){
    mat[row,][mat[row,]=="0"] <- tolower(colnames(mat)[mat[row,]=="0"])
  }
  
  # construct EMS table
  ems <- matrix(nrow=nrow(mat), ncol=nrow(mat),
                dimnames=list(Effect=rownames(mat),
                              VarianceComponent=rev(rownames(mat))))
  # add nesting information to subscripts
  if (!is.null(nested)){
    subs <- lapply(subs, function(x){
      new <- x
      for (nest in seq(length(nested))){
        if (nested[[nest]][2] %in% x) new <- c(new, nested[[nest]][1])
      }
      return(new)
    })
  }
  subs[["e"]] <- colnames(mat)[-1]
  names(subs) <- rownames(mat)
  # rename #-of-reps variable to 'e' invisibly
  colnames(mat)[1] <- "e"
  # fill in EMS table
  for(effect in rownames(ems)){
    for(varcomp in colnames(ems)){
      effectVec <- unlist(strsplit(effect, ":"))
      ans <- mat[varcomp,-1*which(colnames(mat) %in% effectVec)]
      if ("fix" %in% ans) ans <- ""
      if (all(ans=="1")) ans <- "1"
      if (("1" %in% ans | "2" %in% ans) & !all(ans=="1")){
        ans <- ans[!ans %in% c("1","2")]
      }
      varcompVec <- unlist(strsplit(varcomp, ":"))
      if (!all(effectVec %in% subs[[varcomp]])) ans <- ""
      if (effect=="e" & varcomp=="e") ans <- "1"
      ems[effect,varcomp] <- paste(ans, collapse="")
    }
  }
  attr(ems, "terms") <- terms
  return(noquote(ems))
}




