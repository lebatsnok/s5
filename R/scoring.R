scoring.key <- force

# ----------------------------------------------------------------------------
#                 score questionnaires
# ----------------------------------------------------------------------------

#' Score 
#'
#' Score
#' 
#'
#' @param D a df 
#' @param K a key
#' @param R something
#' @param Mid middle value
#' @param S sth
#' @param na.action exclude
#' @param na.tol na.tol
#' @param hofstee boolean
#' @param lims lims
#' @param average boolena
#' @return  x
#' 
#' @details   x
#'
#' @export
score <- function(D, K, R=NULL, Mid=2, S=NULL, na.action = "exclude", 
                  na.tol = 0, hofstee= FALSE, lims=c(0,4), average=FALSE){
  # D -- data (matrix or data frame) ; K -- key; R -- reflected items; 
  # Mid -- scale midpoint; S -- which scales to score
  # na.action -- handling missing data, na.tol = how many items can be missing from a scale
  if(is.null(S)) S <- unique(K)
  nosca  <- length(S)
  resu   <- matrix(NA, ncol=nosca, nrow=nrow(D))
  L <- 0
  if(hofstee) { 
    Mid<-(lims[2]+lims[1])/2
    Ext<-(lims[2]-Mid)
    D <- (D-Mid) / Ext
    Mid<-0
  }
  for(i in S){
    COLS <- which(K==i)
    SUBS <- if(!is.null(R)) ((D[,COLS]-Mid) * (rep(1, nrow(D)) %o% R[COLS]) + Mid) else D[,COLS]
    if(na.action == "Mid") {
      missing <- apply(SUBS, 1, function(x) sum(is.na(x)))
      SUBS[missing<=na.tol,][is.na(SUBS[missing<=na.tol,])] <- Mid
    }
    if(na.action == "rowmean") {
      missing <- apply(SUBS, 1, function(x) sum(is.na(x)))
      rowmeans<- rowMeans(SUBS, 1, na.rm=TRUE)
      for(i in 1:nrow(SUBS)) if(missing[i]<=na.tol && any(is.na(SUBS[i,]))) SUBS[i,][is.na(SUBS[i,])]<- rowmeans[i]
    }
    siuh <- if(average) rowMeans(SUBS) else rowSums(SUBS)   
    L <- L+1
    resu[,L] <- siuh
  }
  colnames(resu) <- S
  resu
}

#' Score2
#'
#' Score2
#' 
#'
#' @param D df 
#' @param test test
#' @param Mid mid
#' @param lims lims
#' @param na.action na.action
#' @param na.tol1 na.tol1
#' @param na.tol2 na.tol2
#' @param what what
#' @param override.mid sth
#' @param ... dot dot dot
#' @return x
#' 
#' @details   x
#'
#' @export
score2 <- function(D, test="NEO EE", Mid = 2, lims = NULL, 
                   na.action="exclude", 
                   na.tol1=1, na.tol2=3, what="DF", override.mid = FALSE, ...) {
  kii <- if(is.list(test)) test else scoring.key(test)
  key <- kii$key
  key2 <- kii$key2
  if(is.null(key2)) key2 <- substr(key, 1,1)
  rev <- kii$rev
  subscales <- kii$subscales
  domains   <- kii$domains
  if(!is.null(kii$Mid) && !override.mid) Mid <- kii$Mid
  if(!is.null(kii$lims) && !override.mid) Lims <- kii$lims else Lims <- c(0,4)
  if(what!="D") foo <-  score(D, K = key,  R = rev, Mid = Mid, S = subscales, na.action=na.action, na.tol=na.tol1, lims = Lims, ...) else foo<- NULL
  if(what!="F") foo2<-  score(D, K = key2, R = rev, Mid = Mid, S = domains, na.action=na.action, na.tol=na.tol2, lims = Lims, ...) else foo2 <- NULL
  resu <- cbind(foo, foo2) 
  resu
}

#' cronbach
#'
#' cronbach
#' 
#'
#' @param v1 v1
#' @return x
#' 
#' @details   x
#'
#' @export
cronbach <- function (v1) 
{
  v1 <- na.omit(v1)
  nv1 <- ncol(v1)
  pv1 <- nrow(v1)
  alpha <- (nv1/(nv1 - 1)) * (1 - sum(apply(v1, 2, var))/var(apply(v1, 
                                                                   1, sum)))
  resu <- list("sample size" = pv1, number.of.items = nv1, alpha = alpha)
  resu
}

#' cronbach2
#'
#' cronbach2
#' 
#'
#' @param D data set
#' @param Key scoring key
#' @param Mid scale middle point
#' @param what with hierarchical scoring keys, use "D" to compute 'domain' scores , "F" for 'facet' scores,  and "DF" for both.  
#' @param format 'm' or 's'
#' @param print.all FALSE
#' @return x
#' 
#' @details   x
#'
#' @export
cronbach2 <- function (D, Key = scoring.key("NEO EE"), Mid=2, 
                       what="DF", format="m", print.all=FALSE) {
  # format = "s" trykib N-id ja K-d lyhidalt, tulemuseks ainult alfad
  if(!is.list(Key)) Key <- scoring.key(Key)
  K <- Key$key
  R <- Key$rev
  domains <- Key$domains
  S <- Key$subscales
  if(is.null(S)) S <- unique(K)
  cronbach3 <- function(D, K, S, R) {
    nosca  <- length(S)
    resu   <- matrix(NA, nrow=nosca, ncol=3)
    L <- 0
    for(i in S){
      COLS <- which(K==i)
      SUBS <- if(!is.null(R)) ((D[,COLS]-Mid) * (rep(1, nrow(D)) %o% R[COLS]) + Mid) else D[,COLS]
      siuh <- cronbach(SUBS)
      if(print.all && ncol(SUBS)>2){ 
        cat("\n-----\nDetailed results for subscale ", i, "\n")
        print(as.data.frame(siuh))
        cat("\n")
        fuuu <- NULL
        for(ii in 1:ncol(SUBS)) {
          fuuu <- rbind(fuuu, as.data.frame(cronbach(SUBS[,-ii])))
        }
        IT <- data.frame(ITN =which(K==i))
        if(!is.null(Key$items)) IT <- cbind(IT, Item.text=Key$items[which(K==i)])
        fuuu<- cbind(IT, fuuu)
        
        print(fuuu)
      }
      L <- L+1
      resu[L,] <- unlist(siuh)
    }
    rownames(resu) <- S
    colnames(resu) <- c("N", "K", "alfa")
    resu
  }
  if(what!="D") resu <- cronbach3(D, K, S, R)
  if(!is.null(domains)) resu2 <- cronbach3(D, Key$key2, domains, R)
  if(what=="DF") resu <- rbind(resu,resu2) else if(what=="D") resu<-resu2
  if(format=="s") {
    cat("N=", min(resu[,1]), " to ", max(resu[,1]), "\n")
    cat("K=", min(resu[,2]), " to ", max(resu[,2]), "\n")
    cat("-----\n")
    resu <- resu[,3]
  }
  resu
}


# --------------------------------------------------------------------------
#                        social desirability
# --------------------------------------------------------------------------

#' sdi
#'
#' sdi
#' 
#'
#' @param andmed x
#' @param soso x
#' @param hofstee x
#' @param lims x
#' @return x
#' 
#' @details   x
#'
#' @export
sdi <- function (andmed, soso, hofstee = FALSE, lims = c(0, 4, 1, 7)) {
  if (hofstee) {
    anc <- (lims[1] + lims[2])/2
    soc <- (lims[3] + lims[4])/2
    ans <- (lims[2] - anc)
    sos <- (lims[4] - soc)
    andmed <- (andmed - anc)/ans
    soso <- (soso - soc)/sos
  }
  foo <- sweep(andmed, 2, soso, "*")
  N <- dim(andmed)[1]
  resu <- rowMeans(foo, na.rm = T)
  resu
}

#' sdi2
#'
#' sdi2
#' 
#'
#' @param D x
#' @param key x
#' @param na.tol x
#' @param hofstee x
#' @param lims x
#' @return x
#' 
#' @details   x
#'
#' @export
sdi2<- function (D, key="NEO EE", na.tol = 10, hofstee=TRUE, 
                 lims=c(0,4,1,7)) {
  if(!is.list(key)) key <- scoring.key(key)
  sdsv <- key$sdsv
  K    <- key$key
  T    <- key$test
  if(T=="NEO EE") sdsv[K=="SD"] <- NA
  resu <- sdi(D, sdsv, hofstee=hofstee, lims = lims)
  nass <- apply(D, 1, function(x) sum(is.na(x)))
  resu[nass>na.tol] <- NA
  resu
}


