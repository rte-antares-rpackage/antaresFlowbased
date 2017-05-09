#' @title Create univers to test error
#'
#' @description  Create univers to test error
#' @param nb \code{numeric} number of points
#' @param bInf \code{numeric} minimum
#' @param bSup \code{numeric} maximum
#' @param seed \code{numeric} a single value, interpreted as an integer, or NULL.
#'
#' @return \code{matrix}
#'
#' @noRd
#'
.univ <- function(nb, bInf, bSup, seed = 123456789)
{
  if(!is.null(seed)){
    set.seed(seed)
  }

  Inuv <- data.table(BE = runif(nb, bInf, bSup),
                     DE = runif(nb, bInf, bSup),
                     FR = runif(nb, bInf, bSup))
  Inuv[,NL := -BE-DE-FR]
  Inuv <- as.matrix(Inuv)
  Inuv
}

#' @title Compute error
#'
#' @description  Compute error inf and sup
#' @param FY \code{data.frame} 4 columns :
#' \itemize{
#'  \item 1 : BE
#'  \item 2 : DE
#'  \item 3 : FR
#'  \item 4 : bSol
#' }
#' @param PTDF \code{data.frame} 5 columns :
#' \itemize{
#'  \item 1 : BE
#'  \item 2 : DE
#'  \item 3 : FR
#'  \item 4 : NL
#'  \item 5 : RAM_0
#' }
#' @param univ \code{matrix} generate which .univ
#'
#' @return \code{data.frame} error inf and error sup
#'
#' @noRd
.giveError <- function(FY, PTDF, univ)
{
  constrainmat <- PTDF[, .SD, .SDcols = c("BE", "DE", "FR", "NL")]%>>%as.matrix
  FYMat <- as.matrix(FY[, .SD, .SDcols = 1:3])
  points1 <- univ %*% t(constrainmat)
  indomaine1 <- which(apply(points1, 1, function(X, Y){all(X<Y)}, Y = PTDF$RAM_0))
  points2 <- univ[, 1:3] %*% t(FYMat)
  indomaine2 <- which(apply(points2, 1, function(X, Y){all(X<Y)}, Y = FY$bSol))
  error1 <- (1-length(intersect(indomaine1, indomaine2))/length(indomaine1))*100
  error2 <- (1-length(intersect(indomaine1, indomaine2))/length(indomaine2))*100
  data.frame(error1, error2)
}
