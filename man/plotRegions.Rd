\name{plotRegions}
\alias{plotRegions}
\title{
Plot regions in probability mass or density functions.
}
\description{
This function plot regions in probability mass or density functions.
}
\usage{
plotRegions(D, add = FALSE, regions = NULL, col = "gray", legend = TRUE, 
            legend.pos = "topright", to.draw.arg = 1, verticals = FALSE, ngrid = 1000, 
            cex.points = par("cex"), mfColRow = FALSE, lwd = par("lwd"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{
%%     ~~Describe \code{D} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
}
  \item{regions}{
%%     ~~Describe \code{regions} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{legend}{
%%     ~~Describe \code{legend} here~~
}
  \item{legend.pos}{
%%     ~~Describe \code{legend.pos} here~~
}
  \item{to.draw.arg}{
%%     ~~Describe \code{to.draw.arg} here~~
}
  \item{verticals}{
%%     ~~Describe \code{verticals} here~~
}
  \item{ngrid}{
%%     ~~Describe \code{ngrid} here~~
}
  \item{cex.points}{
%%     ~~Describe \code{cex.points} here~~
}
  \item{mfColRow}{
%%     ~~Describe \code{mfColRow} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\value{
invisible
}

\seealso{
\code{\link{plot}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (D, add = FALSE, regions = NULL, col = "gray", legend = TRUE, 
    legend.pos = "topright", to.draw.arg = 1, verticals = FALSE, 
    ngrid = 1000, cex.points = par("cex"), mfColRow = FALSE, 
    lwd = par("lwd"), ...) 
{
    dots <- match.call(call = sys.call(0), expand.dots = FALSE)$...
    if (!is.null(dots[["panel.first"]])) {
        pF <- .panel.mingle(dots, "panel.first")
    }
    else if (to.draw.arg == 1) {
        pF <- quote(abline(h = 0, col = "gray"))
    }
    else if (to.draw.arg == 2) {
        pF <- quote(abline(h = 0:1, col = "gray"))
    }
    else {
        pF <- NULL
    }
    dots$panel.first <- pF
    if (!add) {
        do.call(plot, c(list(D, to.draw.arg = to.draw.arg, cex.points = cex.points, 
            mfColRow = mfColRow, verticals = verticals), dots))
    }
    discrete <- is(D, "DiscreteDistribution")
    if (discrete) {
        x <- support(D)
        if (hasArg("xlim")) {
            if (length(xlim) != 2) 
                stop("Wrong length of Argument xlim")
            x <- x[(x >= xlim[1]) & (x <= xlim[2])]
        }
        if (!is.null(regions)) {
            col <- rep(col, length = length(regions))
            for (i in 1:length(regions)) {
                region <- regions[[i]]
                which.xs <- (x > region[1] & x <= region[2])
                xs <- x[which.xs]
                ps <- d(D)(x)[which.xs]
                lines(xs, ps, type = "h", col = col[i], lwd = 3 * 
                  lwd, ...)
                points(xs, ps, pch = 16, col = col[i], cex = 2 * 
                  cex.points, ...)
            }
            if (legend) {
                if (length(unique(col)) > 1) {
                  legend(legend.pos, title = if (length(regions) > 
                    1) 
                    "Regions"
                  else "Region", legend = sapply(regions, function(region) {
                    paste(round(region[1], 2), "to", round(region[2], 
                      2))
                  }), col = col, pch = 15, pt.cex = 2.5, inset = 0.02)
                }
                else {
                  legend(legend.pos, title = if (length(regions) > 
                    1) 
                    "Regions"
                  else "Region", legend = sapply(regions, function(region) {
                    paste(round(region[1], 2), "to", round(region[2], 
                      2))
                  }), inset = 0.02)
                }
            }
        }
    }
    else {
        lower0 <- getLow(D, eps = getdistrOption("TruncQuantile") * 
            2)
        upper0 <- getUp(D, eps = getdistrOption("TruncQuantile") * 
            2)
        me <- (distr::q.l(D))(1/2)
        s <- (distr::q.l(D))(3/4) - (distr::q.l(D))(1/4)
        lower1 <- me - 6 * s
        upper1 <- me + 6 * s
        lower <- max(lower0, lower1)
        upper <- min(upper0, upper1)
        dist <- upper - lower
        if (hasArg("xlim")) {
            if (length(xlim) != 2) 
                stop("Wrong length of Argument xlim")
            x <- seq(xlim[1], xlim[2], length = ngrid)
        }
        else x <- seq(from = lower - 0.1 * dist, to = upper + 
            0.1 * dist, length = ngrid)
        if (!is.null(regions)) {
            col <- rep(col, length = length(regions))
            for (i in 1:length(regions)) {
                region <- regions[[i]]
                which.xs <- (x >= region[1] & x <= region[2])
                xs <- x[which.xs]
                ps <- d(D)(x)[which.xs]
                xs <- c(xs[1], xs, xs[length(xs)])
                ps <- c(0, ps, 0)
                polygon(xs, ps, col = col[i])
            }
            if (legend) {
                if (length(unique(col)) > 1) {
                  legend(legend.pos, title = if (length(regions) > 
                    1) 
                    "Regions"
                  else "Region", legend = sapply(regions, function(region) {
                    paste(round(region[1], 2), "to", round(region[2], 
                      2))
                  }), col = col, pch = 15, pt.cex = 2.5, inset = 0.02)
                }
                else {
                  legend(legend.pos, title = if (length(regions) > 
                    1) 
                    "Regions"
                  else "Region", legend = sapply(regions, function(region) {
                    paste(round(region[1], 2), "to", round(region[2], 
                      2))
                  }), inset = 0.02)
                }
            }
        }
    }
    return(invisible(NULL))
  }
}
