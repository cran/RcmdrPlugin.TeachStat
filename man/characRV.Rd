\name{characRV}
\alias{characRV}
\encoding{UTF-8}

\title{
Characteristics of Random Variables.
}
\description{
This function computes the main characteristics of a random variable (expectation, median, standard deviation, \ldots)
}
\usage{
characRV(D, charact = c("expectation", "median", "sd", "IQR", "skewness", "kurtosis", 
         "moment", "cmoment"), moment = 1, cmoment = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{
An object of the class \code{Distribution}.
}
  \item{charact}{
any of "\code{expectation}", "\code{median}", "\code{sd}", "\code{IQR}", "\code{skewness}", "\code{kurtosis}", "\code{moment}", "\code{cmoment}"
}
  \item{moment}{
an integer indicating the moment with respect to the origin to be calculated.
}
  \item{cmoment}{
an integer indicating the moment with respect to the expectation to be calculated.
}
}

\value{
`characRV` returns a table containing the selected characteristics.
}

\seealso{
\code{\link[distrEx]{E}}, \code{\link[distrEx]{median}}, \code{\link[distrEx]{sd}}, \code{\link[distrEx]{IQR}}, 
\code{\link[distrEx]{skewness}}, \code{\link[distrEx]{kurtosis}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (D, charact = c("expectation", "median", "sd", "IQR", 
    "skewness", "kurtosis", "moment", "cmoment"), moment = 1, 
    cmoment = 2) 
{
    if (missing(charact)) 
        charact <- c("expectation", "sd")
    charact <- match.arg(charact, c("expectation", "median", 
        "sd", "IQR", "skewness", "kurtosis", "moment", "cmoment"), 
        several.ok = TRUE)
    moment <- if ("moment" \%in\% charact) 
        moment
    else NULL
    cmoment <- if ("cmoment" \%in\% charact) 
        cmoment
    else NULL
    mom <- if (!is.null(moment)) 
        paste("alpha_", moment, sep = "")
    else NULL
    cmom <- if (!is.null(cmoment)) 
        paste("mu_", cmoment, sep = "")
    else NULL
    chars <- c(c("expectation", "median", "sd", "IQR", "skewness", 
        "kurtosis")[c("expectation", "median", "sd", "IQR", "skewness", 
        "kurtosis") \%in\% charact], mom, cmom)
    nchars <- length(chars)
    table <- matrix(0, 1, nchars)
    rownames(table) <- gsub("[[:space:]]", "", deparse(substitute(D)))
    colnames(table) <- chars
    if ("expectation" \%in\% chars) 
        table[, "expectation"] <- distrEx::E(D)
    if ("median" \%in\% chars) 
        table[, "median"] <- distrEx::median(D)
    if ("sd" \%in\% chars) 
        table[, "sd"] <- distrEx::sd(D)
    if ("IQR" \%in\% chars) 
        table[, "IQR"] <- distrEx::IQR(D)
    if ("skewness" \%in\% chars) 
        table[, "skewness"] <- distrEx::skewness(D)
    if ("kurtosis" \%in\% chars) 
        table[, "kurtosis"] <- distrEx::kurtosis(D)
    if ("moment" \%in\% charact) 
        table[, mom] <- distrEx::E(D, fun = function(x) {
            x^moment
        })
    if ("cmoment" \%in\% charact) 
        table[, cmom] <- distrEx::E(D, fun = function(x) {
            (x - distrEx::E(D))^cmoment
        })
    print(table)
    return(invisible(table))
  }
}
