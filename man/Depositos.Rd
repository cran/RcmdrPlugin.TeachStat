\name{Depositos}
\alias{Depositos}
\docType{data}
\title{
Deposits with credit institutions in Ourense
}
\description{
Private sector deposits (in millions of euro) with credit institutions in the province of Ourense (Spain) in 2002-2018.
}
\usage{data("Depositos")}
\format{
  A data frame with 17 observations on the following 4 variables.
  \describe{
    \item{\code{year}}{a factor, year}
    \item{\code{quantity}}{a numeric vector, deposit (in millions of euro)  with credit institutions}
    \item{\code{E_IPC_2016}}{a numeric vector, Consumer Price Index (CPI) with base 2016 in Spain}
    \item{\code{G_IPC_2016}}{a numeric vector, Consumer Price Index (CPI) with base 2016 in Galicia}
  }
}

\source{
Galician Institute of Statistics (2019):

- \url{https://www.ige.gal/igebdt/esqv.jsp?ruta=verTabla.jsp?OP=1&B=1&M=&COD=462&R=2\%5B2002:2003:2004:2005:2006:2007:2008:2009:2010:2011:2012:2013:2014:2015:2016:2017:2018\%5D&C=9928\%5B32\%5D;0\%5B3\%5D;1\%5B3\%5D&F=&S=&SCF=#}

- \url{https://www.ige.gal/igebdt/esqv.jsp?ruta=verTabla.jsp?OP=1&B=1&M=&COD=8547&R=0\%5Ball\%5D&C=2\%5B0\%5D;1\%5B0\%5D;9928\%5B108:12\%5D&F=&S=&SCF=}
}

\examples{
data(Depositos)

.Sindex <- Sindex(Depositos, "year", "quantity", "2010")*100
print(.Sindex)

Deflat(Depositos, "year", "quantity", "E_IPC_2016", "2011")
}
\keyword{datasets}
