\name{calcular_frecuencia}
\alias{calcular_frecuencia}
\alias{tabla.frec.cualitativa}
\title{
Frequency distributions for qualitative variables
}
\description{
Performs the frequency distribution for qualitative variables, nominals and/or ordinals. For ordinal variables requested quantile is calculated.
}

\usage{
calcular_frecuencia(df.nominal, ordenado.frec = FALSE,df.ordinal,
                    cuantil.p = 0.5,iprint = TRUE, ...)
}

\arguments{
  \item{df.nominal}{\code{data.frame} with factor type components (including character type) that are interpreted as nominal variables.}
  \item{ordenado.frec}{table ordered frequencies depending on their frequency (only used for nominal variables).}
  \item{df.ordinal}{\code{data.frame} with factor type components (including character type) that are interpreted as ordinal variables.}
  \item{cuantil.p}{requested quantile value (only used for ordinal variables).}
  \item{iprint}{logical value indicating whether or not to display the frequency table.}
  \item{\dots}{further arguments to be passed to or from methods.}
}


\value{\code{calcular_frecuencia} returns a list of three elements:
\item{.nominal}{a matrix containing the table of frequency distribution for nominal variables (\code{ni} = absolute frequencies and \code{fi} = relative frequencies).}
\item{.ordinal}{a matrix containing the table of frequency distribution for ordinal variables (\code{ni} = absolute frequencies, \code{fi} = relative frequencies, \code{Ni} = absolute cumulative frequency, \code{Fi} = cumulative absolute frequencies).}
\item{df.cuantil}{data frame containing the quantiles.}
}

\seealso{
\code{\link{table}} , \code{\link{cumsum}}
}
\examples{
data(cars93)
aa <- calcular_frecuencia(df.nominal=cars93["Type"], ordenado.frec=TRUE, df.ordinal=NULL, 
                          cuantil.p=0.5, iprint = TRUE)
calcular_frecuencia(df.nominal=NULL, ordenado.frec=TRUE, df.ordinal=cars93["Airbags"], 
                    cuantil.p=0.25, iprint = TRUE)
bb <- calcular_frecuencia(df.nominal=cars93["Type"], ordenado.frec=TRUE, 
                          df.ordinal=cars93["Airbags"], cuantil.p=0.25, iprint = FALSE)
str(bb)
bb
}

\encoding{LATIN1}
