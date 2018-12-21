% file MASS/man/cars93.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{cars93}
\alias{cars93}
\title{
  Data from 93 Cars on Sale in the USA in 1993
}
\description{
  The \code{cars93} data frame has 93 rows and 26 columns.
}
\usage{
cars93
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{Manufacturer}}{Manufacturer.}
    \item{\code{Model}}{Model.}
    \item{\code{Type}}{Type: a factor with levels \code{"Small"}, \code{"Sporty"}, \code{"Compact"}, \code{"Midsize"}, \code{"Large"} and \code{"Van"}.}
    \item{\code{MinPrice}}{Minimum Price (in \$1,000): price for a basic version.}
    \item{\code{MidPrice}}{
      Midrange Price (in \$1,000): average of \code{Min.Price} and
      \code{Max.Price}.
    }
    \item{\code{MaxPrice}}{
      Maximum Price (in \$1,000): price for \dQuote{a premium version}.
    }
    \item{\code{CityMPG}}{
      City MPG (miles per US gallon by EPA rating).
    }
    \item{\code{HighwayMPG}}{
      Highway MPG.
    }
    \item{\code{Airbags}}{
      Air Bags standard. Factor: none, driver only, or driver & passenger.
    }
    \item{\code{DriveTrain}}{
      Drive train type: rear wheel, front wheel or 4WD; (factor).
    }
    \item{\code{Cylinders}}{
      Number of cylinders (missing for Mazda RX-7, which has a rotary engine).
    }
    \item{\code{EngineSize}}{
      Engine size (litres).
    }
    \item{\code{Horsepower}}{
      Horsepower (maximum).
    }
    \item{\code{RPM}}{
      RPM (revs per minute at maximum horsepower).
    }
    \item{\code{EngineRevol}}{
      Engine revolutions per mile (in highest gear).
    }
    \item{\code{Manual}}{
      Is a manual transmission version available? (yes or no, Factor).
    }
    \item{\code{FuelCapacity}}{
      Fuel tank capacity (US gallons).
    }
    \item{\code{Passengers}}{
      Passenger capacity (persons)
    }
    \item{\code{Length}}{
      Length (inches).
    }
    \item{\code{Wheelbase}}{
      Wheelbase (inches).
    }
    \item{\code{Width}}{
      Width (inches).
    }
    \item{\code{UTurnSpace}}{
      U-turn space (feet).
    }
    \item{\code{RearSeatRoom}}{
      Rear seat room (inches) (missing for 2-seater vehicles).
    }
    \item{\code{LuggageCapacity}}{
      Luggage capacity (cubic feet) (missing for vans).
    }
    \item{\code{Weight}}{
      Weight (pounds).
    }
    \item{\code{USA}}{
      Of non-USA or USA company origins? (factor).
    }

  }
}
\details{
  Cars were selected at random from among 1993 passenger car models that
  were listed in both the \emph{Consumer Reports} issue and the
  \emph{PACE Buying Guide}.  Pickup trucks and Sport/Utility vehicles were
  eliminated due to incomplete information in the \emph{Consumer Reports}
  source.  Duplicate models (e.g., Dodge Shadow and Plymouth Sundance)
  were listed at most once.

  Further description can be found in Lock (1993).
}
\source{
  Lock, R. H. (1993)
  1993 New Car Data.
  \emph{Journal of Statistics Education}
  \bold{1}(1).
  \url{https://doi.org/10.1080/10691898.1993.11910459}.
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\keyword{datasets}
