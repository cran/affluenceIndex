\name{polar.aff}
\alias{polar.aff}
\title{Polarization index
}
\description{Computes the Wolfson polarization index.
}
\usage{
polar.aff(x)
}
\arguments{
\item{x}{the income vector}
}
\details{
Standard inequality measures do not give any information about polarization. A more polarized income distribution 
is one that has relatively fewer middle income class and more low- and/or high-income households (Alichi et al. 2016). Low income class is
very often identified with poverty and high-income class with richness. One of the measures of polarization is the Wolfson polarization index 
given by (Wolfson 1994)
\deqn{P= \left(T-\frac{G}{2} \right) \frac{\mu}{Me},}
where \eqn{T} is the difference between 0.5 and the income share of bottom half of the population, \eqn{G} is the Gini coefficient,
\eqn{\mu} is the mean income, \eqn{Me} is the median income. \cr
In order to have index from \eqn{ \langle 0,1 \rangle} interval, Wolfson defined the scalar polarization index:
\deqn{P^* = 2 \left( 2T-G \right) \frac{\mu}{Me}.}
}

\value{
\item{gini}{the Gini coefficient}
\item{p}{the Wolfson polarization index}
\item{p.scalar}{the Wolfson scalar polarization index}
\item{T}{the difference between 0.5 and the income share of bottom half of the population}
}


\references{
1. Alichi A., Kantenga K., Solé J. (2016) Income polarization in the United States. IMF Working Paper, WP/16/121.\cr
2. Wolfson M.C. (1994) When inequalities diverge, \emph{The American Economic Review}, 84, pp. 353-358.
}
\author{Alicja Wolny-Dominiak, Anna S±czewska-Piotrowska
}
\examples{
data(affluence)
polar.aff(affluence$income)
}
\keyword{polar}
