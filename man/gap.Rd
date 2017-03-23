\name{gap}
\alias{gap}
\title{Affluence gap
}
\description{Computes the affluence gap.
}
\usage{
gap(x, k)
}
\arguments{
\item{x}{income vector}
\item{k}{multiple of the median income}
}

\details{Medeiros (2006) defined an affluence gap:
\deqn{R^{Med}(\mathbf{x})=\frac{1}{n} \sum_{i=1}^{n}(x_i - \rho)_{+}=\frac{1}{n} \sum_{i=1}^{n} \max \left\{x_i-\rho,0 \right\},}
where \eqn{x_i} is an income of individual \eqn{i}, \eqn{n} is the number of individuals, \eqn{\rho} is the richness line.
Medeiros' index is not standarized and is an absolute measure of richness.
}
\value{
the gap value
}
\references{
Medeiros M. (2006) The rich and the poor: the construction of an affluence line from the poverty line. \emph{Social Indicators Research}, 78, pp. 1-18.
}
\author{Alicja Wolny-Dominiak
}
\examples{
data(affluence)
gap(affluence$income,2)
}
\keyword{ind}
