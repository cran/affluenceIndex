\name{r.cha}
\alias{r.cha}
\title{Concave measure  of affluence
}
\description{Computes measure of affluence analogous to the poverty index of Chakravarty (1983).
}
\usage{
r.cha(x, k, beta)
}

\arguments{
\item{x}{the income vector}
\item{k}{multiple of the median income}
\item{beta}{parameter of the index: \code{beta} > 0}
}
\details{Peichl et. al (2008) defined an affluence index
\deqn{R_{\beta}^{Cha}(\mathbf{x},\rho)=\frac{1}{n} \sum_{i=1}^{n} \left( 1- \left( \frac{\rho}{x_i} \right)^{\beta} \right)_{+}, \beta >0,}
where \eqn{x_i} is an income of individual \eqn{i}, \eqn{n} is the number of individuals, \eqn{\rho} is the richness line.
Index satisfies transfer axiom \eqn{T1} (concave): a richness index should increase when a rank-preserving progressive transfer between two rich people takes place.
}
\value{
\item{r}{elements of the sum in the index formula}
\item{r.cha}{the value of index}
}
\references{
1. Chakravarty S.R. (1983) A new index of poverty. \emph{Mathematical Social Sciences}, 6, pp. 307-313. \cr
2. Peichl A., Schaefer T., Scheicher C. (2008) Measuring richness and poverty - A micro data application to Europe and Germany. IZA Discussion Paper No. 3790, Institute for the Study of Labor (IZA).
}

\author{Alicja Wolny-Dominiak}

\examples{
data(affluence)
r <-r.cha(affluence$income, 2, 2)
print(r)
}
\keyword{ind}

