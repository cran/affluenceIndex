\name{r.fgt}
\alias{r.fgt}
\title{Convex measure of affluence
}
\description{Computes measure of affluence analogous to the convex version of Foster, Greer and Thorbecke (1984) family of poverty indices.
}
\usage{
r.fgt(x, k, alpha)
}
\arguments{
\item{x}{the income vector}
\item{k}{multiple of the median income}
\item{alpha}{parameter of the index: \code{alpha} > 1}
}
\details{Peichl et. al (2008) defined an affluence index
\deqn{R_{\alpha}^{FGT,T2}(\mathbf{x},\rho)=\frac{1}{n} \sum_{i=1}^{n} \left( \left(\frac{x_i - \rho}{\rho} \right)_{+} \right)^{\alpha},\alpha>1,}
where \eqn{x_i} is an income of individual \eqn{i}, \eqn{n} is the number of individuals, \eqn{\rho} is the richness line.
Index satisfies transfer axiom \eqn{T2} (convex): a richness index should decrease when a rank-preserving progressive transfer between two rich people takes place.
}
\value{
\item{r}{values of the sum in the index formula}
\item{r.fgt}{the value of index}
}
\references{
1. Foster J.E., Greer J., Thorbecke E. (1984) A class of decomposable poverty measures. \emph{Econometrica}, 52, pp. 761-766. \cr
2. Peichl A., Schaefer T., Scheicher C. (2008) Measuring richness and poverty - A micro data application to Europe and Germany. IZA Discussion Paper No. 3790, Institute for the Study of Labor (IZA).
}
\author{Alicja Wolny-Dominiak}


\examples{
data(affluence)
r.fgt(affluence$income, 2, 1)$r.fgt
}
\keyword{ind}
