\name{boot.r1}
\alias{boot.r1}
\title{Bootstrap standard error 1
}
\description{Calculates the bootstrap standard errors.

}
\usage{
boot.r1(x, k, nsim, boot.index = c("r.hc", "r.is"))
}
\arguments{
\item{x}{income vector}
\item{k}{multiple of the median income}
\item{nsim}{the number of replications}
\item{boot.index}{the index for which the error is estimated}

}
\details{The function uses quantile method of calculating bootstrap confidence intervals.
}
\value{
\item{se.r}{the bootstrap error}
\item{summary}{bootstrap summary}
}
\references{
Brzeziñski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299.}
\author{Alicja Wolny-Dominiak}

\examples{
data(affluence)
boot3 <-boot.r1(affluence$income[1:300], 0.9,500, "r.is")
boot3$summary

boot4 <-boot.r1(affluence$income[1:300], 2, 500, "r.hc")
boot4$summary
}
\keyword{indboot}
