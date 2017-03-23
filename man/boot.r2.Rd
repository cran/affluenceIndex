\name{boot.r2}
\alias{boot.r2}
\title{Bootstrap standard error 2
}
\description{Calculates the bootstrap standard errors.

}
\usage{
boot.r2(x, k, alpha, nsim, boot.index = c("r.cha", "r.fgt"))
}
\arguments{
\item{x}{income vector}
\item{k}{multiple of the median income}
\item{alpha}{parameter of the index: \code{alpha} > 0 for "\code{r.cha}", \code{alpha} > 1 for "\code{r.fgt}"}
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
boot1 <-boot.r2(affluence$income[1:300], 2,2,500, "r.cha")
boot1$summary

boot2 <-boot.r2(affluence$income[1:300], 2,2,500, "r.fgt")
boot2$summary
}
\keyword{indboot}
