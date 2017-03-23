\name{r.is}
\alias{r.is}
\title{Income share of the top p \%
}
\description{Computes income share of the top p \%.
}
\usage{
r.is(x, p)
}

\arguments{
\item{x}{the vector of income}
\item{p}{the order of quantile. Must be in [0,1] as probability}
}
\details{The most popular measure of richness which takes a form:
\deqn{R^{IS}(\mathbf{x},p)=\frac{\sum_{i=1}^n x_i \mathbf{1}_{x_i>q_{1-p}}}{\sum_{i=1}^n x_i},}
where \eqn{q_{1-p}} is the \eqn{(1-p)} quantile of the population and \eqn{\mathbf{1}} denotes the indicator function, which is equal to 1 when its argument is true and 0 otherwise.
There is always \eqn{p} \% of rich units in the population.
}
\value{the value of index
}
\references{
Brzeziñski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299.
}

\author{Alicja Wolny-Dominiak, Anna S±czewska-Piotrowska}

\examples{
data(affluence)
r <-r.is(affluence$income,0.9)
print(r)
}
\keyword{ind}
