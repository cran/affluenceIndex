\name{r.hc}
\alias{r.hc}
\title{
Richness headcount ratio}
\description{Computes richness headcount ratio.
}
\usage{
r.hc(x, k)
}
\arguments{
\item{x}{the income vector}
\item{k}{multiple of the median income}
}
\details{Richness headcount ratio is a proportion of the population with incomes above the affluence line
\deqn{R^{HC}(\mathbf{x},\rho)=\frac{1}{n} \sum_{i=1}^{n} \mathbf{1}_{x_{i} > \rho} = \frac{r}{n},}
where \eqn{x_i} is an income of individual \eqn{i}, \eqn{n} is the number of individuals, \eqn{\rho} is the richness line, \eqn{r} is the number of rich.
}
\value{
\item{count.rich}{the number of the rich}
\item{r.hc}{the value of index}
}
\references{
1. Brzeziñski M. (2010) Income affluence in Poland. \emph{Social Indicators Research}, 99, pp. 285-299. \cr
2. S±czewska-Piotrowska A. (2015) Identification of determinants of income richness using logistic regression model. 
\emph{Zarz±dzanie i Finanse. Journal of Management and Finance}, 4, Part 2, pp. 241-259 (in Polish).
}

\author{Alicja Wolny-Dominiak, Anna S±czewska-Piotrowska}

\examples{
data(affluence)
r <-r.hc(affluence$income,3)
print(r)
}
\keyword{ind}
