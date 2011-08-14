\name{anaglyph.plot}
\alias{anaglyph.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a 3D Anaglyph Scatterplot
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Serves as a wrapper for the \code{\link{plot}} function to produce a 3D stereogram anaglyph plot.
}
\usage{
anaglyph.plot(x, y, z, left = "red", right = "cyan", depth = "med", style = "pop-out", type = "p", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{ the \code{x} and \code{y} coordinates for the plot. }
  \item{z}{ the \code{z} coordinate for the plot to indicate depth. }
  \item{left}{ color of left lens. }
  \item{right}{ color of right lens. }
  \item{depth}{ "low","med","high" indicating perceived depth of points. }
  \item{style}{ "pop-in","both","pop-out" indicating whether the 3D effect should pop-out of the page, pop-in, or both. }
  \item{type}{ type of scatterplot, same as \code{plot} function: "p" for points, "l" for line etc. }
  \item{\dots}{ other arguments to be passed into \code{plot.default} }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Jonathan Lee
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
attach(mtcars)
anaglyph.plot(wt,mpg,disp)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~hplot }
\keyword{ ~color }
