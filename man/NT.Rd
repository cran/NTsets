\name{NT}
\alias{NT}

\title{
Provide a friendly GUI including all functions and start the GUI.
}
\description{
The graphical user interface (GUI) is started by NT(). And next it is operated on the GUI. This is the interactive way.
}
\usage{
NT()
}

\value{
Return an graphical user interface.
}

\author{
Ren Wen-Long,Xiao Jing,Zhang Ya-Wen,Liang Zhi-Kai
}


\examples{
###produce random number###
x <- runif(100, 5.0, 50.0)

###Kolmogorov-Smirnor (KS)###
ks <- ks.test(jitter(x),"pnorm",mean(x),sd(x))
ks

###histogram###
hist(x)
###boxplot###
boxplot(x)
###qqnorm###
qqnorm(x)

}

