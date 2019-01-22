library(mvtnorm)
library(copula)
library(scatterplot3d)
library(clusterGeneration)
library(tidyverse)
library(rgl)
library(matlib)
library(latex2exp)
library(tikzDevice)
options(tikzLatexPackages=c(getOption("tikzLatexPackages"),"\\usepackage{amsfonts}","\\usepackage{bm}"),
        tikzFooter = "\\caption{a caption}")

set.seed(1989)

Sigma <- matrix(c(7, 3.5,
                  3.5, 5), nrow = 2, ncol = 2, byrow = T)

X <- rmvnorm(50, c(-2, 0), sigma = Sigma) %>% as.data.frame()
X[,3] <- X[,1] + X[,2]

colnames(X) <- c("X1", "X2", "X3")


s3d <- scatterplot3d(X, color = "red",
                     angle=55, pch = 16, grid=TRUE, box=FALSE)

# Add regression plane
my.lm <- lm(X$X3 ~ X$X1 + X$X2)
s3d$plane3d(my.lm, draw_lines = T, draw_polygon = T)
arrows(-5,-6, -2.5, 0.5, lwd=3)
arrows(-5.5,-3.9, 0, -1.5, lwd=3)
text(-3, 0.5, TeX('$\\textbf{S}_1$'),col = 1, adj = c(-.1, -.1))
text(0, -2.35, TeX('$\\textbf{S}_2$'),col = 1, adj = c(-.1, -.1))
text(0, 5, TeX('$\\textbf{S}$'),col = 1, adj = c(-.1, -.1))


