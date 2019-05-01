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

X <- matrix(runif(40),20,2) %>% as.data.frame()
X[,3] <- X[,1] + X[,2]

colnames(X) <- c(expression('$y_A$'), 
                 expression('$y_B$'), 
                 expression('$y_{Tot}$'))

tikz('Schematic_3D.tex',standAlone = FALSE)

s3d <- scatterplot3d(X, color = "red",
                     angle=55, pch = 16, grid=TRUE, box=FALSE,cex.lab = 2)


# Add regression plane
s3d$plane3d(0,1,1, draw_lines = T, draw_polygon = T)

#
#s1pro<-s3d$xyz.convert(1,1,0)
s1 <- s3d$xyz.convert(1,0,1)
s2 <- s3d$xyz.convert(0,1,1)
arrows(0,0,s1$x,s1$y,lwd = 3)
arrows(0,0,s2$x,s2$y,lwd = 3)
# text(s1$x,s1$y, TeX('$\\textbf{s}_1$}'),col = 1, adj = c(-.1, -.1),cex = 2)
# text(s2$x,s2$y, TeX('$\\textbf{s}_2$}'),col = 1, adj = c(-.1, -.1),cex = 2)
text(3, 3, "{\\Huge $\\mathfrak{s}$}",col = 1, adj = c(-.1, -.1))
dev.off()

# Compile the tex file
tools::texi2dvi('Schematic_3D.tex',pdf=T)