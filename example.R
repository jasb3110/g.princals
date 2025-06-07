#to test of function |prueba del paquete

setwd("D:/Pepe/2023/github/g.princals/")
source("D:/Pepe/2023/github/g.princals/g.princals.R")

library("Gifi")
#to begin

# ordinal PCA on PRINCALS
fitord <- princals(ABC,ndim = 2)# ordinal PCA

# to plot.princals, which uses native R plotting
x11();plot(fitord, plot.type = "transplot")
x11();plot(fitord, "loadplot", main = "Loadings Plot ABC Data")  ## aspect ratio = 1
x11();plot(fitord, "biplot", main = "Biplot ABC Data")
x11();plot(fitord, "screeplot")

#to use a g.princals

g.princals(fitord,
           save=0,
           show=1,
           dispersion =3,
           legend.group = 0)#show plots

g.princals(fitord,
           save=0,
           show=1,
           dispersion =3,
           colour.group = c(rep("A",5),rep("B",6)),
           legend.group = 1,
           language = "spanish")#show plots with colour group

g.princals(fitord,
           save=1,
           show=1,
           dispersion =3,
           legend.group = 0)#save plots in PNG format

g.princals(fitord,
           save=1,
           show=0,
           dispersion =3,
           colour.group = c(rep("A",5),rep("B",6)),
           legend.group = 1,
           language = "spanish")#save plots in PNG format with colour group

#to finish