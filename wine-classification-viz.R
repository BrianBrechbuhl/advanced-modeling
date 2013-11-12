library(MMST)
library(lattice)
library(ggplot2)
library(grid)
library(glmnet)
library(effects)
library(nnet)
library(car)

#Descriptive Statistics
data(wine)
summary(wine)
str(wine)
class(wine) 
dim(wine)
levels(wine$class)
nrow(wine)
ncol(wine)
head(wine)

#Scatterplots
plot(wine)
splom(wine[1:12])
plotmatrix(wine[1:5])

plot(wine$Alcohol, wine$MalicAcid)
pairs(wine[ ,-1])
plot(wine$class, wine$color)

#Scatterplot using splom, colored by class
splom(~wine[1:4], groups = class, data=wine)

#Separate scatter plots for each class
splom(~wine[1:4] | class, data=wine)

#Scatterplot using pairs
wine.colors = c("red", "green", "blue")
pairs(wine[1:10], col = wine.colors[wine$class])

#Parallel plot. Each plt is in the same dimension
parallel(~wine[,1:10]|class, data=wine, layout=c(3,1))

#Kernal Density Plots
plot(density(wine$Alcohol),  main="Kernel Density of Alcohol in Wine", col="red",)

#ggplot2 - Kernel Density of by class of wine
class.f <- factor(wine$class, labels=c("Barbera", "Barolo", "Grignolino"))

plot.alcohol <- qplot(wine$Alcohol, data=wine, geom="density", fill=class.f, alpha=I(0.5))
plot.flav <- qplot(wine$Flav, data=wine, geom="density", fill=class.f, alpha=I(0.5))
plot.color <- qplot(wine$Color, data=wine, geom="density", fill=class.f, alpha=I(0.5))
plot.proline <- qplot(wine$Proline, data=wine, geom="density", fill=class.f, alpha=I(0.5))

vplayout <- function(x,y) viewport(layout.pos.row = x, layout.pos.col = y)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2,2)))
print(plot.alcohol, vp = vplayout(1,1))
print(plot.flav, vp = vplayout(1,2))
print(plot.color, vp = vplayout(2,1))
print(plot.proline, vp = vplayout(2,2))

qplot(wine$MalicAcid, data=wine, geom="density", fill=class.f, alpha=I(0.5))
qplot(wine$Ash, data=wine, geom="density", fill=class.f, alpha=I(0.5))
qplot(wine$AlcAsh, data=wine, geom="density", fill=class.f, alpha=I(0.5))
qplot(wine$Mg, data=wine, geom="density", fill=class.f, alpha=I(0.5))
qplot(wine$Phenols, data=wine, geom="density", fill=class.f, alpha=I(0.5))
qplot(wine$NonFlavPhenols, data=wine, geom="density", fill=class.f, alpha=I(0.5))
qplot(wine$Proa, data=wine, geom="density", fill=class.f, alpha=I(0.5))
qplot(wine$Hue, data=wine, geom="density", fill=class.f, alpha=I(0.5))
qplot(wine$OD, data=wine, geom="density", fill=class.f, alpha=I(0.5))

#Multinominal Logit Model
model = multinom(class~Alcohol+MalicAcid+Ash+AlcAsh+Mg+Phenols+Flav+NonFlavPhenols+Proa+Color+Hue+OD+Proline, data=wine)

str(model)
summary(model)
step(model)

model2 <- multinom(class ~ MalicAcid + Ash + Mg + Flav  + Color + Proline, data = wine)

summary(model)
summary(model2)




