data('iris')
head(iris)
irisdata1 = iris[,1:4]
irisdata1
principal_components = princomp(irisdata1, cor=TRUE, score=TRUE)
summary(principal_components)
plot(principal_components)
plot(principal_components, type='l')
biplot(principal_components)


install.packages('MASS')
data(Boston, package='MASS')
pca_out = prcomp(Boston, scale=T)
pca_out
plot(pca_out)
biplot(pca_out, scale=0)
boston_pc = pca_out$x
boston_pc
head(boston_pc)
summary(boston_pc)

