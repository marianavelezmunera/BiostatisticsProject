#This code contains all the steps to perform a paired T-test#

#F test to prove homoscedasticity#

var.test(dataset$abu_control,dataset$abu_restored)
var.test(dataset$taxa_restored,dataset$taxa_control)
var.test(dataset$shannon_restored,dataset$shannon_control)

#Shapiro-Wilks test to prove normality#

shapiro.test(dataset$abu_difference)
shapiro.test(dataset$taxa_difference)
shapiro.test(dataset$shannon_difference)

#All the variables are homoscedastic and 2 out of 3 are normal, so we can perform a paired T- test, we choose a paired one because of the sampling conditions described in the article #

t.test(dataset$abu_restored,dataset$abu_control,paired = T)
t.test(dataset$taxa_restored,dataset$taxa_control,paired = T)
t.test(dataset$shannon_restored,dataset$shannon_control,paired = T)

#Box plots are useful to show graphically the data, but we needed to re-structure the data so it would be easier to graph, so we did an extra Excel called plot data #

plotdata
par(mfrow=c(1,3))
abun<-boxplot(abun~sitio_abu,data=plotdata,main="Abundancia seg?n el rio",
              ylab="Abundancia",xlab="Tipo de rio",col=c("palevioletred1","maroon"))
abun
taxa<-boxplot(taxa~sitio_taxa,data=plotdata,main="N?mero de taxa seg?n el rio",
              ylab="Numero de taxa",xlab="Tipo de rio",col=c("palevioletred1","maroon")
              ,ylim=c(0,60))
shannon<-boxplot(shannon~sitio_shannon,data=plotdata,
                 main="Indice de diversidad de Shannon seg?n el rio",
                 ylab="Indice de diversidad",xlab="Tipo de rio",
                 col=c("palevioletred1","maroon"))

