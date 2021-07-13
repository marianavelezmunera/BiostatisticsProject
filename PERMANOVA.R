#This code is for the PERMANOVA, the data used was manually introduced because it was easier than transforming the Excel into the needed format#

abu<-c(364, 
       678,
       524,
       8361,
       344,
       250,
       828,
       684,
       4458,
       949,
       5685,
       1712,
       1943,
       448,
       2677,
       2138,
       1397,
       2247,
       2191,
       950,
       639,
       429,
       1688,
       1461)
abu
taxa<-c(28,
        48,
        28,
        38,
        34,
        49,
        24,
        21,
        37,
        25,
        26,
        37,
        43,
        30,
        29,
        35,
        32,
        26,
        49,
        42,
        23,
        46,
        26,
        48)
taxa
index<-c(2.2,
         2.7,
         1.6,
         1.7,
         2.5,
         2.9,
         1.7,
         1.5,
         2.1,
         1.3,
         1.4,
         3.1,
         2,
         2.1,
         1.7,
         1.8,
         2.4,
         2.2,
         2.7,
         3.4,
         1.8,
         3.1,
         2.3,
         2.6)
land<-c("cultivo",
        "bosque",
        "cultivo",
        "cultivo",
        "bosque",
        "bosque",
        "pastos",
        "cultivo",
        "bosque",
        "bosque",
        "pastos",
        "bosque",
        "cultivo",
        "cultivo",
        "pastos",
        "bosque",
        "cultivo",
        "bosque",
        "bosque",
        "pastos",
        "bosque",
        "cultivo",
        "cultivo",
        "pastos")
data<-data.frame(land,abu,taxa,index)
data

#This library had the test we needed to prove the multinormality
library(QuantPsyc)
mult.norm(data)$mult.test
data<-data.frame(x1=abu,x2=taxa,x3=index)
mult.norm(data)$mult.test
#I did two different kinds of multinormality test, this one uses 100 bootstrap replicates 
library(energy)
mvnorm.etest(data, R=100)

#To used the adonis function in the vegan package we had to convert the data frame into a distance matrix
datamatrix<-as.matrix(data)
data.root<-sqrt(datamatrix)
data.dist<-vegdist(data.root,method="bray")

#PERMANOVA test
perma<-adonis2(data.dist~land,data=data,permutations = 1000,method = "bray")
perma

#Box plot #
#For this graph we used the dataset (because we had done it before we did the PERMANOVA, but with the data of this code can be made too)
par(mfrow=c(1,3))
abunanova<-boxplot(dataset$abu_control~dataset$landuse,data = dataset,
                   main="Abundancia seg?n el uso de la tierra",xlab = "Uso de la tierra",
                   ylab="Abundancia",col=c("palevioletred1","maroon","mistyrose"))
taxanova<-boxplot(dataset$taxa_control~dataset$landuse,data = dataset,
                  main="Numero de taxa seg?n el uso de la tierra",xlab = "Uso de la tierra",
                  ylab="Numero de taxa",col=c("palevioletred1","maroon","mistyrose"),ylim=c(0,60))
shanova<-boxplot(dataset$shannon_control~dataset$landuse,data = dataset,
                 main="Indice de diversidad de Shannon seg?n el uso de la tierra",
                 xlab = "Uso de la tierra",ylab="Indice de diversidad de Shannon",
                 col=c("palevioletred1","maroon","mistyrose"))