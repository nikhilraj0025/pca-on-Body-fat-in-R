BFT<-read.csv("C:/Users/AKHIL/Desktop/New folder/bodyfat.csv")
View(BFT)
mydata1<-BFT[-1]
mydata1<-scale(mydata1)
mypca1<-prcomp(mydata1)
summary(mypca1)



plot(mypca1,type="l",col=c("blue"))
print(mypca1$rotation)
cor(mypca1$x)


mydata_pca1<-cbind(BFT[,1],as.data.frame(mypca1$x))
mydata_pca1


lm_pca_BFT<-lm(BFT[,1]~PC1+PC2+PC3+PC4+PC5+PC6,data=mydata_pca1)
vif(lm_pca_BFT)
lm_pca_BFT
summary(lm_pca_BFT)
pre_BFT<-predict(lm_pca_BFT,mydata_pca1)
gg<-data.frame(pre_BFT,mydata_pca1$`BFT[, 1]`)
gg<-mutate(gg,e1=(pre_BFT-mydata_pca1$`BFT[, 1]`)^2)
mb<-mean(gg$e1)
rms<-sqrt(mb)
rms
library(car)
