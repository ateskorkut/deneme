library(jpeg)
img<-readJPEG("~/Desktop/sampleimage.jpg")
str(img)

plot(NA,xlim=c(0,nrow(img)),ylim=c(0,ncol(img)))
rasterImage(img,0,0,nrow(img),ncol(img))

par(mfrow=c(2,2))
plot(1:512, type='n')
rasterImage(img, 0,0,nrow(img),ncol(img))
image(1:512,1:512,img[,,1])
image(1:512,1:512,img[,,2])
image(1:512,1:512,img[,,3])

avr1<-img[,1,1]
for(t in 1:512){
  avr1[t]<-(sum(img[,t,1]))/512
}

avr2<-img[,1,2]
for(t in 1:512){
  avr2[t]<-(sum(img[,t,2]))/512
}

avr3<-img[,1,3]
for(t in 1:512){
  avr3[t]<-(sum(img[,t,3]))/512
}

par(mfrow=c(2,2))
plot(avr1, type = "l")
plot(avr2, type = "l")
plot(avr3, type = "l")

img2<-img
for(m in 1:512){
  for(n in 1:256){
    if((img2[m,n,1]-img2[m,n+256,1])>0){
      img2[m,n,1]=img2[m,n,1]-img2[m,n+256,1]
    }
    else{
      img2[m,n,1]=0
    }
  }
}

par(mfrow=c(1,1))
plot(1:2, type='n')
rasterImage(img2, 1, 1, 2, 2)

arr1 <- rep(0, 25)
arr2<- rep(0, 25)
arr3<- rep(0, 25)

t<-img
counter=1
for(m in 1:508){
  for(n in 1:508){
    for(k in 0:4){
      for(l in 0:4){
        arr1[counter]=img[m+k,n+l,1]
        arr2[counter]=img[m+k,n+l,2]
        arr3[counter]=img[m+k,n+l,3]
        counter=counter+1
      }
    }
    counter=1
    t[m+2,n+2,1]=median(arr1)
    t[m+2,n+2,2]=median(arr2)
    t[m+2,n+2,3]=median(arr3)
  }
}

plot(1:2, type='n')
rasterImage(t, 1, 1, 2, 2)

plot(1:2, type='n')
rasterImage(img, 1, 1, 2, 2)

arR1 <- rep(0, 121)
arR2<- rep(0, 121)
arR3<- rep(0, 121)
u<-img
counter=1
for(m in 1:502){
  for(n in 1:502){
    for(k in 0:10){
      for(l in 0:10){
        arR1[counter]=img[m+k,n+l,1]
        arR2[counter]=img[m+k,n+l,2]
        arR3[counter]=img[m+k,n+l,3]
        counter=counter+1
      }
    }
    counter=1
    u[m+5,n+5,1]=median(arR1)
    u[m+5,n+5,2]=median(arR2)
    u[m+5,n+5,3]=median(arR3)
  }
}

plot(1:2, type='n')
rasterImage(u, 1, 1, 2, 2)

plot(1:2, type='n')
rasterImage(img, 1, 1, 2, 2)

aRR1 <- rep(0, 961)
aRR2<- rep(0, 961)
aRR3<- rep(0, 961)
v<-img
counter=1
for(m in 1:482){
  for(n in 1:482){
    for(k in 0:30){
      for(l in 0:30){
        aRR1[counter]=img[m+k,n+l,1]
        aRR2[counter]=img[m+k,n+l,2]
        aRR3[counter]=img[m+k,n+l,3]
        counter=counter+1
      }
    }
    counter=1
    v[m+15,n+15,1]=median(aRR1)
    v[m+15,n+15,2]=median(aRR2)
    v[m+15,n+15,3]=median(aRR3)
  }
}

plot(1:2, type='n')
rasterImage(v, 1, 1, 2, 2)

plot(1:2, type='n')
rasterImage(img, 1, 1, 2, 2)

greyimg<-readJPEG("~/Desktop/greyimg.jpg")
str(greyimg)
hist(greyimg)

mean(greyimg)
mu<-mean(greyimg)
sigma<-sd(greyimg)

upperbound<-sum(qnorm(0.9995)*sigma,mu)
upperbound
lowerbound<-sum(qnorm(0.0005)*sigma,mu)
lowerbound

greyimg2<-greyimg
greyimg2[greyimg2<lowerbound]=0
greyimg2[greyimg2>upperbound]=0

plot(1:2, type='n')
rasterImage(greyimg2, 1, 1, 2, 2)

plot(1:2, type='n')
rasterImage(greyimg, 1, 1, 2, 2)

plot(1:2, type='n')
rasterImage(greyimg, 1, 1, 2, 2)

greyimg3 <-greyimg
for(i in 1:10){
  for( j in 1:10){
    a=mean(greyimg[(51*(i-1)+1):(51*(i-1)+51),(51*(j-1)+1):(51*(j-1)+51),1]) 
    standarddev<-sd(greyimg[(51*(i-1)+1):(51*(i-1)+51),(51*(j-1)+1):(51*(j-1)+51),1])
    Lowerbound<-qnorm(0.0005,a,standarddev)
    Upperbound<-qnorm(0.9995,a,standarddev)
    
    for(k in (51*(i-1)+1):(51*(i-1)+51)){
      for(l in (51*(j-1)+1):(51*(j-1)+51)){
        if(greyimg[k,l,1]<Lowerbound|greyimg[k,l,1]>Upperbound){
          greyimg3[k,l,1]=0
        }
      }
    }
  }
}

plot(1:2, type='n')
rasterImage(greyimg3, 1, 1, 2, 2)

plot(1:2, type='n')
rasterImage(greyimg, 1, 1, 2, 2)