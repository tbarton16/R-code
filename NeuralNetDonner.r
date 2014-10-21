install.packages('neuralnet')
library('neuralnet')
# generate nonlinear data 
genCompare<- function(n){
  x <- sample(seq(1,n*2, by = 2))
  y <- sample(seq(1,n*2, by = 2))
  z <- c()
  for(i in 1:length(x)){
    z[i] <- x[i]+2*y[i]+x[i]*y[i]
  }
  return(z)
}
genTrain <- function(x,y){
  n <- length(x)
  z <- c()
  #for(i in 1:length(x)){
  #  z[i] <- rbinom(1,1,1/(1+exp(-((4.5 * x[i]-5*y[i]+.2*x[i]*y[i])))))   #+rnorm(1,0,3) # sigmoidal function
  #}
  z <- (x-.5)^2 + (y-.5)^2 < .1
  return(z)
}
# sample size 20
n <- 2000
x <- runif(n)
y <- runif(n)
zEst <- genTrain(x,y)
dat = as.data.frame(cbind(x,y,zEst))
# neural net
nn.sse <- neuralnet(zEst~x+y,data = dat, hidden= 5, threshold=.01, err.fct = 'sse' )
nn     <- neuralnet(zEst~x+y,data = dat, threshold=.01, err.fct = 'sse' )
nn.two <- neuralnet(zEst~x+y,data = dat, hidden= c(2,2), threshold=.01 )

plot(x,y)
points(x[zEst==1],y[zEst==1],col='red')
output <- compute(nn,dat[,1:2])
points(x[output$net.result>.5],y[output$net.result>.5],col='green',cex=.5)

output.two <- compute(nn.two,dat[,1:2])
points(x[output$net.result>.5],y[output$net.result>.5],col='dodgerblue',cex=.5)

# Roc curve
install.packages('ROCR')
library('ROCR')

pred <- prediction(output$net.result,zEst)
perf <- performance(pred,"tpr","fpr")
plot(perf, main = 'Network with 1 HL and 1 Node')


#sometimes life just gives you roc curves. like literally.
nntwo <-neuralnet(zEst~x+y,data = dat, hidden= 2, threshold=.01, err.fct = 'sse' )
nnthree <-neuralnet(zEst~x+y,data = dat, hidden= 3, threshold = .01, err.fct = 'sse') 
nnfour <-neuralnet(zEst~x+y,data = dat, stepmax = 1e+08,hidden= c(2,2,2))




calculateAUC <- function(nn){
  #args: object of class nn calculates a measure of area under curve
  output.two <- compute(nn,dat[,1:2])
  pred.two   <- prediction(output.two$net.result,zEst)
  perf.two   <- performance(pred.two,measure = 'auc')
  return(perf.two)
}


rocs <- c(0.8314817819,0.899177749)  
hiddenlayers <- seq(1,5)
plot(hiddenlayers,rocs)

# donner again
`C7.Donner` <- read.csv("/Volumes/kuiper_cd/Assets/Data Sets/Chapter 07/CSV Files/C7 Donner.csv")
data <- C7.Donner
Survive <- data[1:85,4]
Gender  <- data[1:85,2]
Age     <- data[1:85,3]
Fam     <- data[1:85,5]
betterdata = cbind(Survive,Gender,Age,Fam)
doNNer <-neuralnet(Survive~Gender+Age+Fam,data = betterdata, stepmax= 1e+07, hidden= c(9,2), err.fct = 'sse')
goNNer <- neuralnet(Survive~Gender+Age+Fam,data = betterdata, stepmax= 1e+07, hidden= 9, err.fct = 'sse')
calculateAUC <- function(nn){
  #args: object of class nn calculates a measure of area under curve
  output.two <- compute(nn,betterdata[,2:4])
  pred.two   <- prediction(output.two$net.result,Survive)
  perf.two   <- performance(pred.two,measure = 'auc')
  return(perf.two)
}
output1 <- compute(doNNer,betterdata[,2:4])

pred1  <- prediction(output1$net.result,Survive)

perf1 <- performance(pred1,"tpr","fpr")

plot(perf1, main = 'ROC for Network with 9 Nodes and 2 Hidden Layers')

aucs <- c( 0.5302911534, 0.9185330347,0.9056550952,0.9577267637,0.9342105263,0.9538073908,0.9678051512)
Nodes <- c(5,7,8,9,10,12,15)

# 0.7169652856, 0.9795632699
t = c(0,19,2)
p = c(0,19,0)
j = c(1,19,3)
testset = rbind(t,p,j)
as.data.frame(testset)
ourfate <- compute(doNNer,testset[,1:3])
barton <- c(0,19,2)
chandler <- c(1,35,1)
derose <- c(1,20,4)
fiksel <- c(1,21,3)
garnatz <- c(1,21,1)
gerecke <- c(0,22,1)
goldberg <- c(1,20,2)
gu <- c(1,20,1)
kaye <- c(1,20,2)
kellogg <- c(1,20,1)
li <- c(1,21,1)
loukanov <- c(1,20,3)
lu <- c(1,20,2)
mcandrews <- c(1,21,4)
metz <- c(1,19,0)
morgens <- c(1,22,3)
oconnell <- c(1,22,2)
shine <- c(0,22,3)
weinand <- c(1,19,5)
williamson <- c(1,22,5)
yang <- c(1,21,4)
class <- rbind(barton,chandler,derose,fiksel,garnatz,gerecke,goldberg,gu,kaye,kellogg,li,loukanov,lu,mcandrews,metz,morgens,oconnell, shine,weinand,williamson,yang)
classdata <- as.data.frame(class)
ourfate <- compute(doNNer,classdata[,1:3])