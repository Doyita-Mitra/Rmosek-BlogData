#Installing Rmosek
install.packages("Rmosek", type="source", INSTALL_opts="--no-multiarch",repos="http://download.mosek.com/R/8", 
                 configure.vars="PKG_MOSEKHOME=C:/Program Files/Mosek/8/tools/platform/win64x86 PKG_MOSEKLIB=mosek64")

library(Rmosek)

#Read the data
Blogdata <- read.csv('C:\\Users\\doyit\\Documents\\BlogFeedback\\blogData_train.csv', header=TRUE,sep=',')

#Set the dataframe
blog_df <- Blogdata[0:6000, c(51:60,281)]

#Check for number of rows and columns
nrow(Blogdata) #6000 rows
ncol(Blogdata) #11 columns

# Creating training and test sets
set.seed(123)
training <- sample(seq_len(nrow(blog_df)), size=(floor(0.75*nrow(blog_df))))
Blogdata_train <- blog_df[training,]
Blogdata_test <- blog_df[-training,]

# Matrix for Training and Test Sets
Blogdata_train_X = Blogdata_train[,1:ncol(Blogdata_train)-1]
Blogdata_train_Y = Blogdata_train[,ncol(Blogdata_train)]
Blogdata_train_Y = as.matrix(Blogdata_train_Y)
Blogdata_train_X=as.matrix(Blogdata_train_X)
View(Blogdata_train_X)
View(Blogdata_train_Y)

Blogdata_test_X=Blogdata_test[,1:ncol(Blogdata_test)-1]
Blogdata_test_Y=Blogdata_test[,ncol(Blogdata_test)]
Blogdata_test_Y=as.matrix(Blogdata_test_Y)
Blogdata_test_X=as.matrix(Blogdata_test_X)
View(Blogdata_test_X)
View(Blogdata_test_Y)


################ The OLS Function #################

solve.ols<-function(X,y, verb=1){
  p<-dim(X)[2]  # number of parameters of interest
  
  #correspondence between OLS and the Quadratic Program
  xx<-crossprod(X) # X'X=Q variable in the Quadratic program
  c<--crossprod(X,y) # X'y=c variable in the Quadratic program
  xx2<-xx
  xx2[upper.tri(xx)]<-0 #mosek needs Q to be  triangular
  idx <- which(xx2 != 0, arr.ind=TRUE) #index of the nonzero elements of Q
  
  #problem definition in Mosek
  qo1<-list() #empty list that contains the QP problem
  qo1$sense<-"min" #problem sense
  qo1$c<-as.vector(c) #objective coefficients
  qo1$qobj<-list(i = idx[,1],
                 j = idx[,2],
                 v = xx2[idx] ) #the Q matrix is imputed by the row indexes i, the col indexes j and the values v that define the Q matrix
  qo1$A<-Matrix(rep(0,p), nrow=1,byrow=TRUE,sparse=TRUE) #constrain matrix A is a null matrix in this case
  qo1$bc<-rbind(blc=-Inf, buc= Inf) #constraint bounds
  qo1$bx<-rbind(blx=rep(-Inf,p), bux = rep(Inf,p)) #parameter bounds 
  r<-mosek(qo1, opts = list(verbose = verb)) #call mosek solver
  return(r)
}


#Linear Regressio
colnames(Blogdata_train) <- c('IV1','IV2','IV3','IV4','IV5','IV6','IV7','IV8','IV9','IV10','Target')
colnames(Blogdata_test) <- c('IV1','IV2','IV3','IV4','IV5','IV6','IV7','IV8','IV9','IV10','Target')

lm_model1 <- lm(formula = Target ~ .,data = data.frame(Blogdata_train))
lm_predict <- predict(lm_model1,Blogdata_test,se.fit = TRUE)
summary(lm_model1)
lm_predict$fit

#Mean Square Error (MSE)
mse=0
mse_test <- mean((Blogdata_test$Target - (lm_predict$fit))^2)
mse_avg=mse+mse_test
print(mse_avg)

#Calling Mosek Function
rf=solve.ols(Blogdata_train_X, Blogdata_train_Y)
rf

yHat=Blogdata_test_X[,1]*0.11788018 + Blogdata_test_X[,2]*0.01128952 +
  Blogdata_test_X[,3]0.19877664+ Blogdata_test_X[,4](-0.09152098)  + 
  Blogdata_test_X[,5]0.01128952 + Blogdata_test_X[,6](0.25588599) +
  Blogdata_test_X[,7](-0.63562090) + Blogdata_test_X[,8](-0.26849565) +
  Blogdata_test_X[,9]*(-2.00226379) +  Blogdata_test_X[,10]*(0.16376755)
YMinusYHat = Blogdata_test_Y - yHat
mse <- function(error)
{
  
  mean(error^2)
  
}
MSE_Mosek=mse(YMinusYHat)
MSE_Mosek
YMinusYHat
RSS_Mosek=sum(YMinusYHat^2)
RSS_Mosek

