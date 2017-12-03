# author_@Aditya_Sharma
require(seqinr)
require(psych)
require(ggplot2)
require(MASS)
require(car)
require(neuralnet)
require(grid)
require(e1071)
############################################
#loading the data set
trainingdata <- read.csv("training_data.csv")
summary(trainingdata)
rowtotal <- nrow(trainingdata)

#############################################
# separating 0's and 1's response Data

traininigdata0 <- trainingdata[which(trainingdata$Resp==0),]
traininigdata1 <- trainingdata[which(trainingdata$Resp==1),]

#############################################
# Getting the base composition of the sequence IN ACGT 
#N means BASE CAN be either A,C,G,OR T
#Y means Pyrimidine i.e either  C OR T


######## FEATURE EXTRACTION #################
#length of the RTrans
#############################################

for (i in 1:rowtotal) {
  trainingdata$rtlength[i]<- nchar(as.character(trainingdata$RT.Seq[i]))  
}
trainingdata$rtlength
#length of the PRlength is same at 247 for each entry so we wont take it into account 


############################################
################# AS PR SEQ DATA IS NOT AVAILABLE FROM 992 ###########################
#initializing new training data with only 920 entries.
trainingdatanew <- trainingdata[-c(921:1000),]
View(trainingdatanew)


     
#####################CORRECT TILL HERE##############################################################################################################

#K-MERS
####################################################

for (i in 1:920) {
  
  hiv_PRseq <- as.character(trainingdatanew$PR.Seq[i])
  
  
#splitting the sequence into a vector 
  hivseq1 <- strsplit(hiv_PRseq,"")    #strsplit returns a list
  hivseqvectornotation <- unlist(hivseq1)   # unlisting to vector notation so as to use GC function
  #One of the most fundamental properties of a genome sequence is its GC content, 
  #the fraction of the sequence that consists of Gs and Cs, ie. the %(G+C).
  
   GCPR <- GC(hivseqvectornotation)    # GC VALUE IN FRACTIONS NOT PERCENTAGE
  
     # hivseql is a vector of character
  prtable <- table(hivseq1)   #prtable is gives us the A C G T count in a neuclotide
  
  prtable <- as.data.frame(prtable)  # converting to DF so as to manipulate values by picking it specifically
  
  
  trainingdatanew$pr_A[i] <- prtable$Freq[1]
  
  trainingdatanew$pr_C[i] <- prtable$Freq[2] 
  trainingdatanew$pr_G[i] <- prtable$Freq[3]
  trainingdatanew$pr_R[i] <- prtable$Freq[4]
  trainingdatanew$pr_T[i] <- prtable$Freq[5]
  trainingdatanew$pr_Y[i] <- prtable$Freq[6]
  trainingdatanew$PR_GC[i]   <- GCPR
}
trainingdatanew[is.na(trainingdatanew)] <- 0   # Removing N/A values in the data frame to 0


####################################### CORRECT TILL HERE #############################################

#RT data extraction
#########################################################################################
for (i in 1:920) {
  
  hiv_rtseq <- as.character(trainingdatanew$RT.Seq[i])
  
  
  #splitting the sequence into a vector 
  hivseq1 <- strsplit(hiv_rtseq,"")
  hivseqvectornotation <- unlist(hivseq1) # TO GET VECTOR NOTATION OF SPLITTED RT SEQ
  
  GCRT <- GC(hivseqvectornotation)
  
  # hivseql is a vector of character
  prtable <- table(hivseq1)   #prtable is gives us the A C G T count in a neuclotide
  
  prtable <- as.data.frame(prtable)  # converting to DF so as to manipulate values by picking it specifically
  
  
  trainingdatanew$RT_A[i] <- prtable$Freq[1]
  
  trainingdatanew$RT_C[i] <- prtable$Freq[2] 
  trainingdatanew$RT_G[i] <- prtable$Freq[3]
  trainingdatanew$RT_R[i] <- prtable$Freq[4]
  trainingdatanew$RT_T[i] <- prtable$Freq[5]
  trainingdatanew$RT_Y[i] <- prtable$Freq[6]
  trainingdatanew$RT_GC[i] <- GCRT
   
}
trainingdatanew[is.na(trainingdatanew)] <- 0   # Removing N/A values in the data frame to 0
trainingdatanew<-trainingdatanew[,-3]
trainingdatanew<-trainingdatanew[,-3]
trainingdatanew<-trainingdatanew[,-1] #this remnoval came late as i progressed till variable importance
View(trainingdatanew)
#our Data set has been augmented enough now to start building a model 
#############################################################################################
############### correct till here ###########################################################

#1
colnames(trainingdatanew)
neu
nn <- neuralnet(formula = Resp~VL.t0+CD4.t0+rtlength+pr_A+pr_C+pr_G+pr_R+pr_T+pr_Y+PR_GC+RT_A+RT_C+RT_G+RT_R+RT_T+RT_Y+RT_GC,data = trainingdatanew,hidden = 16,err.fct = "ce",linear.output = FALSE,stepmax = 100000000,learningrate = 0.01,algorithm = "backprop")
plot(nn)

nn$net.result[1]
nn1<- as.data.frame(ifelse(nn$net.result[[1]]>0.5,1,0))
nn1$original <- trainingdata$Resp
View(nn1)


svmfit <- svm(trainingdatanew$Resp~trainingdatanew$VL.t0+trainingdatanew$CD4.t0+trainingdatanew$rtlength+trainingdatanew$pr_A,kernel="linear",cost=0.1)
svmfit
plot(svmfit,trainingdatanew[,col])

####################
#RANDOM FOREST ALGO###################################################################################################################################################
####################

#DataPartition
require(randomForest)
set.seed(123)
independentsample <- sample(2,nrow(trainingdatanew),replace=TRUE,prob=c(0.7,0.3))
train <- trainingdatanew[independentsample==1,]
test  <-trainingdatanew[independentsample==2,]

#random forest
#classsification model as well as regression
#as response variable is factor so here classsification
#will help in feauture selection as well as avoinding overfitting
#default 500 trees
#majority vote btw trees will give us the predicted values
train$Resp<-as.factor(train$Resp)   #important to convert in factors to use classification mode brathar than regression
View(trainingdatanew)
set.seed(222)
rf <- randomForest(Resp~.,data=train
                  )
print(rf) # confusion matrix and error rate 16.38%
#out of bag error is 16.38%
# predicting class 1 : 0 respose with greater accuracy
# 58 % error in predicting class 2: response =1
attributes(rf)
rf$confusion
require(caret) 
#######################
#prediction and confusion matrix -train data
p1 <- predict(rf,train)
head(p1)

confusionMatrix(p1,train$Resp)
# accuracy is coming at 100% 
#99.43% confidence interval
# oob error was 16.38% but the accuracy is coming at 100%
# thus a mismatch is seem at place but it is not there 
#as in random forest the out of bag error is the probable error in the data that the model has not seen yet
############################
#prediction with TEST data[out of bag]
############################
p2 <- predict(rf,test)
confusionMatrix(p2,test$Resp)
#accuracy has come down to 79.1208 %
#confidence interval at 73.811% to 83.78%
#############################################
#ERROR RATE
############################
plot(rf)
# the out of bag error decreases initially as the no of trees grow but we are not able to improve the error after around100 trees

#########################
#TUNING 
#########################
View(train)
set.seed(222)
t<-tuneRF(train[,-2],train[,2],
          stepFactor = 0.5,
          plot = TRUE,
          ntreeTry =50 ,
          improve = 0.05)

#bottom at mtry = 2 becomes the lowest where
#onwards we get to a increasing error
#so lets change the rf model with ntreetry at 350 to lower the error from 16.38%
# at ntreetry  = 50 we see that till mtry error depriciates
#again we test at rftuned2

set.seed(222)
rftuned <- randomForest(Resp~.,data=train,
                   ntree=100,
                   mtry=2,
                   importance=TRUE,
                   proximity=TRUE)

print(rftuned)
#the error out of b ag is being 16.85%
#so we will accept just the rf original

set.seed(222)
rftuned2 <- randomForest(Resp~.,data=train,
                         ntree=50,
                         mtry=8,
                         importance=TRUE,
                         proximity=TRUE)
print(rftuned2)
#17.16% out of bag error
# thus we keep rf

#no of nodes for the trees
hist(treesize(rf),
     main="no of nodes for the trees",
     col ="green")


#overall distruibution is btw 50 to 90 nodes
#majority of the trees on average have 70 nodes
#Variable importance
#######################
varImpPlot(rf)
#patient id shall be removed as it seems to be making the most of the difference 
#this point got out of my head before
#VL.T0 AND RT_A THAN RT_C THAN RT_GC ARE THE TOP FOUR VARIABLES THAT PROVIDE ACCURACY

varImpPlot(rf,
           sort=T,
           n.var=6,
           main = " top 6 variable imporatnce"
           )
importance(rf)
varUsed(rf)#which predicter variables were actually used in the random forest model

#14th variable was used the least
#i.e RT_G

partialPlot(rf,
            train,
            VL.t0,
            "1")
#tends to predict class1 more stronmgly
#when vl.t0 is >3.4
###############################################################################################################################
###############################################################################################################################

testfinal <- read.csv("test_data.csv")
View(testfinal)

rowtotaltest <- nrow(trainingdata)

#length of the RTrans
#############################################

for (i in 1:rowtotaltest) {
  testfinal$rtlength[i]<- nchar(as.character(testfinal$RT.Seq[i]))  
}
testfinal$rtlength
#length of the PRlength is same at 247 for each entry so we wont take it into account 





#####################CORRECT TILL HERE##############################################################################################################

#K-MERS
####################################################

for (i in 1:692) {
  
  hiv_PRseq <- as.character(testfinal$PR.Seq[i])
  
  
  #splitting the sequence into a vector 
  hivseq1 <- strsplit(hiv_PRseq,"")    #strsplit returns a list
  hivseqvectornotation <- unlist(hivseq1)   # unlisting to vector notation so as to use GC function
  #One of the most fundamental properties of a genome sequence is its GC content, 
  #the fraction of the sequence that consists of Gs and Cs, ie. the %(G+C).
  
  GCPR <- GC(hivseqvectornotation)    # GC VALUE IN FRACTIONS NOT PERCENTAGE
  
  # hivseql is a vector of character
  prtable <- table(hivseq1)   #prtable is gives us the A C G T count in a neuclotide
  
  prtable <- as.data.frame(prtable)  # converting to DF so as to manipulate values by picking it specifically
  
  
  testfinal$pr_A[i] <- prtable$Freq[1]
  
  testfinal$pr_C[i] <- prtable$Freq[2] 
  testfinal$pr_G[i] <- prtable$Freq[3]
  testfinal$pr_R[i] <- prtable$Freq[4]
  testfinal$pr_T[i] <- prtable$Freq[5]
  testfinal$pr_Y[i] <- prtable$Freq[6]
  testfinal$PR_GC[i]   <- GCPR
}
testfinal[is.na(testfinal)] <- 0   # Removing N/A values in the data frame to 0


####################################### CORRECT TILL HERE #############################################

#RT data extraction
#########################################################################################
for (i in 1:920) {
  
  hiv_rtseq <- as.character(testfinal$RT.Seq[i])
  
  
  #splitting the sequence into a vector 
  hivseq1 <- strsplit(hiv_rtseq,"")
  hivseqvectornotation <- unlist(hivseq1) # TO GET VECTOR NOTATION OF SPLITTED RT SEQ
  
  GCRT <- GC(hivseqvectornotation)
  
  # hivseql is a vector of character
  prtable <- table(hivseq1)   #prtable is gives us the A C G T count in a neuclotide
  
  prtable <- as.data.frame(prtable)  # converting to DF so as to manipulate values by picking it specifically
  
  
  testfinal$RT_A[i] <- prtable$Freq[1]
  
  testfinal$RT_C[i] <- prtable$Freq[2] 
  testfinal$RT_G[i] <- prtable$Freq[3]
  testfinal$RT_R[i] <- prtable$Freq[4]
  testfinal$RT_T[i] <- prtable$Freq[5]
  testfinal$RT_Y[i] <- prtable$Freq[6]
  testfinal$RT_GC[i] <- GCRT
  
}
testfinal[is.na(testfinal)] <- 0   # Removing N/A values in the data frame to 0
testfinal<- testfinal[,-1]
testfinal<- testfinal[,-2]
testfinal<- testfinal[,-2] 
View(testfinal)
#######################################################
pfinal <- predict(rf,testfinal)
testfinal$Resp <- pfinal
View(testfinal) # MY PREDICTED VALUES FOR RESPONSE
confusionMatrix(pfinal,testfinal$Resp)   
#Reference
#Prediction   0   1
#0           569   0
#1            0 123
























































 
  
  
  