
## Authors: Nicollas Cozzolino and Ignacio Ramos garcía

library("ggplot2")
library("lattice")
library("rgl")
library("MASS")
library("VGAM")
library("corrplot")
library("standardize")
library("caret")
library("FactoMineR")
library("monmlp")
library("neuralnet")

df = read.csv("Train.csv") 

## Preprocessing ####
#Replacing the -1 cases for 0 
df = replace(df, df == -1, 0 ) 
attach(df)

# Indexes of the continuous variables
index_cont = c(1,5,12:23)
# Correspondance analysis
#res.ca <- CA(df[,-c(24,index_cont)], graph = FALSE)

# Setting the categorical variables as factors 
df$MARRIAGE = as.factor(df$MARRIAGE) 
df$EDUCATION = as.factor(df$EDUCATION)
df$SEX = as.factor(df$SEX)
df$PAY_0 = as.factor(df$PAY_0)
df$PAY_2 = as.factor(df$PAY_2)
df$PAY_3 = as.factor(df$PAY_3)
df$PAY_4 = as.factor(df$PAY_4)
df$PAY_5 = as.factor(df$PAY_5)
df$PAY_6 = as.factor(df$PAY_6)
df$default.payment.next.month = as.factor(df$default.payment.next.month)



#Function to standarize data
df_std = df
df_std[,index_cont] <- scale(df[,index_cont])


## Data splitting ####

#index of the rows of each folds. 10 in total
idx <- createFolds(df$default.payment.next.month, k = 10, list = TRUE, returnTrain = TRUE)
names(idx)[1:7] <- "train" # 80% for training

df_learning <- df[idx$train,]
df_test     <- df[-idx$train,]
## EDA ####
#Boxplots
boxplot(AGE ~ default.payment.next.month , data = df, main="Age distribution payment",col = c("red","lightblue"))

#Scatter plots
#plot(df$LIMIT_BAL)

# Bar plots
#Sex bar plot
data   <- df[which(df$SEX>0&df$SEX<3),]
counts <- table(data$SEX, data$default.payment.next.month)
barplot(counts, main="Pays distribution based on sex",
        xlab="Number of Gears", col=c("orange","darkblue"),
        args.legend = list(x="center"),legend = c("Male","Female"), beside=TRUE)

#Marriage bar plot
data   <- df[which(df$MARRIAGE>0),]
counts <- table(data$MARRIAGE, data$default.payment.next.month)
barplot(counts, main="Pays distribution based on marriage status",
        xlab="Number of Gears", col=c("orange","darkblue","red"),
        args.legend = list(x="topright"),legend = c("married","single","Others"), beside=TRUE)

#Education bar plot
data   <- df[which(df$EDUCATION>0&df$EDUCATION<5),]
counts <- table(data$EDUCATION, data$default.payment.next.month)
barplot(counts, main="Pays distribution based on education",
        xlab="Number of Gears", col=c("orange","darkblue","red","lightblue"),
        args.legend = list(x=10.5),legend = c("graduate school", "university", "high school","others"), beside=TRUE)


#Histograms
for (i in 1:(length(names(df))-1))
{ hist(df[,i],prob=FALSE,xlab=names(df)[i],main = paste("Histogram of ",names(df)[i]))
}

## PCA ####
#Select the continuos features for the PCA
data_pca <- df[,index_cont]

# Perform the PCA
pca <- prcomp(data_pca,scale=TRUE)
print(pca)
summary(pca)
plot(pca,type="l")
first_variable  =  names(df)[12]
second_variable = names(df)[1]
xyplot(pca$x[,2] ~ pca$x[,1], data_pca, groups = df$default.payment.next.month, pch= 20,
       main='Enhanced Scatter Plot')

## PCA - Rcmdr #####
install.packages("FactoMineR")
library("FactoMineR")
install.packages("Rcmdr")
library(Rcmdr)
data_std.PCA<-data_std[, c("LIMIT_BAL", "AGE", "BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6", "PAY_AMT1", "PAY_AMT2",Rcmdr+    "PAY_AMT3", "PAY_AMT4", "PAY_AMT5", "PAY_AMT6")]

res<-PCA(data_std.PCA , scale.unit=TRUE, ncp=7, graph = FALSE)

plot.PCA(res, axes=c(1, 2), choix="ind", habillage="none", col.ind="black",col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),new.plot=TRUE)

plot.PCA(res, axes=c(1, 2), choix="var", new.plot=TRUE, col.var="black",col.quanti.sup="blue", label=c("var", "quanti.sup"), lim.cos2.var=0)

summary(res, nb.dec = 3, nbelements=10, nbind = 10, ncp = 3, file="")

Call:
  "res<-PCA(data_0.PCA , scale.unit=TRUE, ncp=5, graph = FALSE)" 

## Corelation Matrix #### 

CorMatrix = cor(df_std[,c("AGE","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6","LIMIT_BAL","PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")], use="complete")


## LDA ####

lda <- lda(formula=default.payment.next.month ~.,data=df)
lda$prior
lda$counts
lda$means

ldavalue <- predict(lda)
xyplot(ldavalue$x[,1] ~ ldavalue$x[,2], df, groups = df$default.payment.next.month, pch= 20,
       main='Enhanced Scatter Plot')

## replacing -1 by 0  to run the chi test##
data_0= replace(data_train, data_train == -1, 0)

## Table analysis ####



#Table - Age and default.payment.next.month
tbl_age_payment = as.data.frame.array(table(df$AGE, df$default.payment.next.month)) 
# Table - Education level default.payment.next.month
tbl_educ_payment = as.data.frame.array(table(df$EDUCATION,df$default.payment.next.month))
# Table - Mariage Status and default.payment.next.month
tbl_mar_payment = as.data.frame(table(df$MARRIAGE, df$default.payment.next.month))
#Table - Sex Status and default.payment.next.month
tbl_sex_payment = as.data.frame.array(table(df$SEX, df$default.payment.next.month))

# Tables - Mariage Status and Payment Delay by month 
tbl_mar_delay0 = as.data.frame.array(table(df$MARRIAGE,df$PAY_0))
tbl_mar_delay2 = as.data.frame.array(table(df$MARRIAGE,df$PAY_2))
tbl_mar_delay3 = as.data.frame.array(table(df$MARRIAGE,df$PAY_3))
tbl_mar_delay4 = as.data.frame.array(table(df$MARRIAGE,df$PAY_4))
tbl_mar_delay5 = as.data.frame.array(table(df$MARRIAGE,df$PAY_5))
tbl_mar_delay6 = as.data.frame.array(table(df$MARRIAGE,df$PAY_6))

#Table - Sex and Payment Delay by month
tbl_sex_delay2 = as.data.frame.array(table(df$SEX, df$PAY_2))
tbl_sex_delay0= as.data.frame.array(table(df$SEX, df$PAY_0))
tbl_sex_delay3 = as.data.frame.array(table(df$SEX, df$PAY_3))
tbl_sex_delay4 = as.data.frame.array(table(df$SEX, df$PAY_4))
tbl_sex_delay5 = as.data.frame.array(table(df$SEX, df$PAY_5))
tbl_sex_delay6 = as.data.frame.array(table(df$SEX, df$PAY_6))

#Table - Educational level and Payment Delay by month
tbl_educ_delay0 = as.data.frame.array(table(df$EDUCATION, df$PAY_0))
tbl_educ_delay2 = as.data.frame.array(table(df$EDUCATION, df$PAY_2))
tbl_educ_delay3 = as.data.frame.array(table(df$EDUCATION, df$PAY_3))
tbl_educ_delay4 = as.data.frame.array(table(df$EDUCATION, df$PAY_4))
tbl_educ_delay5 = as.data.frame.array(table(df$EDUCATION, df$PAY_5))
tbl_educ_delay6 = as.data.frame.array(table(df$EDUCATION, df$PAY_6))

# Table - Delay Month 0 and the others 
tbl_delay0_delay2 = as.data.frame.array(table(df$PAY_0, df$PAY_2))
tbl_delay0_delay3 = as.data.frame.array(table(df$PAY_0, df$PAY_3))
tbl_delay0_delay4 = as.data.frame.array(table(df$PAY_0, df$PAY_4))
tbl_delay0_delay5 = as.data.frame.array(table(df$PAY_0, df$PAY_5))
tbl_delay0_delay6 = as.data.frame.array(table(df$PAY_0, df$PAY_6))


## Chisq - Test ####
# Chisq test for default.payment.next.month #
# Test - Educational Leve and default.payment.next.month
cst_sex_payment = chisq.test(tbl_sex_payment)  # Test - Sex and default.payment.next.month
cst_edu_payment =  chisq.test(tbl_educ_payment)
cst_mar_payment = chisq.test(tbl_mar_payment)
# Chisq test for Repayment Status 

# By Sex 
cst_sex_delay0 = chisq.test(tbl_sex_delay0)
cst_sex_delay2 = chisq.test(tbl_sex_delay2)
cst_sex_delay3 = chisq.test(tbl_sex_delay3)
cst_sex_delay4 = chisq.test(tbl_sex_delay4)
cst_sex_delay5 = chisq.test(tbl_sex_delay5)
cst_sex_delay6 = chisq.test(tbl_sex_delay6)

#By Educational Level 
cst_educ_delay0 = chisq.test(tbl_educ_delay0)
cst_educ_delay2 = chisq.test(tbl_educ_delay2)
cst_educ_delay3 = chisq.test(tbl_educ_delay3)
cst_educ_delay4 = chisq.test(tbl_educ_delay4)
cst_educ_delay5 = chisq.test(tbl_educ_delay5)
cst_educ_delay6 = chisq.test(tbl_educ_delay6)

# Checking the estatistical association between the Months 
cst_delay0_delay2 = chisq.test(tbl_delay0_delay2)
cst_delay0_delay3 = chisq.test(tbl_delay0_delay3)
cst_delay0_delay4 = chisq.test(tbl_delay0_delay4)
cst_delay0_delay5 = chisq.test(tbl_delay0_delay5)
cst_delay0_delay6 = chisq.test(tbl_delay0_delay6)

# By Mariage level 
cst_mar_delay0 = chisq.test(tbl_mar_delay0)
cst_mar_delay2 = chisq.test(tbl_mar_delay2)
cst_mar_delay3 = chisq.test(tbl_mar_delay3)
cst_mar_delay4 = chisq.test(tbl_mar_delay4)
cst_mar_delay5 = chisq.test(tbl_mar_delay5)
cst_mar_delay6 = chisq.test(tbl_mar_delay6)


## Logistic regression ####

#K-fold cross validation
set.seed(1)
results = vector("list",10)
for (i in seq_along(idx)){
  fit <- vglm(default.payment.next.month~., family=multinomial, data=df[idx[[i]],])
  
  # Predictions
  probabilities <- predict(fit,df[-idx[[i]],], type="response")
  predictions   <- apply(probabilities, 1, which.max)
  predictions[which(predictions=="1")] <- 0
  predictions[which(predictions=="2")] <- 1
  
  # Confussion matrix
  confusionMatrix <- table(predictions, df[-idx[[i]],]$default.payment.next.month)
  
  # Accuracy
  result[i] = (confusionMatrix[1,1]+confusionMatrix[2,2])/sum(confusionMatrix)
  result[i] 
}
mean(unlist(result))
var(unlist(result))



## Logistic regression with PCA ####
## PCA
#Select the continuos features for the PCA
data_pca <- df[,index_cont]
# Perform the PCA
pca <- prcomp(data_pca,scale=TRUE)
summary(pca)

n_variables = 6 #number of variables used to train the model
data_pca <- data.frame(matrix(nrow = nrow(df),ncol = n_variables))
col_names = ""
for (y in 1:n_variables)
{
  col_names = c(col_names,paste0("PC",toString(y)))
  data_pca[,y]= pca$x[,y]
}
colnames(data_pca) <- col_names[-1]

# Create new dataframe to train the model, 
data_model <- data.frame(matrix(nrow = nrow(df),ncol = (10+n_variables)))
data_model[1:9]  <- df[-c(index_cont,24)]
#Substitute continous variables with the most importat latent variables
data_model[10:(10+n_variables-1)]   <- data_pca
data_model[(10+n_variables)]        <- df[24]
colnames(data_model) <- c(names(df[-c(index_cont,24)]),names(data_pca),names(df[24]))

# Build the model
fit <- vglm(default.payment.next.month~., family=multinomial, data=data_model)
#summary(fit)

# Predictions
probabilities <- predict(fit, type="response")
predictions   <- apply(probabilities, 1, which.max)
predictions[which(predictions=="1")] <- 0
predictions[which(predictions=="2")] <- 1

# Confussion matrix
confusionMatrix <- table(predictions, data_model$default.payment.next.month)

# Accuracy
Accuracy_pca <- (confusionMatrix[1,1]+confusionMatrix[2,2])/sum(confusionMatrix)
Accuracy_pca


## Stepwise Logistic Regression ####

fullmod  <- glm(default.payment.next.month ~.,family=binomial(link="logit"),data=df)
emptymod <- glm(default.payment.next.month ~ 1,family=binomial(link="logit"),data=df)

backwards = step(fullmod)

step(emptymod,
     scope = list(upper=fullmod),
     direction="both",
     test="Chisq",
     data=df)

index_stepwise = c("PAY_AMT2" , "PAY_0",  
                   "PAY_AMT1" , "PAY_4" , "PAY_2" , "PAY_AMT3" , "PAY_5" , "PAY_AMT4" , 
                   "PAY_AMT5" , "BILL_AMT3" , "PAY_AMT6" , "PAY_6" , "PAY_3" , "AGE" , "BILL_AMT6", 
                   "EDUCATION" , "BILL_AMT2", "SEX" , "MARRIAGE" , "LIMIT_BAL")
#K-fold cross validation
set.seed(1)
result = vector("list",10)
for (i in 1:10){
  finalmod <- glm(formula = default.payment.next.month ~ PAY_AMT2 + PAY_0 + 
               PAY_AMT1 + PAY_4 + PAY_2 + PAY_AMT3 + PAY_5 + PAY_AMT4 + 
               PAY_AMT5 + BILL_AMT3 + PAY_AMT6 + PAY_6 + PAY_3 + AGE + BILL_AMT6 + 
               EDUCATION + BILL_AMT2 + SEX + MARRIAGE + LIMIT_BAL, 
               family = binomial(link = "logit"), 
               data = df[idx[[i]],])

  # Predictions
  probabilities <- predict(finalmod,newdata=subset(df[-idx[[i]],],select=index_stepwise),type='response')
  fitted.results <- ifelse(probabilities > 0.5,1,0)
  misClasificError <- mean(fitted.results != df[-idx[[i]],]$default.payment.next.month)
  print(paste('Accuracy',1-misClasificError))
  result[i] = 1-misClasificError
}
mean(unlist(result))
var(unlist(result))

## Decision Tree - Tree ####
install.packages("tree")
library("tree") 

#Creating a String categoriacal variable for the default.payment.next.month
attach(df) 
default.payment.next.month.Y.N = ifelse(default.payment.next.month == 0,"No", "Yes" )

#Adding this cologone to the original data frame

df_yn = data.frame(df, default.payment.next.month.Y.N)
df_yn = df_yn[,-24] #delet the numerical value for default.payment.next.month

df_learning <- df_yn[idx$train,]
df_test <- df_yn[-idx$train, ]


# Fitting the model with the training data 
tree_model = tree(default.payment.next.month.Y.N~., df_learning)

# visualizing the result 
plot(tree_model)
text(tree_model)

# Check the model's performance using the testing data

tree_predict = predict(tree_model,df_test,type = "class")
mean(tree_predict != df_test$default.payment.next.month.Y.N) #26.8%
accuracy_tree = ((1 - mean(tree_predict != df_test$default.payment.next.month.Y.N)) *100) 
accuracy_tree  #73,2%

# New tree using the PCA factor + categorical fetures of df_yn ##

#Creating a new data frame with the 9 new features created by the PCA, instead of 14 as we had in the original df

pca_new_variables = as.data.frame.array(result.PCA$ind$coord)

df_yn_categorical = df_yn [ ,c(2:11, 24)]

df_cat_pca = data.frame(df_yn_categorical, pca_new_variables)

# Splitting this new data frame #

idx_cat_pca <- createFolds(df_cat_pca$SEX, k = 10, list = TRUE, returnTrain = TRUE)
names(idx_cat_pca)[1:7] <- "train_cat_pca" # 80% for training

df_learning_cat_pca <- df_cat_pca[idx_cat_pca$train_cat_pca,]
df_test_cat_pca     <- df_cat_pca[-idx_cat_pca$train_cat_pca,]

# Fitting the model with the training data 
tree_model_cat_pca = tree(default.payment.next.month.Y.N~., df_learning_cat_pca)

# visualizing the result 
plot(tree_model_cat_pca)
text(tree_model_cat_pca)

# Check the model's performance using the testing data

tree_predict_cat_pca = predict(tree_model_cat_pca,df_test_cat_pca, type = "class")
mean(tree_predict_cat_pca != df_test_cat_pca$default.payment.next.month.Y.N) 
accuracy_tree_cat_pca = ((1 - mean(tree_predict_cat_pca != df_test_cat_pca$default.payment.next.month.Y.N)) *100) 
accuracy_tree_cat_pca  #74,2%

#The Accuracy for 8 fetures was 73.3%, too low in compairason with the original one, so we decide to use 9 fetures. 
#### Decision Tree - Rpart ####

# Runing the Tree with other methods to see the difference in gain of accuracy 
install.packages("rpart")
library("rpart")
install.packages("rpart.plot")
library(rpart.plot)

# First tree with all the data 
tree_rpart= rpart(default.payment.next.month.Y.N~., df_learning, method= "class")
rpart.plot(tree_rpart, extra = 108)

#Misclassification
tree_predict_rpart = predict(tree_rpart,df_test,type = "class")
mean(tree_predict_rpart != df_test$default.payment.next.month.Y.N) 
accuracy_tree_rpart = ((1 - mean(tree_predict_rpart != df_test$default.payment.next.month.Y.N)) *100) 
accuracy_tree_rpart #75.98

#Seen that this new methode gave us a gain in accuracy we will run it for the PCA + Categorical features as we have done before
tree_rpart_pca= rpart(default.payment.next.month.Y.N~., df_learning_cat_pca , method = "class")
rpart.plot(tree_rpart_pca,extra =108)

# Accuracy of the new tree
Testpred_rpart_pca= predict(tree_rpart_pca,df_test_cat_pca,type = "class")
tab_rpart_pca = table(Testpred_rpart_pca,df_test_cat_pca$default.payment.next.month.Y.N)
tab_rpart_pca
(sum(diag(tab_rpart_pca))/sum(tab_rpart_pca))*100 #74.26

#### Prune the models ####

#Complete Tree

ctr = rpart.control(maxdepth = 7, cp= 0.0064, minbucket = 20)
tree_rpart_prune= rpart(default.payment.next.month.Y.N~., df_learning, method= "class", control = ctr)
rpart.plot(tree_rpart_prune, extra = 108)


#Misclassification
tree_predict_rpart_prune = predict(tree_rpart_prune,df_test,type = "class")
mean(tree_predict_rpart_prune != df_test$default.payment.next.month.Y.N) 
accuracy_tree_rpart_prune = ((1 - mean(tree_predict_rpart_prune != df_test$default.payment.next.month.Y.N)) *100) 
accuracy_tree_rpart_prune #76.78%


printcp(tree_rpart_prune)
plotcp(tree_rpart_prune) #Gives a visual representation of the cross-validation results in an rpart object.

#Cp = 0.0064 MINIMIZES THE OVERFIT - 0.9% accuracy gain . 75.98% -> 76.78%
# This maximum accuracy only occurs when we dont set a "minsplit" so we can reach leafs with 1% of the data
# Purity Gain, when changing the last variable from PAY_5 to PAY_2 using the "minbkucket = 20" 

#PCA + Categorical Features Tree

ctr_pca = rpart.control(cp= 0.0047, maxdepth = 6 , minbucket = 50)
tree_rpart_pca_pruned = rpart(default.payment.next.month.Y.N~., df_learning_cat_pca , method = "class", control =ctr_pca)
rpart.plot(tree_rpart_pca_pruned,extra =108)

# Accuracy of the new tree
Testpred_rpart_pca_pruned= predict(tree_rpart_pca_pruned,df_test_cat_pca,type = "class")
tab_rpart_pca_pruned = table(Testpred_rpart_pca_pruned,df_test_cat_pca$default.payment.next.month.Y.N)
tab_rpart_pca_pruned
(sum(diag(tab_rpart_pca_pruned))/sum(tab_rpart_pca_pruned))*100 #74.7


printcp(tree_rpart_pca_pruned)
plotcp(tree_rpart_pca_pruned) #Gives a visual representation of the cross-validation results in an rpart object.

#CP = 0.047, we need a CP really small to hve any gain in the Tree's accuracy. 
# Furthermore the impurity is higher at the PCA tree


## KNN ####
# The choice of K equal to the square root of the number of instances is an
# empirical rule-of-thumb popularized by the "Pattern Classification" book by Duda et al.

set.seed(1)
kmax = 30 #max number of neighbors
ks <- 20:kmax
var_knn = vector("list",kmax)
res.k = vector("list",10)
res <- sapply(ks, function(k) {
  ##try out each version of k from 1 to 12
  res.k <- sapply(seq_along(idx), function(i) {
    ##loop over each of the 10 cross-validation folds
    ##predict the held-out samples using k nearest neighbors with Gower's distance
    pred <- dprep::knngow(df_std[idx[[i]], ],df_std[-idx[[i]],], k = k)
    ##the ratio of misclassified samples
    confusionMatrix <- table(df_std[-idx[[i]],'default.payment.next.month'],pred)
    (confusionMatrix[1,1]+confusionMatrix[2,2])/sum(confusionMatrix)
  })
  #var(res.k)
  ##average over the 10 folds
  mean(res.k)
})

#kopt            =  which(res==max(unlist(res))) # kopt = 21 23?

## MLP ####

set.seed(1)
max_hid_units = 10 #max number of hidden units in the first layer
ks    <- 1:max_hid_units
x_mlp = data.matrix(df[,index_cont])
as.numeric(x_mlp)
y_mlp   = data.matrix(df[,'default.payment.next.month'])
var_mlp = vector("list",max_hid_units)
res  <- sapply(ks, function(k) {
  ##try out each version of k from 1 to 12
  print(k)
  results = ""
  res.k <- sapply(seq_along(idx), function(i) {
    print(i)
    form.in <-as.formula('default.payment.next.month~LIMIT_BAL+AGE+BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4+
                 BILL_AMT5+BILL_AMT6+PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6')
    mlp_train = df_std[idx[[i]],]
    mlp_test  = df_std[-idx[[i]],]
    #Compute the weights
    nn         <- neuralnet(form.in,data=mlp_train[,c(index_cont,24)],
                            hidden=4, linear.output=FALSE, stepmax=1e6,threshold=0.1)
    #Make predictions
    nn.results <- compute(nn, mlp_test[,index_cont])
    #Create confussion matrix
    results    <- data.frame(actual = mlp_test$default.payment.next.month, 
                             prediction = nn.results$net.result)
    roundedresults   <- sapply(results,round,digits=0)
    roundedresultsdf = data.frame(roundedresults)
    attach(roundedresultsdf)
    confusionMatrix <- table(actual,prediction)
    #Accuracy
    (confusionMatrix[1,1]+confusionMatrix[2,2])/sum(confusionMatrix)
  })
  var(res.k)
  ##average over the 10 folds
  mean(res.k)
})




##Random Forest####
install.packages("randomForest")
library(randomForest)

# First forest with all the data 
forest = randomForest(default.payment.next.month.Y.N~., df_learning, ntree = 350, mtry= 4, importance = T )
forest

#Misclassification
forest_predic = predict(forest, df_test)
mean(forest_predic != df_test$default.payment.next.month.Y.N) 
accuracy_forest = ((1 - mean(forest_predic != df_test$default.payment.next.month.Y.N)) *100) 
accuracy_forest

#Better way to see accuracy because it gives a range (95% CI)
confusionMatrix(forest_predic, df_test$default.payment.next.month.Y.N)
#76.3%-80.5%

# Error Rate of Random Forest
p = plot(forest.teste) 
# We can see that using 400 or maybe 200 trees we may reach the same result 

#Tuning the RF
t = tuneRF(df_learning[,-24],df_learning[,24],
           stepFactor = 0.5,
           plot = T,
           ntreeTry = 350,
           trace = T,
           improve = 0.05)
#Keeps mtry = 4 ; 

#Importance of the features in the forest construction
importance_forest = as.data.frame.array(importance(forest))
var_importance = varImpPlot(forest,
                            sort = T)

# We can take out variables: MARRIAGE, SEX.

#Variables Usage 
varUsed(forest)

# Feature Selection - Taking only the features who's Mean Decrease Gini > 200. 
df_new = df_learning[,c(-3,-4)]

# Forest applied in the most important features 
forest_1 = randomForest(default.payment.next.month.Y.N~., df_new,  ntree = 350, mtry= 4, importance = T )
forest_1

#Importance of the features in the forest construction
importance_forest_1 = as.data.frame.array(importance(forest_1))
importance_forest_1

#Tuning the RF
t_1 = tuneRF(df_new[,-22],df_new[,22],
             stepFactor = 0.5,
             plot = T,
             ntreeTry = 350,
             trace = T,
             improve = 0.05)

#Misclassification
forest_predic_1 = predict(forest_1, df_test)
mean(forest_predic_1 != df_test$default.payment.next.month.Y.N) 
accuracy_forest_1 = ((1 - mean(forest_predic_1 != df_test$default.payment.next.month.Y.N)) *100) 
accuracy_forest_1

#Better way to see accuracy because it gives a range (95% CI)
confusionMatrix(forest_predic_1, df_test$default.payment.next.month.Y.N)
#76.5% - 80.7%

# Forest using PCA varibles + Categorical varaibles 
forest_pca = randomForest(default.payment.next.month.Y.N~., df_learning_cat_pca, ntree = 350, mtry = 4,importance = T  )

#Importance of the featreus in the PCA forest construction
importance_forest_pca = as.data.frame.array(importance(forest_pca))
importance_forest_pca

#Misclassification
forest_pca_predic = predict(forest_pca, df_test_cat_pca)
mean(forest_pca_predic != df_test_cat_pca$default.payment.next.month.Y.N) 
accuracy_forest_pca = ((1 - mean(forest_pca_predic != df_test_cat_pca$default.payment.next.month.Y.N)) *100) 
accuracy_forest_pca

#Better way to see accuracy because it gives a range (95% CI)
confusionMatrix(forest_pca_predic, df_test_cat_pca$default.payment.next.month.Y.N)
#75.9% - 80.2%


## PREDICTIONS####

# Importing the new dataset 
df_final = read.csv("TestStudent.csv")

df_final = replace(df_final, df_final == -1, 0 ) 
df_final$MARRIAGE = factor(df_final$MARRIAGE,levels = levels(df$MARRIAGE)) 
df_final$EDUCATION = factor(df_final$EDUCATION,levels = levels(df$EDUCATION))
df_final$SEX = factor(df_final$SEX,levels = levels(df$SEX))
df_final$PAY_0 = factor(df_final$PAY_0,levels = levels(df$PAY_0))
df_final$PAY_2 = factor(df_final$PAY_2,levels = levels(df$PAY_2))
df_final$PAY_3 = factor(df_final$PAY_3,levels = levels(df$PAY_3))
df_final$PAY_4 = factor(df_final$PAY_4,levels = levels(df$PAY_4))
df_final$PAY_5 = factor(df_final$PAY_5,levels = levels(df$PAY_5))
df_final$PAY_6 = factor(df_final$PAY_6,levels = levels(df$PAY_6))

df_final$default.payment.next.month.Y.N = 0

#Implementing feature selection
df_final = df_final[,c(-3,-4)]

#Predictions on new data
# Using the best model to create a new model using the entire data as a training data 
rf = randomForest(default.payment.next.month.Y.N~., df_new, ntree = 300, mtry= 4, importance = T )
rf_prediction = predict(rf, df_final)

default.payment.next.month = ifelse(rf_prediction == "No",0, 1 )

#Export as an Excell file
write.table(as.data.frame(default.payment.next.month),file=" LLSMF2013-15-predictions.csv", quote=F,sep=",",row.names=F)
