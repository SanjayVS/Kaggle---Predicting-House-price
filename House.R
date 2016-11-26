rm(list = ls())
setwd("C:/Users/Sanjay/Desktop/Housing Data")
library("ggplot2")
library("rpart")
#Reading csv data
train = read.csv("train.csv" , header= TRUE, stringsAsFactors=FALSE)
test = read.csv("test.csv" , header= TRUE, stringsAsFactors=FALSE)



#test$SalePrice <- rep(NA, 1459)
#df <- rbind(train, test)

dim(train)
str(train)

#Taking out duplicate rows if any
train = unique(train)

#Checking for NA's
sort(apply(train,2, function(x)sum(is.na(x))),decreasing = TRUE)
sort(apply(test,2, function(x)sum(is.na(x))),decreasing = TRUE)

#REMOVING NA's

#POOLDATA
summary(as.factor(train$PoolQC))
train[which(train$PoolArea>0),"PoolQC"]
test[which(test$PoolArea>0),"PoolQC"]
train$PoolQC = as.character(train$PoolQC)
train[train$PoolArea == 0,]$PoolQC = rep("None", 1453)
train$PoolQC = as.factor(train$PoolQC)

test$PoolQC = as.character(test$PoolQC)
test[test$PoolArea == 0,]$PoolQC = rep("None", 1453)
test[which(is.na(test$PoolQC)),"PoolQC"] = "Gd" #setting PoolQc Good for unavailable Pool Quality
test$PoolQC = as.factor(test$PoolQC) 

#MISCFEATURE
train$MiscFeature = as.character(train$MiscFeature)
test$MiscFeature = as.character(test$MiscFeature)
train$MiscFeature[is.na(train$MiscFeature)] = "None"
test$MiscFeature[is.na(test$MiscFeature)] = "None"
train$MiscFeature = as.factor(train$MiscFeature)
test$MiscFeature = as.factor(test$MiscFeature)

#ALLEYTYPE
train$Alley = as.character(train$Alley)
test$Alley = as.character(test$Alley)
train$Alley[is.na(train$Alley)] = "None"
test$Alley[is.na(test$Alley)] = "None"
train$Alley = as.factor(train$Alley)
test$Alley = as.factor(test$Alley)

#FENCEQUALITY
train$Fence = as.character(train$Fence)
test$Fence = as.character(test$Fence)
train$Fence[is.na(train$Fence)] = "None"
test$Fence[is.na(test$Fence)] = "None"
train$Fence = as.factor(train$Fence)
test$Fence = as.factor(test$Fence)

#FIREPLACE
train$FireplaceQu = as.character(train$FireplaceQu)
test$FireplaceQu = as.character(test$FireplaceQu)
train$FireplaceQu[is.na(train$FireplaceQu)] = "None"
test$FireplaceQu[is.na(test$FireplaceQu)] = "None"
train$FireplaceQu = as.factor(train$FireplaceQu)
test$FireplaceQu = as.factor(test$FireplaceQu)

#GARAGE
train$GarageType = as.character(train$GarageType)
train$GarageQual = as.character(train$GarageQual)
train$GarageYrBlt = as.character(train$GarageYrBlt)
train$GarageFinish = as.character(train$GarageFinish)
train$GarageCond = as.character(train$GarageCond)
train[train$GarageArea == 0,]$GarageType = "None"
train[train$GarageArea == 0,]$GarageQual = "None"
train[train$GarageArea == 0,]$GarageYrBlt = "None"
train[train$GarageArea == 0,]$GarageFinish = "None"
train[train$GarageArea == 0,]$GarageCond = "None"
train$GarageType = as.factor(train$GarageType)
train$GarageQual = as.factor(train$GarageQual)
train$GarageYrBlt = as.factor(train$GarageYrBlt)
train$GarageFinish = as.factor(train$GarageFinish)
train$GarageCond = as.factor(train$GarageCond)

test$GarageType = as.character(test$GarageType)
test$GarageQual = as.character(test$GarageQual)
test$GarageYrBlt = as.character(test$GarageYrBlt)
test$GarageFinish = as.character(test$GarageFinish)
test$GarageCond = as.character(test$GarageCond)
test[which(test$GarageArea == 0),"GarageType"] = "None"
test[which(test$GarageArea == 0),"GarageQual"] = "None"
test[which(test$GarageArea == 0),"GarageYrBlt"] = "None"
test[which(test$GarageArea == 0),"GarageFinish"] = "None"
test[which(test$GarageArea == 0),"GarageCond"] = "None"

test[which(is.na(test$GarageArea)),"GarageType"] = "None"
test[which(is.na(test$GarageArea)),"GarageQual"] = "None"
test[which(is.na(test$GarageArea)),"GarageYrBlt"] = "None"
test[which(is.na(test$GarageArea)),"GarageFinish"] = "None"
test[which(is.na(test$GarageArea)),"GarageCond"] = "None"
test[which(is.na(test$GarageArea)),"GarageCars"] = 0
test[which(is.na(test$GarageArea)),"GarageArea"] = 0

which(is.na(test$GarageQual))

# Predict GarageYrBlt
pred <- c("GarageType", "GarageYrBlt", "GarageFinish", "GarageQual","GarageCond","YearBuilt", "GarageCars", "GarageArea")
rpart <- rpart(as.factor(GarageYrBlt) ~ .,
                   data = test[!is.na(test$GarageYrBlt),pred],
                   method = "class",
                   na.action=na.omit)

test$GarageYrBlt[is.na(test$GarageYrBlt)] <- as.numeric(as.character(predict(rpart, test[is.na(test$GarageYrBlt),pred], type = "class")))

#Checking garage year built and using that to predict missing garage values
summary(as.factor(test$GarageFinish[test$GarageType == "Detchd" &  test$GarageYrBlt == 1950]))
summary(as.factor(test$GarageQual[test$GarageType == "Detchd" &  test$GarageYrBlt == 1950]))
summary(as.factor(test$GarageCond[test$GarageType == "Detchd" &  test$GarageYrBlt == 1950]))
#We see that majority were unfinished, had quality and condition as TA
test[which(is.na(test$GarageQual)),"GarageQual"] = "Unf"
test[which(is.na(test$GarageFinish)),"GarageFinish"] = "TA"
test[which(is.na(test$GarageCond)),"GarageCond"] = "TA"

test$GarageType = as.factor(test$GarageType)
test$GarageQual = as.factor(test$GarageQual)
test$GarageYrBlt = as.factor(test$GarageYrBlt)
test$GarageFinish = as.factor(test$GarageFinish)
test$GarageCond = as.factor(test$GarageCond)

#BASEMENT
train$BsmtQual = as.character(train$BsmtQual)
train$BsmtCond = as.character(train$BsmtCond)
train$BsmtExposure = as.character(train$BsmtExposure)
train$BsmtFinType1 = as.character(train$BsmtFinType1)
train$BsmtFinType2 = as.character(train$BsmtFinType2)

train[which(train$TotalBsmtSF == 0),"BsmtQual"] = "None"
train[which(train$TotalBsmtSF == 0),"BsmtCond"] = "None"
train[which(train$TotalBsmtSF == 0),"BsmtExposure"] = "None"
train[which(train$TotalBsmtSF == 0),"BsmtFinType1"] = "None"
train[which(train$TotalBsmtSF == 0),"BsmtFinType2"] = "None"

which(is.na(train$BsmtExposure))
which(is.na(train$BsmtFinType2))
table(train$LandContour)
table(train$LandSlope)
table(train[which((train$LandContour=="Bnk") && (train$LandSlope=="Sev")),"BsmtExposure"])
train[which(is.na(train$BsmtExposure)),"BsmtExposure"] = "No"
table(train[which(train$BsmtFinSF2>0),"BsmtFinType2"])
train[which(is.na(train$BsmtFinType2)),"BsmtFinType2"] = "Rec"


train$BsmtQual = as.factor(train$BsmtQual)
train$BsmtCond = as.factor(train$BsmtCond)
train$BsmtExposure = as.factor(train$BsmtExposure)
train$BsmtFinType1 = as.factor(train$BsmtFinType1)
train$BsmtFinType2 = as.factor(train$BsmtFinType2)


test$BsmtQual = as.character(test$BsmtQual)
test$BsmtCond = as.character(test$BsmtCond)
test$BsmtExposure = as.character(test$BsmtExposure)
test$BsmtFinType1 = as.character(test$BsmtFinType1)
test$BsmtFinType2 = as.character(test$BsmtFinType2)

test[which(test$TotalBsmtSF == 0),"BsmtQual"] = "None"
test[which(test$TotalBsmtSF == 0),"BsmtCond"] = "None"
test[which(test$TotalBsmtSF == 0),"BsmtExposure"] = "None"
test[which(test$TotalBsmtSF == 0),"BsmtFinType1"] = "None"
test[which(test$TotalBsmtSF == 0),"BsmtFinType2"] = "None"

which(is.na(test$BsmtExposure))
which(is.na(test$BsmtFinType2))
which(is.na(test$BsmtCond))
#One observation with all basement fields as NA
test[which(is.na(test$TotalBsmtSF)),"BsmtQual"] = "None"
test[which(is.na(test$TotalBsmtSF)),"BsmtCond"] = "None"
test[which(is.na(test$TotalBsmtSF)),"BsmtExposure"] = "None"
test[which(is.na(test$TotalBsmtSF)),"BsmtFinType2"] = "None"
test[which(is.na(test$TotalBsmtSF)),"BsmtFinType1"] = "None"
test[which(is.na(test$TotalBsmtSF)),"BsmtFinSF1"] = 0
test[which(is.na(test$TotalBsmtSF)),"BsmtFinSF2"] = 0
test[which(is.na(test$TotalBsmtSF)),"BsmtUnfSF"] = 0
test[which(is.na(test$TotalBsmtSF)),"BsmtFullBath"] = 0
test[which(is.na(test$TotalBsmtSF)),"BsmtHalfBath"] = 0
test[which(is.na(test$TotalBsmtSF)),"TotalBsmtSF"] = 0


table(test$BsmtQual)
test[which(is.na(test$BsmtQual)),"BsmtQual"] = "TA"
test[which(is.na(test$BsmtExposure)),"BsmtExposure"] = "No"

pred <- c("BsmtExposure", "BsmtCond", "BsmtQual","BsmtFinType1", "BsmtFinType2","TotalBsmtSF","YearBuilt")
BsmtCond.rpart <- rpart(as.factor(BsmtCond) ~ .,
                        data = test[!is.na(test$BsmtCond),pred], 
                        method = "class", 
                        na.action=na.omit)

test$BsmtCond[is.na(test$BsmtCond)] <- as.character(predict(BsmtCond.rpart, 
                                                        test[is.na(test$BsmtCond),pred], 
                                                        type="class"))

test[which(is.na(test$BsmtFullBath)),"BsmtFullBath"] = 0
test[which(is.na(test$BsmtHalfBath)),"BsmtHalfBath"] = 0


test$BsmtQual = as.factor(test$BsmtQual)
test$BsmtCond = as.factor(test$BsmtCond)
test$BsmtExposure = as.factor(test$BsmtExposure)
test$BsmtFinType1 = as.factor(test$BsmtFinType1)
test$BsmtFinType2 = as.factor(test$BsmtFinType2)


#MSZONING

pred = c("Neighborhood", "Condition1", "Condition2", "MSZoning")
rpart = rpart(as.factor(MSZoning) ~ .,
                   data = test[!is.na(test$MSZoning),pred],
                   method = "class",
                   na.action=na.omit)

test$MSZoning[is.na(test$MSZoning)] <- as.character(predict(rpart, test[is.na(test$MSZoning),pred], type = "class"))


#LOTFRONTAGE
# Predictors
pred <- c("MSSubClass", "MSZoning", "LotFrontage", "LotArea", "Street", "Alley", "LotShape", "LandContour", "LotConfig", "LandSlope", "BldgType", "HouseStyle", "YrSold", "SaleType", "SaleCondition")

# Predict LotFrontage
rpart <- rpart(LotFrontage ~ .,
                       data = train[!is.na(train$LotFrontage),pred],
                       method = "anova",
                       na.action=na.omit)

train$LotFrontage[is.na(train$LotFrontage)] <- ceiling(predict(rpart, train[is.na(train$LotFrontage),pred]))

rpart <- rpart(LotFrontage ~ .,
               data = test[!is.na(test$LotFrontage),pred],
               method = "anova",
               na.action=na.omit)

test$LotFrontage[is.na(test$LotFrontage)] <- ceiling(predict(rpart, test[is.na(test$LotFrontage),pred]))

#MASONARY VENEER
train$MasVnrArea[is.na(train$MasVnrArea)] = 0 
train$MasVnrType[is.na(train$MasVnrType) & train$MasVnrArea == 0] = "None"

test$MasVnrArea[is.na(test$MasVnrArea)] = 0 
test$MasVnrType[is.na(test$MasVnrType) & test$MasVnrArea == 0] = "None"

which((test$MasVnrArea==0) & (test$MasVnrType!="None"))
which(train$MasVnrArea==0 & train$MasVnrType!="None")

test[which((test$MasVnrArea==0) & (test$MasVnrType!="None")),"MasVnrType"] = "None"
train[which(train$MasVnrArea==0 & train$MasVnrType!="None"),"MasVnrType"] = "None"

test[which(test$MasVnrType=="None" & test$MasVnrArea!=0),"MasVnrType"] = NA 
train[which(train$MasVnrType=="None" & train$MasVnrArea!=0),"MasVnrType"] = NA 

table(train$MasVnrArea[train$MasVnrType == "None"])
#Converting Area 1 to 0
train$MasVnrArea <- ifelse(train$MasVnrArea == 1,0,train$MasVnrArea)
test$MasVnrArea <- ifelse(test$MasVnrArea == 1,0,test$MasVnrArea)

#Predicting Masonary Veneer Type for remaining NA's
rpart <- rpart(as.factor(MasVnrType) ~ MasVnrArea,
                    data = train[!is.na(train$MasVnrType),c("MasVnrType","MasVnrArea")], 
                    method = "class", 
                    na.action=na.omit)

train$MasVnrType[is.na(train$MasVnrType)] <- as.character(predict(rpart,train[is.na(train$MasVnrType),c("MasVnrType","MasVnrArea")],
                                                            type="class")) 
rpart <- rpart(as.factor(MasVnrType) ~ MasVnrArea,
               data = test[!is.na(test$MasVnrType),c("MasVnrType","MasVnrArea")], 
               method = "class", 
               na.action=na.omit)

test$MasVnrType[is.na(test$MasVnrType)] <- as.character(predict(rpart,test[is.na(test$MasVnrType),c("MasVnrType","MasVnrArea")],
                                                                type="class")) 

#ELECTRICAL
pred = c("BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "Electrical")
# Predict Electrical
rpart = rpart(as.factor(Electrical) ~ .,
                    data = train[!is.na(train$Electrical),pred],
                    method = "class",na.action=na.omit)

train$Electrical[is.na(train$Electrical)] <- as.character(predict(rpart, train[is.na(train$Electrical),pred], type = "class"))


#FUNCTIONAL
#Data description says to assume typical
test[which(is.na(test$Functional)),"Functional"] = "Typ"

#EXTERIOR
pred <- c("BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd", "MasVnrType", "MasVnrArea", "ExterQual", "ExterCond")

# Predict Exterior1st
rpart = rpart(as.factor(Exterior1st) ~ .,
                    data = test[!is.na(test$Exterior1st),pred],
                    method = "class",
                    na.action=na.omit)

test$Exterior1st[is.na(test$Exterior1st)] = as.character(predict(rpart, test[is.na(test$Exterior1st),pred], type = "class"))

# Predict Exterior2nd
rpart = rpart(as.factor(Exterior2nd) ~ .,
                    data = test[!is.na(test$Exterior2nd),pred],
                    method = "class",
                    na.action=na.omit)

test$Exterior2nd[is.na(test$Exterior2nd)] = as.character(predict(rpart, test[is.na(test$Exterior2nd),pred], type = "class"))

#KITCHENQUAL

pred = c("BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "KitchenQual")

# Predict KitchenQual
rpart = rpart(as.factor(KitchenQual) ~ .,
                   data = test[!is.na(test$KitchenQual),pred],
                   method = "class",
                   na.action=na.omit)

test$KitchenQual[is.na(test$KitchenQual)] = as.character(predict(rpart, test[is.na(test$KitchenQual),pred], type = "class"))

#UTILITIES
table(test$Utilities)
table(train$Utilities)
#We see all have AllPub utilities
test$Utilities[is.na(test$Utilities)] = "AllPub"

#SALETYPE
#Setting sale type as other
test$SaleType[is.na(test$SaleType)] = "Other"

sort(apply(train,2, function(x)sum(is.na(x))),decreasing = TRUE)
sort(apply(test,2, function(x)sum(is.na(x))),decreasing = TRUE)

#SUCCESFULLY REMOVED ALL NA'S FROM THE OBSERVATIONS

#Now Formatting
#GarageYrBlt has 98 factor levels, so converting into numeric
train$GarageYrBlt = as.numeric(train$GarageYrBlt)
test$GarageYrBlt = as.numeric(test$GarageYrBlt)

#Converting character types into factors
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], 
                                       as.factor)
test[sapply(test, is.character)] <- lapply(test[sapply(test, is.character)], 
                                             as.factor)

str(train)
str(test)

library(randomForest)
features <- rbind(train[,-c(1, ncol(train))], test[, -1])
features.train <- features[1:nrow(train),]
features.test <- features[(nrow(train)+1):nrow(features),]

#Converting prices to log for uniformity
LogPrice = log10(train$SalePrice)
simplemodel = randomForest(LogPrice ~ ., data = features.train)
impo = importance(simplemodel)
pimpo = as.data.frame(impo)

importance_df = data.frame(row.names(pimpo), pimpo[, 1])

LogPrice.test = predict(simplemodel, newdata = features.test)

SalePrice = 10.0**LogPrice.test
Id = test$Id

final_prediction = data.frame(Id, SalePrice)
max(final_prediction$SalePrice)
min(final_prediction$SalePrice)

write.csv(final_prediction, "final_prediction.csv", row.names = F)
