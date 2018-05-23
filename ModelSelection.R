# Read prepared data
bcData = read.csv(paste0('https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/',
                         'master/data-imputed-transformed/train_BC_transformed.csv'))
bcData$X = NULL
imputedData = read.csv(paste0('https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/',
                              'master/data-imputed-transformed/train_orig_vars_imputed.csv'))
imputedData$X = NULL
transformedData = read.csv(paste0('https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/',
                                  'master/data-imputed-transformed/train_predictors_transformed.csv'))
transformedData$X = NULL

library(psych)
describe(bcData)

m1BC = lm(data=bcData,formula =SalePrice_BC~. )
m1IMP = lm(data = imputedData, formula = SalePrice~.)
anova(m1IMP,m1TD)
m1TD = lm(data=transformedData,formula = SalePrice~.)
m2BCstep =step(m1BC,direction = 'backward', trace=0)
summary(m2BCstep)
m3BC = lm(data = bcData, formula = SalePrice_BC~OverallCond+Condition2+Condition1+
            Neighborhood+MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+KitchenQual+
            Fireplaces +ScreenPorch+House_Age_Yrs+RoofMatl_WdShngl+
            GarageQual_abv_avg +OverallQual2_x_GrLivArea+
            OverallQual2_x_TotRmsAbvGrd_log+OverallQual2_x_GarageCars) 
m4BC = lm(data = bcData, formula = SalePrice_BC~OverallCond+Condition2+Condition1+
           Neighborhood+MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+KitchenQual+
           Fireplaces+WoodDeckSF+Functional+FullBath+BsmtFullBath+BsmtFinType1+
           BsmtExposure +BsmtQual +LandSlope +LandContour+LotArea +LotFrontage+ 
           LotConfig + Utilities + HouseStyle + RoofStyle + MasVnrArea +
           ScreenPorch+House_Age_Yrs+RoofMatl_WdShngl+GarageQual_abv_avg +
           OverallQual2_x_GrLivArea+OverallQual2_x_TotRmsAbvGrd_log+
           OverallQual2_x_GarageCars) 
m5imp = lm(data = imputedData, formula = log(SalePrice)~OverallCond+Condition2+
             Condition1+Neighborhood+MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+
             KitchenQual+(Fireplaces)^2+WoodDeckSF+Functional+FullBath+
             BsmtFullBath+BsmtFinType1 + BsmtExposure +BsmtQual +LandSlope +
             LandContour+log(LotArea) + LotFrontage+ LotConfig + Utilities + 
             HouseStyle + RoofStyle + MasVnrArea +ScreenPorch+House_Age_Yrs) 
m6TD = lm(data = transformedData, formula = log(SalePrice)~OverallCond+Condition2+
            Condition1+Neighborhood+MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+
            KitchenQual+Fireplaces+WoodDeckSF+Functional+FullBath+BsmtFullBath+
            BsmtFinType1 + BsmtExposure +BsmtQual +LandSlope +LandContour+
            log(LotArea) + LotFrontage+ LotConfig + Utilities + HouseStyle + 
            RoofStyle + MasVnrArea +ScreenPorch+House_Age_Yrs+RoofMatl_WdShngl+
            GarageQual_abv_avg +OverallQual2_x_GrLivArea+
            OverallQual2_x_TotRmsAbvGrd_log+OverallQual2_x_GarageCars)  

# Get AIC
AIC (m1BC, m2BCstep, m3BC, m4BC, m5imp, m6TD)

summary(m1BC)
summary(m2BCstep)
summary(m3BC)
summary(m4BC)
summary(m5imp)
summary(m6TD)

# Read test data
transformedTest = read.csv(paste0('https://raw.githubusercontent.com/kaiserxc/DATA621FinalProject/',
                                  'master/data-imputed-transformed/test_predictors_transformed.csv'))
index <- transformedTest$X
transformedTest$X <- NULL

# Tune model and run prediction
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model_fit <- train(log(SalePrice)~OverallCond+Condition2+
                     Condition1+Neighborhood+MSZoning +X1stFlrSF+X2ndFlrSF+LowQualFinSF+
                     KitchenQual+Fireplaces+WoodDeckSF+Functional+FullBath+BsmtFullBath+
                     BsmtFinType1 + BsmtExposure +BsmtQual +LandSlope +LandContour+
                     log(LotArea) + LotFrontage+ LotConfig + Utilities + HouseStyle + 
                     RoofStyle + MasVnrArea +ScreenPorch+House_Age_Yrs+RoofMatl_WdShngl+
                     GarageQual_abv_avg +OverallQual2_x_GrLivArea+
                     OverallQual2_x_TotRmsAbvGrd_log+OverallQual2_x_GarageCars,  
                   data=transformedData, method="lm", trControl = ctrl, tuneLength = 5)
pred <- predict(model_fit, newdata=transformedTest)
results <- cbind(index, exp(pred))
write.csv(results, "c://temp//results_tune.csv", row.names = FALSE)

summary(model_fit)
