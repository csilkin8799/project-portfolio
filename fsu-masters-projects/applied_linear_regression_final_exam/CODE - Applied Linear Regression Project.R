# ANALYSIS 1:

## Load data:
ames_neighborhoods = read.csv("ames_neighborhoods.csv")

## Load packages:
library(lmtest)
library(MASS)
library(olsrr)
library(corrplot)

## QUESTION 1:

### OLS Model:
a1_q1_ols_model = lm(SalePrice~GrLivArea, data = ames_neighborhoods)

#### OLS Model Shapiro-Wilk Test:
shapiro.test(resid(a1_q1_ols_model))

#### Q-Q Plot:
ols_plot_resid_qq(a1_q1_ols_model)

#### OLS Model Breusch-Pagan Test:
bptest(a1_q1_ols_model)

#### Residuals vs. Fitted Values Plot:
ols_plot_resid_fit(a1_q1_ols_model)

### Find highly influential points (using Cook's Distance)
which(cooks.distance(a1_q1_ols_model) > 4/length(resid(a1_q1_ols_model)))

### Re-fit model w/ noninfluential points and diagnose
a1_q1_noninf_ids = which(cooks.distance(a1_q1_ols_model) <= 4/length(resid(a1_q1_ols_model)))

a1_q1_ols_model_fixed = lm(SalePrice~GrLivArea, data = ames_neighborhoods, subset = a1_q1_noninf_ids)

#### Hypothesis tests and diagnostic plots for fixed model:
shapiro.test(resid(a1_q1_ols_model_fixed))

bptest(a1_q1_ols_model_fixed)

ols_plot_resid_fit(a1_q1_ols_model)

### BoxCox:
a1_e1_boxcox = boxcox(a1_q1_ols_model, lambda=seq(0.25,1.25,by=0.05), plotit=TRUE)

#### Find lambda-hat:
a1_e1_boxcox$x[which.max(a1_e1_boxcox$y)]

#### Load get_lambda_ci function:
get_lambda_ci = function(bc, level = 0.95) {
  # lambda such that 
  # L(lambda) > L(hat(lambda)) - 0.5 chisq_{1, alpha}
  CI_values = a1_e1_boxcox$x[a1_e1_boxcox$y > max(a1_e1_boxcox$y) - qchisq(level, 1)/2]
  
  # 95 % CI 
  CI <- range(CI_values) 
  
  # label the columns of the CI
  names(CI) <- c("lower bound","upper bound")
  
  CI
}

#### extract the 95% CI from the box cox object
get_lambda_ci(a1_e1_boxcox)

### Transformed model:
a1_q1_sqrt_model = lm(SalePrice~sqrt(GrLivArea), data=ames_neighborhoods)

#### Shapiro-Wilk Test:
shapiro.test(resid(a1_q1_sqrt_model))

#### Q-Q Plot:
ols_plot_resid_qq(a1_q1_sqrt_model)

#### Breusch-Pagan Test:
bptest(a1_q1_sqrt_model)

#### Residuals vs. Fitted Values Plot:
ols_plot_resid_fit(a1_q1_sqrt_model)

#### Find highly influential points
which(cooks.distance(a1_q1_sqrt_model) > 4/length(resid(a1_q1_sqrt_model)))

#### Re-fit model w/ noninfluential points and diagnose
a1_q1_sqrt_noninf_ids = which(cooks.distance(a1_q1_sqrt_model) <= 4/length(resid(a1_q1_sqrt_model)))

a1_q1_sqrt_model_fixed = lm(SalePrice~sqrt(GrLivArea), data = ames_neighborhoods, subset = a1_q1_sqrt_noninf_ids)

shapiro.test(resid(a1_q1_sqrt_model_fixed))

bptest(a1_q1_sqrt_model_fixed)

#### Model summary of final model (OLS model excluding outliers):
summary(a1_q1_ols_model_fixed)

## QUESTION 2:

### fit the OLS model of |e_i| ~ predictors.
model_wts = lm(abs(resid(a1_q1_ols_model)) ~ . - SalePrice, data = ames_neighborhoods)

### extract the coefficient estimates.
coef(model_wts)

### calculate the weights as 1 / (fitted values)^2
weights = 1 / fitted(model_wts)^2

### run WLS
a1_q2_model_wls = lm(SalePrice~GrLivArea, data = ames_neighborhoods, weights = weights)

### Fitted vs. residual plots for OLS and WLS
par(mfrow = c(1, 2))

#### OLS fitted-vs-residual plot
plot(fitted(a1_q1_ols_model),
     resid(a1_q1_ols_model), 
     pch = 20,
     xlab = 'Fitted Value', ylab = 'Residual', main = 'OLS')

abline(h=0, lwd=3, col='steelblue')

#### WLS fitted-vs-residual plot
plot(fitted(a1_q2_model_wls), weighted.residuals(a1_q2_model_wls), 
     pch = 20, ylim = c(-10, 15),
     xlab = 'Fitted Value', ylab = 'Weighted Residual',  main = 'WLS')
abline(h=0, lwd=3, col='steelblue')

#### Model summary:
summary(a1_q2_model_wls)

#### Model diagnostic plots and hypothesis tests:
shapiro.test(resid(a1_q2_model_wls))

ols_plot_resid_qq(a1_q2_model_wls)

bptest(a1_q2_model_wls)

## QUESTION 3:

### Model summaries for WLS and fixed OLS models:
summary(a1_q1_ols_model_fixed)
summary(a1_q2_model_wls)

### Scatterplot of y vs. x:
plot(SalePrice~GrLivArea, data = ames_neighborhoods, main = "Scatterplot of Sale Price vs. Living Area", ylab = "Sale Price (dollars)", xlab = "Above grade (ground) living area (sq. ft.)", col = "blue", pch = 16)
abline(a1_q2_model_wls, col = "red")

### Confidence intervals:
confint(a1_q2_model_wls, level = 0.95)

# ANALYSIS 2:

## Load data:
ames_mlr = read.csv("ames_mlr.csv")

## Fit assigned model:
a2_model = lm(log(SalePrice)~TotalBsmtSf + GrLivArea + FirstFlrSf + SecondFlrSf + GarageCars + GarageArea + Fireplaces + ScreenPorch, data = ames_mlr)

## QUESTION 1:

### Collinearity checks w/ original model:
ames_mlr_preds = dplyr::select(ames_mlr,c(TotalBsmtSf, GrLivArea, FirstFlrSf, SecondFlrSf, GarageCars, GarageArea, Fireplaces, ScreenPorch))

corrplot::corrplot(cor(ames_mlr_preds),
                   method = 'color', order = 'hclust',  
                   diag = FALSE, number.digits = 3, 
                   addCoef.col = 'black', tl.pos= 'd', 
                   cl.pos ='r')

faraway::vif(a2_model)

### Variable combos:
a2_model_vcomb = lm(log(SalePrice)~TotalBsmtSf + GrLivArea + I(FirstFlrSf + SecondFlrSf) + GarageCars + GarageArea + Fireplaces + ScreenPorch, data = ames_mlr)
faraway::vif(a2_model_vcomb)

a2_model_vcomb_2 = lm(log(SalePrice)~TotalBsmtSf + I((FirstFlrSf + SecondFlrSf)/GrLivArea) + GarageCars + GarageArea + Fireplaces + ScreenPorch, data = ames_mlr)
faraway::vif(a2_model_vcomb_2)
summary(a2_model_vcomb_2)

a2_model_vcomb_3 = lm(log(SalePrice)~TotalBsmtSf + I((FirstFlrSf + SecondFlrSf)/GrLivArea) + I(GarageCars/GarageArea) + Fireplaces + ScreenPorch, data = ames_mlr)
faraway::vif(a2_model_vcomb_3)
summary(a2_model_vcomb_3)

### PCR:
library(pls) #### Load "pls" package

a2_pcr_model = pcr(log(SalePrice)~TotalBsmtSf + GrLivArea + FirstFlrSf + SecondFlrSf + GarageCars + GarageArea + Fireplaces + ScreenPorch, data = ames_mlr, scale = TRUE, 
                   validation = 'CV')

summary(a2_pcr_model)

which.min(RMSEP(a2_pcr_model)$val[1,1, ]) - 1

#### percentage of variance explained
pve = a2_pcr_model$Xvar / a2_pcr_model$Xtotvar

#### scree plot
plot(pve, type='b', pch=20, lwd=2, 
     xlab = 'Principal Component', 
     ylab = 'Percentage of Variance Explained')

#### Extract y-int and slope coefficients from PCR model
a2_pcr_model$Ymeans
a2_pcr_model$Yloadings[1:7]

## QUESTION 2:

### Restricted model:
a2_q2_model = lm(log(SalePrice) ~ TotalBsmtSf + GrLivArea + FirstFlrSf + GarageCars + Fireplaces + ScreenPorch, data = ames_mlr)

### Full model (all 26 coefficients):
a2_q2_full_model = lm(log(SalePrice)~.,data=ames_mlr)

### Nested model F-test:
anova(a2_q2_model, a2_q2_full_model)

### Model summary:
summary(a2_q2_model)

## QUESTION 3:

### Model assumptions check:

#### OLS Model Shapiro-Wilk Test:
shapiro.test(resid(a2_q2_model))

#### Q-Q Plot:
ols_plot_resid_qq(a2_q2_model)

#### OLS Model Breusch-Pagan Test:
bptest(a2_q2_model)

#### Residuals vs. Fitted Values Plot:
ols_plot_resid_fit(a2_q2_model)

### LAD Regression:
library(quantreg) #### Load "quantreg" package

a2_q3_lad_model = rq(log(SalePrice) ~ TotalBsmtSf + GrLivArea + FirstFlrSf + GarageCars + Fireplaces + ScreenPorch, data = ames_mlr)

summary(a2_q3_lad_model)

#### LAD Model Shapiro-Wilk Test:
shapiro.test(resid(a2_q3_lad_model))

#### LAD Model Breusch-Pagan Test:
bptest(a2_q3_lad_model)

### Huber's method:
library(MASS) #### Load "MASS" package

a2_q3_hub_model = rlm(log(SalePrice) ~ TotalBsmtSf + GrLivArea + FirstFlrSf + GarageCars + Fireplaces + ScreenPorch, data = ames_mlr)

#### Set seed to 42:
set.seed(42)

library(car) #### Load "car" package

#### Bootstrap:
Confint(Boot(a2_q3_hub_model, R = 2000, method = 'residual'))

#### Huber's Method Shapiro-Wilk Test:
shapiro.test(resid(a2_q3_hub_model))

#### Huber's Method Breusch-Pagan Test:
bptest(a2_q3_hub_model)

# ANALYSIS 3, QUESTION 1:

## Load data sets:
ames_train = read.csv("ames_train.csv")
ames_test = read.csv("ames_test.csv")


## Selection Procedures:

### Forward selection
a3_mod_start = lm(log(SalePrice) ~ 1, data=ames_train)

#### AIC
a3_mod_fwd_aic = step(
  a3_mod_start,
  scope = log(SalePrice) ~ LotArea + OverallQual + 
    OverallCond + YearBuilt +YearRemodAdd + 
    BsmtFinSf1 + BsmtFinSf2 + TotalBsmtSf + 
    FirstFlrSf + SecondFlrSf + GrLivArea + 
    BsmtFullBath + BsmtHalfBath + FullBath + 
    HalfBath + BedroomAbvGr + KitchenAbvGr + 
    TotRmsAbvGrd + Fireplaces + GarageCars + 
    GarageArea + WoodDeckSf + OpenPorchSf + 
    EnclosedPorch + ThreeSsnPorch + ScreenPorch,
  direction = 'forward')

coef(a3_mod_fwd_aic)

#### BIC
a3_n = nrow(ames_train)

a3_mod_fwd_bic = step(
  a3_mod_start,
  scope = log(SalePrice) ~ LotArea + OverallQual + 
    OverallCond + YearBuilt +YearRemodAdd + 
    BsmtFinSf1 + BsmtFinSf2 + TotalBsmtSf + 
    FirstFlrSf + SecondFlrSf + GrLivArea + 
    BsmtFullBath + BsmtHalfBath + FullBath + 
    HalfBath + BedroomAbvGr + KitchenAbvGr + 
    TotRmsAbvGrd + Fireplaces + GarageCars + 
    GarageArea + WoodDeckSf + OpenPorchSf + 
    EnclosedPorch + ThreeSsnPorch + ScreenPorch,
  direction = 'forward',
  k = log(a3_n))

coef(a3_mod_fwd_bic)

### Backward selection
a3_mod_all_preds = lm(log(SalePrice) ~ ., data = ames_train)

#### AIC
a3_mod_back_aic = step(a3_mod_all_preds, direction = 'backward')

coef(a3_mod_back_aic)

#### BIC
a3_mod_back_bic = step(a3_mod_all_preds, direction = 'backward', k = log(a3_n))

coef(a3_mod_back_bic)

### Stepwise selection
a3_mod_start = lm(log(SalePrice) ~ 1, data=ames_train)

#### AIC
a3_mod_stepwise_aic = step(
  a3_mod_start,
  scope = log(SalePrice) ~ LotArea + OverallQual + 
    OverallCond + YearBuilt +YearRemodAdd + 
    BsmtFinSf1 + BsmtFinSf2 + TotalBsmtSf + 
    FirstFlrSf + SecondFlrSf + GrLivArea + 
    BsmtFullBath + BsmtHalfBath + FullBath + 
    HalfBath + BedroomAbvGr + KitchenAbvGr + 
    TotRmsAbvGrd + Fireplaces + GarageCars + 
    GarageArea + WoodDeckSf + OpenPorchSf + 
    EnclosedPorch + ThreeSsnPorch + ScreenPorch,
  direction = 'both')

#### BIC
a3_mod_stepwise_bic = step(
  a3_mod_start,
  scope = log(SalePrice) ~ LotArea + OverallQual + 
    OverallCond + YearBuilt +YearRemodAdd + 
    BsmtFinSf1 + BsmtFinSf2 + TotalBsmtSf + 
    FirstFlrSf + SecondFlrSf + GrLivArea + 
    BsmtFullBath + BsmtHalfBath + FullBath + 
    HalfBath + BedroomAbvGr + KitchenAbvGr + 
    TotRmsAbvGrd + Fireplaces + GarageCars + 
    GarageArea + WoodDeckSf + OpenPorchSf + 
    EnclosedPorch + ThreeSsnPorch + ScreenPorch,
  direction = 'both',
  k = log(a3_n))

### Compare RMSE's (w.r.t training data)
calc_loocv_rmse(a3_mod_fwd_aic)
calc_loocv_rmse(a3_mod_fwd_bic)
calc_loocv_rmse(a3_mod_back_aic)
calc_loocv_rmse(a3_mod_back_bic)
calc_loocv_rmse(a3_mod_stepwise_aic)
calc_loocv_rmse(a3_mod_stepwise_bic)

### Compare adj. R^2
summary(a3_mod_fwd_aic)$adj.r.squared
summary(a3_mod_fwd_bic)$adj.r.squared
summary(a3_mod_back_aic)$adj.r.squared
summary(a3_mod_back_bic)$adj.r.squared
summary(a3_mod_stepwise_aic)$adj.r.squared
summary(a3_mod_stepwise_bic)$adj.r.squared

### FINAL MODEL: Backward selection using AIC

## Best Subset Selection:

library(leaps)

a3_mod_exhaustive = summary(regsubsets(log(SalePrice) ~ ., data = ames_train))

p = ncol(a3_mod_exhaustive$which)

### AIC
a3_bs_aic = a3_n * log(a3_mod_exhaustive$rss / a3_n) + 2 * (2:p)

best_aic_ind = which.min(a3_bs_aic)

a3_mod_exhaustive$which[best_aic_ind,]

a3_mod_exhaust_aic = lm(log(SalePrice) ~ LotArea + OverallQual + OverallCond + YearBuilt + BsmtFinSf1 + FirstFlrSf + GrLivArea + GarageArea, data = ames_train)

### BIC
n = nrow(ames_train)
p = ncol(ames_train)

a3_bs_bic = n * log(a3_mod_exhaustive$rss / n) + log(n) * (2:p)

a3_bs_bic

best_bic_ind = which.min(a3_bs_bic)

a3_mod_exhaustive$which[best_bic_ind,]

a3_mod_exhaust_bic = lm(log(SalePrice) ~ LotArea + OverallQual + OverallCond + YearBuilt + BsmtFinSf1 + FirstFlrSf + GrLivArea + GarageArea, data = ames_train)

### R^2
best_r2_ind = which.max(a3_mod_exhaustive$adjr2)

a3_mod_exhaustive$which[best_r2_ind,]

a3_bs_model_final = lm(log(SalePrice) ~ LotArea + OverallQual + OverallCond + YearBuilt + BsmtFinSf1 + FirstFlrSf + GrLivArea + GarageArea, data = ames_train)

## PCR:
a3_mod_pcr = pcr(log(SalePrice) ~., data = ames_train, scale = TRUE, validation = 'CV')

### Scree Plot:
pve = a3_mod_pcr$Xvar / a3_mod_pcr$Xtotvar

plot(pve, type='b', pch=20, lwd=2, 
     xlab = 'Principal Component', 
     ylab = 'Percentage of Variance Explained')

a3_mod_pcr$Ymeans
a3_mod_pcr$Yloadings[1:5]

## Ridge Regression:
library(lmridge) ### Load "lmridge" package

### Create grid
grid = 10 ^ seq(0.25, 1.75 , length = 100)

### Fit ridge model:
a3_mod_ridge = lmridge(log(SalePrice) ~., data = ames_train, scaling = 'scaled', K = grid)

### Get estimated K value:
k_est = kest(a3_mod_ridge)

### a plot of GCV vs. log10(lambda) 
plot(log10(a3_mod_ridge$K), k_est$GCV, type = 'l', lwd = 2,
     xlab = expression(log[10](lambda)), ylab = 'GCV')

points(log10(a3_mod_ridge$K), k_est$GCV, 
       pch = 19, col = 'steelblue', cex = 0.75)

#### horizontal line at log10(kGCV), i.e., the base 10 logarithm of the best lambda value
abline(v=log10(k_est$kGCV), lty = 'dashed', col = 'grey',
       lwd = 2)

### Get best K value and modify model:
k_best = k_est$kGCV
a3_mod_ridge_best = lmridge(log(SalePrice) ~., data = ames_train, scaling = 'scaled', K = k_best)

## Lasso:
library(glmnet) ### Load "glmnet" package

### Obtain x_train and y_train
x_train = model.matrix(log(SalePrice) ~., data = ames_train)[,-1]
y_train = log(ames_train$SalePrice)

### Create lasso
a3_mod_lasso = cv.glmnet(x_train, y_train)

### Create lasso plot:
plot(a3_mod_lasso)
grid = exp(seq(-7,-4,length = 100))
a3_mod_lasso = cv.glmnet(x_train, y_train, lambda = grid)
plot(a3_mod_lasso)

### Get lambda.min and lambda.1se:
a3_mod_lasso$lambda.min
a3_mod_lasso$lambda.1se

### Get model coefficients:
coef(a3_mod_lasso, s = 'lambda.min')
coef(a3_mod_lasso, s = 'lambda.1se')

# RMSE's:

## Analysis 1 Model:
a1_pred = predict(a1_q2_model_wls, ames_test)
sqrt(mean((log(ames_test$SalePrice) - a1_pred)^2))

## Analysis 2, Question 3 Model:
a2_q3_pred = predict(a2_q3_hub_model, ames_test)
sqrt(mean((log(ames_test$SalePrice) - a2_q3_pred)^2))

## AIC Backward Selection:
a3_back_aic_pred = predict(a3_mod_back_aic, ames_test)
sqrt(mean((log(ames_test$SalePrice) - a3_back_aic_pred)^2))

## Best Subset Selection:
a3_bs_pred = predict(a3_bs_model_final, ames_test)
sqrt(mean((log(ames_test$SalePrice) - a3_bs_pred)^2))

## PCR:
a3_pcr_pred = predict(a3_mod_pcr, ames_test, ncomp = 5)
sqrt(mean((log(ames_test$SalePrice) - a3_pcr_pred)^2))

## Ridge Regression:
ridge_pred = predict(a3_mod_ridge_best, newdata = ames_test)
sqrt(mean((log(ames_test$SalePrice) - ridge_pred)^2))

## Lasso:
x_test = model.matrix(log(SalePrice) ~., data = ames_test)[,-1]

lmin_ridge_pred = predict(a3_mod_lasso, newx = x_test, s = 'lambda.min')
sqrt(mean((log(ames_test$SalePrice) - lmin_ridge_pred)^2))

l1se_ridge_pred = predict(a3_mod_lasso, newx = x_test, s = 'lambda.1se')
sqrt(mean((log(ames_test$SalePrice) - l1se_ridge_pred)^2))

# ANALYSIS 3, QUESTION 2:

## Model summaries of best subset selection (from Question 1 -- had least # of predictors)
summary(a3_bs_model_final)

## First model:
a3_q2_model1 = lm(formula = log(SalePrice) ~ LotArea + OverallQual + OverallCond + YearBuilt + BsmtFinSf1 + I(FirstFlrSf/GrLivArea) + GarageArea,
                  data = ames_train)
summary(a3_q2_model1)

### RMSE:
mod1_preds = predict(a3_q2_model1, ames_test)
sqrt(mean((log(ames_test$SalePrice) - mod1_preds)^2))

## Second model:
a3_q2_model2 = lm(formula = log(SalePrice) ~ LotArea +
                    OverallCond + YearBuilt + BsmtFinSf1 + FirstFlrSf + GrLivArea + GarageArea,
                  data = ames_train)
summary(a3_q2_model2)

### RMSE:
mod2_preds = predict(a3_q2_model2, ames_test)
sqrt(mean((log(ames_test$SalePrice) - mod2_preds)^2))

## Third model:
a3_q2_model3 = lm(formula = log(SalePrice) ~ OverallQual + OverallCond + YearBuilt + I(BsmtFinSf1+BsmtFinSf2) + FirstFlrSf + GrLivArea + I(LotArea + GarageArea),
                  data = ames_train)
summary(a3_q2_model3)

### RMSE:
mod3_preds = predict(a3_q2_model3, ames_test) 
sqrt(mean((log(ames_test$SalePrice) - mod3_preds)^2))
