
``` r
library(tidyverse)
```

    ## ─ Attaching packages ──────────────────────────────────────── tidyverse 1.3.0 ─

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ─ Conflicts ───────────────────────────────────────── tidyverse_conflicts() ─
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(dplyr)
library(gapminder)
library(car)
```

    ## Loading required package: carData

    ## 
    ## Attaching package: 'car'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode

    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following object is masked from 'package:car':
    ## 
    ##     logit

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
# Read in Data
insurance <- read.csv("insurance.csv")
insurance_ori <-read.csv("insurance.csv")

##add new data point
new.data <- data.frame(22,"male", 17.32,0,"yes","southwest", 16003.29)
names(new.data) <- c("age", "sex", "bmi", "children","smoker","region","charges")
insurance <- rbind(insurance, new.data)

##change to factor
insurance$smoker=ifelse(insurance$smoker=="yes",1,0) # yes=1 no=0
insurance$sex <- as.numeric(factor(insurance$sex)) #female=1 male=2
insurance$region <- as.numeric(factor(insurance$region))#southwest=4,southeast=3,northwest=2,northeast=1;
```

``` r
##hist
hist(insurance$age,xlab = "Age of Beneficiary",ylab = "Count",main = "Distribution of Age")
```

![](project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
hist(insurance$children,xlab = "Number of Children",ylab = "Count",main = "Distribution of Number Of Children Covered By Medical Insurance")
```

![](project_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
hist(insurance$charges,xlab = "The cost of medical insurance",ylab = "count",main = "Distribution of insurance cost")
```

![](project_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

``` r
summary(insurance$charges)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1122    4742    9386   13272   16622   63770

``` r
hist(insurance$bmi,xlab = "BMI",ylab = "Count",main = "Distribution of BMI")
```

![](project_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

``` r
summary(insurance$bmi)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   15.96   26.25   30.40   30.65   34.69   53.13

``` r
##bar chart
barchart(insurance_ori$smoker,col="black",xlab="Count",main="Bar chart of Smoker and Non-Smoker")
```

![](project_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

``` r
barchart(insurance_ori$region,col="black",xlab="Count",main="Bar chart of Region")
```

![](project_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->

``` r
#step1
#check missing value
mean(is.na(insurance))  # no missing value
```

    ## [1] 0

``` r
##indicator with age charges smoker
ggplot(insurance_ori,aes(x=age,y=charges,color=smoker)) +
  geom_point(aes(group=smoker))
```

![](project_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
mdl3=lm(charges~smoker,data=insurance)
mdl4=lm(charges~smoker+age+smoker*age,data=insurance)
print(anova(mdl3,mdl4))
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: charges ~ smoker
    ## Model 2: charges ~ smoker + age + smoker * age
    ##   Res.Df        RSS Df  Sum of Sq      F    Pr(>F)    
    ## 1   1337 7.4811e+10                                   
    ## 2   1335 5.4685e+10  2 2.0126e+10 245.66 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
fit1 <- lm(charges ~ ., data=insurance) ##FUll model
fit2 <- lm(charges ~ 1, data=insurance) ##Base model

##backward
step(fit1,direction="backward") ##smoker age bmi children region
```

    ## Start:  AIC=23330.83
    ## charges ~ age + sex + bmi + children + smoker + region
    ## 
    ##            Df  Sum of Sq        RSS   AIC
    ## - sex       1 6.4857e+06 4.8919e+10 23329
    ## <none>                   4.8912e+10 23331
    ## - region    1 2.0765e+08 4.9120e+10 23334
    ## - children  1 4.5206e+08 4.9364e+10 23341
    ## - bmi       1 5.3647e+09 5.4277e+10 23468
    ## - age       1 1.7253e+10 6.6166e+10 23733
    ## - smoker    1 1.2294e+11 1.7185e+11 25011
    ## 
    ## Step:  AIC=23329.01
    ## charges ~ age + bmi + children + smoker + region
    ## 
    ##            Df  Sum of Sq        RSS   AIC
    ## <none>                   4.8919e+10 23329
    ## - region    1 2.0751e+08 4.9126e+10 23333
    ## - children  1 4.5040e+08 4.9369e+10 23339
    ## - bmi       1 5.3590e+09 5.4278e+10 23466
    ## - age       1 1.7282e+10 6.6201e+10 23732
    ## - smoker    1 1.2353e+11 1.7245e+11 25014

    ## 
    ## Call:
    ## lm(formula = charges ~ age + bmi + children + smoker + region, 
    ##     data = insurance)
    ## 
    ## Coefficients:
    ## (Intercept)          age          bmi     children       smoker       region  
    ##    -11565.5        257.7        333.8        481.9      23785.5       -360.8

``` r
##both
step(fit2,direction="both",scope=list(upper=fit1,lower=fit2)) ##smoker age bmi children region
```

    ## Start:  AIC=25178.04
    ## charges ~ 1
    ## 
    ##            Df  Sum of Sq        RSS   AIC
    ## + smoker    1 1.2127e+11 7.4811e+10 23890
    ## + age       1 1.7486e+10 1.7860e+11 25055
    ## + bmi       1 7.6573e+09 1.8842e+11 25127
    ## + children  1 9.0196e+08 1.9518e+11 25174
    ## + sex       1 6.4687e+08 1.9543e+11 25176
    ## <none>                   1.9608e+11 25178
    ## + region    1 7.0061e+06 1.9607e+11 25180
    ## 
    ## Step:  AIC=23889.82
    ## charges ~ smoker
    ## 
    ##            Df  Sum of Sq        RSS   AIC
    ## + age       1 2.0053e+10 5.4758e+10 23474
    ## + bmi       1 7.6252e+09 6.7186e+10 23748
    ## + children  1 7.7455e+08 7.4036e+10 23878
    ## <none>                   7.4811e+10 23890
    ## + region    1 6.6379e+06 7.4804e+10 23892
    ## + sex       1 2.4329e+06 7.4808e+10 23892
    ## - smoker    1 1.2127e+11 1.9608e+11 25178
    ## 
    ## Step:  AIC=23474
    ## charges ~ smoker + age
    ## 
    ##            Df  Sum of Sq        RSS   AIC
    ## + bmi       1 5.1898e+09 4.9568e+10 23343
    ## + children  1 4.7083e+08 5.4287e+10 23464
    ## <none>                   5.4758e+10 23474
    ## + region    1 7.3043e+06 5.4751e+10 23476
    ## + sex       1 1.5248e+06 5.4756e+10 23476
    ## - age       1 2.0053e+10 7.4811e+10 23890
    ## - smoker    1 1.2384e+11 1.7860e+11 25055
    ## 
    ## Step:  AIC=23342.67
    ## charges ~ smoker + age + bmi
    ## 
    ##            Df  Sum of Sq        RSS   AIC
    ## + children  1 4.4178e+08 4.9126e+10 23333
    ## + region    1 1.9888e+08 4.9369e+10 23339
    ## <none>                   4.9568e+10 23343
    ## + sex       1 4.7135e+06 4.9563e+10 23344
    ## - bmi       1 5.1898e+09 5.4758e+10 23474
    ## - age       1 1.7618e+10 6.7186e+10 23748
    ## - smoker    1 1.2366e+11 1.7323e+11 25016
    ## 
    ## Step:  AIC=23332.68
    ## charges ~ smoker + age + bmi + children
    ## 
    ##            Df  Sum of Sq        RSS   AIC
    ## + region    1 2.0751e+08 4.8919e+10 23329
    ## <none>                   4.9126e+10 23333
    ## + sex       1 6.3401e+06 4.9120e+10 23334
    ## - children  1 4.4178e+08 4.9568e+10 23343
    ## - bmi       1 5.1607e+09 5.4287e+10 23464
    ## - age       1 1.7352e+10 6.6479e+10 23736
    ## - smoker    1 1.2354e+11 1.7267e+11 25014
    ## 
    ## Step:  AIC=23329.01
    ## charges ~ smoker + age + bmi + children + region
    ## 
    ##            Df  Sum of Sq        RSS   AIC
    ## <none>                   4.8919e+10 23329
    ## + sex       1 6.4857e+06 4.8912e+10 23331
    ## - region    1 2.0751e+08 4.9126e+10 23333
    ## - children  1 4.5040e+08 4.9369e+10 23339
    ## - bmi       1 5.3590e+09 5.4278e+10 23466
    ## - age       1 1.7282e+10 6.6201e+10 23732
    ## - smoker    1 1.2353e+11 1.7245e+11 25014

    ## 
    ## Call:
    ## lm(formula = charges ~ smoker + age + bmi + children + region, 
    ##     data = insurance)
    ## 
    ## Coefficients:
    ## (Intercept)       smoker          age          bmi     children       region  
    ##    -11565.5      23785.5        257.7        333.8        481.9       -360.8

``` r
#validation check   continue select predictor

#loop
##full model
set.seed(1411)

RMSPEfull<-NULL;
Rsqfull<-NULL;
MAPEfull<-NULL;

for(i in 1:10){
  
  n=nrow(insurance)
  nsamp=ceiling(0.7*n)
  training_samps=sample(c(1:n),nsamp)
  training_samps=sort(training_samps)
  train_data  <- insurance[training_samps, ]
  test_data <-   insurance[-training_samps, ]
  
  train.lm_full <- lm(charges ~ smoker+age+bmi+children+region, data = train_data) ##smoker age bmi children region
  
  preds <- predict(train.lm_full,test_data)
  
  Rsqfull[i] = R2(preds, test_data$charges)
  RMSPEfull[i] = RMSE(preds, test_data$charges)
  MAPEfull[i] = MAE(preds, test_data$charges)
}
mean(Rsqfull) 
```

    ## [1] 0.7541579

``` r
mean(RMSPEfull)
```

    ## [1] 5993.825

``` r
mean(MAPEfull)
```

    ## [1] 4155.836

``` r
##remove region
set.seed(12341)
RMSPE_sabc<-NULL;
Rsq_sabc<-NULL;
MAPE_sabc<-NULL;

for(i in 1:10){
  
  n=nrow(insurance)
  nsamp=ceiling(0.7*n)
  training_samps=sample(c(1:n),nsamp)
  training_samps=sort(training_samps)
  train_data  <- insurance[training_samps, ]
  test_data <-   insurance[-training_samps, ]
  
  train.lm_sabc <- lm(charges ~ smoker+age+bmi+children, data = train_data) ##smoker age bmi children
  
  preds1 <- predict(train.lm_sabc,test_data)
  
  Rsq_sabc[i] = R2(preds1, test_data$charges)
  RMSPE_sabc[i] = RMSE(preds1, test_data$charges)
  MAPE_sabc[i] = MAE(preds1, test_data$charges)
}
mean(Rsq_sabc)   
```

    ## [1] 0.7432134

``` r
mean(RMSPE_sabc) 
```

    ## [1] 6194.076

``` r
mean(MAPE_sabc) 
```

    ## [1] 4271.347

``` r
set.seed(987)
##remove children
RMSPE_sabr<-NULL;
Rsq_sabr<-NULL;
MAPE_sabr<-NULL;

for(i in 1:10){
  
  n=nrow(insurance)
  nsamp=ceiling(0.7*n)
  training_samps=sample(c(1:n),nsamp)
  training_samps=sort(training_samps)
  train_data  <- insurance[training_samps, ]
  test_data <-   insurance[-training_samps, ]
  
  train.lm_sabr <- lm(charges ~ smoker+age+bmi+region, data = train_data) ##smoker age bmi region
  
  preds2 <- predict(train.lm_sabr,test_data)
  
  Rsq_sabr[i] = R2(preds2, test_data$charges)
  RMSPE_sabr[i] = RMSE(preds2, test_data$charges)
  MAPE_sabr[i] = MAE(preds2, test_data$charges)
}
mean(Rsq_sabr)  
```

    ## [1] 0.7518959

``` r
mean(RMSPE_sabr)
```

    ## [1] 6080.456

``` r
mean(MAPE_sabr)
```

    ## [1] 4230.362

``` r
set.seed(5876)
##remove bmi
RMSPE_sacr<-NULL;
Rsq_sacr<-NULL;
MAPE_sacr<-NULL;

for(i in 1:10){
  
  n=nrow(insurance)
  nsamp=ceiling(0.7*n)
  training_samps=sample(c(1:n),nsamp)
  training_samps=sort(training_samps)
  train_data  <- insurance[training_samps, ]
  test_data <-   insurance[-training_samps, ]
  
  train.lm_sacr <- lm(charges ~ smoker+age+children+region, data = train_data) ##smoker age children region
  
  preds3 <- predict(train.lm_sacr,test_data)
  
  Rsq_sacr[i] = R2(preds3, test_data$charges)
  RMSPE_sacr[i] = RMSE(preds3, test_data$charges)
  MAPE_sacr[i] = MAE(preds3, test_data$charges)
}
mean(Rsq_sacr)  
```

    ## [1] 0.7317303

``` r
mean(RMSPE_sacr) 
```

    ## [1] 6268.288

``` r
mean(MAPE_sacr)
```

    ## [1] 4078.475

``` r
##remove age
set.seed(512876)
RMSPE_sbcr<-NULL;
Rsq_sbcr<-NULL;
MAPE_sbcr<-NULL;

for(i in 1:10){
  
  n=nrow(insurance)
  nsamp=ceiling(0.7*n)
  training_samps=sample(c(1:n),nsamp)
  training_samps=sort(training_samps)
  train_data  <- insurance[training_samps, ]
  test_data <-   insurance[-training_samps, ]
  
  train.lm_sbcr <- lm(charges ~ smoker+bmi+children+region, data = train_data) ##smoker bmi children region
  
  preds4 <- predict(train.lm_sbcr,test_data)
  
  Rsq_sbcr[i] = R2(preds4, test_data$charges)
  RMSPE_sbcr[i] = RMSE(preds4, test_data$charges)
  MAPE_sbcr[i] = MAE(preds4, test_data$charges)
}
mean(Rsq_sbcr)   
```

    ## [1] 0.673594

``` r
mean(RMSPE_sbcr) 
```

    ## [1] 7054.189

``` r
mean(MAPE_sbcr) 
```

    ## [1] 5379.976

``` r
##remove smoker
set.seed(581476)
RMSPE_abcr<-NULL;
Rsq_abcr<-NULL;
MAPE_abcr<-NULL;

for(i in 1:10){
  
  n=nrow(insurance)
  nsamp=ceiling(0.7*n)
  training_samps=sample(c(1:n),nsamp)
  training_samps=sort(training_samps)
  train_data  <- insurance[training_samps, ]
  test_data <-   insurance[-training_samps, ]
  
  train.lm_abcr <- lm(charges ~ age+bmi+children+region, data = train_data) ##age bmi children region
  
  preds5 <- predict(train.lm_abcr,test_data)
  
  Rsq_abcr[i] = R2(preds5, test_data$charges)
  RMSPE_abcr[i] = RMSE(preds5, test_data$charges)
  MAPE_abcr[i] = MAE(preds5, test_data$charges)
}
mean(Rsq_abcr)   # 0.1213272
```

    ## [1] 0.1372045

``` r
mean(RMSPE_abcr) # 11314.121
```

    ## [1] 11434.38

``` r
mean(MAPE_abcr) #  8980.853
```

    ## [1] 9022.574

``` r
##final model: smoker age bmi children region
finalmdl<- lm(charges~smoker+age+bmi+children+region,data=insurance)
hist(resid(finalmdl),xlab = "Residuals",ylab="Count",main = "Distribution of Residuals") ##not normally distributed
```

![](project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
plot(finalmdl,which=c(1,2,5)) ##not linear
```

![](project_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->![](project_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->![](project_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

``` r
##Robust Regression
finalrrl= rlm(charges~smoker+age+bmi+children+region,data=insurance,psi=psi.huber)
```

    ## Warning in rlm.default(x, y, weights, method = method, wt.method = wt.method, :
    ## 'rlm' failed to converge in 20 steps

``` r
hist(resid(finalrrl),xlab = "Residuals",ylab="Count",main = "Distribution of Residuals") ##not normally distributed
```

![](project_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
plot(finalrrl,which=c(1,2,5))
```

![](project_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->![](project_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->![](project_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

``` r
##transformed

finalrrl_log=rlm(log(charges)~smoker+1/age+bmi+children+region,data=insurance,psi=psi.huber)

hist(resid(finalrrl_log),xlab = "Residuals",ylab="Count",main = "Distribution of Residuals") 
```

![](project_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
plot(finalrrl_log,which=c(1))
```

![](project_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
plot(finalrrl_log,which=c(2))
```

![](project_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
plot(finalrrl_log,which=c(5))
```

![](project_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->

``` r
vif(finalrrl_log) ##check multicollinearity
```

    ##   smoker      bmi children   region 
    ## 1.000040 1.024758 1.000427 1.024802
