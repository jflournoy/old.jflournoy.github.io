---
layout: post
comments: true
title: "Intro to Bootstrapping"
date: 2014-11-14 15:30:00 -0700
categories: R tutorials
---

*"It just works"*

All of the information you have about the population is in your sample -- you can learn something
about the 'empirical sampling distribution' of your parameters by estimating them in new samples drawn, with replacement, from
your sample. Replacement is crucial -- your bootstrap samples will be the same size
as your observed sample, but will be comprised of observations from your sample at a rate proportional to the frequency in the sample.
*Note:* There are several different methods for producing replicates.

<!--more-->

Here's a small example:


```r
N=50 # Number of observations

if (!require(ggplot2)){
  install.packages('ggplot2')
  require(ggplot2)
}

#small_sample<-rnorm(20,0,1)

small_sample<-rexp(N,1) #Let's start with a toy sample drawn from a skewed distribution 
qplot(small_sample)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-1](/../figs/bootstrapexample/boot-unnamed-chunk-1-1.svg) 

```r
(allSamples<-data.frame(values=small_sample,
                       data_source='Observed'))
```

```
##         values data_source
## 1  0.118190073    Observed
## 2  0.191254915    Observed
## 3  0.573657128    Observed
## 4  0.049878858    Observed
## 5  0.478252591    Observed
## 6  0.032529387    Observed
## 7  0.731684625    Observed
## 8  0.949505151    Observed
## 9  0.077770440    Observed
## 10 1.573731154    Observed
## 11 1.739742162    Observed
## 12 0.340228639    Observed
## 13 0.661564070    Observed
## 14 0.997529805    Observed
## 15 0.452988377    Observed
## 16 0.127448814    Observed
## 17 0.098811573    Observed
## 18 1.540961951    Observed
## 19 2.169414936    Observed
## 20 0.477430523    Observed
## 21 2.800425745    Observed
## 22 0.271493065    Observed
## 23 1.240326052    Observed
## 24 0.049801324    Observed
## 25 0.152666418    Observed
## 26 0.332869878    Observed
## 27 0.625473927    Observed
## 28 3.172465622    Observed
## 29 1.176631405    Observed
## 30 0.868410200    Observed
## 31 3.613057842    Observed
## 32 0.131073473    Observed
## 33 0.111732404    Observed
## 34 1.288310545    Observed
## 35 2.270119545    Observed
## 36 1.452499119    Observed
## 37 0.277177117    Observed
## 38 0.593812360    Observed
## 39 0.595079505    Observed
## 40 0.736135014    Observed
## 41 0.127048544    Observed
## 42 4.269269668    Observed
## 43 1.382583047    Observed
## 44 0.926887236    Observed
## 45 0.974405549    Observed
## 46 0.211162893    Observed
## 47 1.114404636    Observed
## 48 0.614059942    Observed
## 49 1.718071455    Observed
## 50 0.001886521    Observed
```

```r
if(!require(plyr)){
  install.packages('plyr')
  require(plyr)
}
replicates<-ldply(1:200,
                  function(i){
                    data.frame(values=sample(small_sample,length(small_sample),replace=T),
                               data_source=paste('subsample',i,sep='_')
                    )
                  })
allSamples<-rbind(allSamples,replicates)
```

We can check to see what these replicates look like using `geom_density` from `ggplot2`: 


```r
ggplot(allSamples[1:(N*20),],aes(x=values))+facet_wrap(~data_source)+geom_density()
```

![plot of chunk unnamed-chunk-2](/../figs/bootstrapexample/boot-unnamed-chunk-2-1.svg) 

Or more traditionally, we can use `geom_histogram`:


```r
ggplot(allSamples[1:(N*20),],aes(x=values))+facet_wrap(~data_source)+geom_histogram(binwidth=.05)
```

![plot of chunk unnamed-chunk-3](/../figs/bootstrapexample/boot-unnamed-chunk-3-1.svg) 

We can learn something about the structure of our sample by resampling in this way, and this saves us from
needing to make an assumption about the distribution of the sample. 

From a great [Cross Validated Post](http://stats.stackexchange.com/questions/26088/explaining-to-laypeople-why-bootstrapping-works):

> Resampling is not done to provide an estimate of the population distribution--we take our sample itself as a
> model of the population. Rather, resampling is done to provide an estimate of the sampling distribution of the 
> sample statistic in question.

Basically, if we estimate our statistic in these resampled data, we get an idea of the amount of variance we might
expect to see in that statistic in an empirical way, which let's us get p-values and confidence intervals without
making assumptions about the sampling distribution of that statistic the way we do with standard errors.

To get close to this idea, we can look at the distribution of some statistic in our little sample; say, the mean:


```r
head(
  resampledMeans<-daply(
    allSamples,
    'data_source',
    function(subDF){
      mean(subDF$values)
    })
  [-1]) # -1 here gets rid of 'Observed' mean
```

```
## subsample_1 subsample_2 subsample_3 subsample_4 subsample_5 subsample_6 
##   0.8077954   0.9983415   0.6552578   0.9664249   0.7573563   0.9613178
```

```r
qplot(resampledMeans) 
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-4](/../figs/bootstrapexample/boot-unnamed-chunk-4-1.svg) 

Our actual observed mean is:


```r
mean(allSamples$values[allSamples$data_source == 'Observed'])
```

```
## [1] 0.9296383
```

We can also estimate the mean in a simple linear model, and thereby easily obtain a confidence interval:


```r
coef(
  aLinearModel<-lm(
    values~1,
    data=allSamples[allSamples$data_source == 'Observed',])
  )
```

```
## (Intercept) 
##   0.9296383
```

```r
confint(aLinearModel)
```

```
##                 2.5 %   97.5 %
## (Intercept) 0.6556851 1.203592
```

The mean and range of means from our subsamples are:


```r
(mean_of_the_means<-mean(resampledMeans))
```

```
## [1] 0.9322001
```

```r
range(resampledMeans)
```

```
## [1] 0.5740711 1.3548330
```

Our empirical standard error is just the standard deviation of this distribution of means. (As you remember,
"The standard error (SE) is the standard deviation of the sampling distribution of a statistic."; 
[Wikipedia](http://en.wikipedia.org/wiki/Standard_error))


```r
(empircal_se_of_the_mean<-sd(resampledMeans))
```

```
## [1] 0.1429904
```

So we can now construct a 95% confidence interval:


```r
round(
  mean_of_the_means+c(-1.96,1.96)*empircal_se_of_the_mean,
  2
  )
```

```
## [1] 0.65 1.21
```

Compare this again to 


```r
confint(aLinearModel)
```

```
##                 2.5 %   97.5 %
## (Intercept) 0.6556851 1.203592
```


## Using `boot` in R

Let's do exactly what we just did, but using the boot package to simplify things.


```r
if(!require(boot)){
  install.packages('boot')
  require(boot)
}
```

Go check out [`?boot`](http://www.inside-r.org/packages/cran/boot/docs/boot) to see the *many* details of this function.

For boot, you first write a function that takes a data set and an vector (e.g., `c(1,2,2,1,5,6)`) that
indicates what rows from the data go into the subsample. It should return a statistic, so in this case, the mean.
`boot` will call your function over and over, for however many bootstrap subsamples, or replicates, you
specify. 


```r
boot.mean<-function(variabelName,data,indices){ # data, indices are the names you should use here.
  d<-data[indices,variabelName] #make a new data frame with just the rows for this particular subsample
  theMean<-mean(d)
  return(theMean)
} 
```

We can use the same observations as before -- I'll just make it a bit cleaner:


```r
justSomeObservations<-allSamples[allSamples$data_source == 'Observed',]
```

Let's test our function:


```r
boot.mean('values', #for the 'values' column
          justSomeObservations, #our data frame
          1:dim(justSomeObservations)[1]) #indices should just be every row, so from one to the number of rows
```

```
## [1] 0.9296383
```

Okay, looks sensible...


```r
(bootstrapped_means<-boot(
  data=justSomeObservations,
  statistic=boot.mean, #our function!
  R=200, # We'll start at 200 like last time 
  variabelName='values')) #We need to pass `boot` this so it can pass it to `boot.mean`
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = justSomeObservations, statistic = boot.mean, R = 200, 
##     variabelName = "values")
## 
## 
## Bootstrap Statistics :
##      original       bias    std. error
## t1* 0.9296383 -0.004835144   0.1337183
```

```r
mean(bootstrapped_means$t)
```

```
## [1] 0.9248032
```

```r
str(bootstrapped_means)
```

```
## List of 11
##  $ t0       : num 0.93
##  $ t        : num [1:200, 1] 0.879 0.829 0.944 0.989 0.805 ...
##  $ R        : num 200
##  $ data     :'data.frame':	50 obs. of  2 variables:
##   ..$ values     : num [1:50] 0.1182 0.1913 0.5737 0.0499 0.4783 ...
##   ..$ data_source: Factor w/ 201 levels "Observed","subsample_1",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ seed     : int [1:626] 403 384 2012471322 -1893598821 1368502390 -321830300 48881363 281841992 638855891 -1216578014 ...
##  $ statistic:function (variabelName, data, indices)  
##   ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 1 12 5 1 12 1 1 5
##   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000000127e4368> 
##  $ sim      : chr "ordinary"
##  $ call     : language boot(data = justSomeObservations, statistic = boot.mean, R = 200,      variabelName = "values")
##  $ stype    : chr "i"
##  $ strata   : num [1:50] 1 1 1 1 1 1 1 1 1 1 ...
##  $ weights  : num [1:50] 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 0.02 ...
##  - attr(*, "class")= chr "boot"
```

`boot` gives us back a lot of info -- check out the help to see what's going on. 
We can use `boot.ci` to give us some useful info:


```r
boot.ci(bootstrapped_means)
```

```
## Warning in boot.ci(bootstrapped_means): bootstrap variances needed for
## studentized intervals
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 200 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = bootstrapped_means)
## 
## Intervals : 
## Level      Normal              Basic         
## 95%   ( 0.6724,  1.1966 )   ( 0.6495,  1.1721 )  
## 
## Level     Percentile            BCa          
## 95%   ( 0.6872,  1.2097 )   ( 0.6952,  1.2274 )  
## Calculations and Intervals on Original Scale
## Some basic intervals may be unstable
## Some percentile intervals may be unstable
## Some BCa intervals may be unstable
```

These estimates may be unstable because we didn't resample very much. Let's redo this with `R=1000`.


```r
(more_bootstrapped_means<-boot(
  data=justSomeObservations,
  statistic=boot.mean, #our function!
  R=1000, # We'll start at 200 like last time 
  variabelName='values')) #We need to pass `boot` this so it can pass it to `boot.mean`
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = justSomeObservations, statistic = boot.mean, R = 1000, 
##     variabelName = "values")
## 
## 
## Bootstrap Statistics :
##      original       bias    std. error
## t1* 0.9296383 -0.001726859   0.1289678
```

```r
boot.ci(more_bootstrapped_means)
```

```
## Warning in boot.ci(more_bootstrapped_means): bootstrap variances needed
## for studentized intervals
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = more_bootstrapped_means)
## 
## Intervals : 
## Level      Normal              Basic         
## 95%   ( 0.6786,  1.1841 )   ( 0.6631,  1.1622 )  
## 
## Level     Percentile            BCa          
## 95%   ( 0.6971,  1.1962 )   ( 0.7102,  1.2257 )  
## Calculations and Intervals on Original Scale
```

## More bootstrapping!

"Means are child's play" you say. Remember, you can bootstrap any parameter -- you just have to return
it from a function. This is especially useful for parameters that don't follow any theoretical distribution,
such as cronbach's alpha. Let's bootstrap some CIs around an alpha estimate.


```r
if(!require(psych)){
  install.packages('psych')
  require(psych)
}
```

Here we define a model for a simulated scale -- 1 factor with 6 items. The vector gives the loadings for that factor.


```r
(fx<-matrix(c(.5,.8,.7,.85,.75,.65),ncol=1))
```

```
##      [,1]
## [1,] 0.50
## [2,] 0.80
## [3,] 0.70
## [4,] 0.85
## [5,] 0.75
## [6,] 0.65
```

```r
something<-sim(fx=fx,alpha=.6,n=200)
str(something)
```

```
## List of 6
##  $ model      : num [1:6, 1:6] 1 0.4 0.35 0.425 0.375 0.325 0.4 1 0.56 0.68 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:6] "V1" "V2" "V3" "V4" ...
##   .. ..$ : chr [1:6] "V1" "V2" "V3" "V4" ...
##  $ reliability: num [1:6] 0.25 0.64 0.49 0.722 0.562 ...
##  $ r          : num [1:6, 1:6] 1 0.373 0.353 0.427 0.33 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : chr [1:6] "V1" "V2" "V3" "V4" ...
##   .. ..$ : chr [1:6] "V1" "V2" "V3" "V4" ...
##  $ observed   : num [1:200, 1:6] 2.131 0.646 0.668 2.572 1.752 ...
##   ..- attr(*, "dimnames")=List of 2
##   .. ..$ : NULL
##   .. ..$ : chr [1:6] "V1" "V2" "V3" "V4" ...
##  $ N          : num 200
##  $ Call       : language sim(fx = fx, alpha = 0.6, n = 200)
##  - attr(*, "class")= chr [1:2] "psych" "sim"
```

```r
head(something$observed) #So this is where our simulated observations live...
```

```
##              V1         V2          V3         V4         V5         V6
## [1,]  2.1306216  0.1763832  0.76159897  0.2249688  0.9984098  0.3064475
## [2,]  0.6460641  1.1468081  0.81096492 -0.1146885  2.6082304  0.1803568
## [3,]  0.6682297 -0.9512511 -0.06235993  0.3997070  0.4501681 -0.4175903
## [4,]  2.5721859 -0.4297980  0.77115578  1.0285876  1.3459597  0.1771619
## [5,]  1.7516395 -0.1485804 -1.24786992 -1.2730166 -0.8687324 -1.0610701
## [6,] -1.5134263 -0.2622380 -0.77461337 -0.6639065 -1.9231006  0.6146709
```

```r
est_alpha<-alpha(something$observed)
str(est_alpha)
```

```
## List of 11
##  $ total        :'data.frame':	1 obs. of  8 variables:
##   ..$ raw_alpha: num 0.826
##   ..$ std.alpha: num 0.827
##   ..$ G6(smc)  : num 0.814
##   ..$ average_r: num 0.443
##   ..$ S/N      : num 4.78
##   ..$ ase      : num 0.0366
##   ..$ mean     : num 0.03
##   ..$ sd       : num 0.706
##  $ alpha.drop   :'data.frame':	6 obs. of  6 variables:
##   ..$ raw_alpha: num [1:6] 0.83 0.781 0.798 0.762 0.793 ...
##   ..$ std.alpha: num [1:6] 0.832 0.781 0.8 0.763 0.795 ...
##   ..$ G6(smc)  : num [1:6] 0.809 0.759 0.781 0.734 0.775 ...
##   ..$ average_r: num [1:6] 0.497 0.417 0.444 0.392 0.436 ...
##   ..$ S/N      : num [1:6] 4.95 3.57 3.99 3.22 3.87 ...
##   ..$ alpha se : num [1:6] 0.0405 0.0454 0.0437 0.0473 0.0442 ...
##  $ item.stats   :'data.frame':	6 obs. of  6 variables:
##   ..$ n     : num [1:6] 200 200 200 200 200 200
##   ..$ r     : num [1:6] 0.609 0.793 0.731 0.849 0.748 ...
##   ..$ r.cor : num [1:6] 0.479 0.752 0.651 0.839 0.681 ...
##   ..$ r.drop: num [1:6] 0.434 0.679 0.595 0.761 0.617 ...
##   ..$ mean  : num [1:6] 0.1891 0.02322 0.01503 -0.02908 0.00717 ...
##   ..$ sd    : num [1:6] 0.961 0.902 1.02 0.946 1.017 ...
##  $ response.freq: NULL
##  $ keys         : num [1:6] 1 1 1 1 1 1
##  $ scores       : num [1:200] 0.7664 0.8796 0.0145 0.9109 -0.4746 ...
##  $ nvar         : int 6
##  $ boot.ci      : NULL
##  $ boot         : NULL
##  $ call         : language alpha(x = something$observed)
##  $ title        : NULL
##  - attr(*, "class")= chr [1:2] "psych" "alpha"
```

```r
est_alpha$total$raw_alpha #This is where the raw alpha estimate lives...
```

```
## [1] 0.8257378
```

You may notice that this has an empty spot for `boot.ci` -- this implimentation actually has bootstrapped CIs built into
it. We'll do it manually, though.


```r
boot.alpha<-function(data,indices){
  d<-data[indices,] # Let's 'boot' set a sample
  raw_alpha<-alpha(d)$total$raw_alpha
  return(raw_alpha)
}

boot.alpha(something$observed,1:dim(something$observed)[1]) # a test...
```

```
## [1] 0.8257378
```

```r
(boot.rezzies<-boot(something$observed,boot.alpha,R=1000)) # Boot it!
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = something$observed, statistic = boot.alpha, R = 1000)
## 
## 
## Bootstrap Statistics :
##      original       bias    std. error
## t1* 0.8257378 -0.001106671  0.01587251
```

```r
# Well, that wasn't terribly fast...

require(ggplot2)
qplot(boot.rezzies$t)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

```
## Warning: position_stack requires constant width: output may be incorrect
```

![plot of chunk unnamed-chunk-20](/../figs/bootstrapexample/boot-unnamed-chunk-20-1.svg) 

```r
boot.ci(boot.rezzies)
```

```
## Warning in boot.ci(boot.rezzies): bootstrap variances needed for
## studentized intervals
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = boot.rezzies)
## 
## Intervals : 
## Level      Normal              Basic         
## 95%   ( 0.7957,  0.8580 )   ( 0.7975,  0.8601 )  
## 
## Level     Percentile            BCa          
## 95%   ( 0.7913,  0.8540 )   ( 0.7917,  0.8543 )  
## Calculations and Intervals on Original Scale
```

We can do it with regression coefficients too!


```r
data(iris)
str(iris)
```

```
## 'data.frame':	150 obs. of  5 variables:
##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
##  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
##  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
qplot(iris$Sepal.Length,iris$Petal.Length)
```

![plot of chunk unnamed-chunk-21](/../figs/bootstrapexample/boot-unnamed-chunk-21-1.svg) 

```r
qplot(iris$Sepal.Length,iris$Petal.Width)
```

![plot of chunk unnamed-chunk-21](/../figs/bootstrapexample/boot-unnamed-chunk-21-2.svg) 

```r
summary(lm(Sepal.Length~Petal.Length+Petal.Width,iris))
```

```
## 
## Call:
## lm(formula = Sepal.Length ~ Petal.Length + Petal.Width, data = iris)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.18534 -0.29838 -0.02763  0.28925  1.02320 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   4.19058    0.09705  43.181  < 2e-16 ***
## Petal.Length  0.54178    0.06928   7.820 9.41e-13 ***
## Petal.Width  -0.31955    0.16045  -1.992   0.0483 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4031 on 147 degrees of freedom
## Multiple R-squared:  0.7663,	Adjusted R-squared:  0.7631 
## F-statistic:   241 on 2 and 147 DF,  p-value: < 2.2e-16
```

```r
coef(lm(Sepal.Length~Petal.Length+Petal.Width,iris))
```

```
##  (Intercept) Petal.Length  Petal.Width 
##    4.1905824    0.5417772   -0.3195506
```

```r
boot.lmCoef<-function(formula,data,indices){
  d<-data[indices,]
  coef(lm(formula,d))
}

(lmCoef.booted<-boot(iris,boot.lmCoef,R=500,formula='Sepal.Length~Petal.Length+Petal.Width'))
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = iris, statistic = boot.lmCoef, R = 500, formula = "Sepal.Length~Petal.Length+Petal.Width")
## 
## 
## Bootstrap Statistics :
##       original       bias    std. error
## t1*  4.1905824 -0.005102153  0.09842502
## t2*  0.5417772  0.005143225  0.07457103
## t3* -0.3195506 -0.013446946  0.16800565
```

```r
boot.ci(lmCoef.booted,index=1)
```

```
## Warning in boot.ci(lmCoef.booted, index = 1): bootstrap variances needed
## for studentized intervals
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 500 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = lmCoef.booted, index = 1)
## 
## Intervals : 
## Level      Normal              Basic         
## 95%   ( 4.003,  4.389 )   ( 4.007,  4.376 )  
## 
## Level     Percentile            BCa          
## 95%   ( 4.005,  4.374 )   ( 4.014,  4.382 )  
## Calculations and Intervals on Original Scale
## Some BCa intervals may be unstable
```

```r
boot.ci(lmCoef.booted,index=2)
```

```
## Warning in boot.ci(lmCoef.booted, index = 2): bootstrap variances needed
## for studentized intervals
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 500 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = lmCoef.booted, index = 2)
## 
## Intervals : 
## Level      Normal              Basic         
## 95%   ( 0.3905,  0.6828 )   ( 0.3955,  0.6850 )  
## 
## Level     Percentile            BCa          
## 95%   ( 0.3985,  0.6880 )   ( 0.3785,  0.6670 )  
## Calculations and Intervals on Original Scale
## Some BCa intervals may be unstable
```

```r
boot.ci(lmCoef.booted,index=3)
```

```
## Warning in boot.ci(lmCoef.booted, index = 3): bootstrap variances needed
## for studentized intervals
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 500 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = lmCoef.booted, index = 3)
## 
## Intervals : 
## Level      Normal              Basic         
## 95%   (-0.6354,  0.0232 )   (-0.6432,  0.0174 )  
## 
## Level     Percentile            BCa          
## 95%   (-0.6565,  0.0041 )   (-0.6028,  0.0355 )  
## Calculations and Intervals on Original Scale
## Some BCa intervals may be unstable
```

