#################################################################
############################# NOTES #############################
#################################################################

#This tutorial has been adapted from https://itchyshin.github.io/Meta-analysis_tutorial/
#and the GitHub repo: https://github.com/itchyshin/Meta-analysis_tutorial.git

#All equations referred to relate back to the tutorial or paper itself:

#Shinichi Nakagawa, Yefeng Yang, Erin Macartney, Rebecca Spake, and Malgorzata Lagisz. 
#Quantitative synthesis: a practical guide to meta-analysis, meta-regression, and publication bias tests for environmental sciences. 
#Environmental Evidence, 2023.


######################################################################
############# Load in required packages ##############################
######################################################################

#you may need to install these first.
install.packages(c('pacman','devtools'))
#this code installs packages directly from github
devtools::install_github("daniel1noble/orchaRd", force = TRUE)
devtools::install_github("Mikata-Project/ggthemr", force = TRUE)
devtools::install_github("kassambara/ggpubr", force = TRUE)

pacman::p_load(tidyverse, 
               here,
               DT,
               ggpubr,
               readxl, 
               metafor,
               clubSandwich,
               orchaRd, 
               MuMIn,
               glmulti,
               metagear,
               patchwork,
               stringr,
               mice,
               GoodmanKruskal,
               networkD3,
               ggplot2,
               plotly,
               ggsignif,
               visdat,
               ggalluvial,
               ggthemr, 
               cowplot,
               grDevices,
               png,
               grid,
               gridGraphics,
               pander,
               formatR,
               ggbeeswarm
               )

# Load custom functions

# We also provide some additional helper functions that are necessary for our worked examples. The most straightforward way to use these custom functions is to source them with:

source("custom_func.R")

#IMPORTANT!
#remember to put the custom_func.R file and the data file Midolo_2019_Global Change Biology.csv in the same working directory folder
#for your R session!


######################################################################
############# Estimating an effect size statistic ####################
######################################################################

## Background to effect size estimation

#Meta-analyses express the outcome of multiple studies on a common scale, through the calculation of an ‘effect size’, 
#representing the magnitude of a difference or the strength of a relationship. The effect size serves as the response variable 
#or dependent variable in a meta-analytical model. Several metrics of effect size are available, and the choice of metric should 
#depend on the scientific question under investigation.

## Calculating effect sizes
#For a chosen effect size, it's point estimate and sampling variance can be calculated using function `escalc()` in `metafor` package.
#Let's load the dataset of our first worked example, which meta-analysed the intraspecific change in seven 
#morpho‐ecophysiological leaf traits along global elevational gradients.

### import dataset
dat_Midolo_2019 <- read.csv("Midolo_2019_Global Change Biology.csv")

#For simplicity, we will only keep the columns that are necessary to demonstrate the meta-analytic techniques of interest in this tutorial.

### drop unused columns
dat_Midolo_2019 <- dat_Midolo_2019[,which(colnames(dat_Midolo_2019) %in% c("Study_ID", "study_name", "species", "trait", "treatment", "control", "sd_treatment", "sd_control", "n_treatment", "n_control", "elevation", "elevation_log"))]

### make a table
t1 <- dat_Midolo_2019 %>% DT::datatable()
t1

#You can find more details about the variables in the supplementary information of the paper(https://doi.org/10.1111/gcb.14646) Table S1, if interested. 
#Here we classify the variables into three categories types:
#1. Bibliographic variables
#Variables that distinguish between articles, such as the title (`study_name`), or publication year.

#2. Descriptive statistics
#Variables that are used to compute effect sizes, such as mean (`treatment` and `control`), standard deviation (`sd_treatment` and `sd_control`) and sample size (`n_treatment` and `n_control`).

#3. Study-related characteristics
#Study-level variables that can contribute to random variability among the effect sizes, such as study identity (`Study_ID`), or systematic variability between the effect sizes, such as treatment intensity, or study location characteristics  such as (`elevation`); which may be of interest as a moderator variable.

#After having an (primary) understanding of the extracted/coded variables, we can use `escalc()` to compute effect size (using lnRR as an example) with.
#Note that the argument `measure` is used to specify the effect size metric that we will calculate. 
#For example, by specifying `measure = "SMD"` , the function will compute the point estimate of SMD (specifically, Hedges' g) and its sampling variance. 

#Here are the most commonly used effect sizes and underappreciated effect sizes.
#For other types, you can look at the help information of `escalc ` by typing `?escalc` in the console of `R`.

#- `measure = "ROM"`: lnRR - quantifies the difference between the means of two groups
#- `measure = "SMD"`: SMD - quantifies the differences between the means of two groups
#- `measure = "ZCOR"`: *Zr* - quantifies the strength of association between two variables
#- `measure = "VR"`: lnVR - quantifies the differences in variances between two groups
#- `measure = "CVR"`: lnCVR - quantifies the differences in variances between two groups, accounting for the mean-variance relationship

#Note that lnVR and lnCVR are variation/dispersion-based effect sizes, which have practical implications for environmental studies. 
#For example, environmental stressors such as pesticides and eutrophication are likely to increase variability in biological systems 
#because stress accentuates individual differences in environmental responses. 

lnRR <- escalc(measure = "ROM",  # "ROM" means ratio of means; lnRR is specified to be calculated (alternative effect sizes: "SMD" – SMD, "CVR" – lnCVR, "VR" – lnVR; see below);
                 m1i = treatment, # mean value of of group 1 (e.g., environmental stressor); in our worked example, m1i denotes the mean value of a trait measured at the higher elevation level;
                 m2i = control, # mean value of group 2 (e.g., control); in our worked example, m2i denotes the mean of the same trait measured at the lower elevation level;
                 sd1i = sd_treatment, # standard deviation of mean of group 1 (e.g., environmental stressor)
                 sd2i = sd_control, # standard deviation of group 2 (e.g., control) 
                 n1i = n_treatment, # sample size of group 1 (e.g., environmental stressor) 
                 n2i = n_control, # sample size of group 2 (e.g., control) 
                 data = dat_Midolo_2019, # dataset containing the above information (here is the dataset of our working example)
)

# computation lnRR
lnRR <- escalc(measure = "ROM",  # the lnRR is specified; "ROM" means ratio of means
               m1i = treatment, # mean of group 1 (e.g., group to which environmental stressor applied) 
               m2i = control, # mean of group 2 (e.g., control group)
               sd1i = sd_treatment, # standard deviation of mean of group 1
               sd2i = sd_control, # standard deviation of group 2 
               n1i = n_treatment, # sample size of group 1 
               n2i = n_control, # sample size of group 2  
               data = dat_Midolo_2019, # dataset containing the above information (here is the dataset of our working example)
               digits = 3,
               append = FALSE)

### bind the effect size estimates and study variables into a single dataframe
metrics_set <- data.frame(lnRR = lnRR$yi, lnRRV = lnRR$vi) # effect size estimate and its variance
dat2_Midolo_2019 <- cbind(dat_Midolo_2019, metrics_set) # bind_cols()


#Let's explore the effect size estimates and their sampling variances.
#The point estimate of effect size (**lnRR**) and their sampling variance (**lnRRV**) for each included study and comparison (higher elevation vs. lower elevation).  

### show the calculated effect sizes and their sampling variances
t2 <- dat2_Midolo_2019 %>% select(c("study_name","lnRR","lnRRV", "elevation")) %>% DT::datatable()
t2



######################################################################
############# Choosing a meta-analytic model #########################
######################################################################

## Two traditional models
##After computing the effect sizes and sampling variance, we can address the **first aim** of a meta-analysis - **estimating overall effect**. 

##Two traditional meta-analytic models can be easily fitted via function `rma()` in `metafor`. These are are rarely appropriate, and we usually need a more complex model, called a multilevel meta-analytic model. However, we fit the traditional models here for comparison.


##- Fixed-effect model (‘common-effect’ or ‘equal-effect’ model; Equation 1)

mod_FE_lnRR <- rma(yi = lnRR, # calculated/estimated effect size are supplied; the outputs of escalc() function; 
                   vi = lnRRV, # calculated/estimated sampling variance of lnRR are supplied; 
                   method = "EE", # fixed-effect model is specified;
                   data = dat2_Midolo_2019) # our dataset

### fit a fixed-effect model, it also can be called common-effect model or equal-effect model
mod_FE_lnRR <- rma(yi = lnRR, # observed effect sizes / estimates of lnRR are supplied; the outputs of escalc() function; 
                   vi = lnRRV, # the estimates of sampling variance of lnRR are supplied;; 
                   method = "EE", # a fixed-effect model is set to fit;
                   data = dat2_Midolo_2019 # the dataset
                   )

#Below is the output of our fitted fixed-effect model. **Model Results**, which address the **first aim** of an environmental meta-analysis: 
#to estimate an overall mean effect size, including the magnitude (beta_0), uncertainty (standard error SE[beta_0]), 
#significance test, and confidence intervals (CIs). We will teach you how to properly interpret all the elements of the above printed model 
#results when we get to the next session - the multilevel meta-analytic model.

# the model results of the fixed-effect model
summary(mod_FE_lnRR)



##- random-effects model (Equation 2)

### fit a random-effects model.
mod_RE_lnRR <- rma(yi = lnRR, # observed effect sizes / estimates of SMD; the outputs of escalc() function; 
                   vi = lnRRV, # the estimates of sampling variance of SMD; 
                   method = "REML", # setting restricted maximum likelihood estimator as the estimators for the amount of heterogeneity;
                   data = dat2_Midolo_2019 # the dataset
                   )

#Below are the outputs of our fitted random-effects model.

# the model results of the random-effects model
summary(mod_RE_lnRR)


#It should be noted by fitting the above two traditional models, we assume that the number of independent effect sizes *k* = 1294 effect sizes. This is wrong, as some studies contributed more than one effect size, which are not independent. We will elaborate on this in the next sections!

#- **Technical Recommendation**

#Setting `method = "REML"` fits a random-effects model using the restricted maximum likelihood estimator to calculate the maximum likelihood of heterogeneity and corresponding model coefficients (e.g., $\beta_0$). We can also fit a random-effects model with the above syntax but setting the `method` argument to different estimators for estimating variance components:
#`method = "PM"`: Paule-Mandel method; `method = "ML"`: maximum likelihood estimator; `method="DL"` -  DerSimonian-Laird estimator; `method = "HE"`: Hedges estimator. Generally, we recommend **REML** as it can produce (approximately) unbiased estimates of variance components (e.g., $\tau^2$) according to various simulation works. But note that the Paule-Mandel method has superior properties for common effect size measures, such as SMD, in the context of random-effects model. Paule-Mandel method is recommended when fitting traditional random-effects models. However, the Paule-Mandel method does not generalize straightforwardly to multilevel models. Put differently, Paule-Mandel method is not feasible in `rma.mv` function. 



## Multilevel meta-analytic model

### Brief theory
#As we explain in the main text, a common statistical issue confronted by environmental meta-analyst is that of **non-independence** among data points (i.e., effect sizes). 
#This is because each study (article) often contributes more than one effect sizes. 
#To handle the non-independent effect sizes, the simplest  meta-analytic model for environmental sciences is the multilevel model.


### Model fitting

#We need to handle  non-independence from the very beginning of our analysis. When preparing our data file (e.g., .CSV file), we need to structure our data 
#in a way that permits the incorporation of non-independence among effect sizes. 
#To do so, we need to code a unique identifier for each primary study (`Study_ID`: *S1*, *S2*, *S3*, ...) 
#and a unique identifier for each effect size (`ES_ID`: *ES1*, *ES2*, *ES3*, ...).

# add a unique ID for each observation; otherwise you would be assuming homogeneity of true effect sizes within studies, which is a strong assumption
dat2_Midolo_2019$ES_ID <- rep("ES", nrow(dat2_Midolo_2019))
dat2_Midolo_2019$ES_ID <- paste(dat2_Midolo_2019$ES_ID, c(1:nrow(dat2_Midolo_2019)), sep = "")

#With these variables coded, we can use function `rma.mv` (rather than `rma`) to fit the multilevel meta-analytic model (Equation 3). 
#Supplying `Study_ID` and `ES_ID` to the argument `random`, we can specify the two random-effects terms in Equation 3, 
#including: the study level mu_j[i] (between-study effect) and the effect size e_i (within-study effect). 

#We use a formula to properly define the random-effects structure specified in `random`: starts with `~ 1`, followed by a `|`, 
#then a identifier denoting random effects (e.g., `Study_ID` or `ES_ID`). The complete syntax to fit Equation 3 is:

mod_ML_lnRR <- rma.mv(yi = lnRR, 
                      V = lnRRV, 
                      random = list(~1 | Study_ID, # allows true effect sizes to vary among different primary studies - account for the between-study effect and quantify between-study heterogeneity;
                                    ~1 | ES_ID), # allows true effect sizes to vary within primary studies - account for the with-study effect and quantify with-study heterogeneity;
                      method = "REML", # REML is assigned as the estimator for variance components as suggested;
                      #test = "t",
                      data = dat2_Midolo_2019 # our dataset
                      )

#Alternatively, you can use `Study_ID/ES_ID` to specify `random` argument, which is a more straightforward way of showing nesting structure:

mod_ML_lnRR <- rma.mv(yi = lnRR, 
                      V = lnRRV, 
                      random = ~1 | Study_ID/ES_ID, # alternative syntax to specify random structure;
                      method = "REML", # REML is assigned as the estimator for variance components as suggested;,
                      #test = "t",
                      data = dat2_Midolo_2019 # our dataset
                      )


### Results interpretation

#The classic results of the fitted multilevel model (Equation 3) looks like:

summary(mod_ML_lnRR)


#Let’s go through the results in order of appearance.  

#We are first reminded of the model we have fitted:
#`k` = the number of effect sizes (*k*) fed to the multilevel model.
#`method: REML` = the REML method was specified as estimation procedure for model fitting to obtain model estimates  (e.g., variance components, model coefficients). 


#- **Fit statistics**  
#The next line contains several fit statistics, which can be used for model comparison), including:
#`logLik` = restricted log-likelihood of the fitted model,
#`Deviance` = goodness-of-fit statistic of the fitted model,
#`AIC` = Akaike information criterion score of the fitted model,
#`BIC` = Bayesian information criterion, and 
#`AICc` = AIC corrected for small sample sizes. 


#- **Variance Components**  
#`sigma^2.1` =  between-study variance tau^2. In this example, this is conceptually equivalent to 
#between-study heterogeneity variance tau^2 in a random-effects model (formulated as Equation 2).
#`sigma^2.2` = within-study variance sigma^2 
#`estim` =  the estimated amount of corresponding variance components
#`sqrt` = standard deviation of variance components
#`nlvls` = the number of levels for each random effect
#`factor` = the name of the variables we supply to argument `random` to specify the random effects.


#- **Test for Heterogeneity**  
#Results of the heterogeneity test, which tests whether there are effect size differences in the data (based on Cochran’s Q-test, 
#which is used to test the null hypothesis that all experimental studies have the same/equal effect).
#`p-val` < 0.05 = effect sizes derived from the environmental studies are heterogeneous. 
#It is also good to know how much of the heterogeneity is due to differences within studies, and how much is due to between-study differences. 


#- **Model Results**  
#Results of model coefficients and corresponding model inference.
#`estimate` = the estimate of pooled/average/overall effect; also as known as grand mean or meta-analytic effect size ($\beta_{0}$ in Equation 3.
#`se` = the standard error of the estimate; here, it is (SE[$\beta_{0}$].
#`zval` = the value of test statistic (in our case: z-value; see **Technical recommendation** for our recommended practices).
#`ci.lb` = lower boundary of (95% by default) confidence intervals (CIs).
#`ci.Ub` = upper boundary of CIs. 


### Technical recommendation
#The argument `test` in functions `rma.mv` and `rma` is used to specify the methods to calculate test statistics and perform 
#significance tests of model coefficient like beta_0 (confidence intervals and p-value). 
#By default, `rma.mv` and `rma` set `test = "z"`, which uses a test that assumes a normal distribution. 
#However, when meta-analysing small number of studies, setting `test = "z"` will lead to a nominally high Type I error rate, 
#leading to high false positive results. To achieve a nominal performance of tests, it is highly recommended to set `test = "t"`, 
#which uses a t-distribution with *k*−*p* degrees of freedom to test model coefficients (e.g., beta_0) and CIs (p = the number of model coefficients). 
#There are also other possible adjustments, for example, adjusting the degrees of freedom of the t-distribution, which can be implemented by setting `dfs = "contain"`.


#Therefore, a multilevel model with improved model inference methods can be fitted with:
### fit multilevel model with an improved model inference methods
mod_ML_lnRR <- rma.mv(yi = lnRR, 
                      V = lnRRV, 
                      random = list(~1 | Study_ID, # allows true effect sizes to vary among different primary studies - account for the between-study effect and quantify between-study heterogeneity;
                                    ~1 | ES_ID), # allows true effect sizes to vary within primary studies - account for the with-study effect and quantify with-study heterogeneity;
                      method = "REML", # REML is assigned as the estimator for variance components as suggested;
                      test = "t", # t-distribution is specified for the tests of model coefficients and CIs;
                      dfs = "contain", # the methods to adjust the (denominator) degrees of freedom;
                      data = dat2_Midolo_2019, # our dataset
                      sparse = TRUE
                      )

#Corresponding results look like:
summary(mod_ML_lnRR)

#As you can see, under **Model Results**, `zval` turns to `tval`. 
#Note that the value has not changed because this worked example has a large number of primary studies
#that a z-distribution (standard normal distribution) is approximately the same as a t-distribution. 
#When the number of primary studies (*N*) is low, setting `test = "t"` and `dfs = "contain"` is highly recommended.



## Statistical non-independence
### Undesirable consequences

#As we explain in the main text, failing to deal with statistical non-independence (details see **Figure 2**) will inflate the Type 1 error, 
#leading to an underestimated standard error of $\beta_0$ and a spurious p-value of beta_0 (our first example shows this problem; see below). 
#Note that the estimate of the magnitude of beta_0 is not necessarily biased. However, the bias can be large and even cause a change in effect size 
#sign/direction (as is the case with our second worked example; see below section). 
#Below, we compare the results of traditional models (fixed and random effects models) and the more appropriate multilevel meta-analytic model. 
#We show how ignoring statistical non-independence using the traditional models distort the the meta-analytic evidence. Let's look at Table S3 
#showing results of traditional models and the multilevel model.


#Comparison of the random-effects and multilevel meta-analytic models.   
t3 <- data.frame("Overall effect (pooled lnRR)" = c(round(mod_RE_lnRR$b[1],2), round(mod_ML_lnRR$b[1],2)),
                 "Standard error" = c(round(mod_RE_lnRR$se,2), round(mod_ML_lnRR$se,2)),
                 "p-value" = c(round(mod_RE_lnRR$pval,4), round(mod_ML_lnRR$pval,3)),
                 "Lower CI" = c(round(mod_RE_lnRR$ci.lb,2), round(mod_ML_lnRR$ci.lb,2)),
                 "Upper CI" = c(round(mod_RE_lnRR$ci.ub,2), round(mod_ML_lnRR$ci.ub,2)),
                 "Between-study variance" = c(round(mod_RE_lnRR$tau2,3), round(mod_ML_lnRR$sigma2[1],3)),
                 "Within-study variance" = c(0, round(mod_ML_lnRR$sigma2[2],3)),
                 "Between-study I2" = c(round(mod_RE_lnRR$I2,2),round(i2_ml(mod_ML_lnRR)[[2]],2)),
                 "Within-study I2" = c(0,round(i2_ml(mod_ML_lnRR)[[3]],2)))

colnames(t3) <- c("Overall effect (pooled lnRR)", "Standard error", "p-value", "Lower CI", "Upper CI", "Between-study variance", "Within-study variance", "Between-study heterogeneity I2 (%)", "Within-study heterogeneity I2 (%)")

t3_2 <- t(t3) %>% as.data.frame()
colnames(t3_2) <- c("random-effects model", "Multi-level model")

t3_2 %>% DT::datatable()
# t3_2 %>% ggtexttable(theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#8cc257"), rownames.style = colnames_style(color = "white", fill = "#8cc257", face = "bold"), tbody.style = tbody_style(color = "black", fill = c("#e8f3de", "#d3e8bb"))))


#- Biased results by ignoring statistical non-independence
#We can see that conclusions based on our proposed multilevel model conflict with those reached by random-effects model; 
#*p-value* and 95% CIs demonstrate that the multilevel model shows statistically non-significant overall effect (beta_0)
# while random-effects model shows statistically significant overall effect (beta_0)。  

#This indicates that using the traditional model to fit non-independent effect sizes underestimates the standard error of 
#model coefficient (SE[beta_0]) 
#and distorts corresponding statistical inference (e.g., by inflating the p-value):

#SE
round(mod_ML_lnRR$se,3) #in the multilevel model 
#vs.
round(mod_RE_lnRR$se,3) #in the random-effects model; 

#p-value 
round(mod_ML_lnRR$pval,4) #in the multilevel model 
#vs. 
round(mod_RE_lnRR$pval,4) #in the random-effects model. 

#The width of 95% CIs in the multilevel model are wider than those in the random-effects model: 
round(mod_ML_lnRR$ci.lb,3) 
round(mod_ML_lnRR$ci.ub,3) #in the multilevel model 

#vs. 
round(mod_RE_lnRR$ci.lb,3) 
round(mod_RE_lnRR$ci.ub,3) #in the random-effects model. 

#Therefore, if using random-effects model fitting this data, we might have wrongly concluded that increasing elevation has, on average, 
#no effect on intraspecific leaf traits.  

#The multilevel model accounts for statistical non-independence due to multiple effect sizes derived from the same study. 
#We can quantify the degree of dependence using an index called the intraclass correlation coefficient:
#The *ICC* index can reflect the intraclass correlation of the true effects within the same study. 
#In the above fitted model, the *ICC* can be computed with:
mod_ML_lnRR$sigma2[1] / sum(mod_ML_lnRR$sigma2)

#An *ICC* value of 0.2703 indicates that the underlying true effects (quantified as lnRR in this case) are weakly correlated within studies. 
#However, the traditional models ignores this dependence and regards all true effects within the same study as independent 
#(which would give *ICC* = 0). 
#Therefore, the meta-analytic conclusions have changed qualitatively (turning from statistically significant into non-significant 
#- inflated Type I error rates).


################################################################
########## Accounting for non-independence #####################
################################################################

#We're going to skip over the code here a bit, as it does get quite complex. Do refer back to the main tutorial.

### Variance-covariance matrix
#As explained in the main text, when multiple effect sizes are contributed by a single article, 
#there will be two broad types of non-independence: 
#- non-independence among effect sizes
#- non-independence among sampling errors

#The multilevel model only deals with cases of non-independence among effect sizes. 
#For non-independence among sampling variances, for example, due to shared control statistics, 
#we can construct a variance-covariance matrix to explicitly capture the non-zero covariance
#that arises from correlation between sampling errors within the same primary studies:

#Constructing a **VCV** matrix is almost impossible because the within-study correlation (e.g., rho) 
#is rarely available from primary environmental studies, although exact covariances can be computed from some 
#comparative effect statistics. But we can impute a **VCV** matrix by assuming a constant rho across different environmental 
#studies. With this simple assumption, #we can impute **VCV** matrix via function `impute_covariance_matrix()` 
#from the package `clubSandwich`:

VCV <- impute_covariance_matrix(vi = dat2_Midolo_2019$lnRRV, # sampling variances that are correlated with each within the same study;
                                cluster = dat2_Midolo_2019$Study_ID, # study identity - clustering variable;
                                r = 0.5 # assuming that the effect sizes within the same study are correlated with rho = 0.5.
)

#This can also be done via function `vcalc()` in `metafor`:
#VCV <- vcalc(vi = lnRRV, # sampling variances that are correlated with each within the same study;
#              cluster = Study_ID,  # study identity - clustering variable;
#              obs = ES_ID, # different effect sizes corresponding to the same response/dependent variable
#              data = dat2_Midolo_2019, 
#              rho = 0.5 # assuming that the effect sizes within the same study are correlated with rho = 0.5
# )

#Setting `rho = 0.5` means we assume that sampling errors within the same study are correlated with rho = 0.5 
#(see below for **Technical Recommendation**). 
#Let's see what the imputed **VCV** matrix looks like. Let's use the Bansal and Germino (2010) study as an example:
VCV[dat2_Midolo_2019$study_name %in% c("Bansal and Germino (2010)"), dat2_Midolo_2019$study_name %in% c("Bansal and Germino (2010)")] %>% round(3)

#As we can see, the **VCV** matrix is a symmetric matrix, with the sampling variance on the diagonal, 
#and the covariance on the off-diagonal. 
#Let's account for both non-independence among effect sizes and sampling errors:

mod_MLMR_lnRR_elevation <- rma.mv(yi = lnRR, 
                                  V = VCV, 
                                  mods = ~ elevation_log, # adding elevation as a moderator variable (for continuous variable, it is a good practice to log-transform them to avoid data skewness);
                                  random = list(~1 | Study_ID, 
                                                ~1 | ES_ID), 
                                  method = "REML", 
                                  test = "t", 
                                  data = dat2_Midolo_2019,
                                  sparse = TRUE
)


#We can explore whether the model coefficients and corresponding significance tests change:

### run multilevel model without accounting for sampling covariance (correlated errors)
mod_ML_lnRR_var <- rma.mv(yi = lnRR, 
                          V = lnRRV, # use sampling variance for comparison with that fitted by covariance
                          random = list(~1 | Study_ID, 
                                        ~1 | ES_ID), 
                          method = "REML", 
                          test = "t", 
                          data = dat2_Midolo_2019,
                          sparse = TRUE
)


# re-run the multilevel model with accounting for the sampling variance using the constructed VCV matrix with rho = 0.5 - medium correlation
mod_ML_lnRR_VCV <- rma.mv(yi = lnRR, 
                          V = VCV, # use covariance for comparison with that fitted by sampling covariance
                          random = list(~1 | Study_ID, 
                                        ~1 | ES_ID), 
                          method = "REML", 
                          test = "t", 
                          data = dat2_Midolo_2019,
                          sparse = TRUE
)

# show the summary of the model account for correlated errors
summary(mod_ML_lnRR_VCV)

#We see that the overall effect beta_0 does not change
round(mod_ML_lnRR_VCV$beta,3)
round(mod_ML_lnRR_var$beta,3)

#95% CIs do not really change
round(mod_ML_lnRR_VCV$ci.lb,3)
round(mod_ML_lnRR_VCV$ci.ub,3)

round(mod_ML_lnRR_var$ci.lb,3)
round(mod_ML_lnRR_var$ci.ub,3)

#and corresponding significance tests such as the *p-value* = 
round(mod_ML_lnRR_VCV$pval,4) 
round(mod_ML_lnRR_var$pval,4) 
#do not change much after accounting for non-independence among sampling errors. 
#But this does not mean we do not need to account for it in general.

#- **Technical Recommendation**
#Certain values of rho have been recommended by several published papers, for example, `rho = 0.5`
#or more conservatively, `rho = 0.8`.
#We should check the robustness of our study results against different values of rho 
#(see **Conducting sensitivity analysis & critical appraisal** in the tutorial for a pipeline conducting such a sensitivity).

#See the main tutorial for another way to account for non-independence: ### Robust variance estimation (RVE)





########################################################
############### Quantifying heterogeneity ##############
########################################################

## Measuring heterogeneity

#The **second aim** of a meta-analysis is to **quantify consistencies (heterogeneity) between studies**. 
#In our main text, we recommend two ways of answering this question: using absolute and relative measures of heterogeneity. 

#- Absolute heterogeneity measure 
#We can use variance components such as tau^2, **which can be directly extracted from the outputs of the fitted model**
#tau^2 in a random-effects model (or tau^2 + sigma^2 in a multilevel model) directly reflects the differences underlying 
#the true effects corresponding to each random effect because the square root can be interpreted as the standard deviation 
#of the true effect sizes. Also, an often neglected insight here is that the sum of the tau^2 and sigma^2 denotes the 
#total amount of variation or heterogeneity in the true effects. 

#In this worked example, the variation in the true effects (tau^2 + sigma^2) is 0.0706, which is quite large given the magnitude 
#of the overall effect (beta_0 = 0.0297). Put differently, the true effects' heterogeneity is more than twice the magnitude of true effects (*CV* = 2.37)

#- Relative heterogeneity measure

#We can use I^2 statistic to express variance due to differences between studies (not due to sampling variance) in the case of random-effects model. 
#We have developed a function `i2_ml()` (`orchaRd` package) to compute Equations 7 to 9 in the main text: 

i2_ml(mod_ML_lnRR_VCV)

#We see there is a small between-study heterogeneity (indicated by I^2_study), while a large within-study heterogeneity (indicated by I^2_effect). 
#If we use a random-effects model to quantify heterogeneity, we might have concluded that the high heterogeneity is due to between-study differences 
#(i.e. tau^2 and I^2). There is an argument `boot` that allows us to compute the (percentile) CIs of I^2. 
#Note that calculating CIs of I^2 may take a long time to run because `i2_ml()` uses the (parametric) bootstrapping method.


########################################################
############### Explaining heterogeneity ###############
########################################################

#The **third aim** of a meta-analysis is to **explain the heterogeneity**. 
#To answer this question, it is recommended to use meta-regression models rather than subgroup analysis 
#(where one divides the dataset according to one predictor variable [sex: male vs. female] and conducting 
#separate meta-analyses for each subset). 

#In the context of meta-analysis, we often name a predictor variable as a moderator, 
#indicating that the magnitude of overall effect may be modified systematically as a function of the moderator.

#Building upon the multilevel model (Equation 3), a meta-regression model can be constructed by adding one moderator variable 
#(*AKA* predictor, independent/explanatory variable, or fixed factor):
  
#Model coefficient beta_1, denotes the slope of the moderator variable x_1, and how it affects the magnitude of the overall effect.

#If we are interested in the effects of more than one moderator, we can put the moderator variables into a single 
#meta-regression model, leading to multiple meta-regression (note that post-hoc hypotheses are generally not recommended, 
#so you need to have a clear hypothesis about each variable, rather than adopting a data-driven strategy):
  
#We can fit a (multiple) regression model (as formulated as Equations 10 and 11) using function `rma.mv()`. 
#The moderator, x_1, can be supplied to the argument `mods` using following formula: starting with a tilde `~`, 
#followed by the name of the moderator (e.g., `mods = ~ x_1`).  

#In our worked example, we can examin whether intraspecific leaf trait variation (measured as lnRR) in response to 
#difference in elevation. To answer this question, we can construct a meta-regression model with difference in 
#elevation as a moderator, which is coded as *elevation* column in the dataset (we log-transformed the value of 
#elevation for each study to achieve normality assumption and labelled it as *elevation_log*). 
#Setting `mods = ~ elevation_log` can fit such a meta-regression model with:
  
mod_MLMR_lnRR_elevation <- rma.mv(yi = lnRR, 
                                    V = VCV, 
                                    mods = ~ elevation_log, # adding elevation as a moderator variable (for continuous variable, it is a good practice to log-transform them to avoid data skewness);
                                    random = list(~1 | Study_ID, 
                                                  ~1 | ES_ID), 
                                    method = "REML", 
                                    test = "t", 
                                    data = dat2_Midolo_2019,
                                    sparse = TRUE
  )

# print model results
summary(mod_MLMR_lnRR_elevation)
#The results of the above fitted meta-regression model are very similar with those of a multilevel meta-analytic model 
summary(mod_ML_lnRR_VCV)

#Here, we only discuss results that are different from those from the multilevel meta-analytic model.

#- **Test for Residual Heterogeneity**  
#`QE` = test statistic used to test whether there is a large amount of "residual heterogeneity" among effect sizes. "Residual heterogeneity" means the amount of heterogeneity that is not explained by the included moderator (here is *elevation*). 
#`p-val` < 0.0001 = residual heterogeneity is still substantial (which is statistically significant larger than that of sampling variance).

#- **Test of Moderators (coefficient 2)**
#This section denotes omnibus test of all model coefficients or joint test of the null hypotheses:
#`coefficient 2` = 2 coefficients are tested. In our case, intercept ($\beta_0$) and slope ($\beta_1$) of $x_1$ (i.e., *elevation*).
#`F(df1 = 1, df2 = 1292)` = the omnibus test is based on F-distribution with *m* (2) and *k* (1294) - *p* (2) (degrees of freedom with *m* representing the number of model coefficients tested and *p* the total number of model coefficients). We recommend to use a F-distribution rather than a chi-square distribution to improve the inference performance. To do so, set `test = "t"` rather than `test = "z"` (default of `rma.mv()`).
#`p-val = 0.0047` = *p-value* of this test, indicating that we can reject the null hypothesis that elevation has a null effect on leaf traits.

#- **Model Results**
#`intrcpt` = intercept $\beta_0$ in Equation 10. Note that this $\beta_0$ is distinct from $\beta_0$ (i.e., overall effect) in Equations 1 - 3. $\beta_0$ here means the average effect size (lnRR) with a elevation of 0. To put it in another way, setting $x_1$ = 0 can get intercept $\beta_0$.
#`elevation_log` = slope beta_1 of x_1 (in this case *elevation*), denoting the estimated effect of the tested moderator x_1.

#With respect to the hypothesis tested, meta-regression confirms that elevation has a positive relationship with intraspecific
#leaf trait (quantified as lnRR).



########################################################
############ Side note on categorical moderators #######
########################################################

#As explained in our main text, a categorical moderator also can be incorporated into a meta-regression by creating ‘dummy’ 
#variables representing the various levels/subgroups (see main text for an example). 
#`R` can create dummy variables for us automatically. 
#Therefore, parameterizing a meta-regression with a categorical moderator is the same as meta-regression with a continuous moderator,
#with one exception: we can decide whether to keep or remove intercept (see details below). 

#Let's say we aim to investigate whether the effect of elevation differs depending on leaf trait types. 
#This variable is coded as *trait* in the dataset, including 7 levels: specific leaf area (*SLA*), 
#leaf mass per area (*LMA*), leaf area (*LA*), nitrogen concentration per unit of area (*Narea*), 
#nitrogen concentration per unit mass (*Nmass*), phosphorous concentration per unit mass (*Pmass*) 
#and carbon isotope composition (*d13C*).

#Traditionally, we would divide the dataset into 7 subgroups and subsequently conduct 7 separate meta-analyses. 
#A more powerful method is to fit a meta-regression with this categorical moderator (*trait*):

mod_MLMR_lnRR_trait <- rma.mv(yi = lnRR, 
                              V = VCV, 
                              mods = ~ trait -1, # setting '-1' is a useful strategy to remove intercept and then directly obtain the estimated effect (slope) for each subgroup or for a particular level within the categorical moderator.
                              random = list(~1 | Study_ID, 
                                                ~1 | ES_ID), 
                              method = "REML", 
                              test = "t", 
                              data = dat2_Midolo_2019
)

#The corresponding model output is then:
summary(mod_MLMR_lnRR_trait)

#Now, results under **Test of Moderators** are joint test of null hypotheses for slopes of 7 dummy variables 
#(beta_1 to beta_7; no intercept beta_0 because we remove it via `-1`):
#`p-val < .0001` means the null hypotheses are rejected, suggesting that the trait types as a whole indeed
#affect the average effect of elevation (there is at least one subgroup showing a statistically significant effect). 

#The results printed under `Model Results` directly give the estimated effect for each of 7 traits because removing
#intercept from model (by `'-1'`) can make all dummy variables be incorporated in the model as moderators. 
#We see that 5 traits (*LA*, *LMA*, *Narea*, *Nmass* and *Pmass*) have statistically significant effects, while 
#the other 2 traits (*dC13* and *SLA*) do not.

#- **Technical Recommendation**
#Setting `mods = ~ x` (in this case, `mods = ~ trait`) will keep intercept (beta_0) in the model: 
mod_MLMR_lnRR_trait2 <- rma.mv(yi = lnRR, 
                              V = VCV, 
                              mods = ~ I(trait),
                              random = list(~1 | Study_ID, 
                                            ~1 | ES_ID), 
                              method = "REML", 
                              test = "t", 
                              data = dat2_Midolo_2019
                              )
# summary results
summary(mod_MLMR_lnRR_trait2)

#By default, `R` alphabetizes the dummy variables. In our case, *dC13* is set as reference level and 
#associated dummy variable is taken out from the meta-regression model. Why *dC13* rather than other dummy variables? 

#Because the letter "d" (*dC13*) comes before other letters, for example, "L" (*LA*) and "S" (SLA). 
#Therefore, the model intercept $\beta_{0}$ (`intrcpt`) represents the estimated effect for subgroup of *dC13* 
#(beta_0 = -0.0136, 95CI% = [-0.0613 to 0.0341], *p-value* =  0.5758). 

#The remaining model coefficients denote contrasts between the reference level (`intrcpt` or *dC13*) and the other 6 levels/groups. 
#Therefore, removing the intercept can allow us to test whether different levels/subgroups differ in terms of average effect. 
#If we aim to compare the estimated effect of one level to other levels, we can set this level as reference by function `relevel()` 
#or `factor()` prior to model fitting. 


#Alternatively, we can use function `anova()` to obtain the contrasts. 
#Note, if you use a subgroup analysis rather than the recommended meta-regression to explain heterogeneity, 
#you have to do it by hand: using Wald-type test to calculate the test statistics and then performing significance tests. 



####################################################
################# Goodness-of-fit ##################
####################################################

#The goodness-of-fit index R^2 can be used to quantify the percentage of variance explained by a moderator. 
#This index has implications of the importance of the examined moderator. 
#It also can be used for model section to select a model with the 'best' set of moderators. 
#A general and readily implementable form of R^2 is the marginal version: 

r2_ml(mod_MLMR_lnRR_elevation)
#The result given under `R2_marginal` shows that elevation can explain 0.98% of the variation between effect sizes.  




###########################################################
####### Visualisation and interpretation ##################
###########################################################

## Classic forest plots
#For environmental meta-analyses, a forest plot is often used to visualize the distribution of effect sizes and the 95% CIs for each study, 
#as well as the overall effects based on model (e.g., beta_0 and its 95% CIs). 
#A classic forest (**Figure S1**) can be made by function `forest()` in `metafor` package:

forest(mod_ML_lnRR_VCV, # rma.mv object
       xlab = "Effect size lnRR")
#An example of forest plot showing the effect sizes from each study (and their 95% CIs) and the overall effect based on model.


#On the bottom of the forest plot made by `forest()`, there is a four-sided polygon (well known as the 'diamond') representing the overall effect 
#based on the fitted model (in our case, *mod_ML_lnRR_VCV* -  `rma.mv` object containing outputs of a multilevel model). 
#The center of the diamond denotes the point estimate and the left/right edges correspond to lower and upper CI boundaries. 
#However, we can not clearly see these different features because the number of studies is very large for this dataset (it gets a bit squashed). 

#Recently, some new types of figures that build upon forest plots have been proposed, including the ‘caterpillar’ plot and 'orchard' plot. 


## Underappreciated plots

#Function `orchard_plot()` in `orchaRd` package can make a forest-like plot (termed as 'orchard' plot) that can accommodate large number of studies. 
#A basic orchard plot can be made with:

orchard_plot(mod_ML_lnRR_VCV, 
             mod = "1", 
             xlab = "Effect size lnRR", 
             group = "Study_ID",  k = TRUE, g = TRUE, trunk.size = 1.5, 
             data = dat2_Midolo_2019) + 
             scale_x_discrete(labels = c("Overall effect")) 


#Orchard plot (forest-like plot) showing the effect sizes from each study (and their 95% CIs), the overall effect and its 95% CIs and 95% prediction intervals.  
#We see that an orchard plot is more informative than a classic forest plot. For example, it shows the distribution of the effect sizes, 
#the number of effect sizes (*k*) and the number of studies (the number in the bracket). An orchard plot not only can visualize the meta-analytic results, 
#but also lets us realize something we are not bale to see from statistical results, such as influential data points and outliers that could threaten the robustness 
#of our results. Further, an orchard plot not only show 95% CIs (thick whiskers)  but also 95% prediction intervals (PIs; thin whiskers), 
#through which we can visually check the heterogeneity among effect sizes. Because CIs only include the standard error of the overall effect beta_0 (SE[beta_0]), 
#which PIs include variance of random effects: 
#If you want to know the detailed value of PIs, you can use `predict`:

### calculate the 95% prediction intervals
predict(mod_ML_lnRR_VCV)


#- **Visualize meta-regression with a categorical moderator**

#A fabulous function of `orchard_plot()` is that it also can nicely show results based on meta-regression model. 
#The results based on meta-regression with *trait* as a moderator can be visualized with:


#Orchard plot (forest-like plot) showing the effect sizes from each study and the overall effect for each subgroup/levels of a given categorical moderator.
orchard_plot(mod_MLMR_lnRR_trait, 
             mod = "trait", 
             xlab = "Effect size lnRR", 
             group = "Study_ID",  k = TRUE, g = TRUE, trunk.size = 1.5, 
             data = dat2_Midolo_2019) + 
             scale_x_discrete(labels = c("SLA","Pmass","Nmass","Narea","LMA","LA","dC13"))



#- **Visualize meta-regression with a continuous moderator**

#For a meta-regression with a continuous moderator, we can use `bubble_plot()` to visualize the results. Let's visualize the meta-regression with *elevation* as the continuous moderator:

#Bubble plot showing the results of a meta-regression model: 
#relationship between a continuous moderator (in our case, *elevation*) and effect size magnitude (lnRR).  
bubble_plot(mod_MLMR_lnRR_elevation, 
              mod = "elevation_log", 
              xlab = "Elevation (log-transformed)", ylab = "Effect size lnRR",
              group = "Study_ID",k = TRUE, g = TRUE,
              data = dat2_Midolo_2019, legend.pos = "top.left") +
  theme(axis.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(size = 12, colour = "black"))

 




######################################################################
############# Checking for publication bias and robustness ###########
######################################################################

## Detecting and correcting for publication bias

#- **Detecting small study effect**
# The most well-known form of publication bias is the **small study effect**, where effect size values from a "small" studies, with low replication and 
# therefore large uncertainty and low precision, show different, often larger, treatment effects than large studies. A straightforward way to detect small study 
# effect is to add the uncertainty of effect size as a moderator, such that the relationship between effect size and its uncertainty can be quantified. 
# We propose to formulate Egger's regression (which is a classic method to detect the symmetry of a funnel plot) in the framework multilevel model to detect the 
# small-study effect for dependent effect sizes:


# Sampling error is a typical measure of effect size uncertainty. However, for some types of effect size, for example, SMD, 
# effect size uncertainty has a intrinsic relationship with its sampling error. 
# Therefore, the sample size is not a valid moderator for detecting a small-study effect. 
# In Equation 16, we use an adapted sampling error based on effective sample size as the moderator. 

# Let's calculate effect sample size (~N) for SMD in our example:
  
ess.var_cal <- function(dat){1/dat$n_control + 1/dat$n_treatment} # write a help function to calculate adapted sampling variance based on effective size based  - tilde n
dat2_Midolo_2019$ess.var <- ess.var_cal(dat2_Midolo_2019) # calculate tilde N
dat2_Midolo_2019$ess.se <- sqrt(dat2_Midolo_2019$ess.var) # calculate adapted sampling error based on effective size - tilde square root n


#Then the Equation 16 can be fitted with:
mod_MLMR_lnRR_ess.se <- rma.mv(yi = lnRR, 
                                 V = VCV, 
                                 mods = ~ ess.se, # add adjusted based sampling error - tilde square root n as a moderator to test small study effect. 
                                 random = list(~1 | Study_ID, 
                                               ~1 | ES_ID), 
                                 method = "REML", 
                                 test = "t", 
                                 data = dat2_Midolo_2019
)

#The outputs of the above fitted model are exactly the same as those of a meta-regression model:
# summary results
summary(mod_MLMR_lnRR_ess.se)

#If you followed the early sections of this tutorial, we assume you can interpret these results by yourself. 
#Briefly, if we look at `ess.se` given under **Model Results**: *p-value* and 95% CIs suggest that there is no 
#statistical relationship between the effect size its error, meaning that no small study effect exits. 


#The bubble plot also indicates that there is no visual correlation between effect size and its error - 
#effect size symmetrically distribute on the funnel plot.

#Bubble plot showing the relationship between the effect size magnitude and its adjusted sampling error 
#(effective sample size based). Small studies (low precision or high SE) do not report large effect sizes.
bubble_plot(mod_MLMR_lnRR_ess.se, 
            mod = "ess.se", 
            xlab = "Adjusted sampling error", ylab = "Effect size lnRR",
            group = "Study_ID", k = TRUE, g = TRUE,
            data = dat2_Midolo_2019, legend.pos = "top.left") +
  theme(axis.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(size = 12, colour = "black"))



# make a funnel plot
#Visual inspection of the typical funnel plot to identify the small study effect.
funnel(mod_MLMR_lnRR_ess.se, yaxis = "seinv",
       xlim = c(-3, 3),
       ylab = "Precision (1/SE)",
       xlab = "Effect size lnRR")

#It is important to note that funnel plots using either inverse standard error or ‘effective’ sample size are not reliable for 
#detecting publication bias. This is because they are only visual checks. 
#To properly detect publication bias, a regression-based statistical test needs to be used, as shown above.

# calculate tilde N
n_tilde_cal <- function(dat){(dat$n_control * dat$n_treatment) / (dat$n_control + dat$n_treatment)} 
dat2_Midolo_2019$n_tilde <- n_tilde_cal(dat2_Midolo_2019)

#Visual inspection of the funnel plot based on effective sample size to identify the small study effect.
funnel(dat2_Midolo_2019$lnRR, dat2_Midolo_2019$lnRRV, ni = dat2_Midolo_2019$n_tilde, 
       yaxis = "ni",
       xlim = c(-3, 3),
       ylab = "Effective sample size",
       xlab = "Effect size (lnRR)")



#- **Detecting decline effect**
  
#The decline effect, also known as time-lag bias, is another prominent form of publication bias, where effect sizes tend to get closer to zero over time. 
#Testing for a decline effect is important because the temporal changes in evidence of a given field poses a threat to environmental policy-making, management, 
#and practices. Decline effects can be tested by a meta-regression with publication year (centered to ease interpretation) as a moderator:

### create a variable containing publication year
dat2_Midolo_2019 <- dat2_Midolo_2019 %>% mutate(Year = as.integer(str_extract(study_name,"\\d+")))

### center publication year to ease interpretation of the results
dat2_Midolo_2019$Year.c <- dat2_Midolo_2019$Year - mean(dat2_Midolo_2019$Year) # we will use this variable as a predictor to estimate decline effect

#Equation 18 can be fitted with code: 
mod_MLMR_lnRR_Year.c <- rma.mv(yi = lnRR, 
                                 V = VCV, 
                                 mods = ~ Year.c, # add centered year as a moderator to detect decline effect.
                                 random = list(~1 | Study_ID, 
                                               ~1 | ES_ID), 
                                 method = "REML", 
                                 test = "t", 
                                 data = dat2_Midolo_2019,
                                 sparse = TRUE
)

# summary results
summary(mod_MLMR_lnRR_Year.c)

#As you see in the results,regression slope is `Year.c` = 0.002 (95% CIs = [-0.0022 to 0.0062]), 
#which is very small and not statistically different from zero (`t_value` = 0.9135 and `p-val` = 0.3611), 
#suggesting studies with statistically significant results do not publish earlier than these with statistically 
#non-significant results results, i.e. no time-lag bias.   

#Bubble plot showing the relationship between the effect size magnitude and publication year (centered on 2008). There is no temporal trend in the changes of the effect size magnitude.
bubble_plot(mod_MLMR_lnRR_Year.c, 
            mod = "Year.c", 
            xlab = "Publication year (centered on 2008)", ylab = "Effect size lnRR",
            group = "Study_ID", k = TRUE, g = TRUE,
            data = dat2_Midolo_2019, legend.pos = "top.left") +
  theme(axis.text.x = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"),
        plot.title = element_text(size = 12, colour = "black"))





#- **Correcting for publication bias**
  
#Besides detecting publication bias, it is necessary to correct for such a bias to check the robustness of model estimates. 
#The intercept beta_0 in Equation 17 can be interpreted as the publication-bias-corrected overall effect ('true' effect). 
#Because beta_0 is the model coefficient when setting ~N = 0, which means the dataset has a infinite sample size (high precision) 
#and thus has no small-study effect. To obtain publication-bias-corrected overall effect, we need to fit Equation 17 with:
  
  mod_MLMR_lnRR_ess.var <- rma.mv(yi = lnRR, 
                                  V = VCV, 
                                  mods = ~ ess.var, # adding adjusted sampling error to obtain publication-bias-corrected overall effect - which is potentially regarded as 'true' effect.
                                  random = list(~1 | Study_ID, 
                                                ~1 | ES_ID), 
                                  method = "REML", 
                                  test = "t", 
                                  data = dat2_Midolo_2019,
                                  sparse = TRUE
  )

# summary results
summary(mod_MLMR_lnRR_ess.var)
  
#`intrcpt` printed under **Model Results** shows that intercept beta_0 (-0.0029, 95% CIs = [-0.0659 to 0.0602]) 
#is still statistically non-significant (*p-value* = 0.9285), although the magnitude and direction change. 
#This suggests that the meta-analytic estimate of this dataset is robust to publication bias.



#Comparison of original meta-analytic estimates and bias-corrected meta-analytic estimates. 

### make a table to compare the original effect size and the adjusted effect size
t4 <- data.frame("Overall effect" = c(round(mod_ML_lnRR_VCV$b[1],4), round(mod_MLMR_lnRR_ess.var$b[1],4)),
                 "Standard error" = c(round(mod_ML_lnRR_VCV$se,4), round(mod_MLMR_lnRR_ess.var$se[1],4)),
                 "p-value" = c(round(mod_ML_lnRR_VCV$pval,3), round(mod_MLMR_lnRR_ess.var$pval[1],3)),
                 "Lower CI" = c(round(mod_ML_lnRR_VCV$ci.lb,4), round(mod_MLMR_lnRR_ess.var$ci.lb[1],4)),
                 "Upper CI" = c(round(mod_ML_lnRR_VCV$ci.ub,4), round(mod_MLMR_lnRR_ess.var$ci.ub[1],4)))

colnames(t4) <- c("Overall effect", "Standard error", "p-value", "Lower CI", "Upper CI")

t4_2 <- t(t4) %>% as.data.frame()
colnames(t4_2) <- c("Original estimate", "Bias-corrected estimate")

t4_2 %>% DT::datatable()


#there's a lot more in the main tutorial so do take a look at it in its entirety. This R script is just an easier way 
#for the purposes of this module for you to play around with the code in R.
