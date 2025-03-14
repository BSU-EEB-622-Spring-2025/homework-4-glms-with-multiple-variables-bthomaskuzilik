

#---------------------#
# HOMEWORK 4 SUBMISSION
#---------------------#


# Becca Thomas-Kuzilik 
# EEB 622: Homework #4
# Due March 13, 2025 


library(marginaleffects)
library(performance)
library(MASS)
library(pROC)


#-----------------------------------------------#
#-----------------------------------------------#
# QUESTION 1: MISTLETOE IMPACTS ON SEEDLINGS ####
#-----------------------------------------------#
#-----------------------------------------------#


# ------------------------#
# ....1a) question.....####
# ------------------------#


# Fit a glm assessing evidence for the following hypothesis: Seedling density is increased beneath trees experiencing mistletoe infection. Describe the rationale you used in selecting a glm structure and probability distribution. Calculate model fit using MAE.


# ----------------------#
# ....1a) answer.....####
# ----------------------#


# Bring in the dataset
mistletoe <- read.csv("mistletoes.csv")

# Using the metadata from the README file, we know for this dataset:

# TreeID = A unique identifier for each tree (sampling unit)
# Treatment = A categorical variable describing whether a tree is parasitized by mistletoe or not
# Seedlings = A count of the number of seedlings in 5m x 5m plots beneath parasitized and unparasitized trees, during the authors’ 2011 and 2012 surveys.
# Year = Whether the survey was taken in 2011 or 2012.

# Look at the data
head(mistletoe)
length(unique(mistletoe$TreeID))
table(mistletoe$Treatment)
range(mistletoe$Seedlings)
hist(mistletoe$Seedlings)

# From the question, we know that:

# $Seedlings (seedling density under each tree) = our response variable
# $Treatment (whether tree was parasitized by mistletoe or not) = our dependent variable

# Since our response variable is discrete and bounded by 0 on the lower end, we have the option to use the binomial, poisson, or negative binomial distribution.

# We are choosing to use the negative binomial distribution. From looking at the range of seedlings found under trees, we can see that our data is overdispersed (there are a lots of zero values, but also some really big numbers). Because of this overdispersion, negative binomial is a better fit than poisson. Also, we shouldn't use the binomial distribution because we don't know the upper limit of what is possible.

# We can eyeball overdispersion (i.e., variation > mean) with a histogram, but we can also check it explicitly.
mean(mistletoe$Seedlings)
var(mistletoe$Seedlings)
isTRUE(var(mistletoe$Seedlings) > mean(mistletoe$Seedlings))

# Now, we want to fit our negative binomial model to assess if/how mistletoe infection influences seedling density beneath host trees.

# We need to use the function glm.nb() from the MASS package to properly use our chosen probability distribution. We just need to specify our response and dependent variable + our data - log is the default link function, which is what we need.
mod_mistletoe <- glm.nb(Seedlings ~ Treatment, data = mistletoe)

# Look at the output of the model
summary(mod_mistletoe)

# Assess the model fit using MAE 
mae(mod_mistletoe)
# I couldn't really find we talked about this, but I think you interpret it on the scale of the response variable. So in this case, we get a mae = 145.8 seedlings, which I think is (maybe) an error metric that is telling us our model may be off by +/- 145.8 seedlings. This doesn't seem that bad when you consider that max(mistletoe$Seedlings) = 2472.


# ------------------------#
# ....1b) question.....####
# ------------------------#


# Use visual (e.g. a marginal effects plot) and written (e.g. effect sizes on scale of response) approaches to interpret the results of your model.

#Based on your fitted model results and model fit, write 3-4 sentences to discuss the following biolgical conclusions: Does mistletoe infection alter seedling density? How much does seedling recruitment differ beneath parasitized and unparasitized trees? Explain which elements of your glm results informed your conclusions and annotate the steps you needed to take to interpret your parameters.


# ----------------------#
# ....1b) answer.....####
# ----------------------#


summary(mod_mistletoe)
plot_predictions(mod_mistletoe, condition="Treatment")
predictions(mod_mistletoe, newdata=data.frame(Treatment=c("parasitized", "unparasitized")))

# From summary table:

# We know that there's a statistically significant difference in the # of seedlings under parasitized vs. non-parasitized trees (p < 0.05). We can't directly interpret the beta-coefficients without doing a backtransformation. 

# From plot_predictions:

# There are more seedlings under trees with mistletoe parasites. 




## 1c) 

# During the course of this study, 2012 was an atypically rainy year, compared to 2011. Fit an additional glm that quantifies how the effect of mistletoe differs between the two years in this study. Write ~2 new sentences that summarize the results of the new model and their biological implications.

# Interaction 
mod3 <- glm.nb(Seedlings ~ Treatment * as.factor(Year), data = mistletoe)
summary(mod3)

plot_predictions(mod3, condition=c("Year", "Treatment"))

predictions(mod3, newdata=data.frame(Treatment=c("parasitized", "unparasitized"),
                                     Year =c("2011", "2011",
                                             "2012", "2012")))


## Question 2:

treemortality <- read.csv("treemortality.csv")
head(treemortality)

## 2a) Fit a glm (using a probability distribution of your choice) that reflects the following research question (including thinning as your only predictor and mortality as your response): Do forest thinning treatments reduce the probability of tree mortality? Interpret the results of the glm by writing 2-4 sentences about the biological significance of the effect of thinning, including descriptions of the sizes of the effect on the scale of the response variable, evidence for/against the hypothesis, and a metric of model fit.

# We can use binomial distribution because both our response and our dependent variables are categorical (0/1), bounded (0-1)

mod_tree <- glm(mortality ~ thinning, data = treemortality,
                family="binomial"(link="logit"))
summary(mod_tree)
plogis(0.9933)
plogis(0.9933-1.8559)

# Mortality in unthinned forest is 73.0%
# Mortality in thinned forest is 29.7%

test_prob <- predict(mod_tree, type = "response")
# Next, we see how well the simulated data/yhats did compared to the actual data
test_roc <- roc(treemortality$mortality ~ test_prob, 
                plot = TRUE,  
                print.auc = TRUE)

# AUC = 0.710


## 2b)

# LOOK AT COLLINEARARITY EXAMPLE.
# OR GRASSHOPPER 

# Because they randomized their experimental design, the researchers removed the confounding influence of tree size on thinning and therefore they don't need to include it in their model. There is no correlation structure, so there won't be an impact on the effect size 

# CAN PUT IT IN THE MODEL AND TEST THIS

## 2c) 

mod_tree <- glm(mortality ~ thinning, data = treemortality,
                family="binomial"(link="logit"))
summary(mod_tree)
plot_predictions(mod_tree, condition= "thinning")


mod_tree_dag <- glm(mortality ~ thinning + slope + roaddist, data = treemortality,
                    family="binomial"(link="logit"))
summary(mod_tree_dag)

# To intrept, have to hold all other values constant
# plogis() is limited when we have multiple variables, so predictions() and plot_predictions() are better
predictions(mod_tree_dag, newdata=data.frame(thinning = c(0, 1),
                                             slope = mean(treemortality$slope),
                                             roaddist = mean(treemortality$roaddist)))
plot_predictions(mod_tree_dag, condition=c("thinning"))
plot_predictions(mod_tree, condition = c("thinning"))

# The estimate decreases, the relationship between thinning and mortality was confounded by slope and roaddist - so we were overestimating the effect size. When we included slope and roaddist into our model as suggested, we still see there is a negative relationship, but it is smaller (look at the predictions() table values)

# THESE PLOTS TELL YOU THE RELATIONSHIPS BETWEEN THE ADDED VARIABLES AND MORTALITY (ASSUMES YOU ARE HOLDING xxx SOMETHING I CAN'T REMEMBER CONSTANT)
plot_predictions(mod_tree_dag, condition=c("roaddist", "thinning"))
plot_predictions(mod_tree_dag, condition=c("slope", "thinning"))


