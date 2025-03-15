

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

# Based on your fitted model results and model fit, write 3-4 sentences to discuss the following biological conclusions: Does mistletoe infection alter seedling density? How much does seedling recruitment differ beneath parasitized and unparasitized trees? Explain which elements of your glm results informed your conclusions and annotate the steps you needed to take to interpret your parameters.


# ----------------------#
# ....1b) answer.....####
# ----------------------#


# Look at the results
summary(mod_mistletoe)
# From the summary table, we can see that there's a statistically significant difference in the # of seedlings under parasitized vs. non-parasitized trees (p < 0.05). However, since we used a negative binomial distribution with a log link function, we can't interpret the intercept or beta-estimate directly. Instead, we have to use plot_predictions() and predictions() from the marginaleffects package to interpret (we could also do it by hand with exp(5.7308) & exp(5.7308- 3.1575) since the model is so simple, but the functions are better).

# In plot_predictions(), the first argument is the model, the "condition =" argument is the dependent variable/what you want on the x-axis/to be compared
plot_predictions(mod_mistletoe, condition = "Treatment")
# Immediately, we can see that as predicted, seedling recruitment is higher under trees that have been parasitized by mistletoe vs. those that have not been parasitized - we know this by looking at the points (mean seedling density, y-axis) for each treatment on the x-axis.

# Using predictions(), you can get the same data/values in a numeric format. The first argument is the model and I'm not 100% sure what the "newdata = data.frame()" argument is doing beyond you tell it what you want to interpret. So in this case, we want to know the effect of mistletoe treatment (dependent variable, which will become the rows in the table the function will create representing "parasitized" and "unparasitizied" treatments) on seedling recruitment (the values, backtransformed to be on the scale of the response variable)
predictions(mod_mistletoe, newdata = data.frame(Treatment = c("parasitized", "unparasitized")))
# In the table, we can see the same numbers as in our figure. The top row is (I think) the mean number of seedlings under parasitized trees with 95% CIs (mean = 308.2, not sure why the upper CI is slightly larger than the lower CI?). The bottom row is (I think) the mean number of seedlings under unparasitized trees. 


# To summarize/interpret our findings in a paper, we could write something like:

# Mistletoe infection has a statistically significant impact on seedling density (p < 0.05). Mean seedling density in the 5m2 plots under trees parasitized by mistletoe was 308.2 , while mean seedling density in the same area under trees without mistletoe was only 13.1. This represents a 23.5% increase in recruitment.


# ------------------------#
# ....1c) question.....####
# ------------------------#


# During the course of this study, 2012 was an atypically rainy year, compared to 2011. Fit an additional glm that quantifies how the effect of mistletoe differs between the two years in this study. Write ~2 new sentences that summarize the results of the new model and their biological implications.


# ----------------------#
# ....1c) answer.....####
# ----------------------#


# Because we want to look at how different years influences the magnitude of mistletoe's effect, we want to add Year as an interaction in the model. We want to add Year as an interaction term vs. an second variable because we aren't interested in how Year influences an overall mean Seedlings (lumping seedling recruitment from both parasitized and unparasitized trees together) - we still want to know how mistletoe alters recruitment, just across a range of years that may differ in some way (in this case, amount of rain)... I think
mod_mistletoe_yrInt <- glm.nb(Seedlings ~ Treatment * as.factor(Year), data = mistletoe)

# We can look at the results to see significances, but just like before, we have to do a backtransformation before we can interpret them.
summary(mod_mistletoe_yrInt)
# We can see that this summary table has two additional rows. (Intercept) is now mean seedling density under parasitized trees in 2011, Treatmentunparasitized is mean seedling density under unparasitized trees in 2011, as.factor(Year)2012 is mean seedling density under parasitized trees in 2012, Treatmentunparasitized:as.factor(Year)2012 is s mean seedling density under unparasitized trees in 2012. 

# For this model specifically, I am a little confused  on interpreting significance (some results are very significant and other aren't, I can't remember if you look at each line seperately or some together). Best guess - we can see that there is a statistically significant impact of mistletoe on seedling recruitment in 2011 (p < 0.05 for both (Intercept) and Treatmentunparasitized), and a nearly significant relationship of mistletoe on seedling recruitment in 2012 (p = 0.07 for as.factor(Year)2012 and p = 0.06 for Treatmentunparasitized:as.factor(Year)2012)

# When you're interpreting results from a model with an interaction, to get effect sizes, you have to add or subtract the value in one line from another one (and maybe also from the Intercept??), but I can't remember exactly. Instead, we can just use our functions from the marginaleffects package with slightly fancier arguments.

# To plot correctly, now the "condition = " argument has to include both the interaction (listed first) and the dependent variable
plot_predictions(mod_mistletoe_yrInt, condition = c("Year", "Treatment"))
# We can see that there's still higher seedling density under parasitized trees vs. non-parasitized trees, but that overall there was higher seedling recruitment in 2012 (the atypically rainy year) for both treatments than in 2011.

# Just like before, we can get the same values in a numeric table. This time, we want our dataframe to include mean seedling recruitment for parasitized trees in 2011, unparasitized trees in 2011, parasitized trees in 2012, and unparasitized trees in 2012. The order of Year values is important - c("2011", "2012", "2011", "2012") would give you incorrect results - but I'm not exactly sure why you list them in the argument like you do... basically, why do we have to repeat Year but not Treatment?
predictions(mod_mistletoe_yrInt, newdata = data.frame(Treatment= c("parasitized", "unparasitized"),
                                                      Year = c("2011", "2011", "2012", "2012")))
# We can see the same numbers as in the plot! Also, important to note that the means of the parasitized/unparasitized seedling densities between the two years is want we got from the model without the interaction. So the interaction gives us more specific, non-pooled results for when there might be variation in some variable. 

# Our new results may add: 

# Interannual variation (e.g., amount of precipitation) had a near significant impact on the relationship between seedling recruitment and mistletoe parasitism. In both 2011 and 2012, seedling density was higher under parasitized trees, but that relationship was even stronger in 2012, an unusually wet year.


#---------------------------------------------------------------#
#---------------------------------------------------------------#
# QUESTION 2: TREE MORTALITY IN THINNED AND UNTHINNED FORESTS####
#---------------------------------------------------------------#
#---------------------------------------------------------------#


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


