######
# this script is for a course project in the Psych Dept at Tufts University
######
# Project title: 
#    Common Ground and Selfishness:  Does Egocentricity Mean the 
#    Same Thing for Linguists and Social Psychologists? 
# Prof: JP de Ruiter
# Author: Paul E. Plonski
# Course: PSY212 Human Communication
# Term: Fall 2020
# Due: 30 November 2020

###############################
### Begin power calculation ###
###############################

# using a frequentist approach, we conducted a power analysis by
# calculating the required sample size to detect a small to medium sized 
# r between any one of four predictor variables using one-tailed 
# significance tests and an overall alpha = .05

# we selected r = .2 as the minimum effect of interest for this study
# we chose one-tailed tests because we hypothesize a direct (positive)
#  correlation between social and linguistic egocentricity 
# we used a Bonferroni correction on the overall type 1 error rate (.05)
#  to split between the four predictors, with each model assigned an error
#  rate of .05/4 = .0125
# 
library(pwr)
pwr.r.test(n=NULL, r = .2, sig.level = .0125, power = 0.95, 
           alternative = "greater")


#############################
### End power calculation ###
#############################



#############################
### Begin data simulation ###
#############################
### COMM DATA ###
# Horton & Keysar, 1996 report a probability dist for the privileged condition 
# with a mean ~ .08, no sd reported
# set seed for reproducibility
set.seed(44)
# create vector of NAs for simulated data
lin.ego.scores <- rep(NA, 420)
# loop 420 times, drawing from normal dist mean=.08, sd=.05
for (i in 1:420){
  draw <- rnorm(1, mean = .08, sd = .05)
  # when draw is negative, use abs()
  lin.ego.scores[i]<-ifelse(draw>=0, draw, (abs(draw)))
}
# plot the simulated communication data (ratios)
plot(lin.ego.scores)


### NARCISSISM DATA ###
# Ames et al., 2006: NPI-16 has mean ~0.3-0.4 and sd~0.2

# set seed for reproducibility
set.seed(32)
# create vector of NAs for simulated data
narcissism.scores <- rep(NA, 420)
# loop 420 times, drawing from normal dist mean=.08, sd=.05
for (i in 1:420){
  draw <- rnorm(1, mean = .35, sd = .20)
  narcissism.scores[i] <- draw
}
plot(narcissism.scores)

### SELFISHNESS DATA ###
# Raine & Uh, 2019 egocentric subscale had means~3.5-6.25 and sds~2.5-4.5
# we will first plot a distribution based on sums, 
# starting with a rough mean = 4.75 and sd = 3.5
### this code is commented out b/c we use the dist based on avg scores
# set seed for reproducibility
# set.seed(1)
# create vector of NAs for simulated data
# sq.ego.scores.sum <- rep(NA, 420)
# loop 420 times, drawing from normal dist for sum mean=.4.25, sd=3.5
# for (i in 1:420){
#  draw <- rnorm(1, mean = 4.25, sd = 3.5)
#  sq.ego.scores.sum[i] <- draw
#}
# plot(sq.ego.scores.sum)

# calculate the same dist with scores calculated by 
# average rather than sum

# if a range 0 - 16 (sum), mean = 4.25,
#   then
# for a range 0 - 2 (average), mean = 4.25/8
# and
# if a range of 0 - 16 (sum), sd = 3.5,
#   then
# for a range 0 - 2 (average), sd = 3.5/8

set.seed(79)
# create vector of NAs for simulated data
sq.ego.scores.avg <- rep(NA, 420)
# loop 420 times, drawing from normal dist mean=0.53125, sd=0.4375
for (i in 1:420){
  draw <- rnorm(1, mean = 0.53125, sd = 0.4375)
  # when draw is negative, use abs()
  sq.ego.scores.avg[i]<-ifelse(draw>=0, draw, (abs(draw)))
  sq.ego.scores.avg[i] <- draw
}
plot(sq.ego.scores.avg)


### EMPATHIC PERSPECTIVE TAKING DATA ###
# Davis 1980 subscale had means~16.75-18 and no reported no sds
# Braun et al., 2015 (french version) subscale had means~16.55-17 and sd~4.5
# we started with a rough mean = 17 and sd = 4.5
# set seed for reproducibility
# set.seed(1)
# create vector of NAs for simulated data
 iri.pt.scores.sum <- rep(NA, 420)
# loop 420 times, drawing from normal dist for sum mean=18, sd=4.5
 for (i in 1:420){
  draw <- rnorm(1, mean = 18, sd = 4.5)
  iri.pt.scores.sum[i] <- draw
}
 plot(iri.pt.scores.sum)
z.sum <-  scale(iri.pt.scores.sum)
# calculate the same dist with scores calculated by 
# average rather than sum

# if a range 0 - 28 (sum), mean = 18,
#   then
# for a range 0 - 4 (average), mean = 18/7
# and
# if a range of 0 - 28 (sum), sd = 4.5,
#   then
# for a range 0 - 4 (average), sd = 4.5/7

set.seed(56)
# create vector of NAs for simulated data
iri.pt.scores.avg <- rep(NA, 420)
# loop 420 times, drawing from normal dist mean=2.571429, sd=0.6428571
for (i in 1:420){
  draw <- rnorm(1, mean = 2.571429, sd = 0.6428571)
  # when draw is negative, use abs()
  iri.pt.scores.avg[i]<-ifelse(draw>=0, draw, (abs(draw)))
}
plot(iri.pt.scores.avg)



### VERTICAL INDIVIDUALISM DATA ###
# Triandis 1998 reported no means or sds
# Germani et al., 2020 (italian version) subscale had avg mean~5 and sd~1.6
# we started with a rough mean = 5 and sd = 1.6
# set seed for reproducibility
 set.seed(95)
# create vector of NAs for simulated data
indcol.vi.scores.avg <- rep(NA, 420)
# loop 420 times, drawing from normal dist for sum mean=5, sd=1.6
for (i in 1:420){
  draw <- rnorm(1, mean = 5, sd = 1.6)
  # when draw is negative, use abs()
  indcol.vi.scores.avg[i]<-ifelse(draw>=0, draw, (abs(draw)))
}
plot(indcol.vi.scores.avg)

### Generate gender variable ###
gender <- as.numeric(rbinom(420, 1, .5))

### Generate ID ###
subj_id <- as.numeric(1:420)

### Create data frame ###
df <- as.data.frame(cbind(
  subj_id, gender, lin.ego.scores, sq.ego.scores.avg,
  narcissism.scores, iri.pt.scores.avg, indcol.vi.scores.avg))

# References (in order of appearance)
#
# Horton, W. S., & Keysar, B. (1996). When do speakers take into account common ground?. Cognition, 59(1), 91-117.
# Ames, D. R., Rose, P., & Anderson, C. P. (2006). The NPI-16 as a short measure of narcissism. Journal of research in personality, 40(4), 440-450.
# Raine, A., & Uh, S. (2019). The selfishness questionnaire: egocentric, adaptive, and pathological forms of selfishness. Journal of personality assessment, 101(5), 503-514.
# Davis, M. H. (1980). A multidimensional approach to individual differences in empathy. Journal of Personality and Social Psychology, 44(1), 113-126.
# Braun, S., Rosseel, Y., Kempenaers, C., Loas, G., & Linkowski, P. (2015). Self-report of empathy: a shortened french adaptation of the interpersonal reactivity index (IRI) using two large Belgian samples. Psychological reports, 117(3), 735-753.
# Triandis, H. C., & Gelfand, M. J. (1998). Converging measurement of horizontal and vertical individualism and collectivism. Journal of personality and social psychology, 74(1), 118.
# Germani, A., Delvecchio, E., Li, J. B., & Mazzeschi, C. (2020). The horizontal and vertical individualism and collectivism scale: Early evidence on validation in an Italian sample. Journal of Child and Family Studies, 29(3), 904-911.

###########################
### End data simulation ###
###########################



#######################################
### Begin frequentist data analyses ###
#######################################
result1 <- cor.test(lin.ego.scores, sq.ego.scores.avg, data = df)
result1
result2 <- cor.test(lin.ego.scores, narcissism.scores, data = df)
result2
result3 <- cor.test(lin.ego.scores, iri.pt.scores.avg, data = df)
result3
result4 <- cor.test(lin.ego.scores, indcol.vi.scores.avg, data = df)
result4
# run multiple regression with all significant
# predictor variables to compare the strength 
# of associations, if they emerge
result.multiple <- lm(lin.ego.scores~sq.ego.scores.avg+narcissism.scores+
                                    iri.pt.scores.avg+indcol.vi.scores.avg)
print(summary(result.multiple))
#####################################
### End frequentist data analyses ###
#####################################



####################################
### Begin bayesian data analyses ###
####################################
# Goal: calculate credible intervals using
# the BayesFactor package with correlationBF() and posterior()

# first model - selfishness
resultBF1 <- correlationBF(lin.ego.scores, sq.ego.scores.avg, data = df)
resultBF1
samplesBF1 <- posterior(resultBF1, iterations = 10000)
summary(samplesBF1)

# second model - narcissism
resultBF2 <- correlationBF(lin.ego.scores, narcissism.scores, data = df)
resultBF2
samplesBF2 <- posterior(resultBF2, iterations = 10000)
summary(samplesBF2)

# third model - empathic perspective taking
resultBF3 <- correlationBF(lin.ego.scores, iri.pt.scores.avg, data = df)
resultBF3
samplesBF3 <- posterior(resultBF3, iterations = 10000)
summary(samplesBF3)

# fourth model - vertical individualism
resultBF4 <- correlationBF(lin.ego.scores, indcol.vi.scores.avg, data = df)
resultBF4
samplesBF4 <- posterior(resultBF4, iterations = 10000)
summary(samplesBF4)

#####################################
### End bayesian data analyses ###
#####################################
