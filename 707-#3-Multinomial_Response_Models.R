---

library(faraway)
library(tidyverse)
library(nnet)
library(MASS)
library(tidyr)



data(hsb)
hsbm <- na.omit(hsb)

# 1.a) Produce a table showing the proportion of males and females choosing
# the three different programs.

prop.gen <- group_by(hsb, gender, prog) %>% summarise(count = n()) %>%
  group_by(gender) %>% mutate(gtotal = sum(count), proportion = count/gtotal)
prop.gen

# We see that within the females, 53% choose academic program type, 22% choose general, and 25% choose vocational. And withing the males, 52% choose academic, 23% choose general, and 25% choose vocational. 

# Do the same for SES
prop.ses <- group_by(hsb, ses, prog) %>% summarise(count = n()) %>%
  group_by(ses) %>% mutate(gtotal = sum(count), proportion = count/gtotal)
prop.ses
# We observe that in the high socioeconomic class 72% choose the academic program, 16% choose the general program, and 12% choose the vocational program. In the middle class, 46% choose academic, 21% choose general, and 33% choose vocational. In the low class, 40% choose academic, 34% choose general, and 26% choose vocational. 


# 1.b) Plot the relationship between program choice and reading score. 

progread <- mutate(hsbm, readscore = cut_number(read, 8)) %>% 
  group_by(readscore, prog) %>% summarise(count = n()) %>% 
  group_by(readscore) %>% mutate(tot = sum(count), proportion = count/tot)

ggplot(progread, aes(x = readscore, y = proportion, group = prog, linetype = prog)) + 
  geom_line()

# Repeat the above plot for math scores 

progmath <- mutate(hsbm, mathscore = cut_number(math, 6)) %>% 
  group_by(mathscore, prog) %>% summarise(count = n()) %>% 
  group_by(mathscore) %>% mutate(tot = sum(count), proportion = count/tot)

ggplot(progmath, aes(x = mathscore, y = proportion, group = prog, linetype = prog)) + 
  geom_line()

# We can see in the plots that for both meath and reading scores, as they increase the proportion of academic program choice increases and the other two program type decrease. 

# 1.c) Compute the correlation matrix for the five subject scores. 

cor(hsb[7:11])

# 1.d) 

multnommod0 <- multinom(prog ~ ., hsbm)

summary(multnommod0)

# The variable math has unusual coefficients. We can see from the correlation matrix and the above plots that math score has the largest effect on the outcome. 


# 1.e)

hsbm1 <- mutate(hsb, scoresum = read + write + math + science + socst)
hsbm1 <- hsbm1[-c(7:11)]
multnommod1 <- multinom(prog ~ ., hsbm1)
summary(multnommod1)

# We can see that the first model that includes the scores seperately has a lower residual deviance and lower AIC, so we conclude that it fits better. 


# 1.f) 

# Use a stepwise method to reduce the model 

bestmodel <- step(multnommod0, trace = 0)
summary(bestmodel)

# Since this method is based on AIC we see that the reduced model with five variable (ses + schtyp + math + science + socst) has a lower AIC, however, the deviance is slightly higher, we proceed with this reduced model. 


# 1.g) 

# Define the observed range of math scores
mathscorlev <- 32:75

# Find the most common levels of the factors in the model and the mean of the 
# other predictors in the model. 
summary(hsbm$ses)
summary(hsbm$schtyp)

# Get the pridected values for the observed range of math scores. 
preds <- data.frame(math=mathscorlev, 
                    predict(bestmodel, newdata = data.frame(ses = "middle", 
                                                            schtyp = "public", math = mathscorlev, science = mean(hsbm$science),
                                                            socst = mean(hsbm$socst)),type = "probs"))


lpred <- gather(preds, prog, probability, -math)

ggplot(lpred, aes(x = math, y = probability, group = prog, linetype = prog)) + geom_line()

#lpred

# Clearly, the probability of choosing the academic program type increases rapidly as the math scores get higher, and the other two programs type have a decreased probability  of begin chosen as the math score goes higher.


# 1.h)

# Compute a table 
# data.frame(ses = hsbm$ses, schtyp = hsbm$schtyp, 
#          predict(bestmodel,newdata = data.frame(ses = hsbm$ses, 
#         schtyp = hsbm$schtyp, math = mean(hsbm$math), science = mean(hsbm$science), 
#        socst = mean(hsbm$socst)), type = "probs"))

xtabs(predict(bestmodel,newdata = data.frame(ses = hsbm$ses, 
                                             schtyp = hsbm$schtyp, math = mean(hsbm$math), science = mean(hsbm$science), 
                                             socst = mean(hsbm$socst)), type = "probs") ~ hsbm$schtyp, hsbm$ses)


# 1.i)

# The student with id 99 is at row 102 in the data set.
predict(bestmodel, newdata = hsb[102,])

# The predicted value is academic which is wrong, the correct program type is general. 

# 1.j) Construct a table of predicted and observed values.

xtabs(~ predict(bestmodel) + hsbm$prog)

# Compute the correct classification rate

(87 + 10 + 29) / nrow(hsbm)

# We see that 63% of the data are correctly classified, which is not impressive given that we expect the model to perform worse than this on new observations. 


# Problem 5, CHapter 7
data("debt")

# Check for NA's in the data and omit them. 
debt <- na.omit(debt)
summary(debt)

# 5.a) Declare the response as an ordered factor and make a plot showing the relationship to prodebt.

# Declare the response variable ccarduse as ordered. 
debt$ccarduse <- factor(debt$ccarduse, ordered = TRUE)

# Verify that the response is indeed ordered. 
is.ordered(debt$ccarduse)

# Create the plot
library(ggplot2)
ggplot(debt, aes(x = prodebt, y = ccarduse)) + 
  geom_jitter(width = 0, height = 0.1, size = 0.15, color = "blue")

# In this plot we can see a more pronounced relationship between the lowest frequency of credit card use, i.e., never, and debt attitude, particularly with lower quantities of the debt attitude score, i.e., less favorable to debt,  However, for higher levels of frequency of card use, 2 and 3, the data points seem  to be similarly scattered, slightly denser in the mid-range of the debt attitude scores. 

# Then response against the income group. 
ggplot(debt, aes(x = incomegp, y = ccarduse)) + 
  geom_jitter(width = 0.14, height = 0.1, size = 0.24, color = "blue")

# In this plot as well, a more pronounced relationship can be seen between the lowest level  of the card use frequency, and income group, particularly the lower levels of income group.  Another slightly pronounced relationship between the third level of card use frequency and  the fifth level of the income group is visible. 


# 5.b) Fit a Proportional Odds model for credit card use to all other variables. 

# Fit the Proportional Odds model
pomod <- polr(ccarduse ~ ., debt )

# Look at the coefficients for largest t-values.
summary(pomod)$coef

# The two most significant predictors (having largest t-values) are the "incomegp" and "bankacc", the later having a t-value that is close to that of the "prodebt" variable.

# bankacc is obviously significantly influential on ccarduse since it is very unlikely to  have a credit card without a bank account. 
# The coefficient for bankacc is about 2.1 which  means that the the odds of moving from ccarduse level of 1 to 2/3 or from 1/2 to 3 increase  
# by a factor of exp(2.1) = 8.16 when the bankacc is equal to 1. The coefficient for incomegp is about 0.47. That is, the odds of moving from ccarduse level of 1 to 2/3 or from 1/2 to 3  increase by a factor of exp(0.47) = 1.60 when income goes to next level. 

# The least significant predictor is security of housing tenure with a t-value of 0.499. 

# 5.c) Fit a Proportional Odds model using the least significant predictor. 

pomodhouse <- polr(ccarduse ~ house, debt)
summary(pomodhouse)$coef

# We see that the t-value for the variable "house" in this model is significantly larger  than in the full model with a lower standard error. The t-value is close to that of  "bankacc" and "prodebt" in the full model, so it is much more significant than the full model suggests.  

# 5.d) Use stepwise AIC to reduce the full model.

reducedpomod <- step(pomod, trace = 0)

summary(reducedpomod)

# The qualitative effect of the predictor, as discussed above, is that the  odds of moving to the next level of ccarduse increases by a factor of exp(coefficient) when the corresponding predictor is increased by one unit or moves to the nect level. We see that the predictor "house" is dropped from the model, but we saw above that this predictor is significant when we use only that in the one-predictor model. 

# 5.e) Compute the median value of the predictors in the reduced model.

median.inc <- median(debt$incomegp)
median.age <- median(debt$agegp)
median.bank <- median(debt$bankacc)
median.bsoc <- median(debt$bsocacc)
median.cig <- median(debt$cigbuy)
median.pdebt <- median(debt$prodebt)

# Compute the predicted probabilities at the median values for smokers
predict(reducedpomod, data.frame(incomegp = median.inc, agegp = median.age, bankacc = median.bank, bsocacc = median.bsoc,cigbuy = debt$cigbuy[debt$cig[] == 1], prodebt = median.pdebt), type = "probs")[1,]

# Compute the predicted probabilities at the median values for non-smokers
predict(reducedpomod, data.frame(incomegp = median.inc, agegp = median.age, bankacc = median.bank, bsocacc = median.bsoc, cigbuy = debt$cigbuy[debt$cig[] == 0], prodebt = median.pdebt), type = "probs")[1,]

# The highest probability in both cases is for the first level of ccarduse.So both groups have a higher probaility of never using their ccards and a lower probability of using their cards regularly, while non-smokers have a higher probability of regularly using their cards than smokers do. 

# 5.f) Fit a Proportional Hazards model

phmod <- polr(ccarduse ~ incomegp + agegp + bankacc + bsocacc +
                cigbuy + prodebt, data = debt, method = "cloglog")
summary(phmod)

# Recompute the two sets of probabilities from the previous part. 

predict(phmod, data.frame(incomegp = median.inc, agegp = median.age,
                          bankacc = median.bank, bsocacc = median.bsoc,
                          cigbuy = debt$cigbuy[debt$cig[] == 1], 
                          prodebt = median.pdebt), type = "probs")[1,]

predict(phmod, data.frame(incomegp = median.inc, agegp = median.age,
                          bankacc = median.bank, bsocacc = median.bsoc,
                          cigbuy = debt$cigbuy[debt$cig[] == 0], 
                          prodebt = median.pdebt), type = "probs")[1,]

# Using the Proportional Hazards model seems to make the two sets of probailities more similar to eachother, making it almost equally likely for both smokers and nonsmokers to never, often, or regularly use their creditcards. The non-smokeres,however, are still more likely to use their credit cars regularly than are smokers. 




