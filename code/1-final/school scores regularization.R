####                               ####
#school size
#simulate the number of students in each school
library(tidyverse)
set.seed(1986, sample.kind = "Rounding")
n <- round(2^rnorm(1000, 8, 1))

#assign a true attribute to each school independent of school size
set.seed(1, sample.kind = "Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS", 1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

#see top 10 schools
schools %>% top_n(10, quality) %>%
  arrange(desc(quality))

#simulate students taking a test
set.seed(1, sample.kind = "Rounding")
mu <- round(80 + 2*rt(1000, 5))
scores <- sapply(1:nrow(schools), function(i) {
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>%
  mutate(score = sapply(scores, mean))
#### citation: above section of code is from HarvardX staff ####

#what are the top schools based on the average score?
#show just the ID, size, and avg score
top_schools <- schools %>% top_n(10, score) %>%
  arrange(desc(score)) %>%
  select(id, size, score)

#compare the median school size to the median school size of the top 10 schools above
median(schools$size)
median(top_schools$size)

#repeat the exercise for the worst 10 schools.
#what is the median school size for the 10 worst schools based on score?
worst_schools <- schools %>%
  top_n(10, -score) %>%
  arrange(score) %>%
  select(id, size, score)
median(worst_schools$size)

#official code
schools %>% top_n(-10, score) %>% .$size %>% median()

#plot the avg score vs school size to see what's going on. highlight the top 10
#schools based on true quality
plot(schools$size, schools$score)
true_top_10 <- schools %>%
  top_n(10, quality)

#use regularization to pick the best schools
#we first need to define the overall avg for all schools
overall <- mean(sapply(scores, mean))

#then define for each school how it deviates from this avg
#estimate the score above the avg for each school but dividing by n + alpha instead
#of n, with n the school size and alpha a regularization parameter. try alpha = 25
#what is the id of the top school with regularization?
alpha <- 25
n <- schools$size
score_deviation <- (n*(schools$score - overall))/(n + alpha)
regul_score <- score_deviation + overall
schools <- schools %>%
  mutate(score_deviation = score_deviation,
         regul_score = regul_score)
top_regul <- schools %>%
  top_n(10, regul_score) %>%
  arrange(desc(regul_score))

#using values of alpha from 10 to 250, find the value that minimizes RMSE
alpha <- seq(10, 250, 1)
rmse <- rep(NA, 1000)
for (i in 1:length(alpha)) {
  score_deviation <- (n*(schools$score - overall))/(n + alpha[i]) #regularized dev
  regul_score <- score_deviation + overall
  rmse[i] <- sqrt(1/1000*sum((schools$quality - regul_score)^2))
}
alpha[which.min(rmse)]

#rank the schools based on the avg obtained by the best alpha
alpha <- alpha[which.min(rmse)]
score_deviation <- (n*(schools$score - overall))/(n + alpha)
regul_score <- score_deviation + overall
schools <- schools %>%
  mutate(regul_score = regul_score)
schools %>% arrange(desc(regul_score))

#official code 
alpha <- alphas[which.min(rmse)]  
score_reg <- sapply(scores, function(x)
  overall+sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

# find alpha which minimizes rmse
alpha <- seq(10, 250, 1)
rmse <- rep(NA, 1000)
for (i in 1:length(alpha)) {
  score_deviation <- (n*schools$score)/(n + alpha[i]) #regularized dev w/o subtr. mean
  regul_score <- score_deviation + overall
  rmse[i] <- sqrt(1/1000*sum((schools$quality - regul_score)^2))
}
alpha[which.min(rmse)]