



# read our data
my_data <- read.csv("Diet_R.csv")



#Here, we have a unbalanced design. 
table(my_data$gender, my_data$Diet)

library("ggpubr")

# Box plot with two factor variables
boxplot(weight6weeks ~ gender * Diet, data=my_data, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="weight6weeks")


# Two-way interaction plot
interaction.plot(x.factor = my_data$Diet, trace.factor = my_data$gender, 
                 response = my_data$weight6weeks, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Diet", ylab="weight6weeks",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))




#used for test unbalanced design

library(car)
res.aov2 <- aov(weight6weeks ~ gender * Diet, data = my_data)
Anova(res.aov2, type = "III")



#Compute some summary statistics
require("dplyr")
group_by(my_data, gender, Diet) %>%
  summarise(
    count = n(),
    mean = mean(weight6weeks, na.rm = TRUE),
    sd = sd(weight6weeks, na.rm = TRUE)
  )


library(tidyverse)
library(ggpubr)
library(rstatix)

#
my_data %>%
  pairwise_t_test(
    weight6weeks ~ Diet, 
    p.adjust.method = "bonferroni"
  )



# pairwise comparisons
library(emmeans)
pwc <- my_data %>% 
  group_by(gender) %>%
  emmeans_test(weight6weeks ~ Diet, p.adjust.method = "bonferroni") 
pwc

my_data %>%
  pairwise_t_test(
    weight6weeks ~ Diet, 
    p.adjust.method = "bonferroni"
  )



#Check ANOVA assumptions: test validity
# 1. Homogeneity of variances
plot(res.aov2, 1)


# 2. Normality
plot(res.aov2, 2)


model  <- lm(weight6weeks ~ Diet*gender, data = my_data)

ggqqplot(residuals(model))


shapiro_test(residuals(model))



  my_data %>%
  group_by(gender, Diet) %>%
  shapiro_test(weight6weeks)

  
  ggqqplot(my_data, "weight6weeks", ggtheme = theme_bw()) +
    facet_grid(gender ~ Diet)


# Extract the residuals
aov_residuals <- residuals(object = res.aov2)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )



