#import the data
nids<-read.csv("./data/nids.csv")

#hhid	 - "Household identifier"
#pid	- "Individual identifier"
#w1_a_best_age_yrs	- "Best age in years"
#w1_a_b2	"b2 - Gender"
#w1_a_n1_1	"n1_1 - Height measure one"
#w1_a_n2_1	"n2_1 - Weight measure one"

#Exploration

head(nids, n = 10L)
tail(nids, n = 10L)

#New height variable in m - for respondents aged 20+
library(dplyr)

nids<-nids %>%
  mutate(height = ifelse (w1_a_n1_1 >= 0 & w1_a_best_age_yrs >= 20, w1_a_n1_1/100, NA))

summary(nids$height)

#Some respondents 20 + with a height less than 1 meter
#How many are there in the sample and print?

nids %>% 
  filter(height<1) %>%
  select(w1_a_best_age_yrs, w1_a_b2, height) %>%
  print()

#New weight variable - for respondents aged 20+ 
nids<-nids %>%
  mutate(weight = ifelse (w1_a_n2_1 >= 0 & w1_a_best_age_yrs >= 20, w1_a_n2_1, NA)) 

summary(nids$weight)


#Generating a variable for BMI
nids<-nids %>%
  mutate(bmi = weight/height^2)


#Check that BMI is missing for people under age 20 and for those who have missing height forweight values.
nids %>% 
  filter(w1_a_best_age_yrs<20 & !is.na(bmi)) %>%
  count

nids %>% 
  filter((is.na(height) | is.na(weight)) & !is.na(bmi)) %>%
  count
  
#How many respondents have non-missing BMI values?
nids %>%
  filter(!is.na(bmi)) %>%
  count

#Age bins
library(sjmisc)
nids<-nids %>%
  mutate(age_bins = rec(w1_a_best_age_yrs, recodes = "20:29=1; 30:39=2; 40:49=3; 50:59=4; 60:69=5; 70:120=6"))

#Check our age bins
nids%>%
  group_by(age_bins)%>%
  summarise(freq = n())

#Gender variable
nids<-nids%>%
  mutate(gender = factor(w1_a_b2, levels=1:2, labels = c("Male", "Female")))


summary(nids$bmi)

#How many people have values outside the range?
nids%>%
  filter(bmi < 15 | (bmi > 50 & !is.na(bmi)))%>%
  nrow
#Or

nrow(subset(nids, bmi < 15 | (bmi > 50 & !is.na(bmi))))


#Create bmi_valid variable to exclude some outliers
nids<-nids%>%
  mutate(bmi_valid = ifelse(bmi > 15 & bmi < 50,1,NA))

#Age
nids<-nids%>%
  mutate(age_adult = ifelse(w1_a_best_age_yrs<20,NA, w1_a_best_age_yrs))


#Is age a strong determinant of BMI?

corr2.df<-nids[nids$bmi_valid==1,c("bmi","age_adult")]
cor(corr2.df,use="complete.obs")

##BMI and age
library(ggplot2)
library(mgcv)

ggplot(corr2.df, aes(x = age_adult, y = bmi)) +
  geom_point(aes(x = age_adult, y = bmi), colour="blue") +
  stat_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "red") + 
  stat_smooth(method = "gam", formula= y ~ s(x, k = 3), se = FALSE, colour = "green", aes(colour = "polynomial")) +
  scale_x_continuous(breaks=seq(20,100,20)) +
  scale_y_continuous(breaks=seq(10,50,10)) +
  xlab("Age in years - adults") + ylab("BMI") +
  ggtitle("Scatter plot of BMI and age")
ggsave("./images/plot1.png")


#Investigate non-linear trend between BMI and age
bmi_age_df<-corr2.df%>%
  group_by(age_adult)%>%
  summarise(bmi_age = mean(bmi, na.rm = T))

head(bmi_age_df, n=20L)


#Plot of mean bmi by age

ggplot(bmi_age_df, aes(x = age_adult, y = bmi_age)) +
  geom_point(aes(x = age_adult, y = bmi_age), colour="blue") +
  xlab("Age") + ylab("Mean BMI (by age_adult)") +
  ggtitle("Mean BMI by Age")
ggsave("./images/plot2.png")
