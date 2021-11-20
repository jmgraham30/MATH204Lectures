library(tidyverse)
library(patchwork)
library(ggmosaic)

set.seed(43)

get_rs <- function(i){
  rn <- paste0("R",as.character(sample(0:9,1)),
               as.character(sample(0:9,1)),
               as.character(sample(0:9,1)),
               as.character(sample(0:9,1)),
               as.character(sample(0:9,1)),
               as.character(sample(0:9,1)),
               as.character(sample(0:9,1)),
               as.character(sample(0:9,1)))
  return(rn)
}

r_num <- map_chr(1:127,get_rs)
amount_coffee <- round(rnorm(127,3,0.25),1)
amount_caffeine <- round(rnorm(127,100,15.0),1)
roast <- sample(c("light","medium","dark"),127,replace=TRUE)
num_roommates <- sample(c(1,2,3,4),127,replace = TRUE,prob=c(0.5,0.25,0.15,0.1))
morning <- sample(c("no","yes"),127,replace = TRUE,prob = c(0.7,0.3))
amount_sleep <- 10 - 0.04*amount_caffeine + rnorm(127,0.25)


coffeesleep_df <- tibble(
  r_num=r_num,
  amount_sleep=amount_sleep,
  amount_coffee=amount_coffee,
  amount_caffeine=amount_caffeine,
  roast=roast,
  num_roommates=num_roommates,
  morning=morning
)

glimpse(coffeesleep_df)


p_sleep <- coffeesleep_df %>% 
  ggplot(aes(x=amount_sleep)) + 
  geom_histogram(binwidth = 1,color="black") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

p_coffee <- coffeesleep_df %>% 
  ggplot(aes(x=amount_coffee)) + 
  geom_histogram(bins = 10,color="black") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

p_caffeine <- coffeesleep_df %>% 
  ggplot(aes(x=amount_caffeine)) + 
  geom_histogram(binwidth = 5,color="black") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

p_roommates <- coffeesleep_df %>% 
  ggplot(aes(x=num_roommates)) + 
  geom_histogram(binwidth = 0.5,color="black") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

(p_sleep + p_coffee) / (p_caffeine + p_roommates)


b_sleep <- coffeesleep_df %>% 
  ggplot(aes(x=amount_sleep)) + 
  geom_boxplot() + coord_flip() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

b_coffee <- coffeesleep_df %>% 
  ggplot(aes(x=amount_coffee)) +
  geom_boxplot() + coord_flip() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

b_caffeine <- coffeesleep_df %>% 
  ggplot(aes(x=amount_caffeine)) + 
  geom_boxplot() + coord_flip() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

b_roommates <- coffeesleep_df %>% 
  ggplot(aes(x=num_roommates)) + 
  geom_boxplot() + coord_flip() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

(b_sleep + b_coffee) / (b_caffeine + b_roommates)

bar_roast <- coffeesleep_df %>% 
  ggplot(aes(x=roast)) + geom_bar() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

bar_morning <- coffeesleep_df %>% 
  ggplot(aes(x=morning)) + geom_bar() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

bar_roast + bar_morning

coffeesleep_df %>% 
  ggplot() +
  geom_mosaic(aes(x = product(morning, roast), fill=morning)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),legend.position = "none")


side_by_side_1 <- coffeesleep_df %>% 
  ggplot(aes(x=roast,y=amount_caffeine)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),legend.position = "none")

side_by_side_2 <- coffeesleep_df %>% 
  ggplot(aes(x=morning,y=amount_sleep)) +
  geom_boxplot() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),legend.position = "none")

side_by_side_1 + side_by_side_2

scatter_1 <- coffeesleep_df %>% 
  ggplot(aes(x=amount_coffee,y=amount_sleep)) + 
  geom_point() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),legend.position = "none")


scatter_2 <- coffeesleep_df %>% 
  ggplot(aes(x=amount_caffeine,y=amount_sleep)) + 
  geom_point() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),legend.position = "none")

scatter_3 <- coffeesleep_df %>% 
  ggplot(aes(x=num_roommates,y=amount_sleep)) + 
  geom_point() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),legend.position = "none")

scatter_1 + scatter_2 + scatter_3

table(coffeesleep_df$morning)

prop.test(37,127,p=0.2,correct = FALSE)

table(coffeesleep_df$roast)

prop.test(c(45,38),c(127,127),correct = FALSE)

t.test(coffeesleep_df$amount_sleep,mu=8.0)

# need a paired sample test

t.test(amount_sleep~morning,data=coffeesleep_df,paired=FALSE)

summary(aov(amount_caffeine~roast,data=coffeesleep_df))

chisq.test(table(coffeesleep_df$roast))

chisq.test(coffeesleep_df$roast,coffeesleep_df$morning)

cor(coffeesleep_df$amount_caffeine,coffeesleep_df$amount_sleep)

summary(lm(amount_sleep~amount_caffeine,data=coffeesleep_df))

summary(lm(amount_sleep~amount_caffeine+amount_coffee+num_roommates+morning))




