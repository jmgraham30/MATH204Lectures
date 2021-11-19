library(tidyverse)
library(patchwork)
library(ggmosaic)

set.seed(1587)

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
amount_coffee <- round(rnorm(127,3,0.2),1)
amount_caffeine <- round(rnorm(127,60,4.5),1)
amount_sleep <- 10 - 0.03*amount_coffee - 0.07*amount_caffeine + rnorm(127,0.2)
roast <- sample(c("light","medium","dark"),127,replace=TRUE)
num_roommates <- sample(c(1,2,3,4),127,replace = TRUE,prob=c(0.6,0.2,0.1,0.1))
morning <- sample(c("no","yes"),127,replace = TRUE,prob = c(0.7,0.3))

coffeesleep_df <- tibble(
  r_num=r_num,
  amount_sleep=amount_sleep,
  amount_coffee=amount_coffee,
  roast=roast,
  num_roommates=num_roommates,
  morning=morning
)


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
  geom_histogram(binwidth = 2.5,color="black") +
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

