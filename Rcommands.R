# load packages
library(tidyverse)
library(openintro)
library(ggformula)
library(ggmosaic)
library(broom)
library(GGally)
library(skimr)

# load data
my_data <- read.csv("epa2021data.csv")

# first look at data
dim(my_data)

summary(my_data)

pillar::glimpse(my_data)

skimr::skim(my_data)

# summary stats for one numerical variable
comb_mpg_mean <- mean(my_data$comb_mpg)
comb_mpg_sd <- sd(my_data$comb_mpg)
comb_mpg_var <- var(my_data$comb_mpg)
comb_mpg_median <- (my_data$comb_mpg)

# summary plots for one numerical variable
gf_histogram(~comb_mpg,data=my_data)

gf_histogram(~comb_mpg,data=my_data,bins=25,color="black")

gf_histogram(~comb_mpg,data=my_data) %>% 
  gf_vline(xintercept = comb_mpg_mean)

gf_boxplot(~comb_mpg,data=my_data)

gf_boxplot(~comb_mpg,data=my_data) + 
  coord_flip()

# summary stats for one categorical variable
table(my_data$drive_desc)

addmargins(table(my_data$drive_desc))

proportions(table(my_data$drive_desc))

addmargins(proportions(table(my_data$drive_desc)))

# proportions plot
ggplot(data = my_data) +
  geom_mosaic(aes(x = product(drive_desc), fill=drive_desc))

ggplot(data = my_data) +
  geom_mosaic(aes(x = product(drive_desc), fill=drive_desc)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# summary plots for one categorical variable
gf_bar(~drive_desc,data=my_data)

gf_bar(~drive_desc,data=my_data) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# summary plot for numerical variable per category
gf_boxplot(comb_mpg~drive_desc,data=my_data)

gf_boxplot(comb_mpg~drive_desc,data=my_data) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# numerical summary for pair of numerical variables
cor(my_data$comb_mpg,my_data$engine_displacement)

# scatterplot for two numerical variables
gf_point(comb_mpg~engine_displacement,data=my_data)

gf_point(comb_mpg~engine_displacement,data=my_data) %>%
  gf_lm()


# multiple scatter plots at once
GGally::ggpairs(my_data[,c(6,7,8,12)])

# pairs of categorical variables

table(my_data$air_aspir_method_desc,my_data$drive_desc)

addmargins(table(my_data$air_aspir_method_desc,my_data$drive_desc))

proportions(table(my_data$air_aspir_method_desc,my_data$drive_desc),margin=1)

addmargins(prop.table(table(my_data$air_aspir_method_desc,my_data$drive_desc),margin=1))


proportions(table(my_data$air_aspir_method_desc,my_data$drive_desc),margin=2)

addmargins(prop.table(table(my_data$air_aspir_method_desc,my_data$drive_desc),margin=2))


ggplot(data = my_data) +
  geom_mosaic(aes(x = product(drive_desc, air_aspir_method_desc), fill=drive_desc)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

### working with distributions

sim_x <- rnorm(35.10,2.5)
dnorm(0)
gf_dist("norm")
pnorm(1.5)
qnorm(0.3)


### statistical tests

prop.test(18,45,p=0.32,correct = FALSE)

prop.test(c(18,15),c(45,27),correct=FALSE)

sim_data <- tibble(x=rnorm(18,2,0.75),y=rnorm(18,2.2,0.75))

t.test(sim_data$x,mu=1)

t.test(sim_data$x,sim_data$y,paired = TRUE)

z <- rnorm(22,2,0.75)

t.test(sim_data$x,z,paired=FALSE)

## an application of difference in means and ANOVA to our data

my_data <- my_data %>% mutate(turbo_yn=ifelse(my_data$air_aspir_method_desc == "Turbocharged","Yes","No"))


my_data %>% group_by(turbo_yn) %>% summarise(mean_comb_mpg=mean(comb_mpg))


gf_boxplot(comb_mpg~turbo_yn,data=my_data)


turbo_no <- my_data %>% dplyr::filter(turbo_yn == "No")
turbo_yes <- my_data %>% dplyr::filter(turbo_yn == "Yes")

t.test(turbo_no$comb_mpg,turbo_yes$comb_mpg,paired=FALSE)

summary(aov(comb_mpg~drive_desc,data=my_data))

## application of chi-square

chisq.test(c(50,11,217,118,453,136,123),p=c(10/100,5/100,20/100,15/100,25/100,15/100,10/100))

table(hsb2$ses,hsb2$prog)

chisq.test(hsb2$ses,hsb2$prog)


## application of lm

summary(lm(comb_mpg~engine_displacement,data=my_data))

