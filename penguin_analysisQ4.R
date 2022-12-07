
#Question 4: 
# Run a statistical test on the Palmer Penguins dataset and produce a figure to explain it.

#The working directory has been set to the Penguins project folder using the following code: 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load the libraries and the cleaning function
source("functions/libraries.R")
source("functions/cleaning.R")

#Load the penguin data 
write.csv(penguins_raw, "data_raw/penguins_raw.csv")
penguins_raw <- read.csv("data_raw/penguins_raw.csv")


#Clean data
penguins_clean <- cleaning(penguins_raw)
penguins_clean_now <- remove_empty_rows(penguins_clean)

#Checking the results 
head(penguins_clean_now)


#Save the clean data in a separate folder called "data_clean_now" in a .csv file.
write.csv(penguins_clean_now, "data_clean/penguins_clean_now.csv")

#Question: Are Gentoo body masses significantly different between males and females?
#For the purpose of this analysis, I will be conducting an independent sample t-test 
#Assumptions:
#1. Independence of the observations. There is no relationship between the observations in each group.
#2. No significant outliers in the groups
#3. the two groups of samples should be normally distributed.
#4. The variances of the two groups should not be significantly different.

#Plot histograms and qqplots to check the normality of the two groups and for the presence of outliers.
#Histogram
ggplot(penguins_clean_now, aes(x=body_mass_g)) +
  geom_histogram() +
  facet_wrap(~sex)
#QQplot
ggplot(penguins_clean_now, aes(sample = body_mass_g)) +
  geom_qq() +
  geom_qq_line(colour = "red")+ 
  facet_wrap(~sex)

#Looking at the histograms and the qqplots, the data appears to be normally distributed for both groups and there are no major outliers in the data sets.  

#Check the equality of variances using the Levene's test null hypothesis which states that the variances are equal. 
levene_test <-leveneTest(body_mass_g ~ sex, centre = median, penguins_clean_now )
levene_test
#The p value is greater than 0.05, which indicates that we fail to reject the null hypothesis, which means that the variances of the two groups are not significantly different from each other. 

#Independent sample t-test 
t.test(penguins_clean_now$body_mass_g ~ penguins_clean_now$sex)
#Since the p-value is lower than 0.05, we can now reject the null hypothesis and accept the alternative, which means that the body mass of females is significantly different than males for the Gentoo species. (p-value < 0.05)
#Results: figure will be a histogram with the means of the different groups included

source(("functions/plot.R"))
#Calculate the means for the females and the males to add to the results histogram 
female_mean <- mean(filter(penguins_clean_now ,sex == "FEMALE")$body_mass_g)
male_mean <- mean(filter(penguins_clean_now ,sex == "MALE")$body_mass_g)

#Making the graph 
plot_mass_sex_figure(penguins_clean_now)

#Save figure 
save_plot_svg(penguins_clean_now, "figures/fig01_vector.svg", size=22, scaling=1)
save_plot_png(penguins_clean_now, "figures/fig01_png.png", size=22, res=600, scaling=1)


