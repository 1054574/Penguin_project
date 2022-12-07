# ---- Plots --------------
#Histogram for the t-test results 
plot_mass_sex_figure <- function(penguins_clean_now){
  ggplot(penguins_clean_now, aes(x=body_mass_g)) + 
  geom_histogram(aes(fill = sex),alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  labs(x = "Body mass (g)",
       y = "Count",
       title = "Body mass differences of male and female penguins of the Gentoo penguin species",
       caption = "Figure 1: Hisotgram of the distribution of body mass for males and females of the Gentoo penguin species, with dashed lines representing the means") + 
   geom_vline(xintercept= c(female_mean,male_mean), size=1, colour = "black", linetype = "dashed", size = 1) + #Add the mean line for the female and male penguins
   annotate(geom = "text", 
            label = c(as.character("Female group mean"), as.character("Male group mean")),
            x = c(4330,5200 ), #Add titles to the lines for the males and the females
            y = c(12, 12)) +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5))
}
# ---- Saving --------------   
# Save the plot as a png and define the size, resolution, and scaling
save_plot_png <- function(penguins_clean_now, filename, size, res, scaling){
  agg_png(filename, 
          width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  mass_sex_plot<- plot_mass_sex_figure(penguins_clean_now)
  print(mass_sex_plot)
  dev.off()
}
# Save the plot as a svg and define the size and scaling
save_plot_svg <- function(penguins_clean_now, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  mass_sex_plot<- plot_mass_sex_figure(penguins_clean_now)
  print(mass_sex_plot)
  dev.off()
}
