install.packages("palmerpenguins")

library(palmerpenguins)
data("penguins")
penguins <- na.omit(penguins) # Removes missing rows

n<- 200
my.student.number <-240408545 
set.seed(my.student.number)
my.penguins <- penguins[sample(nrow(penguins), n), ]

library(ggplot2)
library("GGally")
library(vegan)
library(gridExtra)

summary(my.penguins)

gg1 = ggplot(my.penguins, mapping = aes(bill_length_mm, species)) + geom_boxplot()
gg2 = ggplot(my.penguins, mapping = aes(bill_depth_mm, species)) + geom_boxplot()
gg3 = ggplot(my.penguins, mapping = aes(flipper_length_mm, species)) + geom_boxplot()
gg4 = ggplot(my.penguins, mapping = aes(body_mass_g, species)) + geom_boxplot()
grid.arrange(gg1, gg2, gg3, gg4)

table = xtabs(~ year + species + island, data =my.penguins)
table

par(mfrow = c(1, 2))
hist = hist(my.penguins$bill_depth_mm[my.penguins$island=='Dream'], 
            breaks = 20, freq = FALSE, xlab = "Bill Depth of Dream island penguins", main = "Histogram")

Dream_bill_depth <- my.penguins$bill_depth_mm[my.penguins$island == 'Dream']
# Calculate mean and standard deviation
mean_Dream <- mean(Dream_bill_depth)
sd_Dream <- sd(Dream_bill_depth)
# Display the results
mean_Dream
sd_Dream

# Plot histogram with density
hist(Dream_bill_depth, breaks = 20, freq = FALSE, 
     xlab = "Bill Depth of Dream island penguins", 
     main = "Histogram with Normal Distribution")

# Generate values for the normal distribution curve
x <- seq(min(Dream_bill_depth), max(Dream_bill_depth), length = 100)
y <- dnorm(x, mean = mean_Dream, sd = sd_Dream)

# Overlay the normal distribution curve
lines(x, y, col = "red", lwd = 2)

ggpairs(my.penguins, columns = c(3:6), aes(color = sex), legend = 1,  
        diag = list(continuous = wrap("densityDiag", alpha = 0.5 )), progress = FALSE) +
  theme(legend.position = "bottom") + labs(fill = "Penguin Sex") + scale_color_manual(values = c("male" = "#FF9999", "female" = "#66B2FF"))

ggpairs(my.penguins, columns = c(3:6), aes(color = island), legend = 1,  
        diag = list(continuous = wrap("densityDiag", alpha = 0.5 )), 
        progress = FALSE) + theme(legend.position = "bottom") +
  labs(fill = "Penguin Islands") + scale_color_manual(values = c("Biscoe" = "#E69F00","Dream" = "#56B4E9","Torgersen" = "#009E73"))

t.test(bill_length_mm ~ sex, data = my.penguins)

aov(bill_length_mm ~ island, data = my.penguins)


