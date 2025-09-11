library(ggplot2)
library(dplyr)

#Good labels on plots can help you communicate your key findings, or even comply with the formatting requirements of any outlet (like a scientific journal or your company’s preferred style).
#In ggplot2, you add labels with the labs() function. Let’s start with a title.

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se.e = FALSE) +
  labs(title = "Fuel efficiency generally decreases with engine size")

#Subtitle adds additional detail in a smaller font beneath the title and caption adds text at the bottom right of the plot 
#Caption adds text at the bottom right of the plot, often used to describe the source of the data.

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )

#You can also use labs() to replace axis labels and legend titles.

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )

#Here you can use geom_text() to add textual labels to your plots. This works in the same way as geom_point() but rather than a shape geometry it can add a label. 
#There is also a nudge() function to move your text a certain amount and using other R packages to handle text wrapping etc. 

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_text(aes(label = model), data = best_in_class)

#Changing the default scales on ggplot2 can help you customize your plots and improve communication of your results. 

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_discrete()

#You can also change the ticks on your axes. Breaks controls the position of the ticks and you can use labels as an argument to change the text label associated with ticks. 

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

#What does seq do?

seq(15, 40, by = 5)

#Lays out numbers in sequence by a specified number interval

#Similarly, you can use labels set to NULL to suppress the labels altogether. This is sometimes an option if you’re trying to format your plots in a particular way. 

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

#Legends and Color Schemes

base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right") # the default
base + theme(legend.position = "none")

#There are two types of scales you’re mostly likely to want to switch out: continuous position scales and colour scales.

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() + 
  scale_x_log10() + 
  scale_y_log10()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_colour_brewer(palette = "Set1")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_colour_brewer(palette = "Set1")

#When you have predefined colours you want to use you can set them yourself, using scale_colour_manual()

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))

install.packages('viridis')
install.packages('hexbin')
library(viridis)
library(hexbin)

df <- tibble( # note we're just making a fake dataset so we can plot it
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
  geom_hex() + # a new geom!
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()

#And finally, now you can customize the entire theme of your plot. Themes allow you to change some or all of the non-data elements of your plot with a theme.

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_light()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_classic()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_dark()

#In general, the best way to make a single file will be to export it using the ggsave() function. 

ggplot(mpg, aes(displ, hwy)) + geom_point()

ggsave("my-plot.pdf")
#> Saving 7 x 4.32 in image
