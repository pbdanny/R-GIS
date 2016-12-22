setwd("/Users/Danny/Documents/R Project/KTC Dataset")
load(file = "qry_ALLProduct_KPI.Rdata")
library(dplyr)
library(ggplot2)

## ------------------------
## 1. List of all ggplot Geoms
## ------------------------

df <- data.frame(
  x = c(3, 1, 5),
  y = c(2, 4, 6),
  label = c('a', 'b', 'b')
)

p <- ggplot(data = df, aes(x = x, y = y, label = label)) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 12))

p + geom_point() + ggtitle("point")
p + geom_text() + ggtitle("text")
p + geom_bar(stat = 'identity') + ggtitle("bar")
p + geom_tile() + ggtitle("tile")
p + geom_line() + ggtitle("line")
p + geom_area() + ggtitle("area")
p + geom_path() + ggtitle("path")
p + geom_polygon() + ggtitle("polygon")

## ---------------------------
## 2. List of All Text Labels
## ---------------------------

df <- data.frame(x = 1:3, y = rep(1:3, each = 3), 
                 family = rep(c("sans", "serif", "mono"), each = 3), 
                 face = rep(c("plain", "bold", "italic"), times = 3))
df$label <- paste(df$family, df$face, sep = ",")

ggplot(data = df, aes(x = x, y = y)) +
  geom_text(aes(family = family, fontface = face, label = label), vjust = "inward", hjust = "inward")

df <- data.frame(x = 1:5, y = 1:5, size = seq(10, by = 10, length.out = 5), label = seq(10, by = 10, length.out = 5))

ggplot(data = df, aes(x = x, y = y)) +
  geom_text(aes(size = size, label = label), angle = 30) +  # Angle in degree
  geom_label(aes(label = label, size = size) , nudge_y = -0.5 )  # Use geom_label for box text , nudge_y adjust downward

label <- data.frame(
  waiting = c(55, 80),
  eruptions = c(2, 4.3),
  label = c("peak one", "peak two")
)

ggplot(data = faithfuld, aes(waiting, eruptions)) +  # Still not understand
  geom_tile(aes(fill = density)) +
  geom_label(data = label, aes(label = label))

## -------------------------------
## 3) How to use collective Geom
## -------------------------------
# Example of distinct geom (1 graphic object represent 1 data) : geom_point 
# Example of collectuve geom (1 graphic object repreetn many datum) : box_plot
# use aes( group = ) to define collective geom aesthetics

data(Oxboys, package = "nlme")
head(Oxboys)

# Differnt posiotion of aes(group = ) result different plot
# 1. aes(group = ) in Data Layer : all graphic plot use collective aesthetics for all geom
ggplot(data = Oxboys, aes(x = age, y = height, group = Subject)) +
  geom_point() +
  geom_line(color = "blue")  # Create connected line between each Subject

ggplot(data = Oxboys, aes(x = age, y = height, group = Subject)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)  # Geom stat show linear regression model for each group (Subject)

# 2. aes(group = ) in Geom Layer : collective aesthetics effect only that geom
ggplot(data = Oxboys, aes(age, height)) +  
  geom_line(aes(group = Subject)) +  # Show geom_line by each group (Subject) 
  geom_smooth(method = "lm", se = FALSE)  # Liner regression across default group (all Subject)

# Overriding default collective aesthetic
# Ex: geom_boxplot : aes(x = ??) imply aes(group = ??)

# Default collective aesthetics
ggplot(data = Oxboys, aes(x = Occasion, y = height)) +
  geom_boxplot()

ggplot(data = Oxboys, aes(x = Occasion, y = height)) +
  geom_boxplot() +
  geom_line(color = "#3366FF", alpha = 0.5)  # Geom line use defalt group (Occession), grom_line by Occassion

ggplot(data = Oxboys, aes(x = Occasion, y = height)) +
  geom_boxplot() +
  geom_line(aes(group = Subject), color = "#3366FF", alpha = 0.5)  # Specific new group in geom, override default geom

# Matching graphic to aesthetic object
df <- data.frame(x = 1:3, y = 1:3, color = c(1,3,5))

ggplot(data = df, aes(x, y, color = color)) +
  geom_line(size = 2) + 
  geom_point(size = 5)

ggplot(data = df, aes(x, y, color = color)) +
  geom_line(aes(group = 1), size = 2) +  # By default aes(group = 1) will use the group parameter in data Layer (color)
  geom_point(size = 5)

ggplot(mpg, aes(class)) +
  geom_bar()

ggplot(mpg, aes(class, fill = drv)) +  # This time aes(fill) create collective geom
  geom_bar()

ggplot(mpg, aes(class, fill = hwy)) +  # Continuous cannot create collective parameter
  geom_bar()

ggplot(mpg, aes(class, fill = hwy, group = hwy)) +  # Create collective parameter from group continuous
  geom_bar()

## ------------------
## 4. Surface plot
## ------------------

# Contour plot
ggplot(faithfuld, aes(eruptions, waiting)) +
  geom_contour(aes(z = density, color = ..level..))

# Raster plot
ggplot(faithfuld, aes(eruptions, waiting)) +
  geom_raster(aes(fill = density))

## ---------------------
## 5. Map plot with ggplot2
## ---------------------

thai_map_data <- map_data("world", "thailand") %>%
  select(lon = long, lat, group, id = subregion)

ggplot(data = thai_map_data, aes(lon, lat)) +
  geom_polygon(aes(group = group), fill = NA, color = "grey50") +
  coord_quickmap()

## ------------------------
## 6. Plotting uncertainty
## ------------------------

y <- c(18, 11, 16)
df <- data.frame(x = 1:3, y = y, se = c(1.2, 0.5, 1.0))

# Uncertianty must define the ymin, ymax for plotting
base <- ggplot(data = df, aes(x = x, y = y, ymin = y - se, ymax = y + se))

base + geom_crossbar()
base + geom_pointrange()
base + geom_smooth(stat = "identity")
base + geom_errorbar()
base + geom_linerange()
base + geom_ribbon()

## --------------------------
## 7. Plotting weighted data
## --------------------------

# Good for observation with aggregrated in each rows (obs)

# Not weighted
ggplot(data = midwest, aes(percwhite, percbelowpoverty)) +
  geom_point()

# Use size to show weighted data (poptotal)
ggplot(data = midwest, aes(percwhite, percbelowpoverty)) +
  geom_point(aes(size = poptotal / 1e6)) +
  scale_size_area("Poppulation\n(million)", breaks = c(0.5, 1, 2, 4))

# Explicit define weighted varible
# Match with stat geom type like geom_smooth
ggplot(data = midwest, aes(percwhite, percbelowpoverty)) +
  geom_point(aes(size = poptotal / 1e6)) +
  geom_smooth(method = "lm") +  # Unweighted, all observations equally effect the linear regression line 
  scale_size_area(guide = "none")

ggplot(data = midwest, aes(percwhite, percbelowpoverty)) +
  geom_point(aes(size = poptotal / 1e6)) +
  geom_smooth(aes(weight = poptotal), method = "lm") +  # Explicit define weighted varible, large pop effect more
  scale_size_area(guide = "none")

## -----------------------
## 8. Distribution display
## -----------------------

# 2 famous geom , geom_histogram (geom_bar) and geom_frequencyply

ggplot(data = diamonds, aes(depth)) +
  geom_histogram(binwidth = 0.1) +
  xlim(55, 70)

ggplot(data = diamonds, aes(depth)) +
  geom_freqpoly(aes(color = cut), binwidth = 1) +
  xlim(55, 70)

# geom_density, total area = 1, 
ggplot(data = diamonds, aes(depth, fill = cut, color = cut)) +
  geom_density(alpha = 0.2)


## -------------------
## 9. Over Plotting
## -------------------

# Generally set alpha and for catagorical data use geom_jitter

df <- data.frame(x = rnorm(2000), y = rnorm(2000))
norm <- ggplot(data = df, aes(x = x, y = y)) + xlab(NULL) + ylab(NULL)

norm + geom_point()
norm + geom_point(shape = ".")  # Change to smaller shape
norm + geom_point(alpha = 0.5)  # Use alpha

# Simplify plotting to 2 dimension (bin2d) with geom_bin2d / geom_hex
norm + geom_bin2d()
# install.package("hexbin")
norm + geom_hex()

## -------------------------
## 10. Statistical Summary
## -------------------------

# Change plotting from statistical summary on no. of observation to other varible

ggplot(data = diamonds, aes(x = color)) +
  geom_bar()  # Default statitical summary "bin" type (histogram) by # of observation in each bin

ggplot(data = diamonds, aes(color, price)) +
  geom_bar(stat = "summary_bin", fun.y = mean)  # change geom_bar summary_bin from count -> mean of price

ggplot(data = diamonds, aes(table, depth)) +
  geom_bin2d(binwidth = 1) +  # Default 2d bin, plot summary by # of observation
  xlim(50, 70) +
  ylim(50, 70)

ggplot(data = diamonds, aes(table, depth, z = price)) +
  geom_raster(binwidth = 1, stat = "summary_2d", fun = mean) +  # change geom_bin2d from count -> mean of price
  xlim(50, 70) +
  ylim(50, 70)


## --------------------
## Test with KTC Real data
## --------------------


ggplot(data = dat, aes(x = Month, y = Monthly_Salary)) +
  geom_point() +
  facet_wrap(~ Occupation_Code)  # facet wrap = facet along the coloumn with auto new row

ggplot(data = dat, aes(x = Region, y = Monthly_Salary, color = Monthly_Salary)) +
  geom_boxplot() +
  facet_wrap(~ NewCC) +
  ylim(NA, 50000)  # Undefined ylim with 'NA' value


## ------------------------
# dplyr & ggplot2 chaining
## ------------------------

dat %>%
  group_by(Channel_Group) %>%
  summarise(n = n(), avg_sal = mean(Monthly_Salary)) %>%
  ggplot(aes(x = Channel_Group, y = avg_sal)) + 
  geom_bar(stat = "identity")  # Bar stat = 'identity' not run counts & bin, simple bar data

dat %>%
  mutate(YYYYMM = as.Date(format(DOB, "%Y-%m-01"))) %>%  # Convert to year and month with date fixed to 01
  group_by(YYYYMM) %>%
  summarise(n = n()) %>%
  filter(YYYYMM < "2000-01-01" & YYYYMM > "1950-01-01") %>%
  ggplot(aes(x = YYYYMM, y = n)) +
  geom_line() +
  ylim(NA, 500)