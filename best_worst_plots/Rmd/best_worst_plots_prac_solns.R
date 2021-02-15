library(tidyverse)

# 2. loading data

fev1 <- read_csv("fev1.csv", col_types = cols(id = col_factor()))
# or
fev1 <- read.csv("fev1.csv")
fev1$id <- factor(fev1$id, levels = sort(unique(fev1$id)))

# 3a. correlation

summarise(fev1, r = cor(age, FEV1))
# or
cor(fev1$age, fev1$FEV1)
# or 
with(fev1, cor(age, FEV1))

# Answer: looks like a linear model may be appropriate here

# 3b. make a scatter plot

ggplot(data = fev1, aes(x = age,
                        y = FEV1)) +
    geom_point() +
    xlab("Age (years)") +
    ylab("FEV1 (L)") +
    ggtitle("Spirometry of 300 girls in Topeka, KS")

# Answer: looks like a linear model may not be appropriate here

# 3c. outliers

outliers <- group_by(fev1, id) %>%    # want to get correlation for each id
    summarise(r = cor(age, FEV1)) %>%
    filter(r < 0) %>%                 # decrease in FEV1 with time is a negative correlation
    inner_join(fev1)                  # need to merge with original data on `id`

# or

outliers <- merge(subset(do.call(rbind,
                                 lapply(split(fev1, fev1$id), FUN = function(x){
                                     data.frame(id = unique(x$id),
                                                r  = cor(x$FEV1, x$age))})),
                         r < 0),
                  fev1)

# or
library(data.table)
fev1_dt  <- data.table(fev1)
outliers <- merge(fev1_dt, fev1_dt[ , .(r = cor(FEV1, age)), by = list(id)][r < 0,])

outliers

## Answer: There's one girl whose FEV1 is measured twice, with the second measurement being lower
## Anyone with 1 measurement will have r = NA, with 2 measurements r either -1 or +1.

# 4a. how many?

# count how many times each id appears
counts <- count(fev1, id) 
# or
counts <- aggregate(FEV1 ~ id, data = fev1, length)
names(counts) <- c("id", "n")
# or
counts <- fev1_dt[, .(n = nrow(.SD)), by = list(id)]

# then count how many times each count appears
# could pre-calculate and do a geom_col() but geom_bar() does a count for us
ggplot(data = counts, aes(x = factor(n))) +  # factor(n) helps labels on x
    geom_bar() +
    xlab("Number of observations per individual")

# 4b. each individual's change over time

ggplot(data = fev1, aes(x = age, y = FEV1, group = id)) +
    geom_line(alpha = 0.1) +
    geom_line(data = outliers, color = "red") +
    xlab("Age (years)") +
    ylab("FEV1 (L)") +
    ggtitle("Spirometry of 300 girls in Topeka, KS",
            "Decrease in FEV1 with age shown in red")

# 4c. singletons

singletons <- filter(counts, n == 1) %>% # find all ids with only 1 row
    inner_join(fev1)
# or
singletons <- merge(subset(counts, n == 1), fev1)

# or
singletons <- merge(counts[n==1], fev1)

ggplot(data = fev1, aes(x = age, y = FEV1, group = id)) +
    geom_line(alpha = 0.1) +
    geom_point(data = singletons, alpha = 0.2)  +
    xlab("Age (years)") +
    ylab("FEV1 (L)") +
    ggtitle("Spirometry of 300 girls in Topeka, KS",
            "Individuals with single observations shown with points")

# 4d. facet on age

fev1 <- group_by(fev1, id) %>%
    mutate(age0 = floor(age)) %>%
    ungroup

ggplot(data = fev1, aes(x = FEV1)) +
    geom_histogram(boundary = 1, binwidth = 0.2) +
    facet_wrap(~age0) 
# or

fev1_dt[ , age0 := floor(min(age)), by = list(id) ]

# or with cut
# and this merging of 17 and 18 year olds gives a better facet_wrap()
breaks <- c(seq(from = min(floor(fev1$age)), 
                to   = max(floor(fev1$age))-1, by = 1),
            Inf)

fev1 <- fev1 %>% 
    ungroup %>%
    mutate(age1 = cut(age,
                      breaks = breaks))

ggplot(data = fev1, aes(x = FEV1)) +
    geom_histogram(boundary = 1, binwidth = 0.2,
                   color = "black") +
    facet_wrap(~age1) +
    theme_bw() + 
    xlab(expression(FEV[1]~(L)))


# 5. smooth trend

ggplot(data = fev1, aes(x = age, y = FEV1)) +
    geom_line(alpha = 0.1, aes(group = id)) +
    geom_point(data = singletons, alpha = 0.2) +
    geom_smooth(color = "red", lty = 2, se=FALSE) +
    theme_bw()

# 6. statistical model

library(mgcv)
model <- gamm(data = fev1, FEV1 ~ s(age), random = list(id = ~1))

pred <- data.frame(age = seq(min(fev1$age),
                             max(fev1$age),
                             length.out = 101)) %>%
    mutate(FEV1 = predict(model$gam, .))
# or

pred <- data.table(age = seq(min(fev1$age),
                             max(fev1$age),
                             length.out = 101))

pred[, FEV1 := predict(model$gam, pred)]

# the use of aes(color = ...) here is non-standard
ggplot(data = fev1, aes(x = age, y = FEV1)) +
    geom_line(alpha = 0.05, aes(group = id, color = "Data")) +
    geom_point(data = singletons, alpha = 0.2) +
    geom_smooth(aes(color = "Loess"), se=FALSE, method = "loess") +
    theme_bw() +
    geom_line(data = pred, aes(color = "GAMM")) +
    xlab("Age (years)") +
    ylab("FEV1 (L)") +
    ggtitle("Average spirometry of 300 girls in Topeka, KS") +
    scale_color_manual(values = c("Data" = "black",
                                  "Loess" = "red",
                                  "GAMM" = "blue"),
                       name = "Source")

# E7a. skimr
library(skimr)
skim(fev1)

fev1[,c("id", "age0", "FEV1")] %>% group_by(age0) %>% skim

# E7b. GGally

library(GGally)
ggpairs(fev1, columns = c("age","height","FEV1"))

# change attributes of points to make a little clearer
ggpairs(fev1, columns = c("FEV1","height","age"), 
        upper = list(continuous = wrap("points", alpha = 0.3, size=0.1)),
        lower = list(continuous = "cor"))
