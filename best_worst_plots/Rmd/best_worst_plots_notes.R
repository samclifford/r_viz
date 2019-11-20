library(ggplot2)
library(tidyverse)

## ----fig.margin=T, echo=T, fig.cap='Improper emphasis of most and least fuel efficient cars. We only need look at vertical range to see these are the min and max'----
data(mtcars)
ggplot(data=mtcars, aes(x=factor(cyl), y=mpg)) +
  geom_point(alpha=0.5) +
  geom_point(data=mtcars %>%
               mutate(model=row.names(.)) %>%
               arrange(., mpg) %>%
               filter(mpg %in% range(mpg)), 
               size=4, alpha=0.5 , pch=1, color='red') +
  theme_bw() +
  xlab('Cylinders') +
  ylab('Fuel efficiency (mpg)')


## ----fig.margin=T, echo=T, fig.cap="Fuel efficiency mapped to colour and y coordinate. Gradated pattern of colour implies there's another relationship with fuel efficiency being looked at.", fig.height=4, fig.width=6----
data(mtcars)
ggplot(data=mtcars, aes(x=wt, y=mpg)) +
  geom_point(aes(shape=factor(cyl),
             color=mpg),
              stroke=0, alpha=0.75, size=2) + 
  theme_bw() +
  theme(legend.position = 'right') +
  scale_shape_discrete(name='Number\nof\ncylinders') +
  xlab('Weight (1000 lbs)') +
  ylab('Fuel efficiency (mpg)') + 
  scale_color_gradient(low = 'red',
                       high='blue',name='Fuel\n efficiency\n (mpg)') +
  guides(col = guide_legend(ncol = 1),
         shape = guide_legend())


## ----fig.margin=T, fig.cap='Standard boxplot', echo=T----
data(mtcars)

ggplot(data=mtcars, aes(x=factor(cyl), y=mpg)) +
  geom_boxplot() + theme_bw() +
  xlab('Number of cylinders') +
  ylab('Fuel efficiency (mpg)') 


## ----fig.margin=T, fig.cap="Tufte's interpretation of boxplot that maximises data ink", echo=T, fig.height=1.5, fig.width=4----
library(ggthemes) # for the tufte boxplot geometry
data(mtcars)

ggplot(data=mtcars, aes(x=factor(cyl), y=mpg)) +
  geom_tufteboxplot() + theme_tufte() +
  xlab('Number of\ncylinders') +
  ylab('Fuel efficiency (mpg)') +
  coord_flip()


## ---- echo=T, fig.cap='High dimensional data set with points coloured by number of forward gears and shape mapped to transmission type', fig.margin=T, fig.width=4, fig.height=2.5----
mtcars_am <- mutate(mtcars, 
                    Transmission = 
                      factor(am, 
                             levels = c(0,1),
                             labels = c("Automatic", "Manual")),
                    Gears = factor(gear))

ggplot(data=mtcars_am, aes(x = factor(cyl), y = mpg)) +
  geom_point(aes(color = Gears, group = Gears, shape = Transmission),
             position  = position_dodge(w = 0.25),
             alpha     = 0.5) +
  theme_bw() +
  theme(legend.position='right') +
  xlab('Cylinders') +
  ylab('Fuel efficiency (mpg)')
  


## ---- echo=T, fig.cap='High dimensional data set with small multiples faceting by number of forward gears and transmission type. Two of the six facets are empty, indicating that the graph should be redesigned.', fig.margin=T, fig.height=2.5, fig.width=4----
ggplot(data = mtcars_am,
       aes(x = factor(cyl), y = mpg)) +
  geom_point(position = position_jitter(h = 0, w = 0.1)) +
  facet_grid(Transmission ~ Gears, labeller = labeller(.cols = label_both)) +
  theme_bw() +
  xlab('Number of cylinders') +
  ylab('Fuel efficiency (mpg)')


## ---- eval=T, echo=T, message=F, fig.width=6, fig.height=4, out.width = '100%'----
airquality <- mutate(airquality, 
                     Date = as.Date(paste('1973', Month, Day, sep='-')))

ggplot(data = airquality, aes(x = Date, y = Wind)) +  
  geom_col() + theme_bw() + 
  labs(y = 'Average wind speed (mph)\nbetween 0700-1000 at LGA airport') 



## ---- eval=T, echo=T, message=F, fig.width=4, fig.height=2, out.width = '100%'----
ggplot(data = airquality, aes(x = factor(Month))) +  
  geom_bar() + labs(x = 'Month') + theme_bw()



## ---- eval=T, echo=T, message=F, fig.width=4, fig.height=2, out.width = '100%'----
ggplot(data = airquality, aes(x = Ozone)) +  
  geom_histogram(binwidth = 10, boundary = 0) +  
  labs(x = 'Ozone concentration (ppb)') +
  theme_bw()


## ---- fig.height=6, fig.width=6, eval=T, echo=T, out.width='100%'--------
library(amt)
data(deer)

names(deer) <- c('UTMX', 'UTMY', 'Time', 'burst')
deer <- subset(deer, deer$burst == 8)

ggplot(data = deer, aes(x=UTMX/1000, y=UTMY/1000)) +
  geom_path(aes(color = Time)) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  xlab('Eastings (kilometres, EPSG:3035)') +
  ylab('Northings (kilometres, EPSG:3035)') +
  ggtitle('Movement of a deer across Northern Germany in 2008')


## ---- fig.height=2, fig.width=6, eval=T, echo=T, out.width='100%', fig.cap = "Rate of change of spinal bone mineral density"----
library(ElemStatLearn)
data(bone)

ggplot(data = bone, aes(x = age, y = spnbmd)) +
  geom_point(alpha=0.1) + facet_wrap(~ gender) +
  geom_line(aes(group = idnum)) +  theme_bw() +
  labs(x = 'Age (years)', y = expression(Rel.~BMD~(g~cm^{-3}~yr^{-1})),
       title = 'Relative spinal bone mineral density')



## ---- eval=T, echo=T, fig.height=3, fig.width=6, message=F, out.width='100%', fig.cap = "Histogram bars stacked and coloured by month"----
ggplot(data = airquality, aes(x = Ozone)) +  
  geom_histogram(binwidth = 10, boundary = 0,
                 color = 'black',
                 aes(fill = factor(Month))) +  
  labs(x = 'Ozone concentration (ppb)') +
  theme_bw() + 
  scale_fill_viridis_d(name = 'Month')


## ---- fig.height=6, fig.width=6, eval=T, echo=T, out.width='100%'--------
ggplot(data = airquality, aes(x = Solar.R, y = Temp)) + theme_bw() +
  geom_point(aes(size = Ozone, shape = factor(Month))) +
  theme(legend.position = 'bottom',
        legend.box = 'vertical') +
  scale_size_area(name = 'Ozone concentration (ppb)') +
  scale_shape(name = 'Month') + 
  labs(x = 'Solar radiation (Langleys)',
       y = expression(Max.~daily~temp.~(degree*F)))
   


## ---- echo=T, message=F, fig.width=6, fig.height=2.5, out.width='100%'----
ggplot(data = airquality, aes(x = Solar.R, y = Temp)) + theme_bw() +
  geom_point(alpha = 0.25) + theme(legend.position = 'bottom') +
  geom_smooth() +
  labs(x = 'Solar radiation (Langleys)',
       y = expression(Max.~daily~temp.~(degree*F)))

