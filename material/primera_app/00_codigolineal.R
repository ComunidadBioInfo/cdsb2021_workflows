library(dslabs)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggridges)
data(gapminder)

ymin <- 1960
ymax <- 2016
colorBy <- "continent"
  
gapminder %>%
  filter( country %in% c("Mexico", "South Korea", "Germany") ) %>%
  ggplot( aes( year, life_expectancy, col=get(colorBy), group=country )) +
  geom_point( size=0.3 ) +
  geom_line( ) +
  xlim(c(ymin, ymax)) +
  labs(col=colorBy)

gapminder %>%
  filter( country %in% c("Mexico", "South Korea", "Germany") ) %>%
  ggplot( aes( year, fertility, col=get(colorBy), group=country )) +
  geom_point( size=0.3 ) +
  geom_line( ) +
  xlim(c(ymin, ymax)) +
  labs(col=colorBy)


keep_regions <- gapminder %>%
  filter( country %in% c("Mexico", "South Korea", "Germany") ) %>%
  pull( region ) %>%
  as.character %>%
  unique

gapminder %>%
  mutate( dollars_per_day=gdp/population/365 ) %>%
  filter( year %in% seq(1960, 2010, 10), 
          !is.na(dollars_per_day), 
          region %in% keep_regions ) %>%
  filter( between(year, ymin, ymax) ) %>%
  ggplot( aes( dollars_per_day, factor(year) ) ) +
  scale_x_continuous(trans = "log2")  +
  geom_density_ridges(jittered_points = TRUE) +
  facet_grid( ~region, scales="free")

gapminder %>%
  mutate( dollars_per_day=gdp/population/365 ) %>%
  filter( 
    country %in% c("Mexico", "South Korea", "Germany"), 
    between( year, ymin, ymax) )
