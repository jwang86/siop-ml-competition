### SIOP ML Competition ###

rm(list = ls()) #keep env clean

#set working dir
setwd("~/Dropbox/Stats_n_Programming/R Code/siop-ml-comp/yr2020/code/")

#load libraries
library(dplyr) #masks stats::filter, lag; base::intersect, setdiff, setequal, union
library(purrr)
library(forcats)
library(readr)
library(tidyr)
library(stringr)

#read in data - change feature to lower case
train = read_csv("../data/train.csv") %>% 
    rename_all(list(tolower))

dev = read_csv("../data/dev.csv") %>% 
    rename_all(list(tolower))

sapply(train, function(x) {
    sum(is.na(x))
    })

