### SIOP ML Competition ###

rm(list = ls()) #keep env clean

#set working dir - shortcut = ctrl + shift + H

#load libraries
library(tidyverse) #dplyr masks stats::filter, lag

#load data from github repo
datFile = "https://raw.githubusercontent.com/dkgreen24/siop-ml-competition/master/data/train.csv"

#read in data - change feature to lower case
train = read_csv(datFile) %>% 
    rename_all(list(tolower))

# dev = read_csv(datFile) %>% 
#     rename_all(list(tolower))

#count of missingness
data.frame("value" = sapply(
    train, function(x) {
        sum(is.na(x))
        }
    )) %>%
    rownames_to_column(var = "item")

#varNames = names(train)

#list with predicator variables
predVars = list()

#create dfs for specific variable sets
predVars[["sjt"]] = train %>% 
    select(sj_most_1:sj_time_9) %>% 
    select_at(vars(-contains("time")))

predVars[["sjt_time"]] = select_at(train, vars(contains("sj_time"))) #seconds

predVars[["scene1"]] = select_at(train, vars(contains("scenario1"))) 

predVars[["scene2"]] = select_at(train, vars(contains("scenario2")))

predVars[["bio"]] = select_at(train, vars(contains("bio")))

predVars[["pscale"]] = select_at(train, vars(contains("pscale")))

predVars[["adv_imp"]] = select(train, protected_group)

critVars = select(train, 2:7) #only first 7,890 obs have data

library(psych) #masks ggplot2::%+%, alpha

#compute correlations
predVars_cors = lapply(predVars[-7], 
                       function(x) psych::corr.test(x, use = "pairwise"))


library(GGally) #masks dplyr::nasa

# plot function for correlations
corrPlots = lapply(
    predVars_cors, 
    function(df) {
        ggcorr(data = NULL,
               cor_matrix = df[["r"]],
               #method = c("pairwise", "pearson"),
               size = 3,
               hjust = .75,
               nbreaks = 11,
               palette = "RdBu",
               label = TRUE, 
               label_color = "black",
               digits = 2,
               #label_alpha = .3, 
               label_round = 2, 
               label_size = 2) + 
                    theme(legend.position = "none")
        })

#add criterion variables to list of correlations
corrPlots[["critVars"]] = ggcorr(data = critVars, 
                                 method = c("pairwise", "pearson"), 
                                 size = 3, 
                                 hjust = .75, 
                                 nbreaks = 11, 
                                 palette = "RdBu", 
                                 label = TRUE, 
                                 label_color = "black", 
                                 digits = 2, 
                                 label_round = 2, 
                                 label_size = 2) + 
                                    theme(legend.position = "none")
