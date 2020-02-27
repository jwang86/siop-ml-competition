### SIOP ML Competition ###

rm(list = ls()) #keep env clean

#set working dir - shortcut = ctrl + shift + H

#load libraries
library(tidyverse, quietly = TRUE) #dplyr masks stats::filter, lag

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
feats = list(
    #create dfs for specific variable set 
    sjt = train %>% 
        select(sj_most_1:sj_time_9) %>% 
        select_at(vars(-contains("time"))), 
    
    sjt_time = select_at(train, vars(contains("sj_time"))), #seconds
    
    scene1 = select_at(train, vars(contains("scenario1"))), 
    
    scene2 = select_at(train, vars(contains("scenario2"))), 
    
    bio = select_at(train, vars(contains("bio"))), 
    
    pscale = select_at(train, vars(contains("pscale"))), 
    
    crit_vars = train[, 2:9])


library(psych) #masks ggplot2::%+%, alpha

#compute correlations
featsCorrs = lapply(feats[-7], 
                       function(x) psych::corr.test(x, use = "pairwise"))

#compute polychoric correlations foe criterion variables
featsCorrs[["crit_vars"]] = polychoric(feats[["crit_vars"]], na.rm = TRUE)

#visualize correlation tables
library(GGally) #masks dplyr::nasa

# plot function for correlations
corrPlots = lapply(
    featsCorrs[-7], 
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
               #angle = -45, 
               label_round = 2, 
               label_size = 2) + 
                    theme(legend.position = "none")
        })

#add criterion variables to list of correlations
corrPlots[["crit_vars"]] = ggcorr(#data = feats[[7]], 
                                 #method = c("pairwise", "pearson"), 
                                 cor_matrix = featsCorrs[[7]][["rho"]],
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





