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
(missing.stats <- data.frame("value" = sapply(
    train, function(x) {
        sum(is.na(x))
        }
    )) %>%
    rownames_to_column(var = "item"))

#varNames = names(train)

#list with predicator variables
feats = list(
    #create dfs for specific variable set - can be altered as needed
    sjt = train %>% 
        select(sj_most_1:sj_time_9) %>% 
        select_at(vars(-contains("time"))), 
    
    sjt_time = select_at(train, vars(contains("sj_time"))), #seconds
    
    scene1 = select_at(train, vars(contains("scenario1"))), 
    
    scene2 = select_at(train, vars(contains("scenario2"))), 
    
    bio = select_at(train, vars(contains("bio"))), 
    
    pscale = select_at(train, vars(contains("pscale"))), 
    
    #overall_rating:retained columns
    crit_vars = train[, 2:9])

#descriptive summary statistics for all features
# sapply(feats, function(x) sum(is.na(x)))
sapply(feats, function(x) summary.data.frame(x))

library(psych) #masks ggplot2::%+%, alpha

#compute correlations
featsCorrs = lapply(feats[-7], 
                       function(x) psych::corr.test(x, use = "pairwise"))

#compute polychoric correlations for criterion variables
featsCorrs[["crit_vars"]] = polychoric(feats[["crit_vars"]], na.rm = TRUE)
#only 28 cells used continuity correction, so estimates pretty stable

#visualize correlation tables
library(GGally) #masks dplyr::nasa

# plot function for correlations
corrPlots = lapply(
    featsCorrs[-7], #disregard crit_vars here
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
corrPlots[["crit_vars"]] = ggcorr(data = NULL,
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

# ggsave("tough.svg", path = "../figs/corrPlot-crit_vars.svg")

# personality subscales
pscale = list(
  #create dfs for each personality subscale
  pscale1 = select_at(feats$pscale, vars(contains("pscale01"))),
  pscale2 = select_at(feats$pscale, vars(contains("pscale02"))),
  pscale3 = select_at(feats$pscale, vars(contains("pscale03"))),
  pscale4 = select_at(feats$pscale, vars(contains("pscale04"))),
  pscale5 = select_at(feats$pscale, vars(contains("pscale05"))),
  pscale6 = select_at(feats$pscale, vars(contains("pscale06"))),
  pscale7 = select_at(feats$pscale, vars(contains("pscale07"))),
  pscale8 = select_at(feats$pscale, vars(contains("pscale08"))),
  pscale9 = select_at(feats$pscale, vars(contains("pscale09"))),
  pscale10 = select_at(feats$pscale, vars(contains("pscale10"))),
  pscale11 = select_at(feats$pscale, vars(contains("pscale11"))),
  pscale12 = select_at(feats$pscale, vars(contains("pscale12"))),
  pscale13 = select_at(feats$pscale, vars(contains("pscale13"))))

# compute correlations for each personality subscale
pscaleCorrs = lapply(pscale, 
                    function(x) psych::corr.test(x, use = "pairwise"))

# plot personality subscale correlations
pscale.corrPlots = lapply(
  pscaleCorrs,
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

pscale.names = sapply(pscale, names)

# export plots
for (i in 1:length(pscale.names)){
  pscale.corrPlots[i]
  ggsave(pscale.corrPlots[[i]], 
         file=paste0("../figs/corrPlot-pscales/",names(pscale.names[i]),".png"))
}

# "Items that needs to be reverse coded:
# pscale01: q2, q3
# pscale02: q2
# pscale03: q1
# pscale04: q1
# pscale05: na
# pscale06: q3, q6
# pscale07: q2
# pscale08: q2, q4
# pscale09: q2
# pscale10: q3, q4
# pscale11: q2, q3
# pscale12: q3, q4
# pscale13: q3, q4
# "

# reverse code personality items
keys <- rep(1,55)  # key filler

# index of reversed items
keys[c(2,3,6,9,13,23,26,28,32,34,36,41,42,44,45,49,50,53,54)] <- -1 
pscale.reversed <- as.data.frame(reverse.code(keys,feats$pscale))

# group processed items & check correlations
psubscales.reversed = list(
  #create dfs for each personality subscale
  pscale1 = select_at(pscale.reversed, vars(contains("pscale01"))),
  pscale2 = select_at(pscale.reversed, vars(contains("pscale02"))),
  pscale3 = select_at(pscale.reversed, vars(contains("pscale03"))),
  pscale4 = select_at(pscale.reversed, vars(contains("pscale04"))),
  pscale5 = select_at(pscale.reversed, vars(contains("pscale05"))),
  pscale6 = select_at(pscale.reversed, vars(contains("pscale06"))),
  pscale7 = select_at(pscale.reversed, vars(contains("pscale07"))),
  pscale8 = select_at(pscale.reversed, vars(contains("pscale08"))),
  pscale9 = select_at(pscale.reversed, vars(contains("pscale09"))),
  pscale10 = select_at(pscale.reversed, vars(contains("pscale10"))),
  pscale11 = select_at(pscale.reversed, vars(contains("pscale11"))),
  pscale12 = select_at(pscale.reversed, vars(contains("pscale12"))),
  pscale13 = select_at(pscale.reversed, vars(contains("pscale13"))))

pscaleRevCorrs = lapply(psubscales.reversed, 
                        function(x) psych::corr.test(x, use = "pairwise"))

# pscaleReversedPolyCorrs = lapply(psubscales.reversed, 
#                              function(x) psych::polychoric(x, na.rm = TRUE))

pscale.reversed.corrPlots = lapply(
  pscaleRevCorrs,
  function(df) {
    ggcorr(data = NULL,
           cor_matrix = df[["r"]], #df[["rho]], #when using polychoric corrs
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
######everything looks good######

# export plots
for (i in 1:length(pscale.names)){
  pscale.reversed.corrPlots[i]
  ggsave(pscale.reversed.corrPlots[[i]], 
         file=paste0("../figs/corrPlot-pscales-rev/",names(pscale.names[i]),
                     ".png"))
}

# make 13 composite scores and put back to feats
feats$pcomp <- as.data.frame(do.call(cbind, lapply(psubscales.reversed, 
                                                   function(x) rowMeans(x))))

# write each feature as a separate sheet 
# save into one xslx file
library(openxlsx)
feat.names <- names(feats)

wb <- createWorkbook()
for (i in 1:length(feat.names)){
  addWorksheet(wb, feat.names[i])
  writeData(wb, feat.names[i], feats[[i]])
}
saveWorkbook(wb, file = "../data/features.xlsx", overwrite = TRUE)

