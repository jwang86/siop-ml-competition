#### SIOP ML Competition ####

#custom functions to be sourced for feat engineering support

# plot function for correlations
corrPlot_func = function(x) {
    ggcorr(data = NULL, 
           cor_matrix = x,
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
}
