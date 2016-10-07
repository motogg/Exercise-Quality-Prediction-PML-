missThreshold = function(dataframe, threshold){
        
        set.seed(1)
        ## Shuffles Dataframe
        x = dataframe[sample(seq_len(nrow(dataframe)), nrow(dataframe)), ]
        
        ## Computes the percentage missing in each variable
        percentMissing = sapply(x, function(x){
                sum(is.na(x))* 100/length(x)
        } )
        
        ## Determines which variables to keep or discard
        toKeep = ifelse(percentMissing <= threshold, 1, 0)
        toKeep = toKeep == 1
        x[toKeep]
}
