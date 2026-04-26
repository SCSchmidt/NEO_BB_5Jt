## Function to get the p-values from a Wilcoxon Test in a "list of distances" type

# var 1 needs to be the tested variable (ordinal!)
# var 2 is the grouping variable

# var_name will be the name of the colum with the p-value


get_p_wilcoxon <- function(df, vars, kult,  p.adjust.method){

  df <- df |> select(all_of(c(vars, kult)))
  
  ls <- list()
  
for (i in (1:length(vars)) ) {    
  
  varname <- vars[i]
  
  data <- data.frame(df[[varname]], df[[kult]]) # to be able to filter for NAs later

  colnames(data) <- c("var1", "var2") # makes reuse of old function easier
  
  data$var1 <- as.numeric(data$var1)
  
  data <- data |>
    dplyr::filter(!is.na(var1)) # remove NAs 
  
  d <-  pairwise.wilcox.test(data$var1, data$var2, 
                             p.adjust.method =  p.adjust.method) # calc wilcoxon
  res <- reshape2::melt(d$p.value)                                                 # aus p-value matrix Distanzenliste  
  res$p.value <- round(res$value, 3)      # p-value gerundet auf 3-Nachkommastellen

  res <- res |> dplyr::filter(!is.na(p.value))
  
  colnames(res) <- c("k1", "k2", "value", "p.value")      # colnames ändern

  res$k <- paste0(res$k1, " - ", res$k2)

  anzahlen <- as.data.frame(table(data$var2) ) # for n_note
  
  colnames(anzahlen) <- c("k1", "nK1")
  
  res <- merge(res, anzahlen, by.x = "k1", by.y = "k1", all.x = TRUE )

  colnames(anzahlen) <- c("k2", "nK2")
  
  res <-  merge(res, anzahlen, by.x = "k2", by.y = "k2", all.x = TRUE )
  
  res$n_note <- paste(res$k1, "n = ", res$nK1, "\n", res$k2, "n = ", res$nK2)
 
  res$variable <- paste(varname)
  
  res <- res |> select(k, k1, k2, n_note, p.value, variable) # compatible with the chi square function
  
  res$method <- "paarweiser Wilcoxon"
  
  ls[[varname]] <- res       
 
  }

    return(ls)

}

# p.adjust already done in the pairwise wilcoxon test?
#


## old get_p_wilcoxon to be able to compare (unvollständig)
old_get_p_wilcoxon <- function(var1, var2, var_name){
 
  d <-  pairwise.wilcox.test(as.numeric(var1), var2, p.adjust.method = "BH") # calc wilcoxon
 
  res <- reshape2::melt(d$p.value)                                                 # aus p-value matrix Distanzenliste  
 
  res$value <- round(res$value, 2)                                                 # p-value gerundet auf 2-Nachkommastellen
 
  
  colnames(res) <- c("K1", "K2", var_name)      # colnames ändern
return(res)
  
  }
