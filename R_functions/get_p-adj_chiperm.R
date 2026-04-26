## Chi-Quadrat aber 1. mit simulierten p-Werten und 2.  mit adjusted p-values

## Ausgangslage: Tabelle mit Gefäßinformationen 

## Schleife: nimmt immer eine Spalte mit Kultur und filtert nach 2 Kulturen
  ## 2. Schleife nimmt eine Spalte 
    ## table-mit gefilterter Kultur-Spalte und der zweiten
    ## chi permutation
    ## speichern in einer Tabelle: Kultur 1 Name, Kultur 2 Name und p-WErt

## Ende: alle p-WErte in in p-adjust umwandlen mit p.adjust-function

## kult = vektorname mit Kulturinformation
## vars = variablen, die untersucht werden sollen

## Paket wPerm needed


get_p_chi_perm <- function(df, vars, kult, id) {
  
  df <- df |> dplyr::select(all_of(c(vars, kult, id)))
  
  ls <- list()
  
  kulturen <- df[[kult]]

  kulturen <- unique(kulturen)

  for (l in c(1:length(vars)) ) { 
    
    res_df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("k", "k1", "k2", "n_note", "p.value", "P.value","variable", "method"))
    res_df$k <- as.character(res_df$k)
    res_df$k1 <- as.character(res_df$k1)
    res_df$k2 <- as.character(res_df$k2)
    res_df$n_note <- as.character(res_df$n_note)
    res_df$p.value <- as.numeric(res_df$p.value)
    res_df$variable <- as.character(res_df$variable)
    res_df$method <- as.character(res_df$method)
    
        
    varname <- paste(vars[l])
    
    data <- data.frame(kultur = df[[kult]], variable = df[[varname]], id = df[[id]]) # select rel cols

      for (i in 1:(length(kulturen)-1 ) ) {
  
        k1 <- kulturen[i] 
    
          for (j in 2:(length(kulturen) ) ) {
      
            k2 <- kulturen[j]
      
                   data2 <- data |>                          
                     dplyr::filter(kultur %in% c(k1, k2)  ) |>         # filter for two cultures
                     dplyr::filter(!is.na(variable)) |>
                     dplyr::filter(variable != "NA") |>
                     dplyr::filter(!(stringr::str_detect(variable, pattern = "\\?")) ) |> # alle unsicherheiten raus
                     unique() # dopplungen pro gefäß vermeiden
          
          # check if there are enough infos: 
          
              if (nrow(data2 |> dplyr::filter(kultur == k1)) < 2) {
                  print(paste("Datensatz für ", k1, " unter 2 in ", varname) )
                  } 
                else if (nrow(data2 |> dplyr::filter(kultur == k2)) < 2) {
                  print( paste("Datensatz für ", k2, " unter 2 in ", varname))
                  }  
                else if (length(unique(data2$variable)) < 2) {
                  print(paste("Variable ", varname, " unter 2 in ", k1, k2))
                  }
                else if (length(unique(data2$kultur)) < 2) {
                  print(paste("gleiche Kultur", k1) ) } #
                
                else if (paste0(k2, " - ", k1) %in% res_df$k) {
                     print(paste("Paar", k1, k2, "schon besprochen") ) } #  
                else {
                  
                     # alle Voraussetzungen erfüllt:
  
     
            res <- wPerm::perm.ind.test(data2,       # chi quadrat
                          type = "raw",    # df mit var1 var2 und freq
                          var.names = c("Var1", "Var2"),
                          R = 9999) 
            res$var <- varname
            res$K1 <- k1
            res$K2 <- k2
            res$k <- paste0(k1, " - ", k2)
            res$K1n <- dplyr::n_distinct(data2 |> dplyr::filter(kultur == k1) |> dplyr::select(id) ) # n soll gefäßanzahl entsprechen
            res$K2n <- dplyr::n_distinct(data2 |> dplyr::filter(kultur == k2) |> dplyr::select(id) )
            res$n_note <- paste0(k1, " n = ", res$K1n, ", \n", k2, " n = ", res$K2n)
    
          res_zusammen <- data.frame("k" = res$k, 
                                     "k1" = res$K1, 
                                     "k2" = res$K2, 
                                        "n_note" = res$n_note, 
                                        "p.value" = res$p.value, 
                                        "P.value"  = res$P.value,
                                        "variable" = varname,
                                     "method" = "Chi-Quadrat, Permutationstest" )  
          
  
    
          res_df <-rbind(res_df, res_zusammen)
          
          }
          
      }
      }
    ls[[varname]] <- res_df
    
  }

  return(ls)
  
}

adjust_p <- function(ls) {
p_values <- Reduce(function(x, y) merge(x, y, all=TRUE), ls) # alle listeneinträge zusammenmergen

p_values$p.adj <- p.adjust(p_values$p.value, method = "fdr") # adjust p values

return(p_values)

}
