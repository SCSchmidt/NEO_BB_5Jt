## KS - Test in der Schleife

## Ausgangslage: Tabelle mit DGM-o.ä. Werten pro Fst 

## 1- Schleife filter nach gruppe -> das ist Bundesland

## Schleife: nimmt immer eine Spalte mit Kultur und filtert nach 2 Kulturen
  ## 2. Schleife nimmt eine Spalte 
    ## table-mit gefilterter Kultur-Spalte und der zweiten
    ## ks test
    ## speichern in einer Tabelle: Kultur 1 Name, Kultur 2 Name und p-WErt

## Ende: alle p-WErte in in p-adjust umwandlen mit p.adjust-function

## kult = vektorname mit Kulturinformation
## vars = variablen, die untersucht werden sollen
## gruppe = gruppierende variable (Bundesland)


## dplyr needed

get_ks <- function(df, vars, kult, gruppe) {
  
  df2 <- data.frame(kultur = df[[kult]],       # select rel cols
                    variable = df[[vars]],
                    group = df[[gruppe]])
  
  ls <- list()
  
  kulturen <- levels(df2$kultur)
  
  gr <- unique(df2$group)

  for (l in c(1:length(gr) ) ) { 
    
    res_df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c("k", "k1", "k2", "n_note", "p.value", "gr", "method"))
    res_df$k <- as.character(res_df$k)
    res_df$k1 <- as.character(res_df$k1)
    res_df$k2 <- as.character(res_df$k2)
    res_df$n_note <- as.character(res_df$n_note)
    res_df$p.value <- as.numeric(res_df$p.value)
    res_df$gr <- as.character(res_df$gr)
    res_df$method <- as.character(res_df$method)
    
        
    varname <- paste(gr[l])
    
    data <- df2 |>
      dplyr::filter(group == varname)
    
      for (i in 1:(length(kulturen)-1 ) ) {
  
        k1 <- kulturen[i] 
    
          for (j in 2:(length(kulturen) ) ) {
      
            k2 <- kulturen[j]
            
                   data2 <- data |>
                     filter(kultur %in% c(k1, k2) ) # filter for two cultures

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
              d1 <- data2 |> 
                dplyr::filter(kultur == k1)
              
              d2 <- data2 |> 
                dplyr::filter(kultur == k2)
     
                  res <- ks.test(d1$variable, d2$variable )
                  
            res$gr <- varname
            res$K1 <- k1
            res$K2 <- k2
            res$k <- paste0(k1, " - ", k2)
            res$K1n <- nrow(data2 |> dplyr::filter(kultur == k1)  ) # n soll gefäßanzahl entsprechen
            res$K2n <- nrow(data2 |> dplyr::filter(kultur == k2)  )
            res$n_note <- paste0(k1, " n = ", res$K1n, ", \n", k2, " n = ", res$K2n)
    
          res_zusammen <- data.frame("k" = res$k, 
                                     "k1" = res$K1, 
                                     "k2" = res$K2, 
                                        "n_note" = res$n_note, 
                                        "p.value" = res$p.value, 
                                        "gr" = varname,
                                     "method" = res$method )  
          
  
    
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
