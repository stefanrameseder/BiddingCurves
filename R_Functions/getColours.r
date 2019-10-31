##############################
# Customize Functions
##############################

##############################

##############################
# Calculating Colours
##############################
# this function calculates colours corresponding to the filename
# the filename consists of
# [1]: type of data =LPGs
# [2]: type of rla  =SRL/MRL
# [3]: type of pm   =POS/NEG
# [4]: type of htnt =ALL/HT/NT
# moreover, here is saved the total MW colour
getColours <- function(attributes){
# with the attributes the corresponding list "col" will be filled
  if(attributes[2]=='SRL')
  {
  col <- c("linetype"="solid")   
    if(attributes[3]=='POS')
    {
      if(attributes[4]=='ALL')
      {
        col <- c(col, "Hochtarif (HT)"=colors()[26], "Niedrigtarif (NT)"=colors()[121])  
      }
      else if(attributes[4]=='HT')
      {
        col <- c(col, "Gew. mittlerer LP"=colors()[26], "Max. LP"=colors()[46], "Min. LP"=colors()[45] )
      }
      else if(attributes[4]=='NT')
      {
        col <-c(col, "Gew. mittlerer LP"=colors()[121], "Max. LP"=colors()[46], "Min. LP"=colors()[45] )
      }
    }
    else if(attributes[3]=='NEG')
    {
      if(attributes[4]=='ALL')
      {
        col <-c(col, "Hochtarif (HT)"=colors()[137], "Niedrigtarif (NT)"=colors()[134]) 
      }
      else if(attributes[4]=='HT')
      {
        col <-c(col, "Gew. mittlerer LP"=colors()[137], "Max. LP"=colors()[150], "Min. LP"=colors()[149] )
      }
      else if(attributes[4]=='NT')
      {
        col <-c(col, "Gew. mittlerer LP"=colors()[134], "Max. LP"=colors()[150], "Min. LP"=colors()[149] )
      }
    }
      
  }
  else if(attributes[2]=='MRL')
  {
  col <- c("linetype"="dashed") 
    if(attributes[3]=='POS')
    {
      if(attributes[4]=='ALL')
      {
        col <- c(col, "Hochtarif (HT)"=colors()[26], "Niedrigtarif (NT)"=colors()[121])  
      }
      else if(attributes[4]=='HT')
      {
        col <- c(col, "Gew. mittlerer LP"=colors()[26], "Max. LP"=colors()[46], "Min. LP"=colors()[45] )
      }
      else if(attributes[4]=='NT')
      {
        col <-c(col, "Gew. mittlerer LP"=colors()[121], "Max. LP"=colors()[46], "Min. LP"=colors()[45] )
      }
    }
    else if(attributes[3]=='NEG')
    {
      if(attributes[4]=='ALL')
      {
        col <-c(col, "Hochtarif (HT)"=colors()[137], "Niedrigtarif (NT)"=colors()[134]) 
      }
      else if(attributes[4]=='HT')
      {
        col <-c(col, "Gew. mittlerer LP"=colors()[137], "Max. LP"=colors()[150], "Min. LP"=colors()[149] )
      }
      else if(attributes[4]=='NT')
      {
        col <-c(col, "Gew. mittlerer LP"=colors()[134], "Max. LP"=colors()[150], "Min. LP"=colors()[149] )
      }
    }
  }

# for total mw 
col <-c(col, "Vorgehaltene Leistung"=colors()[49])
return(col)
}
