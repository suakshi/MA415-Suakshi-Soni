drop_one_value_col <- function(df, prt_val = FALSE){ 
  # browser()
  df_id <- ensym(df)
  if(prt_val){
    msg = paste("Looking for single value columns in data frame: ",as.character(df_id) )
    print(msg)}
  ## takes whole dataframe
  dropc <- NULL
  val <- NULL
  ## test each column for a single value
  for(i in 1:dim(df)[2]){   
    if(dim(distinct(df[,i]))[1] == 1){
      dropc <- c(dropc, i)
      val <- c(val, df[1,i])
    }
  } 
  
  if(prt_val){
    if(is.null(dropc)){
      print("No columns dropped")
      return(df)}else{
        print("Columns dropped:")
        # print(colnames(df)[drop])
        print(unlist(val))
        df <- df[, -1*dropc]
        return(df)
      }
  }
  df <- df[, -1*dropc]
  return(df)
}




## function shift_loc
## Moves adjacent data cells in a data.frame on a single row
## Use this function to fix alignment problems after separating 
## columns containing multiple columns of data. 

## Of course the working assumption is that there is room in the 
## data frame for the data you're shifting.
##
## The data cells that are empty after the data shift are NA.
## 
## Input paramaters
## 
## df -- data frame
## col_name -- name of colume where the left-most data item is located
## dat_name -- name of data item in the column
## num_col -- the number of columns is the same as the number of
##            adjacent data to be moved.
## num_shift -- the number of rows to move the data 
##


shift_loc <- function(df, col_name, dat_name, num_col, num_shift){
  # browser()
  col_num = which(colnames(df) == col_name)
  row_num = which(df[,col_num] == dat_name)  ## calcs a vector of rows
  
  for(k in 1:length(row_num)){
    d = rep(0,num_col) ## storage for items to be moved
    for(i in 1:num_col){
      d[i] = df[row_num[k], col_num + i - 1]
    }
    for(i in 1:num_col){
      ra = row_num[k]
      cb = col_num + i - 1
      df[ra, cb] <-  NA
    }
    for(j in 1:num_col){
      rc = row_num[k]
      cd = col_num + j - 1 + num_shift
      df[rc, cd] = d[j]
    }
  }
  # sprintf("Rows adjusted:")
  # print("%d",row_num)
  return(df)
}


## num_in_000_trunc()
## df is a data frame with non-numberic cols on the left
## and numeric columns 
## the numeric columns are strings with comma seperators 
## but without decimal
## the objective is to 
## the number of non-numeric cols is non
## the number of the row to be adjusted is row

num_in_000_trunc <- function(df, row, non){
  #  browser()
  prod = df |> slice(row)
  tvec = length(prod)
  nvec = tvec - non
  prod = as.character(prod[(non+1):tvec])
  prod = gsub(",\\d{3}$", "", prod)
  for(i in (non+1):tvec){df[row,i] = prod[i-non]}
  return(df)
}


##################################################
#### split Data Item  EXAMPLE

strawberry <- strawberry |>
  separate_wider_delim(  cols = `Data Item`,
                         delim = ",",
                         names = c("Fruit",
                                   "Category",
                                   "Item",
                                   "Metric"),
                         too_many = "error",
                         too_few = "align_start"
  )
###########################################


#########################################
# explore coefficient of variation
# 
# library(tidyverse)
# 
# mean = 2
# sd = .5
# from = mean - 4*sd
# to = mean + 4*sd
# 
# x = seq(from=from, to=to, by = .01)
# 
# y = dnorm(x = x, mean = mean, sd = sd)
# 
# cvp <- (sd/mean)*100
# 
# plot(x,y,type = "l", main = cvp)

######################################


