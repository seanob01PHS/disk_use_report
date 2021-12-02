suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)

})


#####################################################################
#####################################################################
#filepaths

setwd("/conf/LIST_analytics/#diskuse/")
filepath_for_input <- "file_list_input/List_file_sizes.txt"
#####################################################################
#####################################################################


#tried to make as safe as possible
#maybe overly so
#could be done with case_when 
unit_to_power <- function(unit){
  unit <- tolower(unit)
    #no unit = bytes
    if        (unit==""){
      power <- 0
    } else if  (unit=="k" | unit=="kb"){
      power <- 3
    }  else if (unit=="m" | unit=="mb"){
      power <- 6
    } else if  (unit=="g" | unit=="gb"){
      power <- 9
    } else if  (unit=="t" | unit=="tb"){
      power <- 12
    } else{
      print("Error, incorrect units provided")
      return(NaN)
    }
  return(power)
}

bytes_from_str <- function(size){
  #strips all chars != (numeric or period)
  number <- as.numeric(gsub("[^0-9.]+","",size))
  #strips all numeric or period or whitespace
  unit <- gsub("(\\d|\\.|\\s)+","",size)
  return(number * 10**unit_to_power(unit))
}

#name shortening
str_from_bytes <- function(bytes){
  utils:::format.object_size(bytes, units="auto",standard="SI")
}

#{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}
#{{     pre-processing     }}
#{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}
raw_lines <- readLines(filepath_for_input)

files_data <- tibble(raw=raw_lines) %>% mutate_if(is.character, utf8_encode)

#there are 2 lines at the end that give capacity/total used information
capacity_info <- files_data %>% tail(1)
total_capacity <- capacity_info[["raw"]][[1]] %>% str_match_all("[^\\s]+") %>% .[[1]] %>% .[[2]]
used_capacity <- capacity_info[["raw"]][[1]] %>% str_match_all("[^\\s]+") %>% .[[1]] %>% .[[3]]
total_usage_str <- capacity_info[["raw"]][[1]] %>% str_match_all("[^\\s]+") %>% .[[1]] %>% .[[5]]

#remove these lines
files_data <- files_data %>% head(-2)

#user = first group of non-whitespace characters
files_data <- files_data %>% mutate(user=str_match_all(raw, "[^\\s]+") %>% map_chr(1))

#this gets the second regex group that matches non whitespace characters
#may be very wasteful, not sure
#other option is to do a separate read_delim(delim=" ") 
#and take the 2nd column from that
files_data <- files_data %>% mutate(file_size_str=raw %>% str_match_all("[^\\s]+") %>% map_chr(2))

#filepath is everything after / (hopefully)
files_data <- files_data %>% mutate(file_path=raw %>% str_extract("/(.*)"))

#make numeric filesize col
files_data <- files_data %>% mutate(file_size=file_size_str %>% sapply(bytes_from_str))

##############
##NOTE
#############
# Filepaths are sometimes truncated
# We must group these together and sum their file sizes
files_data <- files_data %>% group_by(file_path, user) %>% 
                              summarise(file_size = sum(file_size), .groups="drop")

#use above to improve the initial str col
files_data <- files_data %>% mutate(file_size_str= file_size %>% sapply(str_from_bytes))

#group is the 3rd regex group of non "/" characters
#/1st group/second group/the variable i'm calling group/etc
files_data <- files_data %>% mutate(group=file_path %>% str_match_all("[^/]+") %>% map_chr(3))


files_data <- files_data %>% arrange(desc(file_size))

#{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}
#{{     groupings     }}
#{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}

#returns a vector of ... ordered by increasing total file_size use
total_group_use_order <- function(df, ...){
  df %>% group_by(...) %>% 
    summarise(total_use=sum(file_size)) %>% 
    arrange(total_use) %>% 
    pull(...)
}

group_order <- files_data %>% total_group_use_order(group)
user_order <- files_data %>% total_group_use_order(user)

files_by_group <- files_data %>% arrange(group,
                                         desc(file_size))
files_by_user <- files_data %>% arrange(user,
                                        desc(file_size))


#summaries---------------------------------------------------------------------
#to avoid re-typing code
make_summary <- function(df, ...){
  df %>% arrange(desc(file_size)) %>% 
    group_by(...) %>% 
    summarise(total_use=sum(file_size),
              largest_file=first(file_path),
              largest_file_size=first(file_size)) %>%
    mutate(total_use_str=total_use %>% sapply(str_from_bytes),
           largest_file_size_str= largest_file_size %>% sapply(str_from_bytes))
}

group_summary <- files_data %>% make_summary(group) %>% arrange(desc(total_use))

group_user_summary <- files_data %>% make_summary(group, user) %>% arrange(grepl("^\\d", group), group, user, desc(total_use))

user_summary <- files_data %>% make_summary(user) %>% arrange(desc(total_use))


