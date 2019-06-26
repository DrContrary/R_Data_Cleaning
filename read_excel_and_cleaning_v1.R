


# Get all the directors that you will pull from
dirs = str_subset(list.dirs(".", full.names = TRUE), pattern = "") 

# read in all the files from a given directory
readfiles = function(x, sheet_num){
  list = list.files(path = dirs[x], pattern = ".xlsx", full.names = TRUE, recursive = TRUE)
  bad_files = grep("NOT.xlsx", list, value = TRUE)                        # files not to be imported will have NOT at the end
  list = list[!(list %in% bad_files)]
  listfiles = lapply(list, read_excel, sheet = sheet_num, col_types = "text")
  names(listfiles) = str_extract(list, "pattern")                         # the names and id columns will match a pattern in the filepath
  listfiles = lapply(listfiles, function(xx) select(xx, cols))            # set the column names you want first to select by
  listfiles = listfiles[sapply(listfiles, function(y) dim(y)[1]) > 0]     # remove empty data frames
  listfiles = lapply(listfiles, function(xxx) set_names(xxx, cols_new))   # update the column names
  listfiles = lapply(listfiles, function(df) mutate_at(df, .vars = 1:30, as.character))  # for row binding make them all characters
  rbindlist(listfiles, idcol = "id")                                      # row bind into a df and use the names as an id column
}


# write a little function to fix some of the common NA issues
fix.na = function(x) {
  x[x == "N/A"] <- NA
  x[x == "NA"] <- NA
  x[x == "na"] <- NA
  x[x == "n/a"] <- NA
  x[x == "TBD"] <- NA
  x[x == "Not Applicable N/A"] <- NA
  x[x == "Pending"] <- NA
  return(x)
}


# get information about bad text
badtxt = function(x){
  y= grepl("\r|\n|'|=|<|>|(\\$)|,", x)
  table(y, useNA = "always")
}
badtxt_df = lapply(df, badtxt)


# write the tables to the global environment
list2env(dflist,envir=.GlobalEnv)


# get the number of NAs per column
colSums(is.na(df))