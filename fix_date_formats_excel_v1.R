fix_date = function(df, newcol, dtecol, formatcol) {
  newcol <- deparse(substitute(newcol))
  dtecol <- deparse(substitute(dtecol))
  formatcol <- deparse(substitute(formatcol))
  df[[newcol]] <- nchar(df[[dtecol]])
  # add the format types for conversion
  df[[formatcol]]<-ifelse(str_detect(df[[dtecol]], pattern = "[:digit:]{1,2}/[:digit:]{1,2}/[:digit:]{4}|[:digit:]{1,2}/[:digit:]{1,2}/[:digit:]{2}"), "slash_mdy", 
                          ifelse(str_detect(df[[dtecol]], pattern = "^4(?!/)|^3(?!/)"), "unix", 
                                 ifelse(str_detect(df[[dtecol]], pattern = "[:digit:]{2}:[:digit:]{2}:"), "timestamp" , 
                                        ifelse(str_detect(df[[dtecol]], pattern = "-"), "dash", 
                                               ifelse(df[[newcol]] == 8 | df[[newcol]] == 7, "mdy", "NULL" )))))
  df
}

# functions to fix date formats
dash_date_fix = function(x) {as.Date(x, format = "%Y-%m-%d")}
slash_mdy_fix = function(x) {mdy(x)} 
timestamp_fix = function(x) {as.Date(x)}
unix_fix = function(x) {as.Date(as.numeric(x), origin = "1900-01-01")}


df$format[is.na(df$format)] <- "NULL"
dfx = split(df , (df$format), drop = FALSE)

date_col = "column_name"
# perform the date changes
dfx[["dash"]][[date_col]] = dash_date_fix(dfx[["dash"]][[date_col]])
dfx[["mdy"]][[date_col]] = slash_mdy_fix(dfx[["mdy"]][[date_col]])
dfx[["slash_mdy"]][[date_col]] = slash_mdy_fix(dfx[["slash_mdy"]][[date_col]])
dfx[["timestamp"]][[date_col]] = timestamp_fix(dfx[["timestamp"]][[date_col]])
dfx[["unix"]][[date_col]] = unix_fix(dfx[["unix"]][[date_col]])
dfx[["NULL"]][[date_col]] <- NA
dfx[["NULL"]][[date_col]] = as.Date(dfx[["NULL"]][[date_col]])
# bind it all back together
df = rbindlist(dfx)