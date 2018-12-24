
## Change Time Format

load("cbWomenTables.rda")
load("CBWomenTextTables.rda")

years = 2001:2012

convertTime = function(time) {
  
  ## Convert Time format from char to number by mins
  
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  })
}

createDF = function(Res, year, sex) 
{
  
  ## Creat dataframe, determine which time to use,
  ## Remove the rows with missing time
  
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  # Drop rows with no age
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = suppressWarnings(as.numeric(Res[, 'ag'])), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  
  invisible(Results)
}

# Year 2006 has missing time 
separatorIdx = grep("^===", womenTables[["2006"]])
separatorRow = womenTables[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
womenTables[['2006']][separatorIdx] = separatorRowX
womenMat = sapply(womenTables, extractVariables)

womenDF = mapply(createDF, womenMat, year = years,
                 sex = rep("F", 12), SIMPLIFY = FALSE)
womenMat


cbWomen = do.call(rbind, womenDF)
save(cbWomen, file = "cbWomen.rda")
