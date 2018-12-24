
##  Set Format for Varibles in Each File

load("CBWomenTextTables.rda")
years = 2001:2012

findColLocs = function(spacerRow) {
  
  ## Find column locations using space in space rows
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  # Find the next column till no space in the row
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}


selectCols = function(shortColNames, headerRow, searchLocs) {
  
  ## Select the columns I want from header row
  ## becuase some columns are not useful for me
  
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    
    ## Find the columns in header row corresponding to 
    ## the columns I want 
    startPos = regexpr(shortName, headerRow)[[1]]
    if (startPos == -1) 
      return( c(NA, NA) )
    
    index = sum(startPos >= searchLocs)
    c(searchLocs[index] + 1, searchLocs[index + 1])
  }, 
  headerRow = headerRow, searchLocs = searchLocs )
}


extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time")){
    
    ## Extract all variables corresponding with the right columns
    
    # Find which row the space located
    eqIndex = grep("^===", file)
    
    # Extract the two key rows and the data
    # The one row before the space row is header row
    # The rows after the space row is body
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    
    # Remove footnotes and blank rows
    footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
    if ( length(footnotes) > 0 ) body = body[ -footnotes ]
    blanks = grep("^[[:blank:]]*$", body)
    if (length(blanks) > 0 ) body = body[ -blanks ]
    
    # Obtain the starting and ending positions of variables
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    invisible(Values)
  }

womenMat = lapply(womenTables, extractVariables)
save(womenMat, file = "cbWomenTables.rda")

