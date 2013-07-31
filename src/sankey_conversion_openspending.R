# Functions defined for transforming data
# =======================================

# tJSONarray converts dataframe into D3.js JSON objects
# Found at: http://theweiluo.wordpress.com/2011/09/30/r-to-json-for-d3-js-and-protovis/
toJSONarray = function(dtf){
  clnms <- colnames(dtf)
  
  name.value <- function(i){
    quote <- '';
    if(class(dtf[, i])!='numeric'){
      quote <- '"';
    }
    
    paste('"', i, '" : ', quote, dtf[,i], quote, sep='')
  }
  
  objs <- apply(sapply(clnms, name.value), 1, function(x){paste(x, collapse=', ')})
  objs <- paste('{', objs, '}')
  
  res <- paste('[', paste(objs, collapse=', '), ']')
  
  return(res)
}

# Trim whitespaces
trim <- function(x) gsub("^[[:space:]]+|[[:space:]]+$", "", x)

# Load data from OpenSpending API
# ===============================

library("rjson")

# Income breakdown data
json_income_url <- "http://openspending.org/api/2/aggregate?dataset=ugr-income&drilldown=articulo"
json_income_data <- fromJSON(file=json_income_url, method="C")
income <- data.frame(cbind(
  matrix(sapply(sapply(json_income_data$drilldown, "[", c(2)), "[", c(5))), 
  "Universidad de Granada", 
  matrix(sapply(json_income_data$drilldown, "[", c(1)))
))
names(income) <- c("source", "target", "value")

# spending_program breakdown data
json_spending_program_url <- "http://openspending.org/api/2/aggregate?dataset=ugr-spending&drilldown=programa"
json_spending_program_data <- fromJSON(file=json_spending_program_url, method="C")
spending_program <- data.frame(cbind(
  "Universidad de Granada", 
  matrix(sapply(sapply(json_spending_program_data$drilldown, "[", c(3)), "[", c(5))), 
  matrix(sapply(json_spending_program_data$drilldown, "[", c(1)))
))
names(spending_program) <- c("source", "target", "value")

# spending_chapter breakdown data
json_spending_chapter_url <- "http://openspending.org/api/2/aggregate?dataset=ugr-spending&drilldown=programa|to"
json_spending_chapter_data <- fromJSON(file=json_spending_chapter_url, method="C")
spending_chapter <- data.frame(cbind(
  matrix(sapply(sapply(json_spending_chapter_data$drilldown, "[", c(4)), "[", c(5))), 
  matrix(sapply(sapply(json_spending_chapter_data$drilldown, "[", c(1)), "[", c(5))), 
  matrix(sapply(json_spending_chapter_data$drilldown, "[", c(2)))
))
names(spending_chapter) <- c("source", "target", "value")

# Producing nodes and links D3.js JSON data
# ===========================================
  
# Initialization of D3.js nodes dataframe
nodes <- union("Universidad de Granada", union(income$source,union(spending_program$target, spending_chapter$target)))
nodes <- data.frame(cbind(seq(1:length(nodes)), nodes))
names(nodes) <- c("cod", "name")
nodes$name <- trim(nodes$name)

# Initialization of D3.js links dataframe
links <- rbind(income, spending_program, spending_chapter)
links$source <- trim(links$source)
links$target <- trim(links$target)

# Here comes the magic: merging datasets for replacing names with codes
links <- merge(links, nodes, by.x="source", by.y="name")
links <- merge(links, nodes, by.x="target", by.y="name")
links <- links[,c("cod.x", "cod.y", "value")]
names(links) <- c("source","target","value")
links$source <- as.numeric(links$source)-1
links$target <- as.numeric(links$target)-1
links$value <- as.numeric(links$value)

nodes <- data.frame(nodes[,c("name")])
names(nodes) <- c("name")

# Output to JSON D3.js compatible format
output <- paste('{"nodes":', toJSONarray(nodes), ',')
output <- paste(output, '"links":', toJSONarray(links), '}')
write(output, "ugr-sankey-openspending.json")