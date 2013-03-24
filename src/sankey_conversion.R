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

# Load data and aggregate data
# ============================
UGRSpending <- read.csv2('ugr-estado-gastos-2013.csv', sep=',')
UGRIncome <- read.csv2('ugr-estado-ingresos-2013.csv', sep=',')

# Aggregate amounts by income_article, spending program and spending chapter
income_article <- aggregate(cantidad ~ articulo, data=UGRIncome, FUN="sum")
spending_program <- aggregate(cantidad ~ programa, data=UGRSpending, FUN="sum")
spending_cap <- aggregate(cantidad ~ programa + capitulo, data=UGRSpending, FUN="sum")

# Adding Universidad as source and target
income_article$new_col <- "Universidad de Granada"
spending_program$new_col <- "Universidad de Granada"

# Rearrange datasets
names(income_article) <- c("source","value","target")
names(spending_program) <- c("target","value","source")
names(spending_cap) <- c("source", "target", "value")

income_article <- income_article[, c(1,3,2)]
spending_program <- spending_program[, c(3,1,2)]

# Producing nodes and links D3.js JSON data
# ===========================================
  
# Initialization of D3.js nodes dataframe
nodes <- union("Universidad de Granada", union(income_article$source,union(spending_program$target, spending_cap$target)))
nodes <- data.frame(seq(1:length(nodes)), nodes)
names(nodes) <- c("cod", "name")
nodes$name <- trim(nodes$name)

# Initialization of D3.js links dataframe
links <- rbind(income_article, spending_program, spending_cap)
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
write(output, "ugr-sankey.json")