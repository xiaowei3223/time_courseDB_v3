#查看是否已经安装R包
if (!requireNamespace("RNeo4j", quietly = TRUE))
  install.packages("RNeo4j")
if (!requireNamespace("dplyr", quietly = TRUE))
  install.packages("dplyr")
if (!requireNamespace("purrr", quietly = TRUE))
  install.packages("purrr")
if (!requireNamespace("visNetwork", quietly = TRUE))
  install.packages("visNetwork")
if (!requireNamespace("igraph", quietly = TRUE))
  install.packages("igraph")
if (!requireNamespace("neo4r", quietly = TRUE))
  install.packages("neo4r")


library(shiny)
library(visNetwork)
#------获取pathwayTable,为了进行选择pathwayID-----------------------------
library(RNeo4j)
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="xiaowei")
#graph = startGraph("http://10.168.119.229:7474/db/data/", username="neo4j", password="xiaowei")
query_pathwayTable = " match (n:pathway)-[]-(x) with distinct(n) return n.Description as Pathway_Description, n.ID AS pathway_ID "
pathwayTable <- cypher(graph, query_pathwayTable)
pathwayTable$pathway_ID_Description <- paste0(pathwayTable$pathway_ID,": ", pathwayTable$Pathway_Description) 




###########################################################################################################
library(RNeo4j)
#graph = startGraph("http://10.168.119.229:7474/db/data/", username="neo4j", password="xiaowei")
query_geneTable = " match (n:node)-[]-(x) with distinct(n) return n.name as gene_SYMBOL, n.entrezid AS gene_ENTREZID "
genesTable <- cypher(graph, query_geneTable)