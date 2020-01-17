

source("before_Run_Shiny.R")
source("genes_relationships_in_pathway_UI.R") 




ui <- 
  navbarPage(title = "Time-Course DB Query",
             tabPanel("Genes Relationships in Pathway",
                      genes_relationships_in_pathway_UI
                      ), #
             tabPanel("Genes Neighborhoods Relationships",
                      source("GenesNeighborhoodsUI.R")
                      )
             )

  

  


#################################################################################################
#================================================================================================
#################################################################################################

server = function(input, output){
  #source("genes_relationships_in_pathway_Server.R")
  #source("GenesNeighborhoodsSERVER.R")
  #################################################################################################
  #================================================================================================
  #################################################################################################
  #===========================================
  #连接neo4j
  #===========================================
  library(neo4r)
  con <- neo4j_api$new(url = "http://10.168.119.229:7474",user = "neo4j", password = "xiaowei")
  
  ####################################################################################################
  #获取选择pathwayID、weight、timepoint之后，基因之间的关系的数据
  ####################################################################################################
  gene_rel_in_pathway <- reactive({
    #===========================================
    #根据pathwayID设置参数选择
    #===========================================
    pathwayID <- paste0("'", input$pathway_ID, "'")
    
    #===========================================
    #根据时间点设置参数选择
    #===========================================
    days = input$days #通过选择后来获取days参数
    #这里是根据days来设置时间筛选条件
    # if (length(days) == 0){
      # dayOption = " "
      # pdayOption = " "}
    
    # if (length(days) ==1){
      # if (days == "day3"){
        # if (input$unique){
          # dayOption = " and r2.relationship =~ '100' "
        # }else{
          # dayOption = " and r2.relationship =~ '1..' "}
      # }
      
      # if (days == "day5"){
        # if (input$unique){
          # dayOption = " and r2.relationship =~ '010' "
        # }else{
          # dayOption = " and r2.relationship =~ '.1.' "}
      # }
      
      # if (days == "day8"){
        # if (input$unique){
          # dayOption = " and r2.relationship =~ '001' "
        # }else{
          # dayOption = " and r2.relationship =~ '..1' "}
      # }
    # }
    # if (length(days) == 2){
      # if ("day3" %in% days & "day5" %in% days){
        # dayOption = " and r2.relationship =~ '11.' "}
      # if ("day5" %in% days & "day8" %in% days){
        # dayOption = " and r2.relationship =~ '.11' "}
      # if ("day3" %in% days & "day8" %in% days){
        # dayOption = " and r2.relationship =~ '1.1' "}
    # }
    # if (length(days) == 3){
      # if ("day3" %in% days & "day5" %in% days & "day8" %in% days){
        # dayOption = " and r2.relationship =~ '111' "}
    # }
	
	if (length(days) == 0){
      dayOption = " "
      pdayOption = " "}
    
    if (length(days) ==1){
      if (days == "day3"){dayOption = " and r2.relationship =~ '100' "}
      if (days == "day5"){dayOption = " and r2.relationship =~ '010' "}
      if (days == "day8"){dayOption = " and r2.relationship =~ '001' "}
    }
    if (length(days) == 2){
      if ("day3" %in% days & "day5" %in% days){
        dayOption = " and r2.relationship =~ '110' "}
      if ("day5" %in% days & "day8" %in% days){
        dayOption = " and r2.relationship =~ '011' "}
      if ("day3" %in% days & "day8" %in% days){
        dayOption = " and r2.relationship =~ '101' "}
    }
    if (length(days) == 3){
      if ("day3" %in% days & "day5" %in% days & "day8" %in% days){
        dayOption = " and r2.relationship =~ '111' "}
    }
    
    #===========================================
    #设置weight选择功能
    #===========================================
    #-----设置day3weight选择功能----------------------------------
    if (input$day3Weight){
      day3WeightNumber = input$day3WeightNumber
      day3WeightOption <- paste0(" and r2.day3Weight >=", day3WeightNumber)} 
    else{day3WeightOption <- " "}
    
    #-----设置day3weight选择功能----------------------------------
    if (input$day5Weight){
      day5WeightNumber = input$day5WeightNumber
      day5WeightOption <- paste0(" and r2.day5Weight >=", day5WeightNumber)} 
    else{day5WeightOption <- " "}
    
    #-----设置day3weight选择功能----------------------------------
    if (input$day8Weight){
      day8WeightNumber = input$day8WeightNumber
      day8WeightOption <- paste0(" and r2.day8Weight >=", day8WeightNumber)} 
    else{day8WeightOption <- " "}
    
    #===========================================
    #利用cypher语言查询并获取相关数据
    #===========================================
    query = paste0("
                    MATCH (A)-[r1:belong]->(pathway)
                    where pathway.ID = ", pathwayID, 
                   " with collect(distinct id(A)) AS AID
                    MATCH p = (B)-[r2:WGCNA]->(C)
                    where id(B) in AID and id(C) in AID", dayOption, day3WeightOption, day5WeightOption, day8WeightOption,
                   "
                    return p")
    gene_rel_in_pathway <- neo4r::call_neo4j(con, query=query,type = "graph")
    #===========================================
    #利用neo4r包中的unnest_nodes和unnest_relationship解析系欸DNA和relatioship的属性
    #===========================================
    library(dplyr)
    library(purrr)
    
    #解析节点的属性
    #解析节点的属性
    n <- gene_rel_in_pathway$nodes$properties
    n1 <- do.call(rbind, lapply(n, data.frame))
    index1 <- colnames(gene_rel_in_pathway$nodes) %in% c("properties")
    nodes <- cbind(gene_rel_in_pathway$nodes[!index1],n1)
    nodes$label <- "node"
    
    #解析relationship的属性
    p <- gene_rel_in_pathway$relationships$properties
    p1 <- do.call(rbind, lapply(p, data.frame))
    index2 <- colnames(gene_rel_in_pathway$relationships) %in% c("properties")
    relationships <- cbind(gene_rel_in_pathway$relationships[!index2], p1)
    relationships$type <- "WGCNA"
    
    #============================================
    #获取这条通路上的基因之间关系的数据
    #============================================
    list(nodes = nodes, relationships = relationships)
  })
  
  
  ####################################################################################################
  #获取总过有多少个节点和边
  ####################################################################################################
  #总过多少个节点在图中
  output$total_nodes_number <- renderText({
    paste0(nrow(gene_rel_in_pathway()$nodes), " nodes are in this plot." )
  })
  #总共多少条边
  output$total_edges_number <- renderText({
    paste0(nrow(gene_rel_in_pathway()$relationships), " edges are in this plot." )
  })
  
  ####################################################################################################
  #gene_rel_in_pathway数据转换成visNetwork作图的数据
  ####################################################################################################
  gene_rel_in_pathway_plot_data <- reactive({
    nodes <- gene_rel_in_pathway()$nodes
    relationships <- gene_rel_in_pathway()$relationships
    #=====================================================================
    #为nodes设置一个group，根据relationship中relationship的参数来设定
    #=====================================================================
    group <- c()
    for (i in 1:nrow(nodes)){
      if (nodes$id[i] %in% relationships$startNode){A <- unique( relationships$relationship[ relationships$startNode %in% nodes$id[i] ]) } else{A <- NULL} 
      if (nodes$id[i] %in% relationships$endNode){B <- unique( relationships$relationship[ relationships$endNode %in% nodes$id[i] ]) } else{B <- NULL}
      AB <- unique(c(A, B))
      group[i] <- paste0( unique(c(A, B)), collapse = "," )
    }
    nodes$group <- group
    #=====================================================================
    #为relationship设置一个weight列
    #=====================================================================
    relationships$weight <- paste0("day3Weight: ",relationships$day3Weight,
                                   "day5Weight: ",relationships$day5Weight,
                                   "day8Weight: ",relationships$day8Weight)
    #=====================================================================
    #给边增加颜色
    #=====================================================================
    color_rel <- c()
    for (i in 1: nrow(relationships)){
      if (relationships$relationship[i] == '000'){color_rel[i] = 'Brown'} #一天都没有
      if (relationships$relationship[i] == '100'){color_rel[i] = 'red'} #day3 红色
      if (relationships$relationship[i] == '010'){color_rel[i] = 'green'} #day5 绿色
      if (relationships$relationship[i] == '001'){color_rel[i] = 'blue'} #day8 蓝色
      if (relationships$relationship[i] == '110'){color_rel[i] = 'yellow'} #day3, day5  黄色
      if (relationships$relationship[i] == '011'){color_rel[i] = 'Aqua'} #day5, day8 亮蓝色
      if (relationships$relationship[i] == '101'){color_rel[i] = 'pink'} #day3, day8 粉色
      if (relationships$relationship[i] == '111'){color_rel[i] = 'black'} #day3, day5, day8
    }
    relationships$color <- color_rel
    #=====================================================================
    #根据作图需要选择nodes相关的列
    #=====================================================================
    vNodes <- nodes[,c("id", "name","name","group")]
    colnames(vNodes) <- c("id", "label","title","group")
    #=====================================================================
    #根据作图需要选择nodes相关的列
    #=====================================================================
    vRelationship <- relationships[,c("startNode","endNode", "relationship", "weight", "color" )]
    colnames(vRelationship) <- c("from", "to", "label", "title", "color")
    #=====================================================================
    #使线变的平滑
    #=====================================================================
    if (nrow(vRelationship) <20) {vRelationship$smooth = TRUE}  #使线变得平滑，不会是直线，变成一条比较松的线, 前提是边少于30个
    #=====================================================================
    #转换成G来保存  G为visNetwork作图数据
    #=====================================================================
    G <- list(nodes = vNodes, relationships = vRelationship)
    #=====================================================================
    #转换成GI来保存 GI为igraph作图数据
    #=====================================================================
    ##如果大于边10， 就不要label这一列了。
    if (nrow(vRelationship) < 10){
      GI <- list(nodes = vNodes[,c("id","label")], 
                 relationships = vRelationship[,c("from","to", "label")]
      )
    }else{
      GI <- list(nodes = vNodes[,c("id","label")], 
                 relationships = vRelationship[,c("from","to")]
      )
    }
    #=====================================================================
    #返回两组数据
    #=====================================================================
    list(G = G, GI = GI)
    
  })
  #####################################
  #输出选择的通路名称
  ####################################
  output$pathway_plot_Name <- renderText(
    paste0("genes relationships in ",
           pathwayTable$Pathway_Description[pathwayTable$pathway_ID %in% input$pathway_ID], 
           " over time"))
  
  ####################################################################################################
  #visNetwork作图
  ####################################################################################################
  library(visNetwork)
  output$genes_rel_in_pathway_visNetwork <- renderVisNetwork({
    G <- gene_rel_in_pathway_plot_data()$G
    library(visNetwork)
    visNetwork::visNetwork(G$nodes, G$relationships) %>%
      visInteraction(navigationButtons = TRUE) %>% #增加一些控件来对图进行移动啊，放大缩小啊
      visOptions(highlightNearest = list(enabled = TRUE, #highlightNearest:点击一个节点会只显示这个节点所有关系,
                                         hover = FALSE, #hover设定为TRUE,是当鼠标悬停在某个节点时，可以只显示跟这个节点所有关系
                                         algorithm = "hierarchical"), #这个算法， 只查看当前选择节点的关系
                 manipulation = TRUE) %>%  #manipulation编辑按钮，可以增加/删除节点和边  
      #visEdges(color = list( highlight = "red",hover = "red")) %>% #悬停到边的时候这条边变色为红色
      visIgraphLayout()  %>%  #Use igraph layout, use all available layouts in igraph and calculate coordinates
      # visLegend() #设置图例的
      visExport()
    
  })
  ####################################################################################################
  #----增加根据基因名筛选节点-------------------------------------------
  ####################################################################################################
  observe({
    visNetworkProxy("genes_rel_in_pathway_visNetwork") %>%
      visOptions(nodesIdSelection = list(enabled = input$nodesIdSelection,  useLabels = TRUE, main = "Select by gene"),
                 highlightNearest = list(enabled = TRUE, hover = FALSE, algorithm = "hierarchical"), manipulation = TRUE)#根据基因名选择节点
  })
  ####################################################################################################
  #-----增加根据组别筛选节点 -------------------------------------------
  ####################################################################################################
  observe({
    if(input$selectedby){
      visNetworkProxy("genes_rel_in_pathway_visNetwork") %>%
        visOptions(selectedBy = list(variable = 'group', multiple = T, main = "Select by times"),
                   highlightNearest = list(enabled = TRUE, hover = FALSE, algorithm = "hierarchical"), manipulation = TRUE)}#根据时间选择节点
  })
  
  ####################################################################################################
  #igraph 作图
  ####################################################################################################
  output$genes_rel_in_pathway_igraph <- renderPlot({
    GI <- gene_rel_in_pathway_plot_data()$GI
    library(igraph)
    #转换成igraph格式
    ig <- igraph::graph_from_data_frame(
      d = GI$relationships, 
      directed = FALSE, 
      vertices = GI$nodes
    )
    #===========================
    #作图
    #===========================
    #布局
    if (input$Select_igraph_layput){
      if (input$igraph_layout == "circle" ){L <- layout_in_circle(ig)
      }else if (input$igraph_layout == "randomly"){L <- layout_randomly(ig)
      }else if (input$igraph_layout == "sphere" ){L <- layout_on_sphere(ig)
      }else if (input$igraph_layout == "Fruchterman-Reingold"){L <- layout_with_fr(ig)
      }else if (input$igraph_layout == "Kamada-Kawai"){L <- layout_with_kk(ig)
      }else if (input$igraph_layout == "Large_Graph"){L <- layout_with_lgl(ig)
      }else if (input$igraph_layout == "Merging_graph"){L <- layout_components(ig)
      }else if (input$igraph_layout == "appropriate_graph"){L <- layout_nicely(ig)
      }else if (input$igraph_layout == "Davidson-Harel"){L <- layout_with_dh(ig)
      }else if (input$igraph_layout == "DrL_graph"){L <- layout_with_drl(ig)
      }else if (input$igraph_layout == "multidimensional_scaling"){L <- layout_with_mds(ig)
      }else {L <- layout_with_graphopt(ig)}
    }
    else{
      L = layout_randomly(ig)}
    
    #coummunity 和作图
    if (input$Selcet_igraph_community){
      if (input$igraph_community == "edge_betweenness"){ 
        clu <- cluster_edge_betweenness(ig)
        plot(clu, ig, layout = L)}
      if (input$igraph_community == "propagating_labels"){
        clu <- cluster_label_prop(ig) 
        plot(clu, ig, layout = L)}
      if (input$igraph_community =="greedy_optimization_of_modularity"){
        clu <- cluster_fast_greedy(as.undirected(ig))
        plot(clu, as.undirected(ig),layout = L)}
    }
    else{
      plot(ig, layout = L)
    }
    
  }) 
  
  ####################################################################################################
  #修改一下要查看或下载节点/边
  ####################################################################################################
  Look.download_gene_rel_in_pathway <- reactive({
    #数据准备
    nodes <- gene_rel_in_pathway()$nodes
    relationships <- gene_rel_in_pathway()$relationships
    #将relationship中的id转换成基因名
    matchgeneName <- function(x){
      nodes[nodes$id == x,]$name
    }
    relationships$startNode <- unlist(lapply(relationships$startNode,  matchgeneName ))
    relationships$endNode <- unlist(lapply(relationships$endNode, matchgeneName))
    #===================返回结果
    list(nodes = nodes, relationships = relationships)
  })
  ####################################################################################################
  #查看节点/边
  ####################################################################################################
  #-----查看节点--------
  output$Nodes_informations <- renderText({
    #标题Nodes informations
    if(input$See_nodesTable){"Nodes informations"} 
  })
  #表格
  output$nodeTable <- renderTable({
    nodes = Look.download_gene_rel_in_pathway()$nodes
    if(input$See_nodesTable)  #这个按钮放在这，就会运行一次
    {nodes[1:10,]}
  })
  
  #----查看边----------
  output$Edges_informations <- renderText({
    #标题Edges informations
    if(input$See_edgesTable){"Edges informations"} 
  })
  #表格
  output$edgesTable <- renderTable({
    relationship <- Look.download_gene_rel_in_pathway()$relationships
    if(input$See_edgesTable) {relationships[1:10,]}
  })
  
  
  ####################################################################################################
  #下载节点/边
  ####################################################################################################
  #---下载节点信息----
  output$download_nodeTable <- downloadHandler(
    filename = function(){ paste(input$pathway_ID, "_nodes.csv", sep = "")},
    content = function(file){
      write.csv(Look.download_gene_rel_in_pathway()$nodes, file, row.names = FALSE)}
  )
  #---下载边信息---
  output$download_edgeTable <- downloadHandler(
    filename = function(){ paste(input$pathway_ID, "_edges.csv", sep = "")},
    content = function(file){write.csv(Look.download_gene_rel_in_pathway()$relationships, file, row.names = FALSE)}
  )
  
  #################################################################################################
  #================================================================================================
  #################################################################################################
  #===========================================
  #连接neo4j
  #===========================================
  library(neo4r)
  con <- neo4j_api$new(url = "http://10.168.119.229:7474",user = "neo4j", password = "xiaowei")
  
  ####################################################################################################
  #基因neighbor之间的关系的数据
  ####################################################################################################
  
  gene_rel_in_nerghbor <- reactive({
    #===========================================
    #根据gene_SYMBOL设置参数选择
    #===========================================
    gene_SYMBOL <- paste0(" A.name = '", input$gene_SYMBOL, "'")
    
    #===========================================
    #根据时间点设置参数选择
    #===========================================
    days = input$gdays #通过选择后来获取gdays参数
    #这里是根据days来设置时间筛选条件
    # if (length(days) == 0){
      # dayOption = " "}
    
    # if (length(days) ==1){
      # if (days == "day3"){
        # if (input$gunique){
          # dayOption = " and r.relationship =~ '100' "
        # }else{
          # dayOption = " and r.relationship =~ '1..' "}
      # }
      
      # if (days == "day5"){
        # if (input$gunique){
          # dayOption = " and r.relationship =~ '010' "
        # }else{
          # dayOption = " and r.relationship =~ '.1.' "}
      # }
      
      # if (days == "day8"){
        # if (input$gunique){
          # dayOption = " and r.relationship =~ '001' "
        # }else{
          # dayOption = " and r.relationship =~ '..1' "}
      # }
    # }
    # if (length(days) == 2){
      # if ("day3" %in% days & "day5" %in% days){
        # dayOption = " and r.relationship =~ '11.' "}
      # if ("day5" %in% days & "day8" %in% days){
        # dayOption = " and r.relationship =~ '.11' "}
      # if ("day3" %in% days & "day8" %in% days){
        # dayOption = " and r.relationship =~ '1.1' "}
    # }
    # if (length(days) == 3){
      # if ("day3" %in% days & "day5" %in% days & "day8" %in% days){
        # dayOption = " and r.relationship =~ '111' "}
    # }
	
	    if (length(days) == 0){
      dayOption = " "}
    
    if (length(days) ==1){
      if (days == "day3"){dayOption = " and r.relationship =~ '100' "}
      if (days == "day5"){dayOption = " and r.relationship =~ '010' "}
      if (days == "day8"){dayOption = " and r.relationship =~ '001' "}
    }
    if (length(days) == 2){
      if ("day3" %in% days & "day5" %in% days){
        dayOption = " and r.relationship =~ '110' "}
      if ("day5" %in% days & "day8" %in% days){
        dayOption = " and r.relationship =~ '011' "}
      if ("day3" %in% days & "day8" %in% days){
        dayOption = " and r.relationship =~ '101' "}
    }
    if (length(days) == 3){
      if ("day3" %in% days & "day5" %in% days & "day8" %in% days){
        dayOption = " and r.relationship =~ '111' "}
    }
    #===========================================
    #设置weight选择功能
    #===========================================
    #-----设置day3weight选择功能----------------------------------
    if (input$gday3Weight){
      day3WeightNumber = input$gday3WeightNumber
      day3WeightOption <- paste0(" and r.day3Weight >=", day3WeightNumber)} 
    else{day3WeightOption <- " "}
    
    #-----设置day3weight选择功能----------------------------------
    if (input$gday5Weight){
      day5WeightNumber = input$gday5WeightNumber
      day5WeightOption <- paste0(" and r.day5Weight >=", day5WeightNumber)} 
    else{day5WeightOption <- " "}
    
    #-----设置day3weight选择功能----------------------------------
    if (input$gday8Weight){
      day8WeightNumber = input$gday8WeightNumber
      day8WeightOption <- paste0(" and r.day8Weight >=", day8WeightNumber)} 
    else{day8WeightOption <- " "}
    #===========================================
    #利用cypher语言查询并获取相关数据
    #===========================================
    query = paste0("
            match p = (A:node)-[r:WGCNA]-(B)
            where ",gene_SYMBOL, dayOption, day3WeightOption, day5WeightOption, day8WeightOption,"
            return p
            ")
    gene_nb <- neo4r::call_neo4j(con, query=query,type = "graph")
    #===========================================
    #解析node和relatioship的属性
    #===========================================
    #解析节点的属性
    n <- gene_nb$nodes$properties
    n1 <- do.call(rbind, lapply(n, data.frame))
    index1 <- colnames(gene_nb$nodes) %in% c("properties")
    nodes <- cbind(gene_nb$nodes[!index1],n1)
    nodes$label <- "node"
    
    #解析relationship的属性
    p <- gene_nb$relationships$properties
    p1 <- do.call(rbind, lapply(p, data.frame))
    index2 <- colnames(gene_nb$relationships) %in% c("properties")
    relationships <- cbind(gene_nb$relationships[!index2], p1)
    relationships$type <- "WGCNA"
    
    #============================================
    #获取基因及其neighbor之间关系的数据
    #============================================
    list(nodes = nodes, relationships = relationships)
    
  })
  
  ####################################################################################################
  #gene-pathway relationship
  ####################################################################################################
  gene_pathway_relationship <- reactive({
    library(RNeo4j)
    graph = startGraph("http://10.168.119.229:7474/db/data/", username="neo4j", password="xiaowei")
    query = paste0("
            match p = (A:node{name:'",input$gene_SYMBOL,"' })-[r]-(pathway:pathway)
            return pathway.Description AS pathway_description, pathway.ID AS pathway_ID
            ")
    gp <- cypher(graph, query)
    list(gp = gp)
  })
  
  ####################################################################################################
  #说明这个基因属于哪几条通路
  ####################################################################################################
  output$gene_belong_to_pathway <- renderText({
    pathway = paste0(gene_pathway_relationship()$gp$pathway_description, collapse = ", ")
    paste0(input$gene_SYMBOL, " belong to pathway: ",pathway, ".")
  })
  
  ####################################################################################################
  #获取总共有多少个节点和边
  ####################################################################################################
  #总共多少个节点在图中
  output$gtotal_nodes_number <- renderText({
    paste0( nrow(gene_rel_in_nerghbor()$nodes), " nodes are in this plot." )
  })
  #总共多少条边
  output$gtotal_edges_number <- renderText({
    paste0( nrow(gene_rel_in_nerghbor()$relationships), " edges are in this plot." )
  })
  ####################################################################################################
  #gene_rel_in_nerghbor数据转换成visNetwork作图的数据
  ####################################################################################################
  gene_rel_in_nerghbor_plot_data <- reactive({
    nodes <- gene_rel_in_nerghbor()$nodes
    relationships <- gene_rel_in_nerghbor()$relationships
    #=====================================================================
    #为nodes设置一个group，根据relationship中relationship的参数来设定
    #=====================================================================
    group <- c()
    for (i in 1:nrow(nodes)){
      if (nodes$id[i] %in% relationships$startNode){A <- unique( relationships$relationship[ relationships$startNode %in% nodes$id[i] ]) } else{A <- NULL} 
      if (nodes$id[i] %in% relationships$endNode){B <- unique( relationships$relationship[ relationships$endNode %in% nodes$id[i] ]) } else{B <- NULL}
      AB <- unique(c(A, B))
      group[i] <- paste0( unique(c(A, B)), collapse = "," )
    }
    nodes$group <- group
    #=====================================================================
    #为relationship设置一个weight列
    #=====================================================================
    relationships$weight <- paste0("day3Weight: ",relationships$day3Weight,
                                   "day5Weight: ",relationships$day5Weight,
                                   "day8Weight: ",relationships$day8Weight)
    #=====================================================================
    #给边增加颜色
    #=====================================================================
    color_rel <- c()
    for (i in 1: nrow(relationships)){
      if (relationships$relationship[i] == '000'){color_rel[i] = 'Brown'} #一天都没有
      if (relationships$relationship[i] == '100'){color_rel[i] = 'red'} #day3 红色
      if (relationships$relationship[i] == '010'){color_rel[i] = 'green'} #day5 绿色
      if (relationships$relationship[i] == '001'){color_rel[i] = 'blue'} #day8 蓝色
      if (relationships$relationship[i] == '110'){color_rel[i] = 'yellow'} #day3, day5  黄色
      if (relationships$relationship[i] == '011'){color_rel[i] = 'Aqua'} #day5, day8 亮蓝色
      if (relationships$relationship[i] == '101'){color_rel[i] = 'pink'} #day3, day8 粉色
      if (relationships$relationship[i] == '111'){color_rel[i] = 'black'} #day3, day5, day8
    }
    relationships$color <- color_rel
    #=====================================================================
    #根据作图需要选择nodes相关的列
    #=====================================================================
    vNodes <- nodes[,c("id", "name","name","group")]
    colnames(vNodes) <- c("id", "label","title","group")
    #=====================================================================
    #根据作图需要选择relationship相关的列
    #=====================================================================
    vRelationship <- relationships[,c("startNode","endNode", "relationship", "weight", "color" )]
    colnames(vRelationship) <- c("from", "to", "label", "title", "color")
    #=====================================================================
    #使线变的平滑
    #=====================================================================
    if (nrow(relationships) <20) {relationships$smooth = TRUE}  #使线变得平滑，不会是直线，变成一条比较松的线, 前提是边少于30个
    #=====================================================================
    #转换成G来保存  G为visNetwork作图数据
    #=====================================================================
    G <- list(nodes = vNodes, relationships = vRelationship)
    #=====================================================================
    #转换成GI来保存 GI为igraph作图数据
    #=====================================================================
    ##如果大于边10， 就不要label这一列了。
    if (nrow(vRelationship) < 10){
      GI <- list(nodes = vNodes[,c("id","label")], 
                 relationships = vRelationship[,c("from","to", "label")]
      )
    }else{
      GI <- list(nodes = vNodes[,c("id","label")], 
                 relationships = vRelationship[,c("from","to")]
      )
    }
    #=====================================================================
    #返回两组数据
    #=====================================================================
    list(G = G, GI = GI)
  })
  ####################################################################################################
  #输出作图的标题
  ####################################################################################################
  output$gene_nerghbor_plot_Name <- renderText(
    paste0(input$gene_SYMBOL ," neighborhoods relationships"))
  
  
  
  ####################################################################################################
  #visNetwork作图
  ####################################################################################################
  
  output$genes_rel_in_neighbor_visNetwork <- renderVisNetwork({
    G <- gene_rel_in_nerghbor_plot_data()$G
    library(visNetwork)
    visNetwork::visNetwork(G$nodes, G$relationships) %>%
      visInteraction(navigationButtons = TRUE) %>% #增加一些控件来对图进行移动啊，放大缩小啊
      visOptions(highlightNearest = list(enabled = TRUE, #highlightNearest:点击一个节点会只显示这个节点所有关系,
                                         hover = FALSE, #hover设定为TRUE,是当鼠标悬停在某个节点时，可以只显示跟这个节点所有关系
                                         algorithm = "hierarchical"), #这个算法， 只查看当前选择节点的关系
                 manipulation = TRUE) %>%  #manipulation编辑按钮，可以增加/删除节点和边  
      #visEdges(color = list( highlight = "red",hover = "red")) %>% #悬停到边的时候这条边变色为红色
      visIgraphLayout()  %>%  #Use igraph layout, use all available layouts in igraph and calculate coordinates
      # visLegend() #设置图例的
      visExport()
  })
  
  ####################################################################################################
  #----增加根据基因名筛选节点-------------------------------------------
  ####################################################################################################
  observe({
    visNetworkProxy("genes_rel_in_neighbor_visNetwork") %>%
      visOptions(nodesIdSelection = list(enabled = input$gnodesIdSelection,  useLabels = TRUE, main = "Select by gene"),
                 highlightNearest = list(enabled = TRUE, hover = FALSE, algorithm = "hierarchical"), manipulation = TRUE)#根据基因名选择节点
  })
  ####################################################################################################
  #-----增加根据组别筛选节点 -------------------------------------------
  ####################################################################################################
  observe({
    if(input$gselectedby){
      visNetworkProxy("genes_rel_in_neighbor_visNetwork") %>%
        visOptions(selectedBy = list(variable = 'group', multiple = T, main = "Select by times"),
                   highlightNearest = list(enabled = TRUE, hover = FALSE, algorithm = "hierarchical"), manipulation = TRUE)}#根据时间选择节点
  })
  
  ####################################################################################################
  #igraph 作图
  ####################################################################################################
  output$genes_rel_in_neighbor_igraph <- renderPlot({
    GI <- gene_rel_in_nerghbor_plot_data()$GI
    library(igraph)
    #转换成igraph格式
    ig <- igraph::graph_from_data_frame(
      d = GI$relationships, 
      directed = FALSE, 
      vertices = GI$nodes
    )
    #===========================
    #作图
    #===========================
    #布局
    if (input$gSelect_igraph_layput){
      if (input$gigraph_layout == "circle" ){L <- layout_in_circle(ig)
      }else if (input$gigraph_layout == "randomly"){L <- layout_randomly(ig)
      }else if (input$gigraph_layout == "sphere" ){L <- layout_on_sphere(ig)
      }else if (input$gigraph_layout == "Fruchterman-Reingold"){L <- layout_with_fr(ig)
      }else if (input$gigraph_layout == "Kamada-Kawai"){L <- layout_with_kk(ig)
      }else if (input$gigraph_layout == "Large_Graph"){L <- layout_with_lgl(ig)
      }else if (input$gigraph_layout == "Merging_graph"){L <- layout_components(ig)
      }else if (input$gigraph_layout == "appropriate_graph"){L <- layout_nicely(ig)
      }else if (input$gigraph_layout == "Davidson-Harel"){L <- layout_with_dh(ig)
      }else if (input$gigraph_layout == "DrL_graph"){L <- layout_with_drl(ig)
      }else if (input$gigraph_layout == "multidimensional_scaling"){L <- layout_with_mds(ig)
      }else {L <- layout_with_graphopt(ig)}
    }
    else{
      L = layout_randomly(ig)}
    #节点大小
    NodesSize = input$gigraph_nodes_Size
    #节点的标签（基因名） 大小
    FontSize = input$gigraph_nodes_label_font_Size
    #coummunity 和作图
    if (input$gSelcet_igraph_community){
      if (input$gigraph_community == "edge_betweenness"){ 
        clu <- cluster_edge_betweenness(ig)
        plot(clu, ig, layout = L ,vertex.size = NodesSize, vertex.label.cex = FontSize)}
      if (input$gigraph_community == "propagating_labels"){
        clu <- cluster_label_prop(ig) 
        plot(clu, ig, layout = L ,vertex.size = NodesSize, vertex.label.cex = FontSize)}
      if (input$gigraph_community =="greedy_optimization_of_modularity"){
        clu <- cluster_fast_greedy(as.undirected(ig))
        plot(clu, as.undirected(ig),layout = L,vertex.size = NodesSize, vertex.label.cex = FontSize)}
    }
    else{
      plot(ig, layout = L, vertex.size = NodesSize, vertex.label.cex = FontSize)
    }
    
  }) 
  
  ####################################################################################################
  #修改一下要查看或下载节点/边
  ####################################################################################################
  Look.download_gene_rel_in_nerghbor <- reactive({
    #数据准备
    nodes <- gene_rel_in_nerghbor()$nodes
    relationships <- gene_rel_in_nerghbor()$relationships
    #将relationship中的id转换成基因名
    matchgeneName <- function(x){
      nodes[nodes$id == x,]$name
    }
    relationships$startNode <- unlist(lapply(relationships$startNode,  matchgeneName ))
    relationships$endNode <- unlist(lapply(relationships$endNode, matchgeneName))
    #===================返回结果
    list(nodes = nodes, relationships = relationships)
  })
  
  ####################################################################################################
  #查看节点/边
  ####################################################################################################
  #-----查看节点--------
  output$gNodes_informations <- renderText({
    #标题Nodes informations
    if(input$gSee_nodesTable){"Nodes informations"} 
  })
  #表格
  output$gnodeTable <- renderTable({
    nodes = Look.download_gene_rel_in_nerghbor()$nodes
    if(input$gSee_nodesTable)  #这个按钮放在这，就会运行一次
    {nodes[1:10,]}
  })
  
  #----查看边----------
  output$gEdges_informations <- renderText({
    #标题Edges informations
    if(input$gSee_edgesTable){"Edges informations"} 
  })
  #表格
  output$gedgesTable <- renderTable({
    relationships <- Look.download_gene_rel_in_nerghbor()$relationships
    if(input$gSee_edgesTable) {relationships[1:10,]}
  })
  
  ####################################################################################################
  #下载节点/边
  ####################################################################################################
  #---下载节点信息----
  output$gdownload_nodeTable <- downloadHandler(
    filename = function(){ paste(input$ggene_SYMBOL, "_nodes.csv", sep = "")},
    content = function(file){
      write.csv(Look.download_gene_rel_in_nerghbor()$nodes, file, row.names = FALSE)}
  )
  #---下载边信息---
  output$gdownload_edgeTable <- downloadHandler(
    filename = function(){ paste(input$ggene_SYMBOL, "_edges.csv", sep = "")},
    content = function(file){write.csv(Look.download_gene_rel_in_nerghbor()$relationships, file, row.names = FALSE)}
  )

}

shinyApp(ui = ui, server = server)