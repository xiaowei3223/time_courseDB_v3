################################################################################################
#ui
################################################################################################

#================Input=======================================================================
#选择pathway
#根据pathwayID进行选择
selcet_pathway_ID <- selectInput("pathway_ID", "Please select one of pathway ID:",
                                 pathwayTable$pathway_ID)
#设置时间选项，多选
select_time_point <-checkboxGroupInput("days", "Please choose days(timepoint):",
                     c("day3" = "day3",
                       "day5" = "day5",
                       "day8" = "day8"))

#关系是否只存在某一个时间点
select_unique_word <- strong("If you choose one timepoint, whether you ask relationships only at one timepoint and not exist at other timepoints.")
select_unique <- checkboxInput("unique", "unique", FALSE)


#设置关系中day3weight的选择
select_day3Weight_orNot <- checkboxInput("day3Weight"," Weight at day3 ?", FALSE)
#day3weight滑动条
select_day3Weight <- sliderInput("day3WeightNumber", "Number of WGCNA weight at day3:",
            min = 0, max = 0.5, value = 0)

#设置关系中day5weight的选择
select_day5Weight_orNot <- checkboxInput("day5Weight"," Weight at day5 ?", FALSE)
#day5weight滑动条
select_day5Weight <- sliderInput("day5WeightNumber", "Number of WGCNA weight at day5:",
            min = 0, max = 0.6, value = 0)

#设置关系中day8weight的选择
select_day8Weight_orNot <- checkboxInput("day8Weight"," Weight at day8 ?", FALSE)
#day8weight滑动条
select_day8Weight <- sliderInput("day8WeightNumber", "Number of WGCNA weight at day8:",
            min = 0, max = 0.55, value = 0)


#============OutPut=============================================================================
#显示选择的通路名称
genes_relationship_in_pathway_graph_name <- h3(textOutput("pathway_plot_Name") )

#总共多少个节点
nodes_numbers_in_graph <- strong(textOutput("total_nodes_number"))
#总共多少条边
edge_numvers_in_graph <- strong(textOutput("total_edges_number"))
#-----------visNetwork--------------------
#是否增加根据基因来选择节点
whether_chose_nodes_base_on_geneName <- checkboxInput("nodesIdSelection", "nodes Selection", FALSE)
#是否根据分组筛选节点
whether_chose_nodes_base_on_group <- checkboxInput("selectedby", "Groups Selection", FALSE)
#设置输出visNetwork结果
genes_relationship_in_pathway_visNetwork_plot <- visNetworkOutput("genes_rel_in_pathway_visNetwork",  height = "600px")

#-----------igraph-----------------
#设置是否需要指定一个图片布局方式
set_layout_in_igraph <- checkboxInput("Select_igraph_layput", "Select igraph layout", FALSE)
#选择布局的方式
select_layout_in_igraph <- selectInput("igraph_layout", label = NULL,
                                        c("randomly" = "randomly",
                                          "Large Graph" = "Large_Graph",
                                          "circle" = "circle",
                                          "sphere" = "sphere",
                                          "Fruchterman-Reingold" = "Fruchterman-Reingold",
                                          "Kamada-Kawai" = "Kamada-Kawai",
                                          "Merging graph" = "Merging_graph",
                                          "appropriate graph" = "appropriate_graph",
                                          "Davidson-Harel" = "Davidson-Harel",
                                          "DrL graph" = "DrL_graph",
                                          "multidimensional scaling" = "multidimensional_scaling",
                                          "graphopt" = "graphopt" ))

#选择是否需要做聚类分区
set_community_in_igraph <- checkboxInput("Selcet_igraph_community", "Selcet igraph community", FALSE)
#选择一种聚类分区的方法
select_community_in_igraph <- selectInput("igraph_community", label = NULL,
            c("edge betweenness" = "edge_betweenness" ,
              "propagating labels" = "propagating_labels" ,
              "greedy optimization of modularity" = "greedy_optimization_of_modularity"))
#输出igraph图画
genes_relationship_in_pathway_igraph_plot <- plotOutput("genes_rel_in_pathway_igraph", height = "600px")
#=================================================================================================================
#设置查看节点和边数据的按钮
button_look_nodes <- actionButton("See_nodesTable", "Nodes informations")
button_look_edges <- actionButton("See_edgesTable", "Edges informations")

#设置数据下载按钮
button_download_nodes <- downloadButton("download_nodeTable", "download nodes information")
button_download_edges <- downloadButton("download_edgeTable", "download edges information")

#设置数据输出
nodes_information_title <- h2(textOutput("Nodes_informations")) #查看节点数据标题
nodes_information <- tableOutput('nodeTable')  #查看节点数据
edge_information_title <- h2(textOutput("Edges_informations")) #查看边数据标题
edge_information <- tableOutput("edgesTable") #查看边数据

##################################################################################################################

genes_relationships_in_pathway_UI  <- fluidPage(
  column(12,
         sidebarPanel(
           selcet_pathway_ID, #根据pathwayID进行选择 #选择pathway
		   hr(), #画一条线
           select_time_point, #设置时间选项，多选
           #select_unique_word, #文字：关系是否只存在某一个时间点
           #select_unique, #关系是否只存在某一个时间点
           hr(), #画一条线
           select_day3Weight_orNot, #设置关系中day3weight的选择
           select_day3Weight,#day3weight滑动条
           hr(), #画一条线
           select_day5Weight_orNot,#设置关系中day5weight的选择
           select_day5Weight,#day5weight滑动条
           hr(), #画一条线
           select_day8Weight_orNot,#设置关系中day8weight的选择
           select_day8Weight#day8weight滑动条
           ),
         #===================================================================
         mainPanel(
           genes_relationship_in_pathway_graph_name,#显示选择的通路名称
           hr(), #画一条线
           column(6, nodes_numbers_in_graph), #总共多少个节点
           column(6, edge_numvers_in_graph), #总共多少条边
           tabsetPanel(type = "tabs",
                       tabPanel("visNetwork",
                                column(6, whether_chose_nodes_base_on_geneName), #是否增加根据基因来选择节点
                                column(6, whether_chose_nodes_base_on_group), #是否根据分组筛选节点
                                hr(), #画一条线
                                genes_relationship_in_pathway_visNetwork_plot #设置输出visNetwork结果
                                ),
                       tabPanel("igraph",
                                column(6, 
                                       set_layout_in_igraph, #设置是否需要指定一个图片布局方式
                                       select_layout_in_igraph,#选择布局的方式
                                       ),
                                column(6,
                                       set_community_in_igraph,#选择是否需要做聚类分区
                                       select_community_in_igraph,#选择一种聚类分区的方法
                                      ),
                                hr(), #画一条线
                                genes_relationship_in_pathway_igraph_plot#输出igraph图画
                                )
                       )
           )
         ),
  #=============================================================================================
  hr(),
  button_look_nodes,  #设置查看节点数据的按钮
  button_look_edges,  #设置查看边数据的按钮
  button_download_nodes, #设置node数据下载按钮
  button_download_edges, #设置edge数据下载按钮
  column(12,
         nodes_information_title,#查看节点数据标题
         nodes_information,  #查看节点数据
         edge_information_title, #查看边数据标题
         edge_information#查看边数据
         )
)












