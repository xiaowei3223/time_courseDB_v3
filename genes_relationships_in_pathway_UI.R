genes_relationships_in_pathway_UI <-  fluidPage(
  column(12,
         sidebarPanel(
           #选择pathway
           #根据pathwayID进行选择
           selectInput("pathway_ID", "Please select one of pathway ID:",
                       pathwayTable$pathway_ID),
           
           #设置时间选项，多选
           checkboxGroupInput("days", "Please choose days(timepoint):",
                              c("3 days" = "day3",
                                "5 days" = "day5",
                                "8 days" = "day8")),
           strong("If you choose one timepoint, whether you ask relationships only at one timepoint and not exist at other timepoints."),
           checkboxInput("unique", "unique", FALSE),
           
           hr(), #画一条线
           #设置关系中day3weight的选择
           checkboxInput("day3Weight"," Weight in 3 days ?", FALSE),
           #day3weight滑动条
           sliderInput("day3WeightNumber", "Number of WGCNA weight in 3 days:",
                       min = 0, max = 0.5, value = 0),
           
           hr(), #画一条线
           #设置关系中day5weight的选择
           checkboxInput("day5Weight"," Weight in 5 days ?", FALSE),
           #day5weight滑动条
           sliderInput("day5WeightNumber", "Number of WGCNA weight in 5 days:",
                       min = 0, max = 0.6, value = 0),
           
           hr(), #画一条线
           #设置关系中day8weight的选择
           checkboxInput("day8Weight"," Weight in 8 days ?", FALSE),
           #day8weight滑动条
           sliderInput("day8WeightNumber", "Number of WGCNA weight in 8 days:",
                       min = 0, max = 0.55, value = 0)
           
           
         ),
         #===================================================================
         mainPanel(
           #显示选择的通路名称
           h3(textOutput("pathway_plot_Name") ),
           hr(), #画一条线
           #总共有多少个节点和多少条边
           column(6, strong(textOutput("total_nodes_number"))),
           column(6, strong(textOutput("total_edges_number"))),
           
           #----------------------------------------------
           tabsetPanel(type = "tabs",
                       tabPanel("visNetwork",
                                
                                #是否增加根据基因来选择节点
                                column(6, checkboxInput("nodesIdSelection", "nodes Selection", FALSE)),
                                
                                #是否根据分组筛选节点
                                column(6,checkboxInput("selectedby", "Groups Selection", FALSE)),
                                hr(), #画一条线
                                #设置输出visNetwork结果
                                visNetworkOutput("genes_rel_in_pathway_visNetwork",  height = "600px")
                       ),
                       tabPanel("igraph",
                                
                                
                                column(6, 
                                       #设置是否需要指定一个图片布局方式
                                       checkboxInput("Select_igraph_layput", "Select igraph layout", FALSE),
                                       #选择布局的方式
                                       selectInput("igraph_layout", label = NULL,
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
                                ),
                                
                                column(6,
                                       #选择是否需要做聚类分区
                                       checkboxInput("Selcet_igraph_community", "Selcet igraph community", FALSE),
                                       #选择一种聚类分区的方法
                                       selectInput("igraph_community", label = NULL,
                                                   c("edge betweenness" = "edge_betweenness" ,
                                                     "propagating labels" = "propagating_labels" ,
                                                     "greedy optimization of modularity" = "greedy_optimization_of_modularity"))
                                ),
                                
                                hr(), #画一条线
                                #输出igraph图画
                                plotOutput("genes_rel_in_pathway_igraph", height = "600px")
                       )
           ),
           #===========================================================================
           hr(),
           column(6, 
                  #设置查看节点和边数据的按钮
                  actionButton("See_nodesTable", "Nodes informations"),
                  actionButton("See_edgesTable", "Edges informations")
           ),
           column(6,
                  #设置数据下载按钮
                  downloadButton("download_nodeTable", "download nodes information"),
                  downloadButton("download_edgeTable", "download edges information")
           )
         )
  ),
  #=============================================================================================
  column(12,
         #设置数据输出
         h2(textOutput("Nodes_informations")),
         tableOutput('nodeTable'),
         
         h2(textOutput("Edges_informations")),
         tableOutput("edgesTable")
  ),
  
  
  
)