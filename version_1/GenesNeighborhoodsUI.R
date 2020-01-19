fluidPage(
  column(12,
         #==================================================================
         sidebarPanel(
           #----------------------------------------------------------------
           #根据gene_SYMBOL进行选择
           selectInput("gene_SYMBOL", "Select one of gene SYMBOL:",
                       genesTable$gene_SYMBOL),
           #----------------------------------------------------------------
		   hr(), #画一条线
           #设置时间选项，多选
           checkboxGroupInput("gdays", "Please choose days(timepoint):",
                              c("day3" = "day3",
                                "day5" = "day5",
                                "day8" = "day8"),
                              selected = "day3"),
           #strong("If you choose one timepoint, whether you ask relationships only at one timepoint and not exist at other timepoints."),
           #checkboxInput("gunique", "unique", FALSE),
           #--------------------------------------------------------------------------
           hr(), #画一条线
           #设置关系中day3weight的选择
           checkboxInput("gday3Weight"," Weight in at day3 ?", FALSE),
           #day3weight滑动条
           sliderInput("gday3WeightNumber", "Number of WGCNA weight at day3:",
                       min = 0, max = 0.5, value = 0),
           
           hr(), #画一条线
           #设置关系中day5weight的选择
           checkboxInput("gday5Weight"," Weight in day5 ?", FALSE),
           #day5weight滑动条
           sliderInput("gday5WeightNumber", "Number of WGCNA weight at day5:",
                       min = 0, max = 0.6, value = 0),
           
           hr(), #画一条线
           #设置关系中day8weight的选择
           checkboxInput("gday8Weight"," Weight in 8 days ?", FALSE),
           #day8weight滑动条
           sliderInput("gday8WeightNumber", "Number of WGCNA weight at day8:",
                       min = 0, max = 0.55, value = 0),
         ),
         #=====================================================================
         mainPanel(
           #显示选择的基因名称
           h3(textOutput("gene_nerghbor_plot_Name") ),
           hr(), #画一条线
           #---------------------------------------------------
           #总共有多少个节点和多少条边
           column(6, strong(textOutput("gtotal_nodes_number"))),
           column(6, strong(textOutput("gtotal_edges_number"))),
           tabsetPanel(type = "tabs",
                       tabPanel("visNetwork",
                                #-------------------------------------------------------------------
                                #是否增加根据基因来选择节点
                                column(6, checkboxInput("gnodesIdSelection", "nodes Selection", FALSE)),
                                
                                #是否根据分组筛选节点
                                column(6,checkboxInput("gselectedby", "Groups Selection", FALSE)),
                                hr(), #画一条线
                                #设置输出visNetwork结果
                                visNetworkOutput("genes_rel_in_neighbor_visNetwork",  height = "600px"),
                       ),
                       tabPanel("igraph",
                                #--------------------------------------------------------------------
                                column(6, 
                                       #设置是否需要指定一个图片布局方式
                                       checkboxInput("gSelect_igraph_layput", "Select igraph layout", FALSE),
                                       #选择布局的方式
                                       selectInput("gigraph_layout", label = NULL,
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
                                                     "graphopt" = "graphopt" )),
                                       sliderInput("gigraph_nodes_Size", "nodes size",
                                                   min = 0, max = 10, value = 2)
                                ),
                                
                                column(6,
                                       #选择是否需要做聚类分区
                                       checkboxInput("gSelcet_igraph_community", "Selcet igraph community", FALSE),
                                       #选择一种聚类分区的方法
                                       selectInput("gigraph_community", label = NULL,
                                                   c("edge betweenness" = "edge_betweenness" ,
                                                     "propagating labels" = "propagating_labels" ,
                                                     "greedy optimization of modularity" = "greedy_optimization_of_modularity")),
                                       sliderInput("gigraph_nodes_label_font_Size", "nodes labels font size",
                                                   min = 0, max = 1, value = 0.2)
                                ),
                                plotOutput("genes_rel_in_neighbor_igraph", height = "600px")
                       )
           )
         ),
         #=====================================================================
         column(12,
                strong(textOutput("gene_belong_to_pathway")),
                
                #-----------------------------------------------------------
                hr(),
                #设置查看节点和边数据的按钮
                actionButton("gSee_nodesTable", "Nodes informations"),
                actionButton("gSee_edgesTable", "Edges informations"),
                #设置数据下载按钮
                downloadButton("gdownload_nodeTable", "download nodes information"),
                downloadButton("gdownload_edgeTable", "download edges information"),
                #-----------------------------------------------------------
                #设置数据输出
                h2(textOutput("gNodes_informations")),
                tableOutput('gnodeTable'),
                
                h2(textOutput("gEdges_informations")),
                tableOutput("gedgesTable")
         )
  )
)