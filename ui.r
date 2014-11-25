library(markdown)
shinyUI(navbarPage("Health Tweet!",
                   tabPanel("Graph Output",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("selection", "Choose a dataset:" ,choices = datas),
                                #actionButton("update", "Change"),
                                hr(),
                                helpText("This is data of tweets on some infectious diseases. Select the disease on the drop down and see what people said on 
                                         social twiiter --> For more click on the About tab under More")
                                ),#end sidebar panel
                              mainPanel(
                                
                                     div(class="span6" , "On the Tweet", plotOutput('plot', width="auto")),
                                      div(class="span6", "Word cloud", plotOutput("wordPlot",width="auto"))
                              
                              )#end main panel
                            )#end sidebar layout
                   ),#tabpanel
               
                   navbarMenu("More",
                              
                              tabPanel("Table",
                                       dataTableOutput("table")
                              ), 
                              
                              tabPanel("About",
                                       fluidRow(
                                         column(6,includeMarkdown("about.md")
                                         ))
                              )#end of tabpanel
                   )#end of navbar menu
))