
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(devtools)
library(woe)
library(Hmisc)
library(data.table)
library(car)
library(sqldf)
library(ROCR)
library(ineq)
library(Hmisc)
library(pryr)
library(scales)
library(shinythemes)
library(plotly)
library(berryFunctions)
library(rhandsontable)

if (interactive()) {
# Define UI for application that draws a histogram
ui =dashboardPage(title = "LogisticModelDevelopment",
              skin = "blue",
              dashboardHeader(title = div(img(src="Title.jpg",height=50,width=50,align = "left"),
                                          "K2 ANALYTICS"),

                              titleWidth = 250),
              dashboardSidebar(

                width = 250,
                sidebarMenu(id = "MenuTabs",
                            menuItem("Home", tabName = "Home", icon = icon("home")),
                            menuItem("Descriptive Analysis", tabName = "DescriptiveAnalysis", icon = icon("file")),
                            menuItem("Information Value", tabName = "InformationValue", icon = icon("table")),
                            menuItem("P Value", tabName = "PValue", icon = icon("table")),
                            menuItem("Visualization", tabName = "VisualizationPlot", icon = icon("area-chart")),
                            menuItem("Cross Table", tabName = "CrossTable", icon = icon("table")),
                            menuItem("Variable Clustering", tabName = "VariableClustering", icon = icon("line-chart")),
                            menuItem("Variable Updation/Creation", tabName = "VariableUpdation", icon = icon("edit")),
                            menuItem("Development Model", tabName = "DevelopmentModel", icon = icon("connectdevelop")),
                            menuItem("Rank Ordering(Dev)", tabName = "RankOrderingDev", icon = icon("table")),
                            menuItem("All Measures(Dev)", tabName = "AllMeasuresDev", icon = icon("gg")),
                            menuItem("Validation Model", tabName = "ValidationModel", icon = icon("clone")),
                            menuItem("Rank Ordering(Val)", tabName = "RankOrderingVal", icon = icon("table")),
                            menuItem("Model Measures(Val)", tabName = "ModelMeasuresVal", icon = icon("gg")),
                            menuItem("Validation Model on Holdout", tabName = "ValidationModelonHoldout", icon = icon("table")),
                            menuItem("Model Measures(Holdout)", tabName = "ModelMeasuresHoldout", icon = icon("gg")),
                            menuItem("Save Objects", tabName = "SaveObjects", icon = icon("cloud-download"))
                )),

              dashboardBody(
                tabItems(
                  # ***************************Home***************************
                  tabItem(tabName = "Home",
                          {
                            fixedRow(
                              column(12,
                                     align = "left",
                                     h1("Interactive Logistic Model Development : "),
                                     h2("An application for developing Logistic Model
                                        thru Interactive Visualizations"),
                                     HTML("<div style='height: 25px;'>"),
                                     HTML("</div>"),
                                     h3("About"),
                                     h4("Logistic Model is one of the key modeling technique in data modeling.
                                        Logistic regression is among the most popular models for predicting binary targets.
                                        Model Creation can sometimes be time consuming due to reasons like huge number of variables
                                        in the data set, writing code for each plot or group of plots.
                                        In order to eliminate these difficulties and reduce the time here is a simple tool which provides
                                        different interactive visualizations used in EDA, Modeling and Model testing just by some click of
                                        buttons."),
                                     HTML("<div style='height: 5px;'>"),
                                     HTML("</div>"),
                                     h3("Manual"),
                                     h4("LogisticModelDevelopment is a simple application and is easy to use.
                                        Follow the below series of simple steps to utilise the tool."),
                                     tags$ol(
                                       tags$li(h4("Select the dataset file in the corresponding tabs and select appropriate columns
                                                  and click on buttons.")),
                                       tags$li(h4("On submit, you can do pre-processing of Data in the \"Data Processing\"
                                                  Pane on the right side.")),
                                       tags$li(h4("In the \"Descriptive Analysis\" tab, you can get frequancy table of character variable and
                                                  for numeric variable percentile distribution.")),
                                       tags$li(h4("In the \"Information Value\" tab, you will get the information value of all column in
                                                  dataframe, You can remove certain columns as your wish.")),
                                       tags$li(h4("In the \"P Value\" tab, you will get single variable Linear Regression P values.")),
                                       tags$li(h4("In the \"Visualization\" tab, you will get Count Vs Target(1) as bar-line chart .")),
                                       tags$li(h4("In the \"Variable Updation/Creation\" tab, you can
                                                  edit the dataframes,Please double check the syntax before execution.")),
                                       tags$li(h4("In the \"Development Model\" tab, you can create the model by Typing a valid equation.")),
                                       tags$li(h4("In the \"Rank Ordering(Dev)\" tab, you will get rank ordering table and from
                                                  \"All Measures(Dev)\" will give you test results like KS,AUC,Gini,Goodness of Fit & Concordance.")),
                                       tags$li(h4("In the \"Validation Model\" tab, you can select validation data frame, It will create same model
                                                  on validation dataset and check the beta ratio.")),
                                       tags$li(h4("You can see the model performance on validation in \"Rank Ordering(Val)\",\"Model Measures(Val)\" tabs
                                                  and Holdout test results will shown in \"Validation Model on Holdout\" & \"Model Measures(Holdout)\" ")),
                                       tags$li(h4("You can save the objects that you crated with note in \"Save Objects\" tab. "))
                                       ),
                                     h3("More about this app you can see while using."),
                                     HTML("<div style='height: 5px;'>"),
                                     HTML("</div>"),
                                     h3("Contact"),
                                     h4("jishnu.s@k2analytics.co.in"),
                                     HTML("<div style='height: 5px;'>"),
                                     HTML("</div>"),
                                     align = "center",
                                     HTML(paste(icon("copyright"),"K2 Analytics Finishing School Pvt. Ltd.")),
                                     h4(a("http://www.k2analytics.co.in/", href="http://www.k2analytics.co.in/")),
                                     HTML("<div style='height: 5px;'>"),
                                     HTML("</div>")
                                       )
                                     )
                          }),
                  # ***************************Descriptive Analysis***************************
                  tabItem(tabName = "DescriptiveAnalysis",
                          {
                            fluidPage(
                              titlePanel("Descriptive Analysis"),
                              tags$head(tags$script(
                                HTML(
                                  "Shiny.addCustomMessageHandler(
                                  'resetValue',function(variableName) {
                                  Shiny.onInputChange(variableName, null);});"
                              )
                              )),
                              box(title = 'Select Data',width = 3, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  selectInput(
                                    label = "Select Dataframe",
                                    choices = m$names.x...x....TRUE..,
                                    selected = "m",
                                    inputId = "Table987"),
                                  uiOutput("factor987"),
                                  helpText("Factor variable will produce a Frequency"),
                                  uiOutput("numeric987"),
                                  helpText("Numeric variable will produce a Quantile"),
                                  selectInput("num987",
                                              "Please Enter Number(Optional for quantile)",
                                              0:100,
                                              multiple = T,
                                              selected = c(0, 1,5, 10, 25, 50, 75, 90, 95, 99, 100)),
                                  radioButtons(
                                    inputId = "ones987",
                                    label = "Please choose one",
                                    choices = c(
                                      `Factor / Character Variable` = "fac987",
                                      `Numeric Variable` = "nam987"),
                                    selected = "nam987"
                                  ),
                                  actionButton("analysis", "Get Analysis")),
                              box(width = 9, status = "primary", solidHeader = TRUE,title = 'Results',
                                  collapsible = TRUE,
                                  withSpinner(tableOutput("crosstab987")),
                                  tableOutput("count987"))
                                )
                          }),

                  # ***************************Information Value***************************
                  tabItem(tabName = "InformationValue",
                          {
                            fluidPage(
                              titlePanel("Information Value"),
                              box(title = 'Select Data',width = 3, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  uiOutput("dev"),
                                  numericInput("maxlvl", "Max. Permissible Levels", 10, min = 1, max = 100),
                                  uiOutput("ID"),
                                  uiOutput("Target"),
                                  textOutput("binaryout"),
                                  uiOutput("ivv")),
                              box(width = 9, status = "primary", solidHeader = TRUE,title = 'Results',
                                  collapsible = TRUE,
                                  withSpinner(DT::dataTableOutput("ivvalue")))
                            )
                          }),

                  # ***************************P Value***************************
                  tabItem(tabName = "PValue",
                          {
                            fluidPage(
                              titlePanel("P Value"),
                              box(width = 3, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  uiOutput("IDp"),
                                  uiOutput("Targetp"),
                                  uiOutput("pv")),
                              box(width = 9, status = "primary", solidHeader = TRUE,title = 'Results',
                                  collapsible = TRUE,
                                  withSpinner(DT::dataTableOutput("pvalue")))
                            )
                          }),
                  # ***************************Visualization***************************
                  tabItem(tabName = "VisualizationPlot",
                          {
                            fluidPage(
                              titlePanel("Visualization"),
                              box(title = 'Select Data',width = 3, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  uiOutput("Table2"),
                                  uiOutput("factor"),
                                  uiOutput("numeric5678"),
                                  radioButtons(
                                    inputId = "ones",
                                    label = "Please choose one",
                                    choices = c(
                                      `Factor / Character Variable` = "fac",
                                      `Numeric Variable` = "nam"),
                                    selected = NULL,
                                    inline = FALSE,
                                    width = NULL,
                                    choiceNames = NULL,
                                    choiceValues = NULL),
                                  uiOutput("all"),
                                  uiOutput("plot")),
                              box(width = 9, status = "primary", solidHeader = TRUE,title = 'Results',
                                  collapsible = TRUE,
                                  withSpinner(plotlyOutput("trendPlot",
                                                           width = "auto",
                                                           height = "auto"))),

                              box(width = 9, status = "primary", solidHeader = TRUE,title = 'Log Odds',
                                  collapsible = TRUE, collapsed = TRUE,id = "box1",
                                  column(5,withSpinner( rHandsontableOutput('contents')))
                                  , column(7, plotOutput("LOG_plot3"))

                              ))
                            }),
                  # ***************************Cross Table***************************
                  tabItem(tabName = "CrossTable",
                          {
                            fluidPage(
                              tags$head(tags$script(
                                HTML(
                                  "Shiny.addCustomMessageHandler(
                                  'resetValue',function(variableName) {
                                  Shiny.onInputChange(variableName, null);});"
                              )
                              )),
                              titlePanel("Cross Table"),
                              box(title = 'Select Data',width = 3, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  uiOutput("Table266"),
                                  uiOutput("factor266"),
                                  uiOutput("numeric266"),
                                  uiOutput("all266"),
                                  actionButton("crossTable", "Click for Cross Table")),
                              box(width = 9, status = "primary", solidHeader = TRUE,title = 'Results',
                                  collapsible = TRUE,
                                  textOutput("crosstab2299"),
                                  tags$head(
                                    tags$style(
                                      "#crosstab2299{color: red;\n
                                      font-size: 20px;\n
                                      font-style: italic;\n  }")),
                                  withSpinner(tableOutput("crosstab")),
                                  tableOutput("count"))
                                  )
                            }),
                  # ***************************Variable Clustering***************************
                  tabItem(tabName = "VariableClustering",
                          {
                            fluidPage(
                              tags$head(tags$script(
                                HTML(
                                  "Shiny.addCustomMessageHandler(
                                  'resetValue',function(variableName) {
                                  Shiny.onInputChange(variableName, null);});"
                              )
                              )),
                              titlePanel("Variable Clustering"),
                              box(title = 'Select Data',width = 3, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  uiOutput("IDpx"),
                                  uiOutput("vc")),
                              box(width = 9, status = "primary", solidHeader = TRUE,title = 'Results',
                                  collapsible = TRUE,
                                  withSpinner(plotOutput("clusterplot"))))
                          }),
                  # ***************************Variable Updation***************************
                  tabItem(tabName = "VariableUpdation",
                          {
                            fluidPage(
                              tags$head(tags$script(
                                HTML(
                                  "Shiny.addCustomMessageHandler(
                                  'resetValue',function(variableName) {
                                  Shiny.onInputChange(variableName, null);});"
                              )
                              )),
                              titlePanel("Variable Updation/Creation"),
                              box(title = 'Type Code',width = 12, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  tags$div(title="Type a valid syntax the button will automatically come.",
                                           textAreaInput(
                                             label = "variable Updation",
                                             value = "If you want to update any variable please type a valid  syntax. Example
                                             dataframe<<-expression",
                                             inputId = "variableupdation",
                                             width = "700px",height = "130px",
                                             cols = NULL, rows = NULL, placeholder = NULL, resize = NULL)),
                                  textOutput("typed"),
                                  br(),
                                  tags$div(title="If the exicution is happend you can see the
                                           same code again here.",
                                           uiOutput("updatevardev"))))
                            }),
                  # ***************************Development Model***************************
                  tabItem(tabName = "DevelopmentModel",
                          {
                            fluidPage(
                              tags$head(tags$script(
                                HTML(
                                  "Shiny.addCustomMessageHandler(
                                  'resetValue',function(variableName) {
                                  Shiny.onInputChange(variableName, null);});"
                              )
                              )),
                              titlePanel("Model creation"),
                              box(title = 'Type Code',width = 4, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,

                                  textAreaInput(
                                    label = "Formula",
                                    value = "TARGET~AGE+OCCUPATION+GENDER",
                                    inputId = "equation",
                                    width = "250px",height = "100px",
                                    cols = NULL, rows = NULL, placeholder = NULL, resize = NULL
                                  ),
                                  uiOutput("getmodel"),
                                  div(style = 'overflow-y:scroll; max-height: 250px',textOutput("columnnames"))),
                              box(title = 'Summary',width = 8, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  textOutput("Sum"),
                                  tags$head(tags$style("#Sum{color: red;\n
                                                       font-size: 25px;\n
                          }")),
           tableOutput("recd"),
           tableOutput("coef"),
           textOutput("Nulld"),
           textOutput("Residuald"),
           textOutput("AIC"),
           textOutput("FSI"),
           withSpinner(tableOutput("VIF"))))
                          }),
                  # ***************************RankOrdering Dev***************************
                  tabItem(tabName = "RankOrderingDev",
                          {
                            fluidPage(
                              tags$head(tags$script(
                                HTML(
                                  "Shiny.addCustomMessageHandler(
                                  'resetValue',function(variableName) {
                                  Shiny.onInputChange(variableName, null);});"
                              )
                              )),
                              box(title = 'Rank Ordering Development',width = 12, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  div(style = 'overflow-x:scroll',withSpinner(DT::dataTableOutput("Rankordering"))),
                                  uiOutput("Rankord")))
                          }),
                  # ***************************All Measures Dev***************************
                  tabItem(tabName = "AllMeasuresDev",
                          {
                            fluidPage(
                              titlePanel("All Measures"),
                              box(title = 'Click for Results',width = 2, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  uiOutput("measure"),
                                  helpText("Concordance will take time to show the output for large datasets."),
                                  uiOutput("concob")),
                              box(title = 'Results',width = 10, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  withSpinner(tableOutput("stat")),
                                  withSpinner(tableOutput("concordance")),
                                  withSpinner(tableOutput("chi1")),
                                  withSpinner(tableOutput("chi2"))
                              ))
                          }),
                  # ***************************Validation Model***************************
                  tabItem(tabName = "ValidationModel",
                          {
                            fluidPage(
                              titlePanel("Validating the model with validation dataset"),
                              box(title = 'Select Validation Data',width = 3, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  selectInput(
                                    label = "Select Validation Dataframe",
                                    choices = m$names.x...x....TRUE..,
                                    selected = "m",
                                    inputId = "val"),
                                  uiOutput("validate"),
                                  helpText("Equation used for the model creation"),
                                  div(style = 'overflow-x:scroll',textOutput("eqnused"))),
                              box(title = 'Results',width = 9, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  textOutput("Sumv"),
                                  tags$head(tags$style("#Sumv{color: red;\n     font-size: 25px;\n      }")),
                                  withSpinner(tableOutput("betaratio")),
                                  tableOutput("summaryval")))
                            }),
                  # ***************************RankOrdering Val***************************
                  tabItem(tabName = "RankOrderingVal",
                          {
                            fluidPage(
                              # titlePanel("Validation Rank Ordering"),
                              box(title = 'Validation Rank Ordering',width = 12, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  div(style = 'overflow-x:scroll',withSpinner(DT::dataTableOutput("Rankorderingv"))),
                                  uiOutput("Rankordv")))
                          }),
                  # ***************************ModelMeasures Val***************************
                  tabItem(tabName = "ModelMeasuresVal",
                          {
                            fluidPage(
                              # titlePanel("Comparison Dev Vs Val"),
                              box(title = 'Comparison Dev Vs Val',width = 3, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  uiOutput("Compr"),
                                  helpText("Concordance will take time to show the out put for large datasets"),
                                  uiOutput("Comprconc")),
                              box(title = 'Result',width = 9, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  withSpinner(tableOutput("stat2")),
                                  tableOutput("concordanced"),
                                  withSpinner(tableOutput("concordancev"))))
                          }),
                  # ***************************Validation Model on Holdout***************************
                  tabItem(tabName = "ValidationModelonHoldout",
                          {
                            fluidPage(
                              # titlePanel("Validation Rank Ordering"),
                              box(title = 'Select Validation Data',width = 3, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  selectInput(
                                    label = "Select Holdout Dataframe",
                                    choices = m$names.x...x....TRUE..,
                                    selected = "m",
                                    inputId = "Holdout"),
                                  uiOutput("Rankordh"),
                                  helpText("Equation used for the model creation"),
                                  textOutput("eqnused123")),
                              box(title = 'Validation Rank Ordering',width = 9, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  textOutput("Sumh"),
                                  tags$head(tags$style("#Sumh{color: red;\n
                                                       font-size: 25px;\n
                          }")),
      div(style = 'overflow-x:scroll',withSpinner(DT::dataTableOutput("Rankorderingh")))))
                          }),
                  # ***************************ModelMeasures Holdout***************************
                  tabItem(tabName = "ModelMeasuresHoldout",
                          {
                            fluidPage(
                              titlePanel("Comparison Dev Vs Val Vs Holdout"),
                              box(title = 'Click For Results',width = 3, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  uiOutput("Comprvh"),
                                  helpText("Concordance will take time to show the out put for large datasets"),
                                  uiOutput("Comprconcvh")),
                              box(title = 'Result',width = 9, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  withSpinner(tableOutput("stat2vh"))
                                  ,tableOutput("concordancedh"),
                                  withSpinner(tableOutput("concordancedh123")),
                                  withSpinner(tableOutput("concordancevh"))))
                          }),
                  # ***************************Save Objects***************************
                  tabItem(tabName = "SaveObjects",
                          {
                            fluidPage(
                              #titlePanel("save objects"),
                              box(title = 'save objects',width = 12, status = "primary", solidHeader = TRUE,
                                  collapsible = TRUE,
                                  textAreaInput(inputId="documentation", label="Documentation", value = "",
                                                width = "750px", height = "300px",
                                                cols = NULL, rows = NULL, placeholder = NULL, resize = NULL),
                                  helpText("Download  the objects (P_value,Information value,Model Rankordering tables & Comparison tables ) created with documentation"),
                                  downloadButton('downloadData', 'Download   Objects')))
                          })

                  )))

options(shiny.maxRequestSize=200*1024^2)
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  options(warn =-1)

  # ************************************Data Source************************************

  # **************************Select Inputs**************************
  { out_factore987 <-reactive({
    #for reactive factore output based on selected object
    if (input$Table987=="m")
      return(NULL)
    op <- data.frame(get((input$Table987)))
    j <- data.frame(names(Filter(is.factor, op)))
    xx <- data.frame(names(Filter(is.character, op)))
    colnames(j) <- "factor"
    colnames(xx) <- "factor"
    j <- rbind(j, xx)
    j})

  output$factor987 <-   renderUI({
    #reactive input factore
    selectInput(
      label =  "Factor / Character Variable" ,
      choices = out_factore987() ,
      selected = NULL,
      inputId = "factor987")})
  {## Numeric Input##
    out_numeric987 <-
      reactive({
        #for reactive factore output based on selected object
        if (input$Table987=="m")
          return(NULL)
        op <- data.frame(get((input$Table987)))
        j <- data.frame(names(Filter(is.numeric, op)))
        colnames(j) <- "numeric"
        j})
    output$numeric987 <-
      #reactive input factore
      renderUI({
        selectInput(
          label = "Numeric Variable" ,
          choices = out_numeric987() ,
          selected = NULL,
          multiple = T,
          inputId = "numeric987")})


    # **************************Reactive Variable**************************



    head987<-reactive({
      in1<-input$factor987
      in1})

    freq<-reactive({
      y <- data.frame(get((input$Table987)))
      in1 <- input$factor987
      xxop <- y[ in1]
      colnames(xxop)="x"
      FREQ<-table(xxop$x)
      PER<-FREQ/nrow(xxop)
      FREQ<-data.frame(FREQ)
      PER<-data.frame(PER)
      FREQ<-merge(FREQ,PER,by="Var1")
      library(scales)
      FREQ$Freq.y<-percent(FREQ$Freq.y)
      xxob<-list(Var1="Total",Freq.x=nrow(xxop),Freq.y=percent(nrow(xxop)/nrow(xxop)))
      FREQ<-rbind(FREQ,data.frame(xxob))
      colnames(FREQ)<-c(head987(),"FREQ","PER")
      FREQ})

    qunt<-reactive({
      y <- data.frame(get((input$Table987)))
      in1 <- input$numeric987
      xxop <- y[ in1]
      in3<-paste(input$num987,sep=",")
      in4<-as.numeric(in3)
      in4<-sort(in4)
      QUANTILE<- apply( xxop[c(1:ncol(xxop))] , 2 ,quantile ,
                        probs=in4/100,na.rm=TRUE)
      QUANTILE<-as.data.frame(QUANTILE)
      setDT(QUANTILE, keep.rownames = TRUE)[]
      names(QUANTILE)[names(QUANTILE) == "rn"] = "PER"
      QUANTILE})
    mtexxt987 <- eventReactive(input$analysis, {
      if (input$Table987=="m")
        return(NULL)
      # op <- data.frame(get((input$Table987)))
      # op
      if (input$ones987 == "fac987")
        return(freq())
      qunt()
    })

    output$crosstab987 <- renderTable({
      mtexxt987()},
      bordered = TRUE)} # Descriptive Analysis-Output

  } # Descriptive Analysis
  {
    ##Information Value##
    {## Inputs ##
      output$dev<-
        renderUI({
          selectInput(
            label = "Select Development Dataframe",
            choices = m$names.x...x....TRUE..,
            selected = input$Table987,
            inputId = "dev")})
      colnam <-
        reactive({
          #for reactive all output based on selected object
          if (is.null(input$dev) || input$dev=="m")
            return(NULL)
          op <- data.frame(get((input$dev)))
          j <- data.frame(names(op))
          colnames(j) <- "all"
          j
        })
      autocolmns<-reactive({
        if (is.null(input$dev) || input$dev=="m")
          return(NULL)
        op <- data.frame(get((input$dev)))
        lvls<-data.frame(lapply(sapply(op, levels), length))
        lvls<-colnames(lvls [,which(lvls[1,]>=input$maxlvl)])
        lvls})

      autocolmnstar<-reactive({
        if (is.null(input$dev) || input$dev=="m")
          return(NULL)
        op <- data.frame(get((input$dev)))
        dfnum<-op[,lapply (op,class) %in% c("numeric","integer")]
        dfnum$sample1233<-sample(0:1,nrow(dfnum),replace = T)
        lvls<-data.frame(lapply(sapply(dfnum, unique), length))
        lvls<-colnames(lvls[,which(lvls[1,]==2)])
        lvls})

      output$ID <-
        renderUI({
          selectInput(
            label = "Select Variable/s to Ignore" ,
            choices = colnam() ,
            selected = autocolmns(),
            inputId = "ID",
            multiple = T)})

      output$Target<-
        renderUI({
          selectInput(
            label = "Select Target Variable" ,
            choices = autocolmnstar() ,
            selected = NULL,
            inputId = "Target")})

      output$binaryout<-renderText({
        if (is.null(input$dev) || input$dev=="m" )
          return(NULL)
        KPP<-"If Target is not binary column it will not show.
        If you click the button without selecting
        proper Target the app will close automatically"
        KPP})

      output$ivv <-renderUI({
        if (is.null(input$dev) || input$dev=="m" )
          return(NULL)
        actionButton("ivv","Get Information Value")})
    } # Information Value - Inputs
    { ## Table ##
      ivvtb<-reactive({
        op <- data.frame(get((input$dev)))
        id<-input$ID
        targ<-input$Target
        row.names(op) <- 1:nrow(op)
        pp<-iv.mult(op[,!names(op) %in% c(id)],targ,TRUE)
        pp$InformationValue<-round(pp$InformationValue,2)
        pp})

      ntextivv <- eventReactive(input$ivv, {
        if ("iv.mult"%in%ls(getNamespace("woe") )){
          ivvtb()
        } else {
          print("Please install woe package using 'install_github('riv','tomasgreif')'")
        }})

      output$ivvalue<-DT::renderDataTable({ Sys.sleep(2);
        ntextivv()
      },options = list(
        lengthChange = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
          "}"),
        autowidth = TRUE,
        columnDefs = list(list(width = '70%', targets = 1))))

    } # Information Value - Table
  } # Information Value
  {
    ## P Value##
    {
      output$IDp <-
        renderUI({
          selectInput(
            label = "Select Variable/s to Ignore" ,
            choices = colnam() ,
            selected = input$ID,
            inputId = "IDp",
            multiple = T)})
      output$Targetp<-
        renderUI({
          selectInput(
            label = "Select Target Variable" ,
            choices = autocolmnstar() ,
            selected = input$Target,
            inputId = "Targetp")})

      output$pv <-
        renderUI({
          if (is.null(input$dev) || input$dev=="m" )
            return(NULL)
          actionButton("pv", "Get P Value ")})
    } # P Value - Inputs
    {## Table##
      glmfunc<-reactive({
        OneVariableGLM <-
          function(df, target, id) {
            targ<-which( colnames(df)==target)
            id<-which( colnames(df) %in% id )
            tmp<- df
            head(tmp)
            pp<-lapply( tmp[,c(-id,-targ)], function(x) summary(glm(tmp[,targ] ~ x)) )
            inter<-lapply(pp, coef)
            require(reshape2)
            inter$id <- rownames(inter)
            inter
            ohh<-melt(inter)
            got<-ohh[which(ohh$Var2=="Pr(>|t|)" &ohh$Var1 !="(Intercept)"),]
            row.names(got)<-NULL
            colnames(got)<-c("Value","Indicator","P_value","Variable")
            got<-got[c(4,1,2,3)]
            got<-got[order(got$P_value),]
            got$Value<-substring(got$Value, 2)
            got$Indicator<-NULL
            got}

        op <- data.frame(get((input$dev)))
        id<-input$IDp
        targ<-input$Targetp

        OneVariableGLM(df = op,target = targ,id = id)
      })

      ntextp <- eventReactive(input$pv, {
        glmfunc()})

      output$pvalue<-DT::renderDataTable({ Sys.sleep(2);
        ntextp()},
        options = list(
          lengthChange = FALSE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});","}"),
          autowidth = TRUE,
          columnDefs = list(list(width = '30%', targets = 1))))
    } # P Value - Table
  } # P Value
  {
    ##Visualization##
    {
      output$Table2 <-
        #reactive input factore
        renderUI({
          selectInput(
            label = "Select Dataframe",
            choices = m$names.x...x....TRUE..,
            selected = input$Table987,
            inputId = "Table2")})

      out_factore <-
        reactive({
          #for reactive factore output based on selected object
          if (is.null(input$Table2) || input$Table2=="m")
            return(NULL)
          op <- data.frame(get((input$Table2)))
          j <- data.frame(names(Filter(is.factor, op)))
          xx <- data.frame(names(Filter(is.character, op)))
          colnames(j) <- "factor"
          colnames(xx) <- "factor"
          j <- rbind(j, xx)
          j
        })

      output$factor <-
        #reactive input factore
        renderUI({
          selectInput(
            label = "Factor / Character Variable" ,
            choices = out_factore() ,
            selected = NULL,
            inputId = "factor")})

      out_numeric_3567<-
        reactive({
          #for reactive numeric output based on selected object
          if (is.null(input$Table2) || input$Table2=="m")
            return(NULL)
          op <- data.frame(get((input$Table2)))
          dfnum<-op[,lapply (op,class) %in% c("numeric","integer")]
          j <- data.frame(names(dfnum))
          colnames(j) <- "all_numeric"
          j})
      output$numeric5678 <-
        #reactive input numeric
        renderUI({
          selectInput(
            label = "Numeric Variable" ,
            choices = out_numeric_3567() ,
            selected = NULL,
            multiple = F,
            inputId = "numeric5678")})
      out_all <-
        reactive({
          #for reactive all output based on selected object
          if (is.null(input$Table2) || input$Table2=="m")
            return(NULL)
          op <- data.frame(get((input$Table2)))
          dfnum<-op[,lapply (op,class) %in% c("numeric","integer")]
          lvls<-data.frame(lapply(sapply(dfnum, unique), length))
          lvls<-colnames(lvls)
          # [,which(lvls[1,]==2)])
          lvls})
      output$all <-
        #reactive input all
        renderUI({
          selectInput(
            label = "Select Target Variable" ,
            choices = out_all() ,
            selected = NULL,
            multiple = F,
            inputId = "all")})
      output$plot <- renderUI({
        if (is.null(input$Table2) || input$Table2=="m")
          return(NULL)
        actionButton("plot", "Click for plot")})

      output$plot_log <- renderUI({
        if (is.null(input$Table2) || input$Table2=="m" || input$ones == "fac" )
          return(NULL)
        actionButton("plot_log", "Click for log odds graph")})

    } #Visualization - Inputs
    { ## Plot ##
      {
        xxpp <- reactive({
          y <- data.frame(get((input$Table2)))
          library(data.table)
          library(scales)
          ## deciling code
          decile <- function(x) {
            deciles <- vector(length = 10)
            for (i in seq(0.1, 1, .1)) {
              deciles[i * 10] <- quantile(x, i, na.rm = T)
            }
            return (ifelse(x < deciles[1], 1,
                           ifelse(
                             x < deciles[2], 2,
                             ifelse(x < deciles[3], 3,
                                    ifelse(
                                      x < deciles[4], 4,
                                      ifelse(x < deciles[5], 5,
                                             ifelse(
                                               x < deciles[6], 6,
                                               ifelse(x < deciles[7], 7,
                                                      ifelse(x < deciles[8], 8,
                                                             ifelse(x <
                                                                      deciles[9], 9, 10)))
                                             )))))))}

          ## compile the function
          fn_biz_viz <- function(df, target, var)
          {
            tmp <- df[, c(var , target)]
            colnames(tmp)[1] = "Xvar"
            colnames(tmp)[2] = "Target"
            tmp$deciles <- decile(tmp$Xvar)
            library(data.table)
            tmp_DT = data.table(tmp)
            RRate <- tmp_DT[, list(
              min_ = min(Xvar),
              max_ = max(Xvar),
              avg_ = mean(Xvar),
              cnt = length(Target),
              cnt_resp = sum(Target),
              cnt_non_resp = sum(Target == 0)) ,
              by = deciles][order(deciles)]
            RRate$range = paste(RRate$min_ , RRate$max_ , sep = " to ")
            RRate$prob <- round(RRate$cnt_resp / RRate$cnt, 3)
            setcolorder(RRate, c(1, 8, 2:7, 9))
            RRate$cum_tot <- cumsum(RRate$cnt)
            RRate$cum_resp <- cumsum(RRate$cnt_resp)
            RRate$cum_non_resp <- cumsum(RRate$cnt_non_resp)
            RRate$cum_tot_pct <-
              round(RRate$cum_tot / sum(RRate$cnt), 2)
            RRate$cum_resp_pct <-
              round(RRate$cum_resp / sum(RRate$cnt_resp), 2)
            RRate$cum_non_resp_pct <-
              round(RRate$cum_non_resp / sum(RRate$cnt_non_resp), 2)
            RRate$ks <-
              abs(RRate$cum_resp_pct - RRate$cum_non_resp_pct)
            RRate$prob = 100 * (RRate$prob)
            RRate$cum_tot_pct = 100 * (RRate$cum_tot_pct)
            RRate$cum_resp_pct = 100 * (RRate$cum_resp_pct)
            RRate$cum_non_resp_pct = 100 * (RRate$cum_non_resp_pct)
            RRate$ordered_range <-
              factor(RRate$range, levels = RRate$range)
            RRate}
          jp <- fn_biz_viz(df = y,
                           target = input$all,
                           var = input$numeric5678)})


        LOG_OODS<-  reactive({
          if (is.null(input$Table2) || input$Table2=="m" || input$ones=='fac')
            return(NULL)
          y <- data.frame(get((input$Table2)))
          library(data.table)
          library(scales)
          ## deciling code
          decile <- function(x) {
            deciles <- vector(length = 10)
            for (i in seq(0.1, 1, .1)) {
              deciles[i * 10] <- quantile(x, i, na.rm = T)
            }
            return (ifelse(x < deciles[1], 1,
                           ifelse(
                             x < deciles[2], 2,
                             ifelse(x < deciles[3], 3,
                                    ifelse(
                                      x < deciles[4], 4,
                                      ifelse(x < deciles[5], 5,
                                             ifelse(
                                               x < deciles[6], 6,
                                               ifelse(x < deciles[7], 7,
                                                      ifelse(x < deciles[8], 8,
                                                             ifelse(x <
                                                                      deciles[9], 9, 10)))
                                             )))))))}

          lg_fn<- function (df, target, var, ln_trnfm = 0)
          {
            tmp <- df[, c(var, target)]
            head(tmp)
            colnames(tmp)[1] = "Xvar"
            colnames(tmp)[2] = "Target"
            if (ln_trnfm == 1) {
              tmp$Xvar = log(tmp$Xvar + 1)
            }
            tmp$deciles <- decile(tmp$Xvar)
            tmp_DT = data.table(tmp)
            RRate <- tmp_DT[, list(min_ = min(Xvar), max_ = max(Xvar),
                                   avg_ = mean(Xvar), cnt = length(Target), cnt_responder = sum(Target),
                                   cnt_non_responder = sum(Target == 0)), by = deciles][order(deciles)]
            RRate$prob <- RRate$cnt_responder/RRate$cnt
            RRate$log_odds <- log(RRate$prob/(1 - RRate$prob))
            RRate
          }
          mp<-lg_fn(df = y,
                    target = input$all,
                    var = input$numeric5678)
          mp<-mp[,c(4,9)]
        })

        header <- reactive({
          target = input$all
          var = input$numeric5678
          paste(target, "Vs", var)})

        x_var_n <- reactive({
          var <- input$numeric5678
          var})

        xxpp1 <- reactive({
          jp <- xxpp()
          ay <- list(
            ticxxfont = list(color = "red"),
            overlaying = "y",
            side = "right",
            title = "Target Rate",
            range = c(0, max(jp$prob + 2, by = 2)))
          p <- plot_ly() %>%
            add_bars (x = jp$ordered_range,
                      y = jp$cnt,
                      name = "# Customers") %>%
            add_lines(
              x = jp$ordered_range,
              y = jp$prob,
              name = "Target Rate",
              yaxis = "y2") %>%
            add_text(
              x =  jp$ordered_range,
              y = jp$prob,
              text = jp$prob,
              inherit = FALSE,
              name = "Target Rate",
              yaxis = "y2",
              textposition = 'top',
              textfont = list(color = '#000000', size = 16),
              showlegend = FALSE) %>%
            layout(
              title = header(),
              yaxis2 = ay,
              xaxis = list(title = x_var_n()),
              yaxis = list(title = "# Customers"),
              legend = list(
                x = 0.3,y = 1.1,
                orientation = 'h'),
              margin = 10)
          p})
        indat <- reactiveValues(data=empty_dat)
        observe({
          inFile = LOG_OODS()
          if (is.null(inFile))
            return(NULL)
          data1 = LOG_OODS()
          indat$data <- data1
        })

        observe({
          if(!is.null(input$contents))
            indat$data <- hot_to_r(input$contents)

        })
        output$contents <- renderRHandsontable({
          rhandsontable(indat$data)
        })

        lgplt<-eventReactive(input$plot_log, {

        })
        output$LOG_plot3 <- renderPlot({

          RRate<- indat$data
          RRate$avg_<-as.numeric(RRate$avg_)
          RRate$log_odds<-as.numeric( RRate$log_odds)
          plot(x = RRate$avg_, y = RRate$log_odds, type = "b", pch = 20,
               xlab = names(RRate)[1], ylab = " Log Odds")
          abline(fit <- lm(RRate$log_odds ~ RRate$avg_), col = "red")
          legend("topright", bty = "n", legend = paste("R2 is", format(summary(fit)$adj.r.squared,
                                                                       digits = 4)))
        })
      } # Numeric variable
      {
        header123 <- reactive({
          in1 <- input$factor
          in2 <- input$all
          paste(in2, "Vs", in1)})

        x_var <- reactive({
          in1 <- input$factor
          in1})
        chr1 <- reactive({
          if (input$factor == input$all)
            return(NULL)
          y <- data.frame(get((input$Table2)))
          in1 <- input$factor
          in2 <- input$all
          xxop <- y[, c(in1, in2)]
          colnames(xxop) <- c("group_V", "Target")
          pp <- as.data.frame.matrix(table(xxop$group_V, xxop$Target))
          jq <- pp
          jq$all_c <- (jq$`0` + jq$`1`)
          jq$prob <- round(jq$`1` / (jq$`0` + jq$`1`), 3)
          jq$prob <- 100 * jq$prob
          ay <- list(
            ticxxfont = list(color = "red"),
            overlaying = "y",
            side = "right",
            title = "Target Rate",
            range = c(0, max(jq$prob + 2, by = 2)))
          p <- plot_ly() %>%
            add_bars (x = row.names(jq),
                      y = jq$all_c,
                      name = "# Customers") %>%
            add_lines (
              x = row.names(jq),
              y = jq$prob,
              name = "Target Rate",
              yaxis = "y2") %>%
            add_text(
              x =  row.names(jq),
              y = jq$prob,
              text = jq$prob,
              inherit = FALSE,
              yaxis = "y2",
              name = "Target Rate",
              textposition = 'top',
              textfont = list(color = '#000000', size = 16),
              showlegend = FALSE) %>%
            layout(
              title = header123(),
              yaxis2 = ay,
              xaxis = list(title = x_var()),
              yaxis = list(title = "# Customers"),
              legend = list(
                x = 0.3,y = 1.1,
                orientation = 'h'),
              margin = 10)
          p})
      } # Catagorical Variable
      { mtexxt <- eventReactive(input$plot, {
        if (input$ones == "fac")
          return(chr1())
        xxpp1()
      })
        output$trendPlot <- renderPlotly({ Sys.sleep(2);
          options(warn = -1)
          mtexxt()
        })} #output

    } # Visualization - Plot
  } # Visualization
  {
    ## Cross Table ##
    {
      output$Table266 <-
        #reactive input factore
        renderUI({
          selectInput(
            label = "Select Dataframe",
            choices = m$names.x...x....TRUE..,
            selected = input$Table987,
            inputId = "Table266")})
      out_factore266 <-
        reactive({
          #for reactive factore output based on selected object
          if (is.null(input$Table266) || input$Table266=="m")
            return(NULL)
          op <- data.frame(get((input$Table266)))
          j <- data.frame(names(op))
          colnames(j) <- "all"
          j})
      output$factor266 <-
        #reactive input factore
        renderUI({
          selectInput(
            label = "Dimension-1" ,
            choices = out_factore266() ,
            selected = NULL,
            inputId = "factor266")})
      out_numeric266 <-
        reactive({
          #for reactive numeric output based on selected object
          if (is.null(input$Table266) || input$Table266=="m")
            return(NULL)
          op <- data.frame(get((input$Table266)))
          j <- data.frame(names(op))
          colnames(j) <- "all"
          j
        })
      output$numeric266 <-
        #reactive input numeric
        renderUI({
          selectInput(
            label = "Dimension-2" ,
            choices = out_numeric266() ,
            selected = NULL,
            multiple = F,
            inputId = "numeric266")})
      out_all266 <-
        reactive({
          #for reactive all output based on selected object
          if (is.null(input$Table266) || input$Table266=="m")
            return(NULL)
          op <- data.frame(get((input$Table266)))
          dfnum<-op[,lapply (op,class) %in% c("numeric","integer")]
          lvls<-data.frame(lapply(sapply(dfnum, unique), length))
          lvls<-colnames(lvls)
          #[,which(lvls[1,]==2)])
          lvls})
      output$all266 <-
        #reactive input all
        renderUI({
          selectInput(
            label = "Select Target Variable" ,
            choices = out_all266() ,
            selected = NULL,
            multiple = F,
            inputId = "all266")})
    } # Cross Table - Inputs
    {## Tables ##
      header99 <- eventReactive(input$crossTable, {
        in1 <- input$factor266
        in2 <- input$numeric266
        paste(c(in1, "Vs", in2))
      })
      output$crosstab2299 <- renderText({
        header99()})

      {
        crstbl <- reactive({
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          xxop <- y[, c(in1, in2, in3)]
          colnames(xxop) <- c("x", "y", "Ta")
          tb <- as.data.frame.matrix(xtabs(~ xxop$x + xxop$y))
          tb2 <- as.data.frame.matrix(xtabs(xxop$Ta ~ xxop$x + xxop$y))
          per <- tb2 * 100 / tb
          library(data.table)
          setDT(per, keep.rownames = TRUE)[]
          names(per)[names(per) == "rn"] = "Target Rate"
          per})

        crstbl99 <- reactive({
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          xxop <- y[, c(in1, in2, in3)]
          colnames(xxop) <- c("x", "y", "Ta")
          tb <- as.data.frame.matrix(xtabs(~ xxop$x + xxop$y))
          library(data.table)
          setDT(tb, keep.rownames = TRUE)[]
          names(tb)[names(tb) == "rn"] = "#Customers"
          tb})

      } # X- Catagory Y- Catagory
      {
        crstbl2_dia_1 <- reactive({
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          xxop <- y[, c(in1, in2, in3)]
          colnames(xxop) <- c("x", "y", "Ta")
          decile <- function(x) {
            deciles <- vector(length = 10)
            for (i in seq(0.1, 1, .1)) {
              deciles[i * 10] <- quantile(x, i, na.rm = T)
            }
            return (ifelse(x < deciles[1], 1,
                           ifelse(
                             x < deciles[2], 2,
                             ifelse(x < deciles[3], 3,
                                    ifelse(
                                      x < deciles[4], 4,
                                      ifelse(x < deciles[5], 5,
                                             ifelse(
                                               x < deciles[6], 6,
                                               ifelse(x < deciles[7], 7,
                                                      ifelse(
                                                        x < deciles[8], 8,
                                                        ifelse(x <
                                                                 deciles[9], 9, 10)
                                                      ))
                                             )))))))}
          xxop$decile = decile(xxop$x)
          library(data.table)
          tmp_DT = data.table(xxop)
          RRatet <- tmp_DT[, list(min_ = min(x),
                                  max_ = max(x)) ,
                           by = decile][order(decile)]
          RRatet$range = paste(RRatet$min_ , RRatet$max_ , sep = " to ")
          library(plyr)
          xxop <- join(xxop, RRatet, by = "decile")
          xxop})
        crstbl2 <- reactive({
          xxop <- crstbl2_dia_1()
          tb <- as.data.frame.matrix(xtabs(~ xxop$range + xxop$y))
          tb2 <- as.data.frame.matrix(xtabs(xxop$Ta ~ xxop$range + xxop$y))
          per <- tb2 * 100 / tb
          library(data.table)
          setDT(per, keep.rownames = TRUE)[]
          names(per)[names(per) == "rn"] = "Target Rate"
          per})

        crstbl299 <- reactive({
          xxop <- crstbl2_dia_1()
          tb <- as.data.frame.matrix(xtabs(~ xxop$range + xxop$y))
          library(data.table)
          setDT(tb, keep.rownames = TRUE)[]
          names(tb)[names(tb) == "rn"] = "#Customers"
          tb})


      } # X- Numeric Y- Catagory
      {
        crstbl2_dia_2 <- reactive({
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          xxop <- y[, c(in1, in2, in3)]
          colnames(xxop) <- c("x", "y", "Ta")
          decile <- function(x) {
            deciles <- vector(length = 10)
            for (i in seq(0.1, 1, .1)) {
              deciles[i * 10] <- quantile(x, i, na.rm = T)
            }
            return (ifelse(x < deciles[1], 1,
                           ifelse(
                             x < deciles[2], 2,
                             ifelse(x < deciles[3], 3,
                                    ifelse(
                                      x < deciles[4], 4,
                                      ifelse(x < deciles[5], 5,
                                             ifelse(
                                               x < deciles[6], 6,
                                               ifelse(x < deciles[7], 7,
                                                      ifelse(
                                                        x < deciles[8], 8,
                                                        ifelse(x <
                                                                 deciles[9], 9, 10)
                                                      )))))))))}
          xxop$decile = decile(xxop$y)
          library(data.table)
          tmp_DT = data.table(xxop)
          RRatet <- tmp_DT[, list(min_ = min(y),
                                  max_ = max(y)) ,
                           by = decile][order(decile)]
          RRatet$range = paste(RRatet$min_ , RRatet$max_ , sep = " to ")
          library(plyr)
          xxop <- join(xxop, RRatet, by = "decile")
          xxop})
        crstbl3 <- reactive({
          xxop <- crstbl2_dia_2()
          tb <- as.data.frame.matrix(xtabs(~ xxop$x + xxop$range))
          tb2 <- as.data.frame.matrix(xtabs(xxop$Ta ~ xxop$x + xxop$range))
          per <- tb2 * 100 / tb
          library(data.table)
          setDT(per, keep.rownames = TRUE)[]
          names(per)[names(per) == "rn"] = "Target Rate"
          per})
        crstbl399 <- reactive({
          xxop <- crstbl2_dia_2()
          tb <- as.data.frame.matrix(xtabs(~ xxop$x + xxop$range))
          library(data.table)
          setDT(tb, keep.rownames = TRUE)[]
          names(tb)[names(tb) == "rn"] = "#Customers"
          tb})
      } # X- Catagory Y- Numeric
      {
        crstbl2_dia_bth <- reactive({
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          xxop <- y[, c(in1, in2, in3)]
          colnames(xxop) <- c("x", "y", "Ta")
          decile <- function(x) {
            deciles <- vector(length = 10)
            for (i in seq(0.1, 1, .1)) {
              deciles[i * 10] <- quantile(x, i, na.rm = T)
            }
            return (ifelse(x < deciles[1], 1,
                           ifelse(
                             x < deciles[2], 2,
                             ifelse(x < deciles[3], 3,
                                    ifelse(
                                      x < deciles[4], 4,
                                      ifelse(x < deciles[5], 5,
                                             ifelse(
                                               x < deciles[6], 6,
                                               ifelse(x < deciles[7], 7,
                                                      ifelse(
                                                        x < deciles[8], 8,
                                                        ifelse(x <
                                                                 deciles[9], 9, 10)
                                                      )))))))))}
          xxop$decile1 = decile(xxop$y)
          xxop$decile = decile(xxop$x)
          library(data.table)
          tmp_DT = data.table(xxop)
          RRatet <- tmp_DT[, list(min_ = min(x),
                                  max_ = max(x)) ,
                           by = decile][order(decile)]
          RRatet$range = paste(RRatet$min_ , RRatet$max_ , sep = " to ")
          library(plyr)
          xxop <- join(xxop, RRatet, by = "decile")
          tmp_DT = data.table(xxop)
          RRatet <- tmp_DT[, list(min_ = min(y),
                                  max_ = max(y)) ,
                           by = decile1][order(decile1)]
          RRatet$range1 = paste(RRatet$min_ , RRatet$max_ , sep = " to ")
          library(plyr)
          xxop <- join(xxop, RRatet, by = "decile1")
          xxop})

        crstbl4 <- reactive({
          xxop <- crstbl2_dia_bth()
          tb <- as.data.frame.matrix(xtabs(~ xxop$range1 + xxop$range))
          tb2 <-
            as.data.frame.matrix(xtabs(xxop$Ta ~ xxop$range1 + xxop$range))
          per <- tb2 * 100 / tb
          library(data.table)
          setDT(per, keep.rownames = TRUE)[]
          names(per)[names(per) == "rn"] = "Target Rate"
          per})

        crstbl499 <- reactive({
          xxop <- crstbl2_dia_bth()
          tb <- as.data.frame.matrix(xtabs(~ xxop$range1 + xxop$range))
          library(data.table)
          setDT(tb, keep.rownames = TRUE)[]
          names(tb)[names(tb) == "rn"] = "#Customers"
          tb})
      } # X- Numeric Y- Numeric
      {
        mtexxt266 <- eventReactive(input$crossTable, {
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          xxop <- y[, c(in1, in2, in3)]
          colnames(xxop) <- c("x", "y", "Ta")
          ifelse(
            class(xxop$x) %in% c("numeric", "integer") == "TRUE"
            &
              class(xxop$y) %in% c("numeric", "integer") == "TRUE" ,
            return(crstbl4()),
            ifelse(
              class(xxop$x) %in% c("numeric", "integer") == "FALSE"
              &
                class(xxop$y) %in% c("numeric", "integer") == "TRUE" ,
              return(crstbl3()),
              ifelse(
                class(xxop$x) %in% c("numeric", "integer") == "TRUE"
                &
                  class(xxop$y) %in% c("numeric", "integer") == "FALSE" ,
                return(crstbl2()),
                return(crstbl())
              )))})
        output$crosstab <- renderTable({
          mtexxt266()},
          bordered = TRUE)
      } # Target Rate
      {
        mtexxt26699 <- eventReactive(input$crossTable, {
          y <- data.frame(get((input$Table266)))
          in1 <- input$factor266
          in2 <- input$numeric266
          in3 <- input$all266
          xxop <- y[, c(in1, in2, in3)]
          colnames(xxop) <- c("x", "y", "Ta")
          ifelse(
            class(xxop$x) %in% c("numeric", "integer") == "TRUE"
            &
              class(xxop$y) %in% c("numeric", "integer") == "TRUE" ,
            return(crstbl499()),
            ifelse(
              class(xxop$x) %in% c("numeric", "integer") == "FALSE"
              &
                class(xxop$y) %in% c("numeric", "integer") == "TRUE" ,
              return(crstbl399()),
              ifelse(
                class(xxop$x) %in% c("numeric", "integer") == "TRUE"
                &
                  class(xxop$y) %in% c("numeric", "integer") == "FALSE" ,
                return(crstbl299()),
                return(crstbl99())
              )))})

        output$count <- renderTable({
          mtexxt26699()},
          bordered = TRUE)
      } # Count
    } # Cross Table - Tables

  } # Cross Table
  {

    {
      out_numeric <-
        reactive({
          #for reactive numeric output based on selected object
          if (input$dev=="m")
            return(NULL)
          op <- data.frame(get((input$dev)))
          dfnum<-op[,lapply (op,class) %in% c("numeric","integer")]
          j <- data.frame(names(dfnum))
          colnames(j) <- "all_numeric"
          j})
      output$IDpx <-
        #reactive input numeric
        renderUI({
          selectInput(
            label = "Select Numeric Variable/s to Ignore " ,
            choices = out_numeric() ,
            selected = NULL,
            multiple = T,
            inputId = "IDpx")})

      output$vc <- renderUI({
        if (is.null(input$dev) || input$dev=="m")
          return(NULL)
        actionButton("vc","Get Graph")})
    } # Variable Clustering - Input
    {
      plotdata<-reactive({
        op <- data.frame(get((input$dev)))
        in1 <- input$IDpx
        rc<-which( colnames(op)== in1 )
        kop <- op[,!names(op) %in% c(in1)]
        vcls <- function(df)
        {
          tmp <- df[, lapply (df, class) %in% c("numeric", "integer")]
          tmp <- data.matrix(tmp)
          v <- varclus(tmp)
          v}
        ppk<-vcls(kop)
        ppk})

      ntextpx <- eventReactive(input$vc, {
        plotdata()})

      output$clusterplot<-renderPlot({
        ppq<-ntextpx()
        plot(ppq)})
    } # Variable Clustering - plot

  } # Variable Clustering
  {

    updatecmd<-reactive({
      kp<-input$variableupdation
      kp
    })

    testing<-reactive({
      ifelse(is.error(eval(parse(text = updatecmd()))), T ,F)
    })

    output$updatevardev <- renderUI({
      if (testing())
        return(NULL)
      actionButton("updatevardev", "Execute on data")})

    output$updatedout <-
      renderText({
        ifelse(testing(), "Error: Please Check the code", "No Error: You can Proceed")
      })

    typewdt<-eventReactive(input$updatevardev,{
      updatecmd()
    })
    output$typed<-renderText({
      typewdt()
    })
    observeEvent(input$updatevardev, {
      eval(parse(text = typewdt()))
    })

  } # Variable Updation
  {

    {

      testing_cmd<-reactive({
        op <- data.frame(get((input$dev)))
        equ<-input$equation
        llp4<-gsub('\\+', ",", equ)
        llp4<-gsub('\\~', ",", llp4)

        llp4<-strsplit(llp4, '\\,')
        llp4<-unlist(llp4)

        ifelse(is.error(op[llp4]), T ,F)
      })



      output$getmodel <- renderUI({
        if (is.null(input$dev) || input$dev=="m" || testing_cmd())
          return(NULL)
        actionButton("getmodel", "Create Model")})

      out_columnnames1 <-
        reactive({
          op <- data.frame(get((input$dev)))
          # j <- data.frame(names(op))
          # colnames(j) <- "Colnames"
          j<-c('Column Names:',names(op))
          j})
      ntext <- eventReactive(input$ivv, {
        if (input$dev=="m")
          return(NULL)
        out_columnnames1()      })

      output$columnnames<-renderText({
        ntext()      })

      sumt<-eventReactive(input$getmodel, {
        kp<-paste0("MODEL SUMMARY") })

      output$Sum<-renderText({
        sumt() })
    } # Preparations(Colnames,heder,select input)
    { mylogit<-eventReactive(input$getmodel, {
      op <- data.frame(get((input$dev)))
      equ<-input$equation
      mylogit1<-glm(formula = equ,family = "binomial",data = op)
      mylogit1
    })} # Model creation
    {
      {cofelogit<-eventReactive(input$getmodel, {
        mylogit1<-mylogit()
        summ<-summary(mylogit1)
        coeff<-data.table(summ$coefficients)
        coeff1<-data.frame(summ$coefficients)
        Coefficients<-row.names(coeff1)
        coeff<-cbind(Coefficients,coeff)
        coeff

      })

      output$coef<-renderTable({
        cofelogit()
      })}# Coefficients
      {
        Nulldt<-eventReactive(input$getmodel, {
          mylogit1<-mylogit()
          summ<-summary(mylogit1)
          rsevalue<-round(summ$null.deviance,4)
          df<-summ$df.null
          rsevalue1<-paste("Null deviance:",rsevalue,"on",df,"degrees of freedom")
          rsevalue1
        })
        output$Nulld<-renderText({
          Nulldt()
        })
        Residualt<-eventReactive(input$getmodel, {
          mylogit1<-mylogit()
          summ<-summary(mylogit1)
          rsevalue<-round(summ$deviance,4)
          df<-summ$df.residual
          rsevalue1<-paste("Residual deviance:",rsevalue,"on",df,"degrees of freedom")
          rsevalue1
        })
        output$Residuald<-renderText({
          Residualt()
        })
      } # Null deviance & residual
      {
        aict<-eventReactive(input$getmodel, {
          mylogit1<-mylogit()
          summ<-summary(mylogit1)
          rsevalue<-round(summ$aic,4)
          rsevalue1<-paste("AIC: ",rsevalue)
          rsevalue1       })
        output$AIC<-renderText({
          aict()  })
        FSIt<-eventReactive(input$getmodel, {
          mylogit1<-mylogit()
          summ<-summary(mylogit1)
          rsevalue<-summ$iter
          rsevalue1<-paste("Number of Fisher Scoring iterations:",rsevalue)
          rsevalue1 })
        output$FSI<-renderText({
          FSIt() })
        VIFT<-reactive({
          mylogit1<-mylogit()
          mm<-round(vif(mylogit1),3)
          Variable<-data.frame(vif(mylogit1))
          Variable<-row.names(Variable)
          Variable<-cbind(Variable,mm)
          Variable
        })
        VIFC<-eventReactive(input$getmodel, {
          VIFT()
        })
        output$VIF<-renderTable({
          VIFC()
        },bordered =T,
        caption = "VIF Table",
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL))
      } # AIC,FSI,VIF

    } # results


  } # Development Model
  {

    output$Rankord <- renderUI({
      if (is.null(input$dev) || input$dev=="m" || is.null(mylogit()))
        return(NULL)
      actionButton("Rankord", "Get Rank Ordering Table")})

    rnkorder<-reactive({
      op <- data.frame(get((input$dev)))
      mylogit1<-mylogit()
      op$prob<-predict.glm(mylogit1,op,type="response")
      decile <- function(x){
        deciles <- vector(length=10)
        for (i in seq(0.1,1,.1)){
          deciles[i*10] <- quantile(x, i, na.rm=T)
        }
        return (
          ifelse(x<deciles[1], 1,
                 ifelse(x<deciles[2], 2,
                        ifelse(x<deciles[3], 3,
                               ifelse(x<deciles[4], 4,
                                      ifelse(x<deciles[5], 5,
                                             ifelse(x<deciles[6], 6,
                                                    ifelse(x<deciles[7], 7,
                                                           ifelse(x<deciles[8], 8,
                                                                  ifelse(x<deciles[9], 9, 10
                                                                  ))))))))))
      }
      ROTable <- function(df, target, probability)
      {
        tmp <- df[, c(target,probability)]
        colnames(tmp)[1] = "Target"
        colnames(tmp)[2] = "prob"
        tmp$deciles<-decile(tmp$prob)
        mydata.DT = data.table(tmp) ## Converting the data frame to data table object
        ## Creating Aggregation and Group By similar to as in SQL
        Target_Rate = sum(mydata.DT$Target)/nrow(mydata.DT)
        rank <- mydata.DT[, list(
          min_prob = round(min(prob),3),
          max_prob = round(max(prob),3),
          cnt = length(Target),
          cnt_resp = sum(Target),
          cnt_non_resp = sum(Target == 0)
        ) ,
        by = deciles][order(-deciles)]
        rank$RRate <- rank$cnt_resp / rank$cnt ## computing response rate
        rank$cum_tot <- cumsum(rank$cnt) ## computing cum total customers
        rank$cum_resp <- cumsum(rank$cnt_resp) ## computing cum responders
        rank$cum_non_resp <-
          cumsum(rank$cnt_non_resp) ## computing cum non-responders
        rank$cum_RRate = rank$cum_resp / rank$cum_tot
        rank$cum_rel_resp <- rank$cum_resp / sum(rank$cnt_resp)
        rank$cum_rel_non_resp <- rank$cum_non_resp / sum(rank$cnt_non_resp)
        rank$ks <- rank$cum_rel_resp - rank$cum_rel_non_resp
        rank$lift <- round(rank$cum_RRate / Target_Rate,1)
        rank$RRate<-percent( rank$RRate)
        rank$cum_RRate<-percent( rank$cum_RRate)
        rank$cum_rel_resp<-percent(rank$cum_rel_resp)
        rank$cum_rel_non_resp<-percent(rank$cum_rel_non_resp)
        rank$ks <- percent( rank$ks)
        ## KS
        rank ## display Rank Ordering Table
      }
      names(op)[names(op)==input$Target]="Target"
      rot<- ROTable(op,"Target","prob")
      rot})
    rankorderr<-eventReactive(input$Rankord, {
      kp<-rnkorder()
      kp<-data.frame(kp)
      colnames(kp)<-c("Deciles","Min.prob","Max.prob","Cnt","Cnt.Resp",
                      "Cnt.Non.Resp","RRate","Cum.Tot","Cum.Resp",
                      "Cum.Non.Resp","Cum.RRate","Cum.Per.Resp",
                      "Cum.Per.Non.Resp","KS","Lift")
      kp})
    output$Rankordering<-DT::renderDataTable(
      rankorderr(),rownames= FALSE
      ,    options = list(
        lengthChange = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
          "}"),
        autowidth = TRUE,
        columnDefs = list(list(width = '100px', targets = "_all"))))

  } # RankOrdering Dev
  {

    output$measure <- renderUI({
      if (is.null(input$dev) || input$dev=="m" || is.null(mylogit()))
        return(NULL)
      actionButton("measure","Get All measure")
    })

    output$concob <- renderUI({
      if (is.null(input$dev) || input$dev=="m" || is.null(mylogit()))
        return(NULL)
      actionButton("concob","Get Concordance")
    })

    {## Gini,AUC,KS ##
      statT<-eventReactive(input$measure, {

        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")


        names(op)[names(op)==input$Target]="Target"
        library(ineq)
        gini=ineq(op$prob,type = "Gini")

        library(ROCR)
        pred=prediction(op$prob,op$Target)
        perf=performance(pred,"tpr","fpr")
        ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
        ks
        auc=performance(pred,"auc")
        auc=as.numeric(auc@y.values)
        auc
        op<-data.frame(statistics="GINI",value=gini*100)
        pp<-data.frame(statistics="AUC",value=auc*100)
        kp<-data.frame(statistics="KS",value=ks*100)

        df<-rbind(kp,pp,op)

        df$value<-round(df$value,2)
        df
      })

      output$stat<-renderTable({
        statT()

      },bordered =T,
      caption = "Model Statistics",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
    } # Gini,AUC,KS
    {## Concordance ##
      concotable<-reactive({
        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        concordance <- function(df, target, probability)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          concordance1 = function(y, yhat)
          {
            Con_Dis_Data = cbind(y, yhat)
            ones = Con_Dis_Data[Con_Dis_Data[, 1] == 1, ]
            zeros = Con_Dis_Data[Con_Dis_Data[, 1] == 0, ]
            conc = matrix(0, dim(zeros)[1], dim(ones)[1])
            disc = matrix(0, dim(zeros)[1], dim(ones)[1])
            ties = matrix(0, dim(zeros)[1], dim(ones)[1])
            for (j in 1:dim(zeros)[1])
            {
              for (i in 1:dim(ones)[1])
              {
                if (ones[i, 2] > zeros[j, 2])
                {
                  conc[j, i] = 1
                }
                else if (ones[i, 2] < zeros[j, 2])
                {
                  disc[j, i] = 1
                }
                else if (ones[i, 2] == zeros[j, 2])
                {
                  ties[j, i] = 1
                }}}
            Pairs = dim(zeros)[1] * dim(ones)[1]
            PercentConcordance = (sum(conc) / Pairs) * 100
            PercentDiscordance = (sum(disc) / Pairs) * 100
            PercentTied = (sum(ties) / Pairs) * 100
            return(
              list(
                "Percent Concordance" = PercentConcordance,
                "Percent Discordance" = PercentDiscordance,
                "Percent Tied" = PercentTied,
                "Pairs" = Pairs
              ))}
          concordance_output <- concordance1(tmp$Target, tmp$prob)
          concordance_output
        }
        names(op)[names(op)==input$Target]="Target"
        concordance(op,"Target","prob") })

      concordanceb<-eventReactive(input$concob, {
        concotable() })

      output$concordance<-renderTable({
        concordanceb()
      },bordered =T,caption = "Concordance",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
    } # Concordance
    {## Chi-Goodness of Fit ##
      chi1cal<-reactive({
        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }

        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)

          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          hl_gof}
        names(op)[names(op)==input$Target]="Target"
        kp<-data.table(t(hosmerlem_gof(op,"Target","prob")))
        kp })
      chi1b<-eventReactive(input$measure, {
        chi1cal() })

      output$chi1<-renderTable({
        chi1b()
      },bordered =T,
      caption = "Chi Sq - Goodness of Fit",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
      chi2cal<-reactive({
        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }

        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)

          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          sqldf ("select deciles, count(1) as cnt,
             sum (Target) as Obs_Resp, count (case when Target == 0 then 1 end) as Obs_Non_Resp,
             sum (prob) as Exp_Resp, sum (1 - prob) as Exp_Non_Resp
             from tmp
             group by deciles
             order by deciles desc")

        }
        names(op)[names(op)==input$Target]="Target"
        kp<-data.table(hosmerlem_gof(op,"Target","prob"))
        kp })
      chi2b<-eventReactive(input$measure, {
        chi2cal() })

      output$chi2<-renderTable({
        chi2b()
      },bordered =T,
      caption = "Chi-Sq Calculation",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
    } # Chi-Goodness of Fit

  } # All Measures Dev
  {

    output$validate <- renderUI({
      if (is.null(input$dev) || input$dev=="m" || is.null(mylogit()) ||
          is.null(input$val) || input$val=="m"  )
        return(NULL)
      actionButton("validate", "Validate the model ")
    })
    eqnusedt<-eventReactive(input$getmodel, {
      kp<-input$equation
      kp})
    output$eqnused<-renderText({
      eqnusedt()})
    sumtv<-eventReactive(input$validate, {
      kp<-paste0("VALIDATION MODEL SUMMARY")})
    output$Sumv<-renderText({
      sumtv()})
    mylogit2<-eventReactive(input$validate, {
      op <- data.frame(get((input$val)))
      equ<-input$equation
      mylogit1<-glm(formula = equ,family = "binomial",data = op)
      mylogit1
    })
    cofelogit2<-eventReactive(input$validate, {
      mylogit2<-mylogit2()
      summ<-summary(mylogit2)
      coeff<-data.table(summ$coefficients)
      coeff1<-data.frame(summ$coefficients)
      Coefficients<-row.names(coeff1)
      coeff<-cbind(Coefficients,coeff)
      coeff})
    betacal<-eventReactive(input$validate, {
      mylogit2<-mylogit2()
      summ<-summary(mylogit2)
      coeff<-data.table(summ$coefficients)
      coeff1<-data.frame(summ$coefficients)
      Coefficients<-row.names(coeff1)
      coeff<-cbind(Coefficients,coeff)
      coeff<-data.frame(coeff)
      coeff
      mylogit1<-mylogit()
      summd<-summary(mylogit1)
      coeffd<-data.table(summd$coefficients)
      coeff1d<-data.frame(summd$coefficients)
      Coefficients<-row.names(coeff1d)
      coeffd<-cbind(Coefficients,coeffd)
      coeffd<-data.frame(coeffd)
      coeffd<-coeffd[,1:2]
      coeffd<-merge(coeffd,coeff,by="Coefficients")
      coeffd<-coeffd[,1:3]
      colnames(coeffd)<-c("Coefficients","Estimate_dev","Estimate_val")
      coeffd$beta_ratio<-coeffd$Estimate_dev/coeffd$Estimate_val
      coeffd})
    output$betaratio<-renderTable({
      betacal()
    },bordered =T,
    caption = "Beta Ratio Test",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL))
    output$summaryval<-renderTable({
      cofelogit2()
    },bordered =T)


  } # Validation Model
  {

    output$Rankordv <- renderUI({
      if (is.null(input$dev) || input$dev=="m" || is.null(mylogit()) ||
          is.null(input$val) || input$val=="m"  )
        return(NULL)
      actionButton("Rankordv","Get Rank Ordering Table")
    })
    rnkorderv<-reactive({
      op <- data.frame(get((input$val)))
      mylogit1<-mylogit()
      op$prob<-predict.glm(mylogit1,op,type="response")
      decile <- function(x){
        deciles <- vector(length=10)
        for (i in seq(0.1,1,.1)){
          deciles[i*10] <- quantile(x, i, na.rm=T)
        }
        return (
          ifelse(x<deciles[1], 1,
                 ifelse(x<deciles[2], 2,
                        ifelse(x<deciles[3], 3,
                               ifelse(x<deciles[4], 4,
                                      ifelse(x<deciles[5], 5,
                                             ifelse(x<deciles[6], 6,
                                                    ifelse(x<deciles[7], 7,
                                                           ifelse(x<deciles[8], 8,
                                                                  ifelse(x<deciles[9], 9, 10
                                                                  ))))))))))
      }
      ROTable <- function(df, target, probability)
      {
        tmp <- df[, c(target,probability)]
        colnames(tmp)[1] = "Target"
        colnames(tmp)[2] = "prob"
        tmp$deciles<-decile(tmp$prob)
        mydata.DT = data.table(tmp) ## Converting the data frame to data table object
        ## Creating Aggregation and Group By similar to as in SQL
        Target_Rate = sum(mydata.DT$Target)/nrow(mydata.DT)
        rank <- mydata.DT[, list(
          min_prob = round(min(prob),3),
          max_prob = round(max(prob),3),
          cnt = length(Target),
          cnt_resp = sum(Target),
          cnt_non_resp = sum(Target == 0)
        ) ,
        by = deciles][order(-deciles)]
        rank$RRate <- rank$cnt_resp / rank$cnt ## computing response rate
        rank$cum_tot <- cumsum(rank$cnt) ## computing cum total customers
        rank$cum_resp <- cumsum(rank$cnt_resp) ## computing cum responders
        rank$cum_non_resp <-
          cumsum(rank$cnt_non_resp) ## computing cum non-responders
        rank$cum_RRate = rank$cum_resp / rank$cum_tot
        rank$cum_rel_resp <- rank$cum_resp / sum(rank$cnt_resp)
        rank$cum_rel_non_resp <- rank$cum_non_resp / sum(rank$cnt_non_resp)
        rank$ks <- rank$cum_rel_resp - rank$cum_rel_non_resp
        rank$lift <- round(rank$cum_RRate / Target_Rate,1)
        rank$RRate<-percent( rank$RRate)
        rank$cum_RRate<-percent( rank$cum_RRate)
        rank$cum_rel_resp<-percent(rank$cum_rel_resp)
        rank$cum_rel_non_resp<-percent(rank$cum_rel_non_resp)
        rank$ks <- percent( rank$ks)
        rank ## display Rank Ordering Table
      }
      names(op)[names(op)==input$Target]="Target"
      rot<- ROTable(op,"Target","prob")
      rot})
    rankorderrv<-eventReactive(input$Rankordv, {
      kp<- rnkorderv()
      kp<-data.frame(kp)
      colnames(kp)<-c("Deciles","Min.prob","Max.prob","cnt","cnt.Resp","cnt.Non.Resp","RRate","cum.Tot","cum.Resp",
                      "cum.Non.Resp","cum.RRate","cum.Per.Resp", "cum.Per.Non.Resp","KS","Lift")
      kp})
    output$Rankorderingv<-DT::renderDataTable(
      rankorderrv(),rownames= FALSE
      ,    options = list(
        lengthChange = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
          "}"),
        autowidth = "100%",
        columnDefs = list(list(width = '70%', targets = 1))))

  } # RankOrdering Val
  {

    output$Compr <- renderUI({
      if (is.null(input$dev) || input$dev=="m" || is.null(mylogit()) || is.null(input$val) || input$val=="m"  )
        return(NULL)
      actionButton("Compr","Get Comparison")
    })

    output$Comprconc <- renderUI({
      if (is.null(input$dev) || input$dev=="m" || is.null(mylogit()) || is.null(input$val) || input$val=="m"  )
        return(NULL)
      actionButton("Comprconc","Get Comparison Concordance")
    })

    {
      statT1<-eventReactive(input$Compr, {
        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        rtdev<- rnkorder()
        td<-data.frame(statistics="3rd Decile Capture",value=rtdev[3,12])
        lt<-data.frame(statistics="1st lift",value=rtdev[1,15])
        colnames(td)<-c("statistics","value")
        colnames(lt)<-c("statistics","value")
        names(op)[names(op)==input$Target]="Target"
        library(ineq)
        gini=ineq(op$prob,type = "Gini")
        library(ROCR)
        pred=prediction(op$prob,op$Target)
        perf=performance(pred,"tpr","fpr")
        ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
        ks
        auc=performance(pred,"auc")
        auc=as.numeric(auc@y.values)
        auc
        mp<-data.frame(statistics="GINI",value=gini*100)
        pp<-data.frame(statistics="AUC",value=auc*100)
        kp<-data.frame(statistics="KS",value=ks*100)
        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }
        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)
          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          hl_gof }
        names(op)[names(op)==input$Target]="Target"
        oo<-data.table(t(hosmerlem_gof(op,"Target","prob")))
        ch<-data.frame(statistics="X^2", value=oo$`X^2`)
        ch1<-data.frame(statistics="X^2 P(>Chi)", value=oo$`P(>Chi)`)
        df<-rbind(kp,pp,mp,ch,ch1)
        df$value<-round(df$value,3)
        df<-rbind(df,td,lt)
        df })
      statT2<-eventReactive(input$Compr, {
        op <- data.frame(get((input$val)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        rtval<-rnkorderv()
        td<-data.frame(statistics="3rd Decile Capture",value=rtval[3,12])
        lt<-data.frame(statistics="1st lift",value=rtval[1,15])
        colnames(td)<-c("statistics","value")
        colnames(lt)<-c("statistics","value")
        names(op)[names(op)==input$Target]="Target"
        library(ineq)
        gini=ineq(op$prob,type = "Gini")
        library(ROCR)
        pred=prediction(op$prob,op$Target)
        perf=performance(pred,"tpr","fpr")
        ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
        ks
        auc=performance(pred,"auc")
        auc=as.numeric(auc@y.values)
        auc
        mp<-data.frame(statistics="GINI",value=gini*100)
        pp<-data.frame(statistics="AUC",value=auc*100)
        kp<-data.frame(statistics="KS",value=ks*100)
        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }
        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)
          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          hl_gof }
        names(op)[names(op)==input$Target]="Target"
        oo<-data.table(t(hosmerlem_gof(op,"Target","prob")))
        ch<-data.frame(statistics="X^2", value=oo$`X^2`)
        ch1<-data.frame(statistics="X^2 P(>Chi)", value=oo$`P(>Chi)`)
        df<-rbind(kp,pp,mp,ch,ch1)
        df$value<-round(df$value,3)
        df<-rbind(df,td,lt)
        df})
      merg<-eventReactive(input$Compr, {
        tb1<-statT1()
        tb2<-statT2()
        tb3<-merge(tb1,tb2,by="statistics")
        colnames(tb3)<-c("Statistics","Development","Validation")
        tb3<-tb3[c(5,2,1,4,6,7,3),]
        tb3})
      output$stat2<-renderTable({
        merg()
      },bordered =T,
      caption = "Model Statistics",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
    } # comparison table
    {
      concordancebd<-eventReactive(input$Comprconc, {
        concotable()
      })
      output$concordanced<-renderTable({
        concordancebd()
      },bordered =T,caption = "Concordance (Development)",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))

      concotablev<-reactive({
        op <- data.frame(get((input$val)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        concordance <- function(df, target, probability)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          concordance1 = function(y, yhat)
          {
            Con_Dis_Data = cbind(y, yhat)
            ones = Con_Dis_Data[Con_Dis_Data[, 1] == 1, ]
            zeros = Con_Dis_Data[Con_Dis_Data[, 1] == 0, ]
            conc = matrix(0, dim(zeros)[1], dim(ones)[1])
            disc = matrix(0, dim(zeros)[1], dim(ones)[1])
            ties = matrix(0, dim(zeros)[1], dim(ones)[1])
            for (j in 1:dim(zeros)[1])
            {
              for (i in 1:dim(ones)[1])
              {
                if (ones[i, 2] > zeros[j, 2])
                {
                  conc[j, i] = 1
                }
                else if (ones[i, 2] < zeros[j, 2])
                {
                  disc[j, i] = 1
                }
                else if (ones[i, 2] == zeros[j, 2])
                {
                  ties[j, i] = 1
                }}}
            Pairs = dim(zeros)[1] * dim(ones)[1]
            PercentConcordance = (sum(conc) / Pairs) * 100
            PercentDiscordance = (sum(disc) / Pairs) * 100
            PercentTied = (sum(ties) / Pairs) * 100
            return(
              list(
                "Percent Concordance" = PercentConcordance,
                "Percent Discordance" = PercentDiscordance,
                "Percent Tied" = PercentTied,
                "Pairs" = Pairs
              ))}
          concordance_output <- concordance1(tmp$Target, tmp$prob)
          concordance_output }
        names(op)[names(op)==input$Target]="Target"
        concordance(op,"Target","prob") })
      concordancebv<-eventReactive(input$Comprconc, {
        concotablev() })
      output$concordancev<-renderTable({
        concordancebv()
      },bordered =T,caption = "Concordance (Validation)",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))

    } # Concordance table



  } # ModelMeasures Val
  {

    output$Rankordh <- renderUI({
      if (is.null(input$dev) || input$dev=="m" || is.null(mylogit()) ||
          is.null(input$val) || input$val=="m" || is.null(input$Holdout) || input$Holdout=="m"  )
        return(NULL)
      actionButton("Rankordh","Get Rank Ordering Table") })
    rnkorderh<-reactive({
      op <- data.frame(get((input$Holdout)))
      mylogit1<-mylogit()
      op$prob<-predict.glm(mylogit1,op,type="response")
      decile <- function(x){
        deciles <- vector(length=10)
        for (i in seq(0.1,1,.1)){
          deciles[i*10] <- quantile(x, i, na.rm=T)
        }
        return (
          ifelse(x<deciles[1], 1,
                 ifelse(x<deciles[2], 2,
                        ifelse(x<deciles[3], 3,
                               ifelse(x<deciles[4], 4,
                                      ifelse(x<deciles[5], 5,
                                             ifelse(x<deciles[6], 6,
                                                    ifelse(x<deciles[7], 7,
                                                           ifelse(x<deciles[8], 8,
                                                                  ifelse(x<deciles[9], 9, 10
                                                                  ))))))))))
      }
      ROTable <- function(df, target, probability)
      {
        tmp <- df[, c(target,probability)]
        colnames(tmp)[1] = "Target"
        colnames(tmp)[2] = "prob"
        tmp$deciles<-decile(tmp$prob)
        mydata.DT = data.table(tmp) ## Converting the data frame to data table object
        ## Creating Aggregation and Group By similar to as in SQL
        Target_Rate = sum(mydata.DT$Target)/nrow(mydata.DT)
        rank <- mydata.DT[, list(
          min_prob = round(min(prob),3),
          max_prob = round(max(prob),3),
          cnt = length(Target),
          cnt_resp = sum(Target),
          cnt_non_resp = sum(Target == 0)
        ) ,
        by = deciles][order(-deciles)]
        rank$RRate <- rank$cnt_resp / rank$cnt ## computing response rate
        rank$cum_tot <- cumsum(rank$cnt) ## computing cum total customers
        rank$cum_resp <- cumsum(rank$cnt_resp) ## computing cum responders
        rank$cum_non_resp <-
          cumsum(rank$cnt_non_resp) ## computing cum non-responders
        rank$cum_RRate = rank$cum_resp / rank$cum_tot
        rank$cum_rel_resp <- rank$cum_resp / sum(rank$cnt_resp)
        rank$cum_rel_non_resp <- rank$cum_non_resp / sum(rank$cnt_non_resp)
        rank$ks <- rank$cum_rel_resp - rank$cum_rel_non_resp
        rank$lift <- round(rank$cum_RRate / Target_Rate,1)
        rank$RRate<-percent( rank$RRate)
        rank$cum_RRate<-percent( rank$cum_RRate)
        rank$cum_rel_resp<-percent(rank$cum_rel_resp)
        rank$cum_rel_non_resp<-percent(rank$cum_rel_non_resp)
        rank$ks <- percent( rank$ks)
        rank ## display Rank Ordering Table
      }
      names(op)[names(op)==input$Target]="Target"
      rot<- ROTable(op,"Target","prob")
      rot })
    rankorderrh<-eventReactive(input$Rankordh, {
      kp<- rnkorderh()
      kp<-data.frame(kp)
      colnames(kp)<-c("Deciles","Min.prob","Max.prob","cnt","cnt.Resp","cnt.Non.Resp","RRate","cum.Tot","cum.Resp",
                      "cum.Non.Resp","cum.RRate","cum.Per.Resp", "cum.Per.Non.Resp","KS","Lift")
      kp })
    output$Rankorderingh<-DT::renderDataTable(
      rankorderrh(),rownames= FALSE
      ,    options = list(
        lengthChange = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
          "}"),
        autowidth = "100%",
        columnDefs = list(list(width = '70%', targets = 1))))

  } # Validation Model on Holdout
  {

    output$Comprvh <- renderUI({
      if (is.null(input$dev) || input$dev=="m" || is.null(mylogit()) || is.null(input$val)
          || input$val=="m" || is.null(input$Holdout) || input$Holdout=="m"  )
        return(NULL)
      actionButton("Comprvh", "Get Comparison")
    })
    output$Comprconcvh <- renderUI({
      if (is.null(input$dev) || input$dev=="m" || is.null(mylogit()) || is.null(input$val)
          || input$val=="m" || is.null(input$Holdout) || input$Holdout=="m"  )
        return(NULL)
      actionButton("Comprconcvh", "Get Comparison Concordance")
    })

    {
      statT1h<-eventReactive(input$Comprvh, {
        op <- data.frame(get((input$dev)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        rtdev<- rnkorder()
        td<-data.frame(statistics="3rd Decile Capture",value=rtdev[3,12])
        lt<-data.frame(statistics="1st lift",value=rtdev[1,15])
        colnames(td)<-c("statistics","value")
        colnames(lt)<-c("statistics","value")
        names(op)[names(op)==input$Target]="Target"
        library(ineq)
        gini=ineq(op$prob,type = "Gini")
        library(ROCR)
        pred=prediction(op$prob,op$Target)
        perf=performance(pred,"tpr","fpr")
        ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
        ks
        auc=performance(pred,"auc")
        auc=as.numeric(auc@y.values)
        auc
        mp<-data.frame(statistics="GINI",value=gini*100)
        pp<-data.frame(statistics="AUC",value=auc*100)
        kp<-data.frame(statistics="KS",value=ks*100)
        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }

        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)

          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          hl_gof
        }
        names(op)[names(op)==input$Target]="Target"
        oo<-data.table(t(hosmerlem_gof(op,"Target","prob")))
        ch<-data.frame(statistics="X^2", value=oo$`X^2`)
        ch1<-data.frame(statistics="X^2 P(>Chi)", value=oo$`P(>Chi)`)
        df<-rbind(kp,pp,mp,ch,ch1)
        df$value<-round(df$value,3)
        df<-rbind(df,td,lt)
        df
      })
      statT2h<-eventReactive(input$Comprvh, {
        op <- data.frame(get((input$val)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        rtval<-rnkorderv()
        td<-data.frame(statistics="3rd Decile Capture",value=rtval[3,12])
        lt<-data.frame(statistics="1st lift",value=rtval[1,15])
        colnames(td)<-c("statistics","value")
        colnames(lt)<-c("statistics","value")
        names(op)[names(op)==input$Target]="Target"
        library(ineq)
        gini=ineq(op$prob,type = "Gini")

        library(ROCR)
        pred=prediction(op$prob,op$Target)
        perf=performance(pred,"tpr","fpr")
        ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
        ks
        auc=performance(pred,"auc")
        auc=as.numeric(auc@y.values)
        auc
        mp<-data.frame(statistics="GINI",value=gini*100)
        pp<-data.frame(statistics="AUC",value=auc*100)
        kp<-data.frame(statistics="KS",value=ks*100)

        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }

        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)

          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          hl_gof
        }
        names(op)[names(op)==input$Target]="Target"
        oo<-data.table(t(hosmerlem_gof(op,"Target","prob")))
        ch<-data.frame(statistics="X^2", value=oo$`X^2`)
        ch1<-data.frame(statistics="X^2 P(>Chi)", value=oo$`P(>Chi)`)
        df<-rbind(kp,pp,mp,ch,ch1)
        df$value<-round(df$value,3)
        df<-rbind(df,td,lt)
        df
      })
      statT3h<-eventReactive(input$Comprvh, {
        op <- data.frame(get((input$Holdout)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        rtval<-rnkorderh()
        td<-data.frame(statistics="3rd Decile Capture",value=rtval[3,12])
        lt<-data.frame(statistics="1st lift",value=rtval[1,15])
        colnames(td)<-c("statistics","value")
        colnames(lt)<-c("statistics","value")
        names(op)[names(op)==input$Target]="Target"
        library(ineq)
        gini=ineq(op$prob,type = "Gini")
        library(ROCR)
        pred=prediction(op$prob,op$Target)
        perf=performance(pred,"tpr","fpr")
        ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
        ks
        auc=performance(pred,"auc")
        auc=as.numeric(auc@y.values)
        auc
        mp<-data.frame(statistics="GINI",value=gini*100)
        pp<-data.frame(statistics="AUC",value=auc*100)
        kp<-data.frame(statistics="KS",value=ks*100)
        decile <- function(x){
          deciles <- vector(length=10)
          for (i in seq(0.1,1,.1)){
            deciles[i*10] <- quantile(x, i, na.rm=T)
          }
          return (
            ifelse(x<deciles[1], 1,
                   ifelse(x<deciles[2], 2,
                          ifelse(x<deciles[3], 3,
                                 ifelse(x<deciles[4], 4,
                                        ifelse(x<deciles[5], 5,
                                               ifelse(x<deciles[6], 6,
                                                      ifelse(x<deciles[7], 7,
                                                             ifelse(x<deciles[8], 8,
                                                                    ifelse(x<deciles[9], 9, 10
                                                                    ))))))))))
        }

        hosmerlem_gof <- function(df, target, probability,g=10)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          tmp$deciles<-decile(tmp$prob)
          hosmerlem <-
            function (y, yhat, g1=g) {
              cutyhat <-
                cut(yhat,
                    breaks = quantile(yhat, probs = seq(0, 1, 1 / g1)),
                    include.lowest = T)
              obs <-xtabs(cbind(1 - y, y) ~ cutyhat)
              expect <-xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
              chisq <-sum((obs - expect) ^ 2 / expect)
              P <-1 - pchisq(chisq, g1 - 2)
              c("X^2" = chisq,Df = g1 - 2,"P(>Chi)" = P)
            }
          hl_gof <- hosmerlem(tmp$Target, tmp$prob)
          hl_gof
        }
        names(op)[names(op)==input$Target]="Target"
        oo<-data.table(t(hosmerlem_gof(op,"Target","prob")))
        ch<-data.frame(statistics="X^2", value=oo$`X^2`)
        ch1<-data.frame(statistics="X^2 P(>Chi)", value=oo$`P(>Chi)`)
        df<-rbind(kp,pp,mp,ch,ch1)
        df$value<-round(df$value,3)
        df<-rbind(df,td,lt)
        df })
      mergh<-eventReactive(input$Comprvh, {
        tb1<-statT1h()
        tb2<-statT2h()
        tb3<-statT3h()
        tb4<-merge(tb1,tb2,by="statistics")
        tb4<-merge(tb4,tb3,by="statistics")

        colnames(tb4)<-c("Statistics","Development","Validation","Holdout")
        tb4<-tb4[c(5,2,1,4,6,7,3),]
        tb4
      })
      output$stat2vh<-renderTable({
        mergh()
      },bordered =T,
      caption = "Model Statistics",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
    } # Comparison table
    {
      concordancebdh<-eventReactive(input$Comprconcvh, {
        concotable() })
      output$concordancedh<-renderTable({
        concordancebdh()
      },bordered =T,caption = "Concordance (Development)",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
      concordancebdh1<-eventReactive(input$Comprconcvh, {
        concotablev() })
      output$concordancedh123<-renderTable({
        concordancebdh1()
      },bordered =T,caption = "Concordance (Validation)",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
      concotablevh<-reactive({
        op <- data.frame(get((input$Holdout)))
        mylogit1<-mylogit()
        op$prob<-predict.glm(mylogit1,op,type="response")
        concordance <- function(df, target, probability)
        {
          tmp <- df[, c(target, probability)]
          colnames(tmp)[1] = "Target"
          colnames(tmp)[2] = "prob"
          concordance1 = function(y, yhat)
          {
            Con_Dis_Data = cbind(y, yhat)
            ones = Con_Dis_Data[Con_Dis_Data[, 1] == 1, ]
            zeros = Con_Dis_Data[Con_Dis_Data[, 1] == 0, ]
            conc = matrix(0, dim(zeros)[1], dim(ones)[1])
            disc = matrix(0, dim(zeros)[1], dim(ones)[1])
            ties = matrix(0, dim(zeros)[1], dim(ones)[1])
            for (j in 1:dim(zeros)[1])
            {
              for (i in 1:dim(ones)[1])
              {
                if (ones[i, 2] > zeros[j, 2])
                {
                  conc[j, i] = 1
                }
                else if (ones[i, 2] < zeros[j, 2])
                {
                  disc[j, i] = 1
                }
                else if (ones[i, 2] == zeros[j, 2])
                {
                  ties[j, i] = 1
                }
              }
            }
            Pairs = dim(zeros)[1] * dim(ones)[1]
            PercentConcordance = (sum(conc) / Pairs) * 100
            PercentDiscordance = (sum(disc) / Pairs) * 100
            PercentTied = (sum(ties) / Pairs) * 100
            return(
              list(
                "Percent Concordance" = PercentConcordance,
                "Percent Discordance" = PercentDiscordance,
                "Percent Tied" = PercentTied,
                "Pairs" = Pairs
              ))}
          concordance_output <- concordance1(tmp$Target, tmp$prob)
          concordance_output
        }
        names(op)[names(op)==input$Target]="Target"
        concordance(op,"Target","prob")
      })

      concordancebvh<-eventReactive(input$Comprconcvh, {
        concotablevh()
      })
      output$concordancevh<-renderTable({
        concordancebvh()
      },bordered =T,caption = "Concordance (Holdout)",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL))
    } # Concordance

  } # ModelMeasures Holdout
  {


    { information_value<-reactiveValues()
    p_value<-reactiveValues()
    Plot_data_varclus<-reactiveValues()
    glmmodel <- reactiveValues()
    vif_table<-reactiveValues()
    rank_order_development<-reactiveValues()
    model_stat<-reactiveValues()
    chi_sq<-reactiveValues()
    chi_sq_calculation<-reactiveValues()
    concordance_dev<-reactiveValues()
    beta_ratio_table_val<-reactiveValues()
    rank_order_validation<-reactiveValues()
    comparison_table<-reactiveValues()
    concordance_val<-reactiveValues()
    rank_order_holdout<-reactiveValues()
    comparison_table_holdout<-reactiveValues()
    concordance_holdout<-reactiveValues()
    } # reactive values
    {
      observe({
        if(is.null(ntextivv()))
          isolate(
            information_value <<- NULL
          )
        if(!is.null(ntextivv()))
          isolate(
            information_value <<- ntextivv()
          )
      })


      observe({
        if(is.null(ntextp()))
          isolate(
            p_value <<- NULL
          )
        if(!is.null(ntextp()))
          isolate(
            p_value <<- ntextp()
          )
      })


      observe({
        if(is.null(ntextpx()))
          isolate(
            Plot_data_varclus <<- NULL
          )

        if(!is.null(ntextpx()))
          isolate(
            Plot_data_varclus <<- ntextpx()
          )
      })


      observe({
        if(is.null(mylogit()))
          isolate(
            glmmodel <<- NULL
          )

        if(!is.null(mylogit()))
          isolate(
            glmmodel <<- mylogit()
          )
      })


      observe({

        if(is.null(VIFC()))
          isolate(
            vif_table <<- NULL
          )

        if(!is.null(VIFC()))
          isolate(
            vif_table <<- VIFC()
          )
      })


      observe({

        if(is.null(rankorderr()))
          isolate(
            rank_order_development <<-NULL
          )

        if(!is.null(rankorderr()))
          isolate(
            rank_order_development <<- rankorderr()
          )
      })


      observe({
        if(is.null(statT()))
          isolate(
            model_stat <<- NULL
          )

        if(!is.null(statT()))
          isolate(
            model_stat <<- statT()
          )
      })
      observe({
        if(is.null(chi1b()))
          isolate(
            chi_sq <<- NULL
          )

        if(!is.null(chi1b()))
          isolate(
            chi_sq <<- statT()
          )
      })
      observe({

        if(is.null(chi2b()))
          isolate(
            chi_sq_calculation <<- NULL
          )


        if(!is.null(chi2b()))
          isolate(
            chi_sq_calculation <<- chi2b()
          )
      })


      observe({
        if(is.null(concordanceb()))
          isolate(
            concordance_dev <<- NULL
          )

        if(!is.null(concordanceb()))
          isolate(
            concordance_dev <<- concordanceb()
          )
      })


      observe({
        if(is.null(betacal()))
          isolate(
            beta_ratio_table_val <<- NULL
          )

        if(!is.null(betacal()))
          isolate(
            beta_ratio_table_val <<- betacal()
          )
      })



      observe({
        if(is.null(rankorderrv()))
          isolate(
            rank_order_validation <<- NULL
          )

        if(!is.null(rankorderrv()))
          isolate(
            rank_order_validation <<- rankorderrv()
          )
      })


      observe({
        if(is.null(merg()))
          isolate(
            comparison_table <<- NULL
          )
        if(!is.null(merg()))
          isolate(
            comparison_table <<- merg()
          )

      })


      observe({
        if(is.null(concordancebv()))
          isolate(
            concordance_val <<-NULL
          )

        if(!is.null(concordancebv()))
          isolate(
            concordance_val <<- concordancebv()
          )
      })
      observe({
        if(is.null(rankorderrh()))
          isolate(
            rank_order_holdout <<-NULL
          )

        if(!is.null(rankorderrh()))
          isolate(
            rank_order_holdout <<- rankorderrh()
          )
      })
      observe({
        if(is.null(mergh()))
          isolate(
            comparison_table_holdout <<-NULL
          )

        if(!is.null(mergh()))
          isolate(
            comparison_table_holdout <<- mergh()
          )
      })
      observe({
        if(is.null(concordancebvh()))
          isolate(
            concordance_holdout <<-NULL
          )

        if(!is.null(concordancebvh()))
          isolate(
            concordance_holdout <<- concordancebvh()
          )
      })

      documentation1<-reactive({
        pp<-input$documentation
        pp
      })

      documentation<-reactiveValues()

      observe({
        if(is.null(documentation1()))
          isolate(
            documentation <<- NULL
          )

        if(!is.null(documentation1()))
          isolate(
            documentation <<- documentation1()
          )
      })
    } # observe

    output$downloadData <- downloadHandler(
      filename <- function(){
        paste("All.RData")
      },

      content = function(file) {
        save( information_value,p_value,Plot_data_varclus,glmmodel , vif_table,
              rank_order_development, model_stat,chi_sq,chi_sq_calculation,
              concordance_dev,beta_ratio_table_val,
              rank_order_validation, comparison_table,concordance_val,
              rank_order_holdout, comparison_table_holdout,concordance_holdout,documentation,
              file = file)
      })


  } # Save Objects

  })

shinyApp(ui, server)
}
