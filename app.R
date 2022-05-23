library(shiny)
library(DT)
library(shinyjs)

caLangJP <- FALSE
panelTitle <- "Curator Agent - R version 0"
if (caLangJP){
    panelTitle <- "キュレータエージェント - R版 試作品 0" 
}
source("mexFunctions.R")
dname <- names(MeXds)[5:11]
dnameL <- as.list(seq(dname))
names(dnameL) <- dname
dnameJ<- c("温度","湿度","気圧","明るさ","人数","人の動き","音量")
interval <- c(60, 60*60, 60*60*24, 60*60*24*30)
funcs <-c(mean, max, min, sum)
funname<-c("mean","max","min","sum")


# Define UI for random distribution app ----
ui <- fluidPage(
  useShinyjs(),
  # App title ----
  titlePanel(panelTitle),

  # Sidebar layout with  output definitions ----
  sidebarLayout(
    sidebarPanel(
      ##column(12,
        ## Data List
        h2("Datas"),
        DT::dataTableOutput("datalist"),
        ## # br() element to introduce extra vertical spacing ----
        ##br(),
        h2("Location of sensor node"),
        plotOutput("nodemap"),
        actionButton("clearKnowledge", label = "Clear Knowledges")
    ),

    # Main panel for input and displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel(h2("Get Explored"), 
          column(8,
            h2("Guide for Explore"),
            tags$iframe(src="Guide.html", width="1800", height="600"),
            h2("Example Misson and Scenario"),
            tags$iframe(src="Mission_Scenario.html", width="1800", height="600")
          )
        ),
        tabPanel(h2("Collect"), 
          h2("Visualize & Finding"),
          hr(),
          column(4, 
            # Input: Select data ----
            selectInput("dataname", label = h3("Select Data"), 
              choices = dnameL, selected = 1),
            # Input: Select Node ----
            checkboxGroupInput("node", label = h3("Tartget Node"), 
              inline=TRUE, choices = as.list(Node$No), selected = Node$No),
            # Input: Date Rangee ----
            dateRangeInput("daterange", label = h3("Date range"), 
              start="2021-07-03", end="2021-09-30"),
            # Input : Interval
            radioButtons("interval", label = h3("Summarizing interval"), 
              inline=TRUE, choices = list("60 seconds" = 1, "60 minutes" = 2, "24 hours" = 3, "30 days"=4), selected = 2),
            # Input : Function
            radioButtons("funname", label = h3("Summarizing Function"), 
              inline=TRUE, choices = list("Mean" = 1, "Max" = 2, "Min" = 3, "Sum"=4), selected = 2)
          ),
          column(8,
            textAreaInput("finding", label=h3("Finding:"), cols=600, rows=3),
            actionButton("recfinding", label = "Rec Finding"),
            hr(),
            plotOutput("plot"),
            hr(),
            DT::dataTableOutput("table")
          )
        ),
        tabPanel(h2("Explore"), 
          h2("Learn from Findings"),
          hr(),
          fluidRow(
            column(4, 
              h3("Select Findings"),
              dataTableOutput("fdlist")
            ),
            column(8, 
              textAreaInput("learning", label=h3("Learning:"), cols=600, rows=3),
              actionButton("recLearning", label = "Rec Learning"),
              hr(),
              h3("Findings selected from the list"),
              uiOutput("fdcontent")
            )
          )
        ),
        tabPanel(h2("Create"), 
          h2("Create ideas from learning"),
          hr(),
          fluidRow(
            column(4, 
              h3("Select Learning"),
              dataTableOutput('lnglist'), ##--
              hr(),
              h3("Findings of Learnings"),
              dataTableOutput('lngfdlist')              
            ),
            column(8, 
              textAreaInput("idea", label=h3("Improvement plan:"), cols=600, rows=3), 
              actionButton("recIdea", label = "Rec Idea"),
              hr(),
              h3("Findings that are the basis of learning"),
              uiOutput("lngfdcontent") ##--
            )
          )
        ),
        tabPanel(h2("Donate"), 
          h2("Browse Ideas, Learning, Findings as knowledge "),
          fluidRow(
            column(4, 
              h3("Select Created ideas"),
              dataTableOutput('idealist'), ##--
              hr(),
              h3("Select Learning that was the basis of the idea"),
              dataTableOutput('idlnglist'), 
              hr(),
              h3("Findings that are the basis of learning"),
              dataTableOutput('idlngfdlist'), ##--
              hr(),
              downloadButton('downloadPlan', "Generate Improvement Plan Sheet")
            ),
            column(8, 
              h3("Findings that are the basis of learning"),
              uiOutput("idlngfdcontent") ##--
            )
          )
        )  
      ) 
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  ## Side Panel
  output$datalist <- renderDataTable({DataListJP}, selection="none")
  output$nodemap <- renderPlot({
    old.par <- par(no.readonly = TRUE)
    par(plt=c(0,1,0,1))
    plot( 0,0, xlim=c(0,1817), ylim=c(817, 0), type="n", asp=1)
    rasterImage(sensorNodeMap, 0, 817, 1817, 0)
    par(old.par)
  })

  observeEvent(input$clearKnowledge, {
    Findings<- read_rds("Findings_init.rds")
    fdParams <- read_rds("fdParams_init.rds")
    Learnings<- read_rds("Learnings_init.rds")
    Learn_Find<- read_rds("Learn_Find_init.rds")
    ImprovementIdea<- read_rds("ImprovementIdea_init.rds")
    write_rds(Findings, file = "Findings.rds")
    write_rds(fdParams, file = "fdParams.rds")
    write_rds(Learnings, file = "Learnings.rds")
    write_rds(Learn_Find, file = "Learn_Find.rds")
    write_rds(ImprovementIdea, file = "ImprovementIdea.rds")
    refresh()
  })

  ## ui tab : Tutorial 
  ## No operation is needed on the server.

  ## ui tab : Collect
  output$plot <- renderPlot({
    mexPlot(y=dname[eval(parse(text=input$dataname))], interval=interval[eval(parse(text=input$interval))], f=funname[eval(parse(text=input$funname))], node=as.integer(input$node), fdt=input$daterange[1], tdt=input$daterange[2])
  })
  
  output$table <- renderDataTable({
    DT::datatable(mexSum(interval=interval[eval(parse(text=input$interval))], f=funname[eval(parse(text=input$funname))], fdt=input$daterange[1], tdt=input$daterange[2]), selection="none") %>% formatDate(c(2), method="toLocaleString",params=list("se", list(timeZone="UTC")))
  })

  observeEvent(input$recfinding, {
    params <- list(x="time", y=dname[eval(parse(text=input$dataname))], interval=interval[eval(parse(text=input$interval))], f=funname[eval(parse(text=input$funname))], node=as.integer(input$node), fdt=input$daterange[1], tdt=input$daterange[2])
    Findings <- read_rds("Findings.rds")
    fdParams <- read_rds("fdParams.rds")
    Findings <- rbind(Findings, data_frame(time=as.POSIXct(Sys.time(),tz="Asia/Tokyo"), finding=input$finding))
    fdParams <- c(fdParams, list(params))
    write_rds(Findings, file = "Findings.rds")
    write_rds(fdParams, file = "fdParams.rds")
    output$fdlist <-  DT::renderDataTable(DT::datatable(Findings) %>% formatDate(c(1), method="toLocaleString",params=list("se", list(timeZone="Asia/Tokyo"))), server=FALSE)
  })

  ## ui tab : Explore
  output$fdlist <-  DT::renderDataTable({
    Findings <- read_rds("Findings.rds")
    if (length(Findings) > 0){
      DT::datatable(Findings) %>% formatDate(c(1), method="toLocaleString",params=list("se", list(timeZone="UTC")))
    }
  }
  , server=FALSE)

  output$fdcontent <- renderUI({
    fd_output_list <- lapply(input$fdlist_rows_selected, function(s) {
      ftxttag <- paste("ftxt", s, sep="")
      fplttag <- paste("fplt", s, sep="")
      fluidRow(
        hr(),
        textOutput(ftxttag),
        plotOutput(fplttag, height = 420, width = 750)
      )
      })
    do.call(tagList, fd_output_list)
  })

  observe({
    if (!is.null(input$fdlist_rows_selected)){
      s <- input$fdlist_rows_selected
      Findings <- read_rds("Findings.rds")
      fdParams <- read_rds("fdParams.rds")
      for (.j in seq(length(s))){
        local({
          j <- .j
          i <- s[j]
          fdg <- Findings[i,]
          param <- fdParams[[i]]
          ftxttag <- paste("ftxt", i, sep="")
          fplttag <- paste("fplt", i, sep="")
          output[[ftxttag]] <- renderText({paste("Finding:", fdg$finding)})
          output[[fplttag]] <- renderPlot({mexPlot(y=param$y, interval=param$interval, f=param$f, node=param$node, fdt=param$fdt, tdt=param$tdt)})
        })
      }
    }
  })  
    
observeEvent(input$recLearning, {
  if (length(input$fdlist_rows_selected) > 0){
    Findings <- read_rds("Findings.rds")
    Learnings <- read_rds("Learnings.rds")
    Learn_Find <- read_rds("Learn_Find.rds")
    Learnings <- rbind(Learnings, data.frame(time=as.POSIXct(Sys.time(),tz="Asia/Tokyo"), learning=input$learning))
    Learn_Find <- c(Learn_Find, list(input$fdlist_rows_selected))
    write_rds(Learnings, file = "Learnings.rds")
    write_rds(Learn_Find, file = "Learn_Find.rds")
    output$lnglist <-  DT::renderDataTable(Learnings, server=FALSE,  selection = 'single')
  }
})

## ui tab : create

output$lnglist <- local({
  Learnings <- read_rds("Learnings.rds")
  if (length(Learnings) > 0){
    DT::renderDataTable(DT::datatable(Learnings,  selection = 'single') %>% formatDate(c(1), method="toLocaleString",params=list("se", list(timeZone="Asia/Tokyo"))), server=FALSE)
  }
})


observeEvent(input$recIdea, {
    ImprovementIdea <- read_rds("ImprovementIdea.rds")
    Learn_Find <- read_rds("Learn_Find.rds")
    lf <- Learn_Find[[as.integer(input$lnglist_rows_selected)]]
    ImprovementIdea <- rbind(ImprovementIdea, data.frame(time=as.POSIXct(Sys.time(), tz="Asia/Tokyo"), idea=input$idea, learning=input$lnglist_rows_selected)) 
    write_rds(ImprovementIdea, file = "ImprovementIdea.rds")
    output$idealist <-  DT::renderDataTable(ImprovementIdea, server=FALSE,  selection = 'single')
  })


output$lngfdcontent <- renderUI({
  if (!is.null(input$lnglist_rows_selected)){
    Learn_Find <- read_rds("Learn_Find.rds")
    lf <- Learn_Find[[as.integer(input$lnglist_rows_selected)]]
    lng_output_list <- lapply(lf, function(s) {
      ltxttag <- paste("ltxt", s, sep="")
      lplttag <- paste("lplt", s, sep="")
      fluidRow(
        hr(),
        textOutput(ltxttag),
        plotOutput(lplttag, height = 420, width = 750)
      )
    })
    do.call(tagList, lng_output_list)
  }
  })


observe({
  if (!is.null(input$lnglist_rows_selected)){
    Learn_Find <- read_rds("Learn_Find.rds")
    fd <- Learn_Find[[as.integer(input$lnglist_rows_selected)]]
    Findings <- read_rds("Findings.rds")
    fdParams <- read_rds("fdParams.rds")
    output$lngfdlist <- DT::renderDataTable(DT::datatable(Findings[fd,], selection="none") %>% formatDate(c(1), method="toLocaleString",params=list("se", list(timeZone="Asia/Tokyo"))), server=FALSE) 
    for (.j in seq(length(fd))){
      local({
        j <- .j
        i <- fd[j]
        fdg <- Findings[i,]
        param <- fdParams[[i]]
        ltxttag <- paste("ltxt", i, sep="")
        lplttag <- paste("lplt", i, sep="")
        output[[ltxttag]] <- renderText({paste("Finding:", fdg$finding)})
        output[[lplttag]] <- renderPlot({mexPlot(y=param$y, interval=param$interval, f=param$f, node=param$node, fdt=param$fdt, tdt=param$tdt)})
      })
    }
  }
  })  

## ui tab : Browse Knowledge
output$idealist <- local({
  ImprovementIdea <- read_rds("ImprovementIdea.rds")
  if (length(ImprovementIdea) > 0){
    DT::renderDataTable(DT::datatable(ImprovementIdea[,c(1,2)],  selection = 'single') %>% formatDate(c(1), method="toLocaleString",params=list("se", list(timeZone="Asia/Tokyo"))), server=FALSE)
  }
})

observe({
  if (!is.null(input$idealist_rows_selected)){
    ImprovementIdea <- read_rds("ImprovementIdea.rds")
    Learnings <- read_rds("Learnings.rds")
    lngs <-Learnings[ImprovementIdea[input$idealist_rows_selected,]$learning,]
    if (length(Learnings) > 0){
        output$idlnglist <- DT::renderDataTable(DT::datatable(lngs,  selection = 'none') %>% formatDate(c(1), method="toLocaleString",params=list("se", list(timeZone="Asia/Tokyo"))), server=FALSE)
    }
  }
})


output$idlngfdcontent <- renderUI({
  if (!is.null(input$idealist_rows_selected)){
    ImprovementIdea <- read_rds("ImprovementIdea.rds")
    lngidx <-ImprovementIdea[input$idealist_rows_selected[1],]$learning
    Learn_Find <- read_rds("Learn_Find.rds")
    lf <- Learn_Find[[lngidx]]
    idea_output_list <- lapply(lf, 
      function(s) {
        iltxttag <- paste("iltxt", s, sep="")
        ilplttag <- paste("ilplt", s, sep="")
        ilytag <- paste("ily", s, sep="")
        ilinttag <- paste("ilint", s, sep="")
        ilfuntag <- paste("ilfun", s, sep="")
        ilrangetag <- paste("ilrange", s, sep="")
        fluidRow(
          hr(),
          textOutput(iltxttag),
          textOutput(ilytag),
          textOutput(ilinttag),
          textOutput(ilfuntag),
          textOutput(ilrangetag),
          plotOutput(ilplttag)
        )
      })
    do.call(tagList, idea_output_list)
  }  
})

observe({
  if (!is.null(input$idealist_rows_selected)){
    ImprovementIdea <- read_rds("ImprovementIdea.rds")
    lngidx <-ImprovementIdea[input$idealist_rows_selected[1],]$learning
    Learn_Find <- read_rds("Learn_Find.rds")
    fd <- Learn_Find[[lngidx]]
    Findings <- read_rds("Findings.rds")
    fdParams <- read_rds("fdParams.rds")
    output$idlngfdlist <- DT::renderDataTable(DT::datatable(Findings[fd,], selection="none") %>% formatDate(c(1), method="toLocaleString",params=list("se", list(timeZone="Asia/Tokyo"))), server=FALSE) 
    for (.j in seq(length(fd))){
      local({
        j <- .j
        i <- fd[j]
        fdg <- Findings[i,]
        param <- fdParams[[i]]
        iltxttag <- paste("iltxt", i, sep="")
        ilplttag <- paste("ilplt", i, sep="")
        ilytag <- paste("ily", i, sep="")
        ilinttag <- paste("ilint", i, sep="")
        ilfuntag <- paste("ilfun", i, sep="")
        ilrangetag <- paste("ilrange", i, sep="")
        output[[iltxttag]] <- renderText({paste("Finding:", fdg$finding)})
        output[[ilytag]] <- renderText({paste("Data:",param$y)})
        output[[ilinttag]] <- renderText({paste("Summarizing interval:",param$interval,"Sec")})
        output[[ilfuntag]] <- renderText({paste("Summarizing function:",param$f)})
        output[[ilrangetag]] <- renderText({paste("Date Range:",param$fdt,"-", param$tdt)})
        output[[ilplttag]] <- renderPlot({mexPlot(y=param$y, interval=param$interval, f=param$f, node=param$node, fdt=param$fdt, tdt=param$tdt)})
      })
    }
  }
})


######################################################
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  

  output$downloadPlan<- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("Improvement-report.Rmd", tempReport, overwrite = TRUE)
        ##file.copy("report.Rmd", "tempReport", overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        Findings <- read_rds("Findings.rds")
        Learnings <- read_rds("Learnings.rds")
        Learn_Find <- read_rds("Learn_Find.rds")
        ImprovementIdea <- read_rds("ImprovementIdea.rds")
        iidx<-as.integer(input$idlnglist_rows_selected)
        idea <-ImprovementIdea[iidx,]
        lng <- Learnings[idea$learning,]
        params <- list(itime=idea$time, idea=idea$idea, ltime=lng$time, learning=lng$learning, findings=Learn_Find[[idea$learning]])

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
          params = params
        )
      }
    )

}

# Create Shiny app ----
shinyApp(ui, server)