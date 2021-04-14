#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
source("global.R")


# Define UI for application
ui <- dashboardPage(title = "EffVisR",
    dashboardHeader(title = span(tagList(
        img(src = "Kinnate Logo SMALL.jpg", width = 40), "Efficacy VisR"
    ))),
    dashboardSidebar(sidebarMenu(
        menuItem("Load Study Data", tabName = "studyload", icon = icon("upload")),
        menuItem("Tumor Volume & BW", tabName = "tvbw", icon=icon("chart-line")),
        menuItem("Response & PK", tabName = "resp_pk", icon = icon("bar-chart")),
        menuItem("PK & PD", tabName = "pkpd", icon=icon("microscope"))
    )),
    dashboardBody(tabItems(
        tabItem(tabName = "studyload",
                sidebarLayout(
                    sidebarPanel(width=3,
                        h4("Load Studies:"),
                        radioButtons(
                            inputId = "datafrom",
                            label = "Data Source:",
                            choices = c("Spreadsheet", "Database"),
                            selected = "Spreadsheet"
                        ),
                        hr(),
                        conditionalPanel(
                            condition = "input.datafrom=='Database'",
                            uiOutput("DBstudies")
                        ),
                        conditionalPanel(
                            condition = "input.datafrom=='Spreadsheet'",
                            fileInput(
                                "fileTVBW",
                                "TV/BW File",
                                multiple = FALSE,
                                accept = "xlxs"
                            ),
                            fileInput(
                                "filePK1",
                                "PK File (Efficacy Groups)",
                                multiple = FALSE,
                                accept = "xlxs"
                            ),
                            fileInput(
                                "filePK2",
                                "PK File (pERK Groups)",
                                multiple = FALSE,
                                accept = "xlxs"
                            ),
                            fileInput(
                                "filePDwb",
                                "pERK WB File",
                                multiple = FALSE,
                                accept = "xlxs"
                            ),
                            fileInput(
                                "filePDmsd",
                                "pERK MSD File",
                                multiple = FALSE,
                                accept = "xlxs"
                            )
                        )
                    ),
                    mainPanel(width = 9,
                        h4("Review Study Info, Design and Data"),
                        tabBox(
                            width = 12,
                            tabPanel("Study Info", dataTableOutput("StudyInfo")),
                            tabPanel("Study Design", dataTableOutput("StudyDesign")),
                            tabPanel(
                                "TV & BW",
                                conditionalPanel(condition="input.datafrom=='Spreadsheet'",
                                actionButton("Submit_tvbw", "Submit to Database")),
                                dataTableOutput("preview_tvbw")
                            ),
                            tabPanel(
                                "PK (Efficacy Groups)",
                                conditionalPanel(condition="input.datafrom=='Spreadsheet'",
                                actionButton("Submit_pk_efficacy", "Submit to Database")),
                                dataTableOutput("preview_pk_efficacy")
                            ),
                            tabPanel(
                                "PK Params (Efficacy Groups)",
                                conditionalPanel(condition="input.datafrom=='Spreadsheet'",
                                actionButton("Submit_pkparams_efficacy", "Submit to Database")),
                                dataTableOutput("preview_pkparams_efficacy")
                            ),
                            tabPanel(
                                "PK (pERK Groups)",
                                conditionalPanel(condition="input.datafrom=='Spreadsheet'",
                                actionButton("Submit_pk_pd", "Submit to Database")),
                                dataTableOutput("preview_pk_pd")
                            ),
                            tabPanel(
                                "PK Params (PD Groups)",
                                conditionalPanel(condition="input.datafrom=='Spreadsheet'",
                                actionButton("Submit_pkparams_pd", "Submit to Database")),
                                dataTableOutput("preview_pkparams_pd")
                            ),
                            tabPanel(
                                "pERK (WB)",
                                conditionalPanel(condition="input.datafrom=='Spreadsheet'",
                                actionButton("Submit_perk_wb", "Submit to Database")),
                                dataTableOutput("preview_perk_wb")
                            ),
                            tabPanel(
                                "pERK (MSD)",
                                conditionalPanel(condition="input.datafrom=='Spreadsheet'",
                                                 actionButton("Submit_perk_msd", "Submit to Database")),
                                dataTableOutput("preview_perk_msd")
                            )
                        )
                    )
                )),
        tabItem(
            tabName = "tvbw",
            uiOutput("SelectedStudy1"), 
            fluidRow(tabBox(
                tabPanel("Group TV",
                         sidebarLayout(
                             sidebarPanel(
                                 width = 3,
                                 uiOutput("TVsumGroups"),
                                 checkboxInput(label="BW Inset", inputId = "BWinset", value = TRUE),
                                 radioButtons(label = "Txt Names", inputId = "TxtNames", 
                                              choices = c("BatchID", "Compound"), selected = "BatchID"),
                                 hr(),
                                 selectInput(
                                     label = "File Type:",
                                     inputId = "TVsumPlot.device",
                                     choices = c(".png", ".svg", ".eps"),
                                     selected = ".png"
                                 ),
                                 numericInput(
                                     label = "Figure Width (in):",
                                     inputId = "TVsumPlot.width",
                                     value = 6.5
                                 ),
                                 numericInput(
                                     label = "Figure Height (in):",
                                     inputId = "TVsumPlot.height",
                                     value = 3.5
                                 ),
                                 downloadButton(label = "Download",
                                                outputId = "TVsumPlot.dl")
                             ),
                             mainPanel(width = 9,
                                       plotOutput("TVsumPlot"))
                         )),
                tabPanel("Animal TV",
                         sidebarLayout(
                             sidebarPanel(
                                 width = 3,
                                 uiOutput("TVanimalGroups"),
                                 numericInput(
                                     label = "No. of Rows:",
                                     inputId = "TVanimalPlot.rows",
                                     value = 2
                                 ),
                                 hr(),
                                 selectInput(
                                     label = "File Type:",
                                     inputId = "TVanimalPlot.device",
                                     choices = c(".png", ".svg", ".eps"),
                                     selected = ".png"
                                 ),
                                 numericInput(
                                     label = "Figure Width (in):",
                                     inputId = "TVanimalPlot.width",
                                     value = 7
                                 ),
                                 numericInput(
                                     label = "Figure Height (in):",
                                     inputId = "TVanimalPlot.height",
                                     value = 4.5
                                 ),
                                 downloadButton(label = "Download",
                                                outputId = "TVanimalPlot.dl")
                             ),
                             mainPanel(width = 9,
                                       plotOutput("TVanimalPlot"))
                         ))
            ),
            tabBox(
                tabPanel("Group BW",
                         sidebarLayout(
                             sidebarPanel(
                                 width = 3,
                                 uiOutput("BWsumGroups"),
                                 hr(),
                                 selectInput(
                                     label = "File Type:",
                                     inputId = "BWsumPlot.device",
                                     choices = c(".png", ".svg", ".eps"),
                                     selected = ".png"
                                 ),
                                 numericInput(
                                     label = "Figure Width (in):",
                                     inputId = "BWsumPlot.width",
                                     value = 6.5
                                 ),
                                 numericInput(
                                     label = "Figure Height (in):",
                                     inputId = "BWsumPlot.height",
                                     value = 3.5
                                 ),
                                 downloadButton(label = "Download",
                                                outputId = "BWsumPlot.dl")
                             ),
                             mainPanel(width = 9,
                                       plotOutput("BWsumPlot"))
                         )),
                tabPanel("Animal BW",
                         sidebarLayout(
                             sidebarPanel(
                                 width = 3,
                                 uiOutput("BWanimalGroups"),
                                 numericInput(
                                     label = "No. of Rows:",
                                     inputId = "BWanimalPlot.rows",
                                     value = 2
                                 ),
                                 hr(),
                                 selectInput(
                                     label = "File Type:",
                                     inputId = "BWanimalPlot.device",
                                     choices = c(".png", ".svg", ".eps"),
                                     selected = ".png"
                                 ),
                                 numericInput(
                                     label = "Figure Width (in):",
                                     inputId = "BWanimalPlot.width",
                                     value = 7
                                 ),
                                 numericInput(
                                     label = "Figure Height (in):",
                                     inputId = "BWanimalPlot.height",
                                     value = 4.5
                                 ),
                                 downloadButton(label = "Download",
                                                outputId = "BWanimalPlot.dl")
                             ),
                             mainPanel(width = 9,
                                       plotOutput("BWanimalPlot"))
                         ))
            )),
            fluidRow(tabBox(
                tabPanel("Group % TV",
                         sidebarLayout(
                             sidebarPanel(
                                 width = 3,
                                 uiOutput("TVgrpPctGroups"),
                                 checkboxInput(label="BW Inset", inputId = "BWpctinset", value = TRUE),
                                 radioButtons(label="Txt Names", inputId = "TxtgrpPctNames", 
                                              choices = c("BatchID", "Compound"), selected = "BatchID"),
                                 hr(),
                                 selectInput(
                                     label = "File Type:",
                                     inputId = "TVgrpPctPlot.device",
                                     choices = c(".png", ".svg", ".eps"),
                                     selected = ".png"
                                 ),
                                 numericInput(
                                     label = "Figure Width (in):",
                                     inputId = "TVgrpPctPlot.width",
                                     value = 6.5
                                 ),
                                 numericInput(
                                     label = "Figure Height (in):",
                                     inputId = "TVgrpPctPlot.height",
                                     value = 3.5
                                 ),
                                 downloadButton(label = "Download",
                                                outputId = "TVgrpPctPlot.dl")
                             ),
                             mainPanel(width = 9,
                                       plotOutput("TVgrpPctPlot"))
                         )),
                tabPanel("Animal % TV",
                         sidebarLayout(
                             sidebarPanel(
                                 width = 3,
                                 uiOutput("TVpctGroups"),
                                 numericInput(
                                     label = "No. of Rows:",
                                     inputId = "TVpctPlot.rows",
                                     value = 2
                                 ),
                                 hr(),
                                 selectInput(
                                     label = "File Type:",
                                     inputId = "TVpctPlot.device",
                                     choices = c(".png", ".svg", ".eps"),
                                     selected = ".png"
                                 ),
                                 numericInput(
                                     label = "Figure Width (in):",
                                     inputId = "TVpctPlot.width",
                                     value = 7
                                 ),
                                 numericInput(
                                     label = "Figure Height (in):",
                                     inputId = "TVpctPlot.height",
                                     value = 4.5
                                 ),
                                 downloadButton(label = "Download",
                                                outputId = "TVpctPlot.dl")
                             ),
                             mainPanel(width = 9,
                                       plotOutput("TVpctPlot"))
                         ))
                ),
            tabBox(
                tabPanel("Group % BW",
                         sidebarLayout(
                             sidebarPanel(
                                 width = 3,
                                 uiOutput("BWgrpPctGroups"),
                                 hr(),
                                 selectInput(
                                     label = "File Type:",
                                     inputId = "BWgrpPctPlot.device",
                                     choices = c(".png", ".svg", ".eps"),
                                     selected = ".png"
                                 ),
                                 numericInput(
                                     label = "Figure Width (in):",
                                     inputId = "BWgrpPctPlot.width",
                                     value = 6.5
                                 ),
                                 numericInput(
                                     label = "Figure Height (in):",
                                     inputId = "BWgrpPctPlot.height",
                                     value = 3.5
                                 ),
                                 downloadButton(label = "Download",
                                                outputId = "BWgrpPctPlot.dl")
                             ),
                             mainPanel(width = 9,
                                       plotOutput("BWgrpPctPlot"))
                         )),
                tabPanel("Animal % BW",
                         sidebarLayout(
                             sidebarPanel(
                                 width = 3,
                                 uiOutput("BWpctGroups"),
                                 numericInput(
                                     label = "No. of Rows:",
                                     inputId = "BWpctPlot.rows",
                                     value = 2
                                 ),
                                 hr(),
                                 selectInput(
                                     label = "File Type:",
                                     inputId = "BWpctPlot.device",
                                     choices = c(".png", ".svg", ".eps"),
                                     selected = ".png"
                                 ),
                                 numericInput(
                                     label = "Figure Width (in):",
                                     inputId = "BWpctPlot.width",
                                     value = 7
                                 ),
                                 numericInput(
                                     label = "Figure Height (in):",
                                     inputId = "BWpctPlot.height",
                                     value = 4.5
                                 ),
                                 downloadButton(label = "Download",
                                                outputId = "BWpctPlot.dl")
                             ),
                             mainPanel(width = 9,
                                       plotOutput("BWpctPlot"))
                         ))
            ))
        ),
        tabItem(tabName="resp_pk",
                uiOutput("SelectedStudy2"), 
                fluidRow(
                    tabBox(width = 10,
                        tabPanel("Waterfall",
                                 sidebarLayout(
                                     sidebarPanel(
                                         width = 3,
                                         uiOutput("WfallDayUi"),
                                         uiOutput("WfallGroups"),
                                         radioButtons(label = "Txt Names", inputId="WfallTxtNames", 
                                                      choices = c("BatchID", "Compound"), selected = "BatchID"), 
                                         hr(),
                                         selectInput(
                                             label = "File Type:",
                                             inputId = "WfallPlot.device",
                                             choices = c(".png", ".svg", ".eps"),
                                             selected = ".png"
                                         ),
                                         numericInput(
                                             label = "Figure Width (in):",
                                             inputId = "WfallPlot.width",
                                             value = 7
                                         ),
                                         numericInput(
                                             label = "Figure Height (in):",
                                             inputId = "WfallPlot.height",
                                             value = 3.5
                                         ),
                                         downloadButton(label = "Download",
                                                        outputId = "WfallPlot.dl")
                                     ),
                                     mainPanel(width = 9,
                                               plotOutput("WfallPlot"))
                                 )
                        ),
                        tabPanel("% Inhibition TV",
                                 sidebarLayout(
                                     sidebarPanel(
                                         width = 3,
                                         uiOutput("tgiControlGroup"),
                                         uiOutput("tgiGroups"),
                                         hr(),
                                         selectInput(
                                             label = "File Type:",
                                             inputId = "tgiPlot.device",
                                             choices = c(".png", ".svg", ".eps"),
                                             selected = ".png"
                                         ),
                                         numericInput(
                                             label = "Figure Width (in):",
                                             inputId = "tgiPlot.width",
                                             value = 6.5
                                         ),
                                         numericInput(
                                             label = "Figure Height (in):",
                                             inputId = "tgiPlot.height",
                                             value = 3.5
                                         ),
                                         downloadButton(label = "Download",
                                                        outputId = "tgiPlot.dl")
                                     ),
                                     mainPanel(width = 9,
                                               plotOutput("tgiPlot"))
                                 )
                        ),
                        tabPanel("Statistics",
                            sidebarLayout(
                                sidebarPanel(
                                    width=3,
                                    selectInput(
                                        label="Choose Stat Test:",
                                        inputId="stattest",
                                        choices=c("anova_tukey", "kw_dunn"),
                                        selected="anova_tukey"),
                                    hr(),
                                    dataTableOutput("Txt.Ref.Stat")),
                                mainPanel(width = 9,
                                          dataTableOutput("stat.results"))
                                    )
                                ),
                        tabPanel("TGI Plot",
                            sidebarLayout(
                                sidebarPanel(width = 3,
                                             h4("Duration must match for all growth curves..."),
                                             uiOutput("TGIControl"),
                                             uiOutput("TGICutoff"),
                                             selectInput(
                                                 label = "TGI Method",
                                                 inputId = "tgiMethod",
                                                 choices = c("deltaV", "AUC"), 
                                                 selected = "deltaV")
                                             ),
                                mainPanel(width=9,
                                          plotOutput("drapTGI"))
                            )
                        ),
                  tabPanel("Compare All Growth Curves",
                            sidebarLayout(
                                sidebarPanel(width = 3,
                                             h4("Duration must match for all growth curves..."),
                                             uiOutput("GCCutoff"),
                                             hr(),
                                             dataTableOutput("Txt.Ref.GC"),
                                    h5("Be patient. Can take several minutes to complete permutations.")
                                             ),
                                mainPanel(width = 9,
                                          dataTableOutput("AllGC.results")))
                                )
                            )
                        ),
                fluidRow(
                    tabBox(width = 12,
                        tabPanel("PK Plots",
                            sidebarLayout(
                                sidebarPanel(width = 3,
                                             uiOutput("PKcurvesGroups"),
                                             checkboxInput(label = "log10 Scale", inputId = "PKlog", value = TRUE),
                                             selectInput(
                                                 label = "File Type:",
                                                 inputId = "PKcurves.device",
                                                 choices = c(".png", ".svg", ".eps"),
                                                 selected = ".png"
                                             ),
                                             numericInput(
                                                 label = "Figure Width (in):",
                                                 inputId = "PKcurves.width",
                                                 value = 6
                                             ),
                                             numericInput(
                                                 label = "Figure Height (in):",
                                                 inputId = "PKcurves.height",
                                                 value = 4.5
                                             ),
                                             downloadButton(label = "Download",
                                                            outputId = "PKcurves.dl")
                                ),
                                mainPanel(width = 9,
                                    plotOutput("PKcurves")
                                )
                            )
                        ),
                        tabPanel("PK Parameters",
                                 dataTableOutput("PKparams")
                        ),
                        tabPanel("% TGI vs AUClast",
                                 sidebarLayout(
                                     sidebarPanel(
                                         width = 3,
                                         uiOutput("tgiAucDayUi"),
                                         uiOutput("tgiAucGroups"),
                                         hr(),
                                         selectInput(
                                             label = "File Type:",
                                             inputId = "tgiAucPlot.device",
                                             choices = c(".png", ".svg", ".eps"),
                                             selected = ".png"
                                         ),
                                         numericInput(
                                             label = "Figure Width (in):",
                                             inputId = "tgiAucPlot.width",
                                             value = 6
                                         ),
                                         numericInput(
                                             label = "Figure Height (in):",
                                             inputId = "tgiAucPlot.height",
                                             value = 4.5
                                         ),
                                         downloadButton(label = "Download",
                                                        outputId = "tgiAucPlot.dl")
                                     ),
                                     mainPanel(width = 9,
                                               plotOutput("tgiAucPlot"))
                                 )
                        )
                    )
                )
        ),
        tabItem(tabName = "pkpd",
                uiOutput("SelectedStudy3"), 
                fluidRow(tabBox(
                    width = 12,
                    tabPanel("pERK/ERK WB",
                             sidebarLayout(
                                 sidebarPanel(width=2,
                                              uiOutput("pERKcontrolGroup")),
                                 mainPanel(width=10,
                                           plotOutput("pERK_wb_plot"))
                             ))
                    
                )),
                fluidRow(tabBox(
                    width=12,
                    tabPanel("pERK/ERK MSD",
                             sidebarLayout(
                                 sidebarPanel(width=2
                                 ),
                                 mainPanel(width=10,
                                           plotOutput("pERK_msd_plot"))
                             )),
                    tabPanel("PK/PD MSD",
                             sidebarLayout(
                                 sidebarPanel(width=2
                                 ),
                                 mainPanel(width=10,
                                           plotOutput("pk_pd_msd"))
                             ))
                ))
                )
        )
    )
)  

# Define server logic
server <- function(input, output) {
    
    
    tvbw <- reactive({
        if (input$datafrom == "Database") {
            db.con <-
                DBI::dbConnect(RSQLite::SQLite(),
                               "Data/EffVisR-database-output.sqlite")
            data <- tbl(db.con, "tvbw") %>%
                filter(`Quotation No` == local(input$DBselected)) %>%
                collect() %>%
                mutate(Date=as_date(Date),
                       GroupNum = as.integer(str_extract(Group, "[0-9]{1,2}$")),
                       `Inoculation Date`=as_date(`Inoculation Date`),
                       `Dosing Date`=as_date(`Dosing Date`),
                       `Invivo Study Ending Date`=as_date(`Invivo Study Ending Date`)) %>%
                mutate(Group=fct_reorder(Group, GroupNum)) %>%
                select(-GroupNum)
                
            DBI::dbDisconnect(db.con)
        } else if (input$datafrom == "Spreadsheet") {
            # read data from excel spreadsheet
            req(input$fileTVBW)
            
            header.tmp <-
                lapply(grep("^PG",
                            excel_sheets(input$fileTVBW$datapath),
                            value = TRUE), function(x)
                                read_excel(input$fileTVBW$datapath,
                                           x,
                                           col_names = FALSE))
            
            names(header.tmp) <-
                grep("^PG",
                     excel_sheets(input$fileTVBW$datapath),
                     value = TRUE)
            
            pull.heads <- function(x) {
                x %>%
                    mutate(`...1` = str_replace(`...1`, "\\:", "")) %>%
                    slice(1:2) %>%
                    select(1:2) %>%
                    column_to_rownames("...1") %>%
                    t %>% as_tibble() %>%
                    mutate(Date = as_date(as.Date(as.numeric(Date), origin =
                                                      "1899-12-30")))
            }
            
            PG.headers <- map(header.tmp, pull.heads)
            
            PG.data <-
                lapply(grep("^PG", excel_sheets(input$fileTVBW$datapath), value = TRUE), function(x)
                    read_excel(
                        input$fileTVBW$datapath,
                        x,
                        skip = 2,
                        col_types = "text"
                    ))
            
            names(PG.data) <-
                grep("^PG", excel_sheets(input$fileTVBW$datapath), value = TRUE)
            
            stinfo <-
                read_excel(input$fileTVBW$datapath, sheet = "Index") %>%
                filter(!is.na(`Master Schedule`)) %>%
                mutate(`Master Schedule` = str_replace_all(`Master Schedule`, c(
                    "\\:" = "", "\\." = ""
                ))) %>%
                slice(1:9) %>%
                select(1:2) %>%
                column_to_rownames("Master Schedule") %>%
                t %>% as_tibble() %>%
                mutate(`Quotation No`=str_replace(`Quotation No`, "KIN-", "KIN"),
                    `Inoculation Date` = as_date(as.Date(
                        as.numeric(`Inoculation Date`), origin = "1899-12-30"
                    )),
                    `Dosing Date` = as_date(as.Date(
                        as.numeric(`Dosing Date`), origin = "1899-12-30"
                    )),
                    `Invivo Study Ending Date` = as_date(as.Date(
                        as.numeric(`Invivo Study Ending Date`), origin = "1899-12-30"
                    ))
                )
            
            stdesign <-
                read_excel(input$fileTVBW$datapath, sheet = "Design") %>%
                mutate(Group = parse_factor(Group, levels = unique(Group))) %>%
                mutate(
                    Treatment = case_when(
                        !is.na(Treatment_2) &
                            !is.na(Dose_1) ~ str_c(
                                str_c(Treatment_1, Dose_1, Frequency_1, sep = " "),
                                str_c(Treatment_2, Dose_2, Frequency_2, sep = " "),
                                sep = " &\n"
                            ),
                        !is.na(Treatment_2) &
                            is.na(Dose_1) ~ str_c(Treatment_1, Treatment_2, sep = " & "),
                        is.na(Treatment_2) &
                            is.na(Dose_1) ~ Treatment_1,
                        is.na(Treatment_2) &
                            !is.na(Dose_1) ~ str_c(Treatment_1, Dose_1, Frequency_1, sep = " ")
                    )
                )
            
            data <-
                map2(PG.data, PG.headers, bind_cols) %>%
                bind_rows() %>%
                select(-starts_with("...")) %>%
                rename(`L (mm)` = `D (mm)`) %>%
                rename(`W (mm)` = `d (mm)`) %>%
                mutate(
                    `L (mm)` = as.double(`L (mm)`),
                    `W (mm)` = as.double(`W (mm)`),
                    `TV (mm3)` = as.double(`TV (mm3)`),
                    `BW (g)` = as.double(`BW (g)`)
                ) %>%
                mutate(Group = if_else(!str_detect(Group, "^G"), as.character(NA), Group)) %>%
                filter(!`Animal ID` %in% c("SD", "SEM", "Mean")) %>%
                fill(Group) %>%
                mutate(Group = factor(Group, levels = levels(stdesign$Group))) %>%
                left_join(stdesign) %>%
                left_join(stinfo) %>%
                mutate(
                    `Days on Study Treatment` = as.double(Date - `Dosing Date` + 1),
                    `Days from Tumor Inoculation` = as.double(Date - `Inoculation Date` +
                                                                  1)
                ) %>%
                group_by(`Animal ID`) %>%
                mutate(
                    `TV % Change` = ((`TV (mm3)` - `TV (mm3)`[`Days on Study Treatment` == min(`Days on Study Treatment`)]) /
                                         `TV (mm3)`[`Days on Study Treatment` ==
                                                        min(`Days on Study Treatment`)]) * 100,
                    `BW % Change` = ((`BW (g)` - `BW (g)`[`Days on Study Treatment` ==
                                                              min(`Days on Study Treatment`)]) / `BW (g)`[`Days on Study Treatment` == min(`Days on Study Treatment`)] * 100)
                ) %>%
                select(
                    `Quotation No`,
                    `Study ID`,
                    `Tumor Cell Line`,
                    Group,
                    Treatment,
                    `Animal ID`,
                    `BW (g)`,
                    `L (mm)`,
                    `W (mm)`,
                    `TV (mm3)`,
                    Comment,
                    Date,
                    `Days on Study Treatment`,
                    `Days from Tumor Inoculation`,
                    `BW % Change`,
                    `TV % Change`,
                    everything()
                )
        }
        return(data)
    })
       
    stinfo <- reactive({
        if (input$datafrom == "Database") {
            db.con <-
                DBI::dbConnect(RSQLite::SQLite(),
                               "Data/EffVisR-database-output.sqlite")
            data <- tbl(db.con, "tvbw") %>%
                filter(`Quotation No` == local(input$DBselected)) %>%
                select(`Study ID`, `Quotation No`, `Tumor Cell Line`, `Injected Cell Numbers`, `Animal`, `Injected / Implanted position`, 
                       `Inoculation Date`, `Dosing Date`, `Invivo Study Ending Date`) %>%
                distinct() %>%
                collect() %>%
                mutate(`Inoculation Date`=as_date(`Inoculation Date`),
                       `Dosing Date`=as_date(`Dosing Date`),
                       `Invivo Study Ending Date`=as_date(`Invivo Study Ending Date`))
                
            
            DBI::dbDisconnect(db.con)
        } else if (input$datafrom == "Spreadsheet") {
            # read data from excel spreadsheet
        req(input$fileTVBW)
        data <- read_excel(input$fileTVBW$datapath, sheet = "Index") %>%
            filter(!is.na(`Master Schedule`)) %>%
            mutate(`Master Schedule` = str_replace_all(`Master Schedule`, c("\\:"="", "\\."=""))) %>%
            slice(1:9) %>%
            select(1:2) %>%
            column_to_rownames("Master Schedule") %>%
            t %>% as_tibble() %>%
            mutate(`Quotation No`=str_replace(`Quotation No`, "KIN-", "KIN"),
                `Inoculation Date` = as_date(as.Date(as.numeric(`Inoculation Date`), origin = "1899-12-30")),
                `Dosing Date` = as_date(as.Date(as.numeric(`Dosing Date`), origin = "1899-12-30")),
                `Invivo Study Ending Date` = as_date(as.Date(
                    as.numeric(`Invivo Study Ending Date`), origin = "1899-12-30"
                )))
        }
        return(data)
    })
    
    stdesign <- reactive({
        if (input$datafrom == "Database") {
            db.con <-
                DBI::dbConnect(RSQLite::SQLite(),
                               "Data/EffVisR-database-output.sqlite")
            data <- tbl(db.con, "tvbw") %>%
                filter(`Quotation No` == local(input$DBselected)) %>%
                select(Group, Objectives, Treatment, Treatment_1, Dose_1, Volume_1, Route_1, Frequency_1, Formulation_1,
                       Treatment_2, Dose_2, Volume_2, Route_2, Frequency_2, Formulation_2) %>%
                distinct() %>%
                collect()
            
            DBI::dbDisconnect(db.con)
        } else if (input$datafrom == "Spreadsheet") {
        req(input$fileTVBW)
        data <-
            read_excel(input$fileTVBW$datapath, sheet = "Design") %>%
            mutate(Group = parse_factor(Group, levels = unique(Group))) %>%
            mutate(
                Treatment = case_when(
                    !is.na(Treatment_2) &
                        !is.na(Dose_1) ~ str_c(
                            str_c(Treatment_1, Dose_1, Frequency_1, sep = " "),
                            str_c(Treatment_2, Dose_2, Frequency_2, sep = " "),
                            sep = " &\n"
                        ),
                    !is.na(Treatment_2) &
                        is.na(Dose_1) ~ str_c(Treatment_1, Treatment_2, sep = " & "),
                    is.na(Treatment_2) &
                        is.na(Dose_1) ~ Treatment_1,
                    is.na(Treatment_2) &
                        !is.na(Dose_1) ~ str_c(Treatment_1, Dose_1, Frequency_1, sep = " ")
                )
            )}
        return(data)
    })
    
# Read in PK data from efficacy groups
    pk_efficacy <- reactive({
        if (input$datafrom == "Database") {
            db.con <-
                DBI::dbConnect(RSQLite::SQLite(),
                               "Data/EffVisR-database-output.sqlite")
            data <- tbl(db.con, "pk_efficacy") %>%
                filter(`Quotation No` == local(input$DBselected)) %>% 
                collect()
            DBI::dbDisconnect(db.con)
        } else if (input$datafrom == "Spreadsheet") {
            # read data from excel spreadsheet
            req(input$filePK1)
            data <- read_excel(input$filePK1$datapath, sheet = "Raw Data", skip=16) %>%
                fill(`Quotation No`) %>%
                mutate(`Quotation No`= str_replace(`Quotation No`, "KIN-", "KIN")) %>%
                filter(!is.na(`File Name`), `File Name` !="File Name", str_detect(`Sample Name`, "^PO_")|str_detect(`Sample Name`, "^G")) %>%
                mutate(`Calculated Concentration (ng/mL)`=parse_double(`Calculated Concentration (ng/mL)`)) %>%
                mutate(Sample = if_else(str_detect(`Sample Name`, "^G"), paste("X_", `Sample Name`, sep=""), `Sample Name`)) %>%
                mutate(Sample = str_replace_all(Sample, pattern=c("30min"="0.5h","15min"="0.25h"))) %>%
                separate(Sample, into=c("Route", "Group", "Animal ID", "Timepoint"), remove=TRUE, sep="_") %>%
                separate(`File Name`, into=c("Compound", "Junk"), sep="_") %>%
                separate(Timepoint, into = c("Hours Post Dose", "Junk2"), sep="h") %>%
                mutate(`Hours Post Dose`= parse_double(`Hours Post Dose`)) %>%
                select(-Route, -Junk, -Junk2) %>%
                select(`Quotation No`, `Sample Name`, Group, `Animal ID`, `Hours Post Dose`, everything())
        }
        return(data)
    })
    
    pkparams_efficacy <- reactive({
        if (input$datafrom == "Database") {
            db.con <-
                DBI::dbConnect(RSQLite::SQLite(),
                               "Data/EffVisR-database-output.sqlite")
            data <- tbl(db.con, "pkparams_efficacy") %>%
                filter(`Quotation No` == local(input$DBselected)) %>% 
                collect()
            DBI::dbDisconnect(db.con)
        } else if (input$datafrom == "Spreadsheet") {
            # read data from excel spreadsheet
            req(input$filePK1)
            data <- read_excel(input$filePK1$datapath, sheet = "Data Upload") %>%
                fill(Group) %>%
                rename(`Quotation No`=`Report No.`) %>%
                mutate(`Quotation No`= str_replace(`Quotation No`, "KIN-", "KIN")) %>%
                mutate(Result=as.double(Result)) %>%
                select(`Quotation No`, everything())
        }
        return(data)
    })
    
    # Read in PK data from pd groups
    pk_pd <- reactive({
        if (input$datafrom == "Database") {
            db.con <-
                DBI::dbConnect(RSQLite::SQLite(),
                               "Data/EffVisR-database-output.sqlite")
            data <- tbl(db.con, "pk_pd") %>%
                filter(`Quotation No` == local(input$DBselected)) %>% 
                collect()
            DBI::dbDisconnect(db.con)
        } else if (input$datafrom == "Spreadsheet") {
            # read data from excel spreadsheet
            req(input$filePK2)
            data <- read_excel(input$filePK2$datapath, sheet = "Raw Data", skip=16) %>%
                fill(`Quotation No`) %>%
                mutate(`Quotation No`= str_replace(`Quotation No`, "KIN-", "KIN")) %>%
                filter(!is.na(`File Name`), `File Name` !="File Name", str_detect(`Sample Name`, "^PO_")|str_detect(`Sample Name`, "^G")) %>%
                mutate(`Calculated Concentration (ng/mL)`=parse_double(`Calculated Concentration (ng/mL)`)) %>%
                mutate(Sample = if_else(str_detect(`Sample Name`, "^G"), paste("X_", `Sample Name`, sep=""), `Sample Name`)) %>%
                mutate(Sample = str_replace_all(Sample, pattern=c("30min"="0.5h","15min"="0.25h"))) %>%
                separate(Sample, into=c("Route", "Group", "Animal ID", "Timepoint"), remove=TRUE, sep="_") %>%
                separate(`File Name`, into=c("Compound", "Junk"), sep="_") %>%
                separate(Timepoint, into = c("Hours Post Dose", "Junk2"), sep="h") %>%
                mutate(`Hours Post Dose`= parse_double(`Hours Post Dose`)) %>%
                select(-Route, -Junk, -Junk2) %>%
                select(`Quotation No`, `Sample Name`, Group, `Animal ID`, `Hours Post Dose`, everything())
        }
        return(data)
    })
    
    # Read in PK params from PD groups 
    pkparams_pd <- reactive({
        if (input$datafrom == "Database") {
            db.con <-
                DBI::dbConnect(RSQLite::SQLite(),
                               "Data/EffVisR-database-output.sqlite")
            data <- tbl(db.con, "pkparams_pd") %>%
                filter(`Quotation No` == local(input$DBselected)) %>% 
                collect()
            DBI::dbDisconnect(db.con)
        } else if (input$datafrom == "Spreadsheet") {
            # read data from excel spreadsheet
            req(input$filePK2)
            data <- read_excel(input$filePK2$datapath, sheet = "Data Upload") %>%
                fill(Group) %>%
                rename(`Quotation No`=`Report No.`) %>%
                mutate(`Quotation No`= str_replace(`Quotation No`, "KIN-", "KIN")) %>%
                mutate(Result=as.double(Result)) %>%
                select(`Quotation No`, everything())
        }
        return(data)
    })
    
    # read in Phospho-ERK data
    perk_wb <- reactive({
        if (input$datafrom == "Database") {
            db.con <-
                DBI::dbConnect(RSQLite::SQLite(),
                               "Data/EffVisR-database-output.sqlite")
            data <- tbl(db.con, "perk_wb") %>%
                filter(`Quotation No` == local(input$DBselected)) %>% 
                collect()
            DBI::dbDisconnect(db.con)
        } else if (input$datafrom == "Spreadsheet") {
            # read data from excel spreadsheet
            req(input$filePDwb)
            data <- read_excel(input$filePDwb$datapath, sheet = "Data process", skip = 2) %>%
                select(1:10) %>%
                slice(-1) %>%
                filter(!is.na(ERK), ERK!="ERK") %>%
                fill(`Quotation No`) %>%
                fill(Group) %>%
                fill(Drug) %>%
                fill(Dose) %>%
                fill(Gel) %>%
                fill(`Collection Timepoint`) %>%
                mutate("Hours Post Dose" = parse_double(str_replace(`Collection Timepoint`, "h", "")),
                       "Group Number"= parse_double(str_extract(Group, "[0-9]{1,2}$")),
                       Dose = str_replace(Dose, "PO Single dose  ", ""),
                       ERK=as.double(ERK),
                       `p-ERK`=as.double(`p-ERK`),
                       `p-ERK/ERK`=as.double(`p-ERK/ERK`))
        }
        return(data)
    })
    
    perk_msd <- reactive({
        if (input$datafrom == "Database") {
            db.con <-
                DBI::dbConnect(RSQLite::SQLite(),
                               "Data/EffVisR-database-output.sqlite")
            data <- tbl(db.con, "perk_msd") %>%
                filter(`Quotation No` == local(input$DBselected)) %>%
                collect()
            DBI::dbDisconnect(db.con)
        } else if (input$datafrom == "Spreadsheet") {
            # read data from excel spreadsheet
            req(input$filePDmsd)
            data <-
                read_excel(input$filePDmsd$datapath,
                           sheet = "pERK-ERK",
                           skip = 1) %>%
                select(1:19) %>%
                rename(
                    "pERK_n1" = Readout,
                    "pERK_n2" = "...11",
                    "ERK_n1" = "...12",
                    "ERK_n2" = "...13",
                    "pERK/ERK_n1" = `pERK/ERK`,
                    "pERK/ERK_n2" = "...15",
                    "pERK/ERK_avg" = "...16",
                    "%Veh_n1" = "% Vehicle",
                    "%Veh_n2" = "...18",
                    "%Veh_avg" = "...19",
                    "Animal ID" = Sample
                ) %>%
                filter(!pERK_n1 %in% c("pERK", "n=1", "Readout"),
                       !is.na(pERK_n1),
                       `Animal ID` != "-") %>%
                fill(`Quotation No`) %>%
                fill(Group) %>%
                fill(Drug) %>%
                fill(Dose) %>%
                fill(Route) %>%
                fill(Regimen) %>%
                fill(Plate) %>%
                fill(Duration) %>%
                mutate(
                    "Hours Post Dose" = parse_double(str_replace(Duration, "h", "")),
                    "Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                    `%Veh_avg` = as.double(`%Veh_avg`),
                    pERK_n1 = as.double(pERK_n1),
                    pERK_n2 = as.double(pERK_n2),
                    ERK_n1 = as.double(ERK_n1),
                    ERK_n2 = as.double(ERK_n2),
                    `pERK/ERK_n1` = as.double(`pERK/ERK_n1`),
                    `pERK/ERK_n2` = as.double(`pERK/ERK_n2`),
                    `%Veh_n1` = as.double(`%Veh_n1`),
                    `%Veh_n2` = as.double(`%Veh_n2`),
                    `pERK/ERK_avg` = as.double(`pERK/ERK_avg`)
                ) %>%
                unite(Treatment, Drug, Dose, Route, Regimen, sep = "\n", remove = FALSE) %>%
                select(
                    `Quotation No`,
                    Plate,
                    Group,
                    `Group Number`,
                    Treatment,
                    Duration,
                    `Hours Post Dose`,
                    everything()
                )
        }
        return(data)
    })
    
    perkNorm <- reactive({
        perk_wb() %>%
            unite(Treatment, Drug, Dose, sep = "\n") %>%
            #left_join(stdesign()) %>%
            group_by(`Collection Timepoint`, `Hours Post Dose`) %>%
            mutate(`% of Vehicle`= `p-ERK/ERK`/mean(`p-ERK/ERK`[Group==input$pERKgroup.control])*100,
                   LFC =log2(`p-ERK/ERK`/mean(`p-ERK/ERK`[Group==input$pERKgroup.control]))) %>%
            arrange(`Group Number`, `Hours Post Dose`) %>%
            mutate(`Collection Timepoint`=factor(`Collection Timepoint`, levels=unique(`Collection Timepoint`)),
                   Treatment=factor(Treatment, levels=unique(Treatment)))
    })
    
    pk_msd <- reactive({
        full_join(perk_msd(), pk_pd()) %>%
            filter(!is.na(`%Veh_avg`), !is.na(Compound)) %>%
            mutate(`Animal ID` = parse_integer(`Animal ID`)) %>%
            arrange(`Hours Post Dose`, `Group Number`, `Animal ID`) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`))
    })
    
# Show user the study info, design, and data preview 
    
    output$StudyInfo <- renderDT({
        stinfo()
    }, options=list(scrollX=TRUE))
    
    output$StudyDesign <- renderDT({
        stdesign()
        }, filter="top", options=list(scrollX=TRUE))
    
    output$preview_tvbw <- renderDT({
        tvbw()
    }, filter="top", options=list(scrollX=TRUE))
    
    output$Txt.Ref.Stat <- renderDT({
        stdesign() %>%
            select(Group, Treatment) %>%
            mutate(Treatment = paste(Group, Treatment, sep=": ")) %>%
            select(-Group)
    }, options=list(dom="t", pageLength = 20))
    
    output$Txt.Ref.GC <- renderDT({
        stdesign() %>%
            select(Group, Treatment) %>%
            mutate(Treatment = paste(Group, Treatment, sep=": ")) %>%
            select(-Group)
    }, options=list(dom="t", pageLength = 20))
    
    output$preview_pk_efficacy <- renderDT({
        pk_efficacy()
    }, filter="top", options=list(scrollX=TRUE))
    
    output$preview_pkparams_efficacy <- renderDT({
        pkparams_efficacy()
    }, filter="top", options=list(scrollX=TRUE))
    
    output$preview_pk_pd <- renderDT({
        pk_pd()
    }, filter="top", options=list(scrollX=TRUE))
    
    output$preview_pkparams_pd <- renderDT({
        pkparams_pd()
    }, filter="top", options=list(scrollX=TRUE))
    
    output$preview_perk_wb <- renderDT({
        perk_wb()
    }, filter="top", options=list(scrollX=TRUE))
    
    output$preview_perk_msd <- renderDT({
        perk_msd()
    }, filter="top", options=list(scrollX=TRUE))
    
    output$PKparams <- renderDataTable({
        pkparams_efficacy() %>%
            left_join(stdesign()) %>%
            select(Group, Treatment, `Batch ID`, Administration, Dose, Dose_Unit, `Dose Frequency`, Mixture, Formulation, `Result Type`, Unit, Result) %>%
            pivot_wider(names_from = c(`Result Type`, Unit), names_sep="__", values_from=Result)
    }, filter="top", options=list(scrollX=TRUE))
    

# Submit data to sqlite database
    observeEvent(input$Submit_tvbw, {
        db.con <-
            DBI::dbConnect(RSQLite::SQLite(),
                           "Data/EffVisR-database-output.sqlite")
        
        dbWriteTable(
            conn = db.con,
            "tvbw",
            tvbw(),
            append = TRUE,
            overwrite = FALSE
        )
        DBI::dbDisconnect(db.con)
    })
    
    observeEvent(input$Submit_pk_efficacy, {
        db.con <-
            DBI::dbConnect(RSQLite::SQLite(),
                           "Data/EffVisR-database-output.sqlite")
        
        dbWriteTable(
            conn = db.con,
            "pk_efficacy",
            pk_efficacy(),
            append = TRUE,
            overwrite = FALSE
        )
        DBI::dbDisconnect(db.con)
    })
    
    observeEvent(input$Submit_pkparams_efficacy, {
        db.con <-
            DBI::dbConnect(RSQLite::SQLite(),
                           "Data/EffVisR-database-output.sqlite")
        
        dbWriteTable(
            conn = db.con,
            "pkparams_efficacy",
            pkparams_efficacy(),
            append = TRUE,
            overwrite = FALSE
        )
        DBI::dbDisconnect(db.con)
    })
    
    observeEvent(input$Submit_pk_pd, {
        db.con <-
            DBI::dbConnect(RSQLite::SQLite(),
                           "Data/EffVisR-database-output.sqlite")
        
        dbWriteTable(
            conn = db.con,
            "pk_pd",
            pk_pd(),
            append = TRUE,
            overwrite = FALSE
        )
        DBI::dbDisconnect(db.con)
    })
    
    observeEvent(input$Submit_pkparams_pd, {
        db.con <-
            DBI::dbConnect(RSQLite::SQLite(),
                           "Data/EffVisR-database-output.sqlite")
        
        dbWriteTable(
            conn = db.con,
            "pkparams_pd",
            pkparams_pd(),
            append = TRUE,
            overwrite = FALSE
        )
        DBI::dbDisconnect(db.con)
    })
    
    observeEvent(input$Submit_perk_wb, {
        db.con <-
            DBI::dbConnect(RSQLite::SQLite(),
                           "Data/EffVisR-database-output.sqlite")
        
        dbWriteTable(
            conn = db.con,
            "perk_wb",
            perk_wb(),
            append = TRUE,
            overwrite = FALSE
        )
        DBI::dbDisconnect(db.con)
    })
    
    observeEvent(input$Submit_perk_msd, {
        db.con <-
            DBI::dbConnect(RSQLite::SQLite(),
                           "Data/EffVisR-database-output.sqlite")
        
        dbWriteTable(
            conn = db.con,
            "perk_msd",
            perk_msd(),
            append = TRUE,
            overwrite = FALSE
        )
        DBI::dbDisconnect(db.con)
    })
    
# Show user list of studies from database
    output$DBstudies <- renderUI({
        db.con <-
            DBI::dbConnect(RSQLite::SQLite(),
                           "Data/EffVisR-database-output.sqlite")
        
        EFFstudies<-tbl(db.con, "tvbw") %>%
            select(`Quotation No`) %>%
            distinct() %>%
            collect()
        
        WBstudies<-tbl(db.con, "perk_wb") %>%
            select(`Quotation No`) %>%
            distinct() %>%
            collect()
        
        MSDstudies<-tbl(db.con, "perk_msd") %>%
            select(`Quotation No`) %>%
            distinct() %>%
            collect()
        
        DBStudies <- sort(unique(c(EFFstudies$`Quotation No`, WBstudies$`Quotation No`, MSDstudies$`Quotation No`)))
        
        selectInput(
            label = "Select Studies:",
            inputId = "DBselected",
            choices = DBStudies,
            selected = NULL,
            multiple = FALSE
        )
        #DBI::dbDisconnect(db.con)
    })
    
    output$SelectedStudy1 <- renderUI({
        if (input$datafrom=="Spreadsheet"){
            h4(paste("Study:", stinfo()$`Quotation No`, "Model:", stinfo()$`Tumor Cell Line`, "--loaded from Spreadsheet", sep="\t"))
        }else if(input$datafrom=="Database"){
            h4(paste("Study:", input$DBselected, "Model:", stinfo()$`Tumor Cell Line`, "--loaded from Database", sep="   "))
        }
    })
    
    output$SelectedStudy2 <- renderUI({
        if (input$datafrom=="Spreadsheet"){
            h4(paste("Study:", stinfo()$`Quotation No`, "Model:", stinfo()$`Tumor Cell Line`, "--loaded from Spreadsheet", sep="   "))
        }else if(input$datafrom=="Database"){
            h4(paste("Study:", input$DBselected, "Model:", stinfo()$`Tumor Cell Line`, "--loaded from Database", sep="   "))
        }
    })
        
    output$SelectedStudy3 <- renderUI({
        if (input$datafrom=="Spreadsheet"){
            h4(paste("Study:", stinfo()$`Quotation No`, "Model:", stinfo()$`Tumor Cell Line`, "--loaded from Spreadsheet", sep="   "))
        }else if(input$datafrom=="Database"){
            h4(paste("Study:", input$DBselected, "Model:", stinfo()$`Tumor Cell Line`, "--loaded from Database", sep="   "))
        }
    })
# Setup reactive user interface elements
    output$TVsumGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "TVsumPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$TVanimalGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "TVanimalPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$BWsumGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "BWsumPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$BWanimalGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "BWanimalPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$TVgrpPctGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "TVgrpPctPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$TVpctGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "TVpctPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$WfallDayUi <- renderUI({
        days <- tvbw() %>%
            ungroup() %>%
            filter(!is.na(`TV (mm3)`), !is.na(`Days on Study Treatment`)) %>%
            select(`Days on Study Treatment`) %>%
            distinct() %>%
            filter(`Days on Study Treatment` != min(`Days on Study Treatment`))
    selectInput(
        inputId = "wfallDay",
        label = "Choose Day:",
        choices = days$`Days on Study Treatment`,
        selected = max(days$`Days on Study Treatment`, na.rm = TRUE),
        width = "100px"
        )
    })
    
    output$WfallGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "WfallPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$tgiGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "tgiPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$tgiControlGroup <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Control:",
            inputId = "tgiPlot.control",
            choices = Groups,
            selected = "G1",
            multiple = FALSE
        )
    })
    
    output$tgiAucDayUi <- renderUI({
        days <- tvbw() %>%
            ungroup() %>%
            filter(!is.na(`TV (mm3)`), !is.na(`Days on Study Treatment`)) %>%
            select(`Days on Study Treatment`) %>%
            distinct() %>%
            filter(`Days on Study Treatment` != min(`Days on Study Treatment`))
        selectInput(
            inputId = "tgiAucDay",
            label = "Choose Days:",
            choices = days$`Days on Study Treatment`,
            selected = days$`Days on Study Treatment`,
            multiple = TRUE
        )
    })
    
    output$tgiAucGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "tgiAucPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$BWgrpPctGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "BWgrpPctPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$BWpctGroups <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "BWpctPlot.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$TGIControl <- renderUI({
        Groups <- unique(tvbw()$Group)
        selectInput(
            label = "Select Control:",
            inputId = "drapTGI.control",
            choices = Groups,
            selected = "G1",
            multiple = FALSE
        )
    })
    
    output$TGICutoff <- renderUI({
        days <- tvbw() %>%
            ungroup() %>%
            filter(!is.na(`TV (mm3)`), !is.na(`Days on Study Treatment`)) %>%
            select(`Days on Study Treatment`) %>%
            distinct() %>%
            filter(`Days on Study Treatment` != min(`Days on Study Treatment`))
        selectInput(
            label="TGI Cutoff Day:",
            inputId = "TGIdays",
            choices = days$`Days on Study Treatment`,
            selected = max(days$`Days on Study Treatment`))
    })
    
    output$GCCutoff <- renderUI({
        days <- tvbw() %>%
            ungroup() %>%
            filter(!is.na(`TV (mm3)`), !is.na(`Days on Study Treatment`)) %>%
            select(`Days on Study Treatment`) %>%
            distinct() %>%
            filter(`Days on Study Treatment` != min(`Days on Study Treatment`))
    selectInput(
        label="Growth Curve Cutoff Day:",
        inputId = "GCdays",
        choices = days$`Days on Study Treatment`,
        selected = max(days$`Days on Study Treatment`))
    })
    
    output$PKcurvesGroups <- renderUI({
        Groups <- unique(pk_efficacy()$Group)
        selectInput(
            label = "Select Groups:",
            inputId = "PKcurves.groups",
            choices = Groups,
            selected = Groups,
            multiple = TRUE
        )
    })
    
    output$pERKcontrolGroup <- renderUI({
        Groups <- unique(perk_wb()$Group)
        selectInput(
            label = "Control Group:",
            inputId = "pERKgroup.control",
            choices = Groups,
            multiple = FALSE
        )
    })
    
    # output$pERKcontrolGel <- renderUI({
    #     Gels <- unique(perk_wb()$Gel)
    #     selectInput(
    #         label = "Control Gel:",
    #         inputId = "pERKgel.control",
    #         choices = Gels,
    #         multiple = FALSE
    #     )
    # })
    
# Calculate TV and BW group summary statistics
    TVBW.summary <- reactive({
        tvbw() %>%
            group_by(`Study ID`, Group, Treatment, `Days on Study Treatment`) %>%
            summarise(
                `TV (n)` = sum(!is.na(`TV (mm3)`)),
                `BW (n)` = sum(!is.na(`BW (g)`)),
                `TV Mean` = mean(`TV (mm3)`, na.rm = TRUE),
                `BW Mean` = mean(`BW (g)`, na.rm = TRUE),
                `TV SD` = sd(`TV (mm3)`, na.rm = TRUE),
                `BW SD` = sd(`BW (g)`, na.rm = TRUE),
                `TV SEM` = `TV SD` / sqrt(`TV (n)`),
                `BW SEM` = `BW SD` / sqrt(`BW (n)`),
                `TV Avg Percent Change` = mean(`TV % Change`, na.rm =
                                                   TRUE),
                `BW Avg Percent Change` = mean(`BW % Change`, na.rm =
                                                   TRUE),
                `TV SD Percent Change` = sd(`TV % Change`, na.rm = TRUE),
                `BW SD Percent Change` = sd(`BW % Change`, na.rm = TRUE),
                `TV SEM Percent Change` = `TV SD Percent Change` / sqrt(`TV (n)`),
                `BW SEM Percent Change` = `BW SD Percent Change` / sqrt(`BW (n)`)
            ) 
    })
    
    tgi <- reactive({
        TVBW.summary() %>%
            filter(`TV (n)` > 0) %>%
            group_by(`Days on Study Treatment`) %>%
            mutate(`TV Control`= `TV Mean`[Group==input$tgiPlot.control]) %>%
            group_by(`Study ID`, Group, Treatment) %>%
            mutate(`T/C` = (`TV Mean`-`TV Mean`[`Days on Study Treatment`==min(`Days on Study Treatment`)])/(`TV Control`-`TV Control`[`Days on Study Treatment`==min(`Days on Study Treatment`)])*100) %>%
            mutate(TGI=1-`T/C`+100) %>%
            mutate(`% Inhibition TV`=(`TV Control`-`TV Mean`)/`TV Control`*100) %>%
            arrange(`Days on Study Treatment`) %>%
            slice(-1)

    })
    
    PkTgi <- reactive({
        pkparams_efficacy() %>%
        left_join(tgi())
        })
        
    stat.testing <- reactive({
        if (input$stattest == "anova_tukey"){
            tvbw() %>% 
                ungroup() %>%
                filter(!is.na(`TV (mm3)`)) %>%
                group_by(`Days on Study Treatment`) %>%
                tukey_hsd(`TV (mm3)`~Group) %>%
                arrange(desc(`Days on Study Treatment`))
        } else if(input$stattest == "kw_dunn"){
            tvbw() %>% 
                ungroup() %>%
                filter(!is.na(`TV (mm3)`)) %>%
                group_by(`Days on Study Treatment`) %>%
                dunn_test(`TV (mm3)`~Group) %>%
                arrange(desc(`Days on Study Treatment`))
        }
    })
    
    output$stat.results <-renderDT({
        stat.testing()
    }, filter="top")
        
    output$AllGC.results <- renderDT({
        compareGC(data = oneAN.tv()%>%filter(Times <= input$GCdays), compare.to = 'all', n = 300, 
              fun = MeanT, adjust = "BH")
    }, filter="top")
        
# Color Scheme
    c25 <- c(
        "grey60",
        "darkturquoise",
        "dodgerblue2", 
        "mediumpurple",
        "deepskyblue4",
        "blue3",
        "deepskyblue3",
        "#CAB2D6", # lt purple
        "orchid1",
        "#E31A1C", # red
        "green4",
        "#FF7F00", # orange
        "gold1",
        "skyblue2", "#FB9A99", # lt pink
        "palegreen2",
        "#FDBF6F", # lt orange
        "gray70", "khaki2",
        "maroon",  "deeppink1", "blue1", "steelblue4",
         "green1", "yellow4", "yellow3",
        "darkorange4", "brown"
    )
    
    # Shape Scheme
    s25 <- c(21, 22, 23, 24, 25, 21, 22, 23, 24, 25, 21, 22, 23, 24, 25, 21, 22, 23, 24, 25, 21, 22, 23, 24, 25)
    
# Setup reactive plotting functions

    TVsumPlot <- reactive({
        
        TVBW.summary() %>%
            filter(Group %in% input$TVsumPlot.groups,
                   !is.na(`TV Mean`)) %>%
                ungroup() %>%
                mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                       Treatment = paste(Group, Treatment, sep=": ")) %>%
                mutate(Treatment_Syn = str_replace_all(Treatment, c("FT002787-12"="KIN002787", "FT000960-07"="LXH254"))) %>%
                mutate(BatchID = fct_reorder(Treatment, `Group Number`)) %>%
                mutate(Compound = fct_reorder(Treatment_Syn, `Group Number`)) %>%
            ggplot() +
            aes_string("`Days on Study Treatment`",
                "`TV Mean`" ,
                group = "Group",
                color = input$TxtNames) +
            geom_line() +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            scale_shape_manual(values=s25) +
            geom_errorbar(aes(ymax = `TV Mean` + `TV SEM`, 
                              ymin =
                                  `TV Mean` - `TV SEM`), width = 0.3) +
            geom_point(aes_string(fill = input$TxtNames, shape=input$TxtNames), color =
                           "black", size=2.5) +
                labs(color="Treatment", fill="Treatment", shape="Treatment") +
            ylab(expression(paste("Mean Tumor Volume (",mm^3,")", "+/- SEM", sep = " "))) +
                {if (input$BWinset==TRUE) annotation_custom(ggplotGrob(BWsumPlot.ins()), 
                                  xmin = min(TVBW.summary()$`Days on Study Treatment`)-0.5, 
                                  xmax = max(TVBW.summary()$`Days on Study Treatment`/2.5), 
                                  ymin = max(TVBW.summary()$`TV Mean`, na.rm = TRUE) - 
                                      (max(TVBW.summary()$`TV Mean`, na.rm = TRUE)-min(TVBW.summary()$`TV Mean`, na.rm=TRUE))*0.35, 
                                  ymax = max(TVBW.summary()$`TV Mean`, na.rm = TRUE)+max(TVBW.summary()$`TV SEM`, na.rm = TRUE))} +
            scale_x_continuous(breaks=unique(TVBW.summary()$`Days on Study Treatment`)) +
            theme_classic(base_size = 12) +
            theme(axis.text = element_text(color="black"))
            
        })
    
    output$TVsumPlot <- renderPlot({
        TVsumPlot()
    })
    
    TVanimalPlot <- reactive({
        tvbw() %>%
            ungroup() %>%
            filter(Group %in% input$TVanimalPlot.groups,
                   !is.na(`TV (mm3)`)) %>%
            ungroup() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                   Treatment = paste(Group, Treatment, sep=": ")) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            mutate(Group = fct_reorder(Group, `Group Number`)) %>%
            ggplot() +
            aes(`Days on Study Treatment`,
                `TV (mm3)`,
                group = `Animal ID`,
                color = Group) +
            geom_line() +
            geom_point(color = "grey30",
                       shape = 1,
                       size = 0.5) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            theme_bw(base_size = 12) +
            ylab(expression(bold(paste("Tumor Volume (",mm^3,")",sep = " ")))) +
            facet_wrap(. ~ Treatment, nrow = input$TVanimalPlot.rows) +
            theme(strip.text = element_text(face = "bold", size=8),
                  axis.text=element_text(color="black"),
                  legend.position = "none")
    })
    
    output$TVanimalPlot <- renderPlot({
        TVanimalPlot()
    })
    
    TVgrpPctPlot <- reactive({
        
        TVBW.summary() %>%
            filter(
                Group %in% input$TVgrpPctPlot.groups,
                !is.na(`TV Avg Percent Change`)
            ) %>%
                ungroup() %>%
                mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                       Treatment = paste(Group, Treatment, sep=": ")) %>%
                mutate(Treatment_Syn = str_replace_all(Treatment, c("FT002787-12"="KIN002787", "FT000960-07"="LXH254", "FT000959-04"="Futi"))) %>%
                mutate(BatchID = fct_reorder(Treatment, `Group Number`)) %>%
                mutate(Compound = fct_reorder(Treatment_Syn, `Group Number`)) %>%
            ggplot() +
            aes_string(
                "`Days on Study Treatment`",
                "`TV Avg Percent Change`",
                group = "Group",
                color = input$TxtgrpPctNames
            ) +
            geom_hline(yintercept = 0, lty=2) +
            geom_line() +
            geom_point(aes_string(fill = input$TxtgrpPctNames, shape=input$TxtgrpPctNames), color =
                           "black", size=2.5) +
            geom_errorbar(
                aes(
                    ymax = `TV Avg Percent Change` + `TV SEM Percent Change`,
                    ymin = `TV Avg Percent Change` - `TV SEM Percent Change`
                ),
                width = 0.3
            ) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            scale_shape_manual(values=s25) +
            ylab("Average % Tumor Volume Change") +
            labs(color="Treatment", fill="Treatment", shape="Treatment") +
                 {if(input$BWpctinset==TRUE) annotation_custom(ggplotGrob(BWgrpPctPlot.ins()), 
                                                               xmin = min(TVBW.summary()$`Days on Study Treatment`)-0.5, 
                                                               xmax = max(TVBW.summary()$`Days on Study Treatment`/2.5), 
                                                               ymin = max(TVBW.summary()$`TV Avg Percent Change`, na.rm = TRUE) - 
                                                                   (max(TVBW.summary()$`TV Avg Percent Change`, na.rm = TRUE) - 
                                                                        min(TVBW.summary()$`TV Avg Percent Change`, na.rm=TRUE))*0.35, 
                                                               ymax = max(TVBW.summary()$`TV Avg Percent Change`, na.rm = TRUE) + 
                                                                   max(TVBW.summary()$`TV SEM Percent Change`, na.rm = TRUE))} +
            scale_x_continuous(breaks=unique(TVBW.summary()$`Days on Study Treatment`)) +
            theme_classic(base_size = 12) +
            theme(axis.text = element_text(color="black")) 
            
    })
    
    output$TVgrpPctPlot <- renderPlot({
        TVgrpPctPlot()
    })
    
    TVpctPlot <- reactive(
        tvbw() %>%
            ungroup() %>%filter(Group %in% input$TVpctPlot.groups,
                                !is.na(`TV % Change`)) %>%
            ungroup() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                   Treatment = paste(Group, Treatment, sep=": ")) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            mutate(Group = fct_reorder(Group, `Group Number`)) %>%
            ggplot() +
            aes(
                `Days on Study Treatment`,
                `TV % Change`,
                group = `Animal ID`,
                color = Group
            ) +
            geom_hline(yintercept = 0, lty=2) +
            geom_line() +
            geom_point(
                color = "grey30",
                size = 0.5,
                shape = 1
            ) +
            theme_bw(base_size = 12) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            ylab("% Change in Tumor Volume") +
            facet_wrap(. ~ Treatment, nrow = input$TVpctPlot.rows) +
            theme(strip.text = element_text(face = "bold", size=8),
                  axis.text=element_text(color="black"),
                  legend.position = "none")
    )
    
    output$TVpctPlot <- renderPlot({
        TVpctPlot()
    })
    
    WfallPlot <- reactive(
        tvbw() %>%
            filter(`Days on Study Treatment` == input$wfallDay,
                   Group %in% input$WfallPlot.groups,
                   !is.na(`TV % Change`)) %>%
            ungroup() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                   Treatment = paste(Group, Treatment, sep=": ")) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            mutate(Group = fct_reorder(Group, `Group Number`)) %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$"))) %>%
            mutate(Treatment_Syn = str_replace_all(Treatment, c("FT002787-12"="KIN002787", "FT000960-07"="LXH254"))) %>%
            mutate(BatchID = fct_reorder(Treatment, `Group Number`)) %>%
            mutate(Compound = fct_reorder(Treatment_Syn, `Group Number`)) %>%
            ungroup() %>%
            arrange(Treatment, desc(`TV % Change`)) %>%
            mutate(
                `Animal ID` = factor(`Animal ID`, levels = `Animal ID`)
            ) %>%
            ggplot() +
            aes_string("`Animal ID`", "`TV % Change`", fill =
                    input$WfallTxtNames) +
            geom_col(color="black", size=0.3) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            theme_classic(base_size = 12) +
            geom_hline(yintercept = 0) +
            geom_hline(yintercept = -30, lty=2) +
            ylab(paste(
                "% Change in Tumor Volume on Day", input$wfallDay
            )) +
            labs(fill="Treatment", color="Treatment") +
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y=element_text(color="black"))
    )
    
    output$WfallPlot <- renderPlot({
        WfallPlot()
    })
    
    tgiPlot <- reactive(
        tgi() %>%
            filter(Group %in% input$tgiPlot.groups,
                   !is.na(`TV Mean`)) %>%
            ungroup() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                   Treatment = paste(Group, Treatment, sep=": ")) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            ggplot() +
            aes(`Days on Study Treatment`, `% Inhibition TV`, color=Treatment) +
            geom_point(aes(fill = Treatment, shape=Treatment), color = "black", size=2.5) +
            geom_line() +
            #geom_col(position=position_dodge()) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            scale_shape_manual(values=s25) +
            theme_classic(base_size = 12) +
            geom_hline(yintercept = 0) +
            theme(axis.text.x = element_text(color="black"),
                  axis.ticks.x = element_blank(),
                  axis.text.y=element_text(color="black"))
    )
    
    output$tgiPlot <- renderPlot({
        tgiPlot()
    })
    
    tgiAucPlot <- reactive(
        PkTgi() %>%
            filter(`Result Type`=="AUClast") %>%
            mutate(`AUClast (hr*ng/mL)`=Result) %>%
            filter(`Days on Study Treatment` %in% input$tgiAucDay, Group %in% input$tgiAucPlot.groups) %>%
            arrange(Group) %>%
            mutate(Treatment = factor(Treatment, levels = unique(Treatment))) %>%
            ggplot() +
            aes(`AUClast (hr*ng/mL)`, TGI) +
            geom_point(aes(fill=Treatment), color="black", size=3.5, shape=21) +
            #scale_color_manual(values=c25) +
            #scale_fill_manual(values=c25) +
            theme_bw(base_size = 12) +
            facet_grid(`Days on Study Treatment`~`Batch ID`, scales = "free") +
            theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1, color="black"), 
                  strip.text = element_text(face="bold"))
    )
    
    output$tgiAucPlot <- renderPlot({
        tgiAucPlot()
    })
    
    BWsumPlot <- reactive(
        TVBW.summary() %>%
            filter(Group %in% input$BWsumPlot.groups,
                   !is.na(`BW Mean`)) %>%
            ungroup() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                   Treatment = paste(Group, Treatment, sep=": ")) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            ggplot() +
            aes(
                `Days on Study Treatment`,
                `BW Mean`,
                group = Group,
                color = Treatment
            ) +
            geom_line() +
            geom_point(aes(fill = Treatment, shape=Treatment), color = "black", size=2.5) +
            ylim(15, NA) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            scale_shape_manual(values=s25) +
            geom_errorbar(aes(
                ymax = `BW Mean` + `BW SEM`, ymin = `BW Mean` - `BW SEM`
            ), width = 0.3) +
            ylab("Mean Body Weight (g) +/- SEM") +
            scale_x_continuous(breaks=unique(TVBW.summary()$`Days on Study Treatment`)) +
            theme_classic(base_size = 12) +
            theme(axis.text = element_text(color="black"))
    )
    
    output$BWsumPlot <- renderPlot({
        BWsumPlot()
    })
    
    BWsumPlot.ins <- reactive(
        TVBW.summary() %>%
            filter(Group %in% input$TVsumPlot.groups,
                   !is.na(`BW Mean`)) %>%
            ungroup() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                   Treatment = paste(Group, Treatment, sep=": ")) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            ggplot() +
            aes(
                `Days on Study Treatment`,
                `BW Mean`,
                group = Group,
                color = Treatment
            ) +
            geom_line() +
            geom_point(aes(fill = Treatment, shape=Treatment), color = "black", size=1.5) +
            ylim(15, NA) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            scale_shape_manual(values=s25) +
            geom_errorbar(aes(
                ymax = `BW Mean` + `BW SEM`, ymin = `BW Mean` - `BW SEM`
            ), width = 0.3) +
            ylab("Mean BW (g) +/- SEM") +
            scale_x_continuous(breaks=unique(TVBW.summary()$`Days on Study Treatment`)) +
            xlab("") +
            theme_classic(base_size = 8) +
            theme(axis.text = element_text(color="black", size = 6),
                  legend.position = "none",
                  plot.margin = margin(0,0,-1, 0, "pt"),
                  plot.background = element_rect(fill="transparent", color = "transparent"),
                  panel.background = element_rect(fill="transparent", color = "transparent"))
    )
    
    BWanimalPlot <- reactive(
        tvbw() %>%
            ungroup() %>%
            filter(Group %in% input$BWanimalPlot.groups,
                   !is.na(`BW (g)`)) %>%
            ungroup() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                   Treatment = paste(Group, Treatment, sep=": ")) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            mutate(Group = fct_reorder(Group, `Group Number`)) %>%
            ggplot() +
            aes(
                `Days on Study Treatment`,
                `BW (g)`,
                group = `Animal ID`,
                color = Group
            ) +
            geom_line() +
            geom_point(
                shape = 1,
                size = 0.5,
                color = "grey30"
            ) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            theme_bw(base_size = 12) +
            ylab("Body Weight (g)") +
            facet_wrap(. ~ Treatment, nrow = input$BWanimalPlot.rows) +
            theme(strip.text = element_text(face = "bold", size=8),
                  axis.text=element_text(color="black"),
                  legend.position = "none")
    )
    
    output$BWanimalPlot <- renderPlot({
        BWanimalPlot()
    })
    
    BWgrpPctPlot <- reactive({
        
        TVBW.summary() %>%
            filter(
                Group %in% input$BWgrpPctPlot.groups,
                !is.na(`BW Avg Percent Change`)
            ) %>%
            ungroup() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                   Treatment = paste(Group, Treatment, sep=": ")) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            ggplot() +
            aes(
                `Days on Study Treatment`,
                `BW Avg Percent Change`,
                group = Group,
                color = Treatment
            ) +
            annotate(
                geom = "rect",
                xmin = -Inf,
                xmax = Inf,
                ymin = -15,
                ymax = -10,
                fill = "pink",
                colour = "transparent",
                alpha = 0.5
            ) +
            annotate(
                geom = "rect",
                xmin = -Inf,
                xmax = Inf,
                ymin = -Inf,
                ymax = -15,
                fill = "violetred2",
                colour = "transparent",
                alpha = 0.3
            ) +
            geom_hline(yintercept = 0, lty=2) +
            geom_line() +
            geom_errorbar(
                aes(
                    ymax = `BW Avg Percent Change` + `BW SEM Percent Change`,
                    ymin = `BW Avg Percent Change` - `BW SEM Percent Change`
                ),
                width = 0.3
            ) +
            geom_point(aes(fill = Treatment, shape=Treatment), color =
                           "black", size=2.5) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            scale_shape_manual(values=s25) +
            ylab("Average % Body Weight Change") +
            scale_x_continuous(breaks=unique(TVBW.summary()$`Days on Study Treatment`)) +
            theme_classic(base_size = 12) +
            theme(axis.text = element_text(color="black"))
        
    })
    
    output$BWgrpPctPlot <- renderPlot({
        BWgrpPctPlot()
    })
    
    BWgrpPctPlot.ins <- reactive(
        TVBW.summary() %>%
            filter(
                Group %in% input$TVgrpPctPlot.groups,
                !is.na(`BW Avg Percent Change`)
            ) %>%
            ungroup() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                   Treatment = paste(Group, Treatment, sep=": ")) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            ggplot() +
            aes(
                `Days on Study Treatment`,
                `BW Avg Percent Change`,
                group = Group,
                color = Treatment
            ) +
            annotate(
                geom = "rect",
                xmin = -Inf,
                xmax = Inf,
                ymin = -15,
                ymax = -10,
                fill = "pink",
                colour = "transparent",
                alpha = 0.5
            ) +
            annotate(
                geom = "rect",
                xmin = -Inf,
                xmax = Inf,
                ymin = -Inf,
                ymax = -15,
                fill = "violetred2",
                colour = "transparent",
                alpha = 0.3
            ) +
            geom_hline(yintercept = 0, lty=2) +
            geom_line() +
            geom_errorbar(
                aes(
                    ymax = `BW Avg Percent Change` + `BW SEM Percent Change`,
                    ymin = `BW Avg Percent Change` - `BW SEM Percent Change`
                ),
                width = 0.3
            ) +
            geom_point(aes(fill = Treatment, shape=Treatment), color =
                           "black", size=1.5) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            scale_shape_manual(values=s25) +
            ylab("Average % BW Change") +
            xlab("") +
            scale_x_continuous(breaks=unique(TVBW.summary()$`Days on Study Treatment`)) +
            theme_classic(base_size = 8) +
            theme(axis.text = element_text(color="black"),
                  legend.position = "none",
                  plot.margin = margin(0,0,-1, 0, "pt"),
                  plot.background = element_rect(fill="transparent", color = "transparent"),
                  panel.background = element_rect(fill="transparent", color = "transparent"))
    )
    
    BWpctPlot <- reactive(
        tvbw() %>%
            ungroup() %>%
            filter(Group %in% input$BWpctPlot.groups, !is.na(`BW % Change`)) %>%
            ungroup() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$")),
                   Treatment = paste(Group, Treatment, sep=": ")) %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            mutate(Group = fct_reorder(Group, `Group Number`)) %>%
            ggplot() +
            aes(
                `Days on Study Treatment`,
                `BW % Change`,
                group = `Animal ID`,
                color = Group
            ) +
            geom_hline(yintercept = 0, lty=2) +
            annotate(
                geom = "rect",
                xmin = -Inf,
                xmax = Inf,
                ymin = -15,
                ymax = -10,
                fill = "pink",
                colour = "transparent",
                alpha = 0.5
            ) +
            annotate(
                geom = "rect",
                xmin = -Inf,
                xmax = Inf,
                ymin = -Inf,
                ymax = -15,
                fill = "violetred2",
                colour = "transparent",
                alpha = 0.3
            ) +
            scale_color_manual(values=c25) +
            scale_fill_manual(values=c25) +
            geom_line() +
            geom_point(
                size = 0.5,
                color = "grey30",
                shape = 1
            ) +
            theme_minimal(base_size = 12) +
            ylab("% Change in Body Weight") +
            facet_wrap(. ~ Treatment, nrow = input$BWpctPlot.rows) +
            theme(strip.text = element_text(face = "bold", size=8),
                  axis.text=element_text(color="black"),
                  legend.position = "none")
    )
    
    output$BWpctPlot <- renderPlot({
        BWpctPlot()
    })
    
    oneAN.tv <- reactive({
        tvbw() %>%
            ungroup() %>%
            select(Group, Treatment, `Animal ID`, `Days on Study Treatment`, `TV (mm3)`) %>%
            rename(Arms=Group, ID=`Animal ID`, Times=`Days on Study Treatment`, Volume=`TV (mm3)`) %>%
            filter(!is.na(Volume)) 
    })
  
    # output$stat.results <- renderPrint({
    #     DRAnalysis(oneAN.tv(), pattern="oneAN", method=input$stattest)
    # })
      
    oneAN.tgi <- reactive({
        TGI(data = oneAN.tv(), neg.control = input$drapTGI.control, method=input$tgiMethod, pattern = "oneAN")
    })
    
    output$drapTGI <- renderPlot({
        oneAN.tgi() %>%
            ggplot() +
            aes(Times, TGI, group=Arms, color=Arms) +
            geom_line() +
            geom_point(aes(shape=Arms, fill=Arms), size=2.5, color="black") +
            theme_bw(base_size = 12) +
            scale_color_manual(values=c25[-1]) +
            scale_fill_manual(values=c25[-1]) +
            scale_shape_manual(values=s25) +
            theme(axis.text = element_text(color="black"))
    })
    
    output$PKcurves <- renderPlot({
        if (input$PKlog==TRUE) {
        pk_efficacy() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$"))) %>%
            mutate(Group = fct_reorder(Group, `Group Number`),
                   Compound = fct_reorder(Compound, `Group Number`)) %>%
            group_by(Compound, Group, `Hours Post Dose`) %>%
            summarise(N=sum(!is.na(`Calculated Concentration (ng/mL)`)),
                      `Avg Conc (ng/mL)`=mean(`Calculated Concentration (ng/mL)`, na.rm=TRUE),
                      SEM = sd(`Calculated Concentration (ng/mL)`, na.rm=TRUE)/sqrt(N)) %>%
            filter(Group %in% input$PKcurves.groups) %>%
            arrange(Group) %>%
            ggplot() +
            aes(`Hours Post Dose`, `Avg Conc (ng/mL)`, group=Group, color=Group) +
            geom_point(size=2.5) +
            geom_line() +
            geom_errorbar(aes(ymax=`Avg Conc (ng/mL)`+SEM, ymin=`Avg Conc (ng/mL)`-SEM), width=0.5) +
            scale_y_log10() +
            scale_x_continuous(breaks = unique(pk_efficacy()$`Hours Post Dose`), limits = c(0, NA)) +
            scale_color_manual(values=c25[-1]) +
            theme_bw(base_size = 12) +
            facet_grid(.~Compound) +
            theme(strip.text = element_text(face="bold"),
                  axis.text.y = element_text(color = "black"), 
                  axis.text.x = element_text(angle=90, vjust=0.5, hjust=1, color="black"))
        } else {
        pk_efficacy() %>%
            mutate("Group Number" = parse_double(str_extract(Group, "[0-9]{1,2}$"))) %>%
            mutate(Group = fct_reorder(Group, `Group Number`),
                   Compound = fct_reorder(Compound, `Group Number`)) %>%
            group_by(Compound, Group, `Hours Post Dose`) %>%
            summarise(N=sum(!is.na(`Calculated Concentration (ng/mL)`)),
                      `Avg Conc (ng/mL)`=mean(`Calculated Concentration (ng/mL)`, na.rm=TRUE),
                      SEM = sd(`Calculated Concentration (ng/mL)`, na.rm=TRUE)/sqrt(N)) %>%
            filter(Group %in% input$PKcurves.groups) %>%
            arrange(Group) %>%
            ggplot() +
            aes(`Hours Post Dose`, `Avg Conc (ng/mL)`, group=Group, color=Group) +
            geom_point(size=2.5) +
            geom_line() +
            geom_errorbar(aes(ymax=`Avg Conc (ng/mL)`+SEM, ymin=`Avg Conc (ng/mL)`-SEM), width=0.5) +
            scale_x_continuous(breaks = unique(pk_efficacy()$`Hours Post Dose`), limits = c(0, NA)) +
            scale_color_manual(values=c25[-1]) +
            theme_bw(base_size = 12) +
            facet_grid(.~Compound) +
            theme(strip.text = element_text(face="bold"),
                  axis.text.y = element_text(color = "black"), 
                  axis.text.x = element_text(angle=90, vjust=0.5, hjust=1, color="black"))
        }
    })
    
    output$pERK_wb_plot <- renderPlot({
        perkNorm() %>%
        ggplot() +
            aes(`Hours Post Dose`, LFC, color=Treatment) +
            geom_hline(yintercept = 0, lty=3) +
            stat_summary(geom="area", fun=median, alpha=0.2, fill="deeppink4") +
            stat_summary(aes(group=Treatment), geom = "smooth", fun=median) +
            stat_summary(fun=median, fun.max=max, fun.min = min) +
            geom_point(shape=1, color="black") +
            theme_bw(base_size = 12) +
            ylab("Log2-Fold-Change") +
            scale_color_manual(values=c25) +
            scale_x_continuous(breaks=unique(perkNorm()$`Hours Post Dose`)) +
            facet_grid(.~Treatment) +
            theme(strip.text = element_text(face="bold", size=8),
                  axis.text = element_text(color = "black"), 
                  legend.position = "none")
    })
    
    output$pERK_msd_plot <- renderPlot({
        perk_msd() %>%
            mutate(Treatment = fct_reorder(Treatment, `Group Number`)) %>%
            ggplot() +
            aes(`Hours Post Dose`, `%Veh_avg`, color=Treatment) +
            geom_hline(yintercept = 100, lty=3) +
            #stat_summary(geom="area", fun=median, alpha=0.2, fill="deeppink4") +
            stat_summary(aes(group=Treatment), geom = "smooth", fun=median) +
            stat_summary(fun=median, fun.max=max, fun.min = min) +
            geom_point(shape=1, color="black") +
            theme_bw(base_size = 12) +
            ylab("% of Vehicle") +
            scale_color_manual(values=c25) +
            scale_x_continuous(breaks=unique(perk_msd()$`Hours Post Dose`)) +
            facet_grid(.~Treatment) +
            theme(strip.text = element_text(face="bold", size=8),
                  axis.text = element_text(color = "black"), 
                  legend.position = "none")
    })
    
    output$pk_pd_msd <- renderPlot({
        pk_msd() %>%
            mutate(`Hours Post Dose` = as.factor(`Hours Post Dose`)) %>%
        ggplot() +
        aes(`Calculated Concentration (ng/mL)`, `%Veh_avg`) +
        geom_smooth(method = drm, method.args = list(fct = L.4()), se = FALSE, color="grey30", size=1.5) +
        geom_point(aes(fill=`Hours Post Dose`, shape=`Hours Post Dose`), color="black", stroke=0.1, size=3.5) +
        geom_point(aes(color=Regimen), size=1.2, shape=16) +
        scale_x_log10() +
        geom_hline(yintercept = 100) +
        geom_hline(yintercept=50, lty=3) +    
        scale_shape_manual(values=s25) +
        scale_fill_manual(values=c25) +
        scale_color_manual(values=c("darkgoldenrod1", "black")) +
        facet_grid(.~Compound, scales = "free_x") +
        theme_bw(base_size = 12) +
            theme(strip.text = element_text(face="bold", size=8),
                  axis.text = element_text(color = "black"))
    })
    
    output$pk_pd_wb <- renderPlot({
        pk_wb() %>%
            filter(!is.na(`Calculated Concentration (ng/mL)`), !is.na(Treatment)) %>%
            ggplot() +
            aes(`Calculated Concentration (ng/mL)`, `%Vehicle`, color=Treatment) +
            geom_point() +
            geom_smooth() +
            scale_x_log10() +
            theme_bw(base_size = 12) +
            geom_hline(yintercept = 100) +
            facet_grid(`Hours Post Dose`~Compound, scales="free") +
            theme(legend.position = "right")
    })
    
# Handlers to download figures
    output$TVsumPlot.dl <- downloadHandler(
        filename=function() {
            paste("Group_Mean_TVs_", Sys.Date(), input$TVsumPlot.device, sep="")
        },
        content=function(file) {
            if(input$TVsumPlot.device==".png"){
                png(file, width = input$TVsumPlot.width, height=input$TVsumPlot.height, res=300, units="in")
            }
            else if (input$TVsumPlot.device==".svg"){
                svg(file, width = input$TVsumPlot.width, height=input$TVsumPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$TVsumPlot.width, height=input$TVsumPlot.height)
            }
            print(TVsumPlot())
            dev.off()
        },
        contentType="image/png"
    )
    
    output$TVanimalPlot.dl <- downloadHandler(
        filename=function() {
            paste("Animal_TVs_", Sys.Date(), input$TVanimalPlot.device, sep="")
        },
        content=function(file) {
            if(input$TVanimalPlot.device==".png"){
                png(file, width = input$TVanimalPlot.width, height=input$TVanimalPlot.height, res=300, units="in")
            }
            else if (input$TVanimalPlot.device==".svg"){
                svg(file, width = input$TVanimalPlot.width, height=input$TVanimalPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$TVanimalPlot.width, height=input$TVanimalPlot.height)
            }
            print(TVanimalPlot())
            dev.off()
        },
        contentType="image/png"
    )
    
    output$BWsumPlot.dl <- downloadHandler(
        filename=function() {
            paste("Group_Mean_BWs_", Sys.Date(), input$BWsumPlot.device, sep="")
        },
        content=function(file) {
            if(input$BWsumPlot.device==".png"){
                png(file, width = input$BWsumPlot.width, height=input$BWsumPlot.height, res=300, units="in")
            }
            else if (input$BWsumPlot.device==".svg"){
                svg(file, width = input$BWsumPlot.width, height=input$BWsumPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$BWsumPlot.width, height=input$BWsumPlot.height)
            }
            print(BWsumPlot())
            dev.off()
        },
        contentType="image/png"
    )
    
    output$BWanimalPlot.dl <- downloadHandler(
        filename=function() {
            paste("Animal_BWs_", Sys.Date(), input$BWanimalPlot.device, sep="")
        },
        content=function(file) {
            if(input$BWanimalPlot.device==".png"){
                png(file, width = input$BWanimalPlot.width, height=input$BWanimalPlot.height, res=300, units="in")
            }
            else if (input$BWanimalPlot.device==".svg"){
                svg(file, width = input$BWanimalPlot.width, height=input$BWanimalPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$BWanimalPlot.width, height=input$BWanimalPlot.height)
            }
            print(BWanimalPlot())
            dev.off()
        },
        contentType="image/png"
    )
    
    output$TVgrpPctPlot.dl <- downloadHandler(
        filename=function() {
            paste("Group_Mean_Percent_Change_TB_", Sys.Date(), input$TVgrpPctPlot.device, sep="")
        },
        content=function(file) {
            if(input$TVgrpPctPlot.device==".png"){
                png(file, width = input$TVgrpPctPlot.width, height=input$TVgrpPctPlot.height, res=300, units="in")
            }
            else if (input$TVgrpPctPlot.device==".svg"){
                svg(file, width = input$TVgrpPctPlot.width, height=input$TVgrpPctPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$TVgrpPctPlot.width, height=input$TVgrpPctPlot.height)
            }
            print(TVgrpPctPlot())
            dev.off()
        },
        contentType="image/png"
    )
    
    output$WfallPlot.dl <- downloadHandler(
        filename=function() {
            paste("WaterFall_", Sys.Date(), input$WfallPlot.device, sep="")
        },
        content=function(file) {
            if(input$WfallPlot.device==".png"){
                png(file, width = input$WfallPlot.width, height=input$WfallPlot.height, res=300, units="in")
            }
            else if (input$WfallPlot.device==".svg"){
                svg(file, width = input$WfallPlot.width, height=input$WfallPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$WfallPlot.width, height=input$WfallPlot.height)
            }
            print(WfallPlot())
            dev.off()
        },
        contentType="image/png"
    )
    
    output$tgiPlot.dl <- downloadHandler(
        filename=function() {
            paste("WaterFall_", Sys.Date(), input$tgiPlot.device, sep="")
        },
        content=function(file) {
            if(input$tgiPlot.device==".png"){
                png(file, width = input$tgiPlot.width, height=input$tgiPlot.height, res=300, units="in")
            }
            else if (input$tgiPlot.device==".svg"){
                svg(file, width = input$tgiPlot.width, height=input$tgiPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$tgiPlot.width, height=input$tgiPlot.height)
            }
            print(tgiPlot())
            dev.off()
        },
        contentType="image/png"
    )
    
    output$tgiAucPlot.dl <- downloadHandler(
        filename=function() {
            paste("TGI_by_AUClast_", Sys.Date(), input$tgiAucPlot.device, sep="")
        },
        content=function(file) {
            if(input$tgiAucPlot.device==".png"){
                png(file, width = input$tgiAucPlot.width, height=input$tgiAucPlot.height, res=300, units="in")
            }
            else if (input$tgiAucPlot.device==".svg"){
                svg(file, width = input$tgiAucPlot.width, height=input$tgiAucPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$tgiAucPlot.width, height=input$tgiAucPlot.height)
            }
            print(tgiAucPlot())
            dev.off()
        },
        contentType="image/png"
    )
    
    output$TVpctPlot.dl <- downloadHandler(
        filename=function() {
            paste("Animal_Percent_Change_TV_", Sys.Date(), input$TVpctPlot.device, sep="")
        },
        content=function(file) {
            if(input$TVpctPlot.device==".png"){
                png(file, width = input$TVpctPlot.width, height=input$TVpctPlot.height, res=300, units="in")
            }
            else if (input$TVpctPlot.device==".svg"){
                svg(file, width = input$TVpctPlot.width, height=input$TVpctPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$TVpctPlot.width, height=input$TVpctPlot.height)
            }
            print(TVpctPlot())
            dev.off()
        },
        contentType="image/png"
    )
    
    
    output$BWpctPlot.dl <- downloadHandler(
        filename=function() {
            paste("Animal_Percent_Change_BW_", Sys.Date(), input$BWpctPlot.device, sep="")
        },
        content=function(file) {
            if(input$BWpctPlot.device==".png"){
                png(file, width = input$BWpctPlot.width, height=input$BWpctPlot.height, res=300, units="in")
            }
            else if (input$BWpctPlot.device==".svg"){
                svg(file, width = input$BWpctPlot.width, height=input$BWpctPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$BWpctPlot.width, height=input$BWpctPlot.height)
            }
            print(BWpctPlot())
            dev.off()
        },
        contentType="image/png"
    )

    output$BWgrpPctPlot.dl <- downloadHandler(
        filename=function() {
            paste("Group_Mean_Percent_Change_BW_", Sys.Date(), input$BWgrpPctPlot.device, sep="")
        },
        content=function(file) {
            if(input$BWgrpPctPlot.device==".png"){
                png(file, width = input$BWgrpPctPlot.width, height=input$BWgrpPctPlot.height, res=300, units="in")
            }
            else if (input$BWgrpPctPlot.device==".svg"){
                svg(file, width = input$BWgrpPctPlot.width, height=input$BWgrpPctPlot.height)
            } else {
                setEPS()
                postscript(file, width = input$BWgrpPctPlot.width, height=input$BWgrpPctPlot.height)
            }
            print(BWgrpPctPlot())
            dev.off()
        },
        contentType="image/png"
    )
    
    output$PKcurves.dl <- downloadHandler(
        filename=function() {
            paste("PK_Curves_", Sys.Date(), input$PKcurves.device, sep="")
        },
        content=function(file) {
            if(input$PKcurves.device==".png"){
                png(file, width = input$PKcurves.width, height=input$PKcurves.height, res=300, units="in")
            }
            else if (input$PKcurves.device==".svg"){
                svg(file, width = input$PKcurves.width, height=input$PKcurves.height)
            } else {
                setEPS()
                postscript(file, width = input$PKcurves.width, height=input$PKcurves.height)
            }
            print(PKcurves())
            dev.off()
        },
        contentType="image/png"
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
