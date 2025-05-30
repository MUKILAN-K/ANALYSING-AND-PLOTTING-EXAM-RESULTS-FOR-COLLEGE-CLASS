library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(shinyjs)
library(bslib)

custom_theme <- bs_theme(
  bootswatch = "minty",
  base_font = font_google("Poppins"),
  version = 5,
  bg = "#f8f9fa",
  fg = "#343a40"
)

ui <- dashboardPage(
  title = "LAN - Student Performance Portal",
  header = dashboardHeader(
    title = span("Student Performance Portal", style = "font-family: 'Poppins', sans-serif; font-weight: bold;"),
    tags$li(class = "dropdown",
            actionButton("logout_btn", "Logout", class = "btn btn-danger", style = "margin-top: 10px;")
    )
  ),
  sidebar = dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
      menuItem("Upload Results", tabName = "upload", icon = icon("upload")),
      menuItem("Prediction", tabName = "prediction", icon = icon("magic")),
      menuItem("Logout", tabName = "logout", icon = icon("sign-out-alt"))
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .main-header { background-color: #28a745 !important; }
        .sidebar-menu > li > a { font-size: 14px; font-family: 'Poppins', sans-serif; }
        .content-wrapper { background-color: #f8f9fa; padding: 20px; }
        .well { background-color: #ffffff; border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
        .btn-primary { background-color: #28a745; border-color: #28a745; }
        .btn-primary:hover { background-color: #218838; border-color: #1e7e34; }
        .btn-danger { background-color: #dc3545; border-color: #dc3545; }
        .btn-danger:hover { background-color: #c82333; border-color: #bd2130; }
        h3 { font-family: 'Poppins', sans-serif; color: #343a40; }
        p { font-family: 'Poppins', sans-serif; color: #6c757d; }
        .shiny-plotly { border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
      "))
    ),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                column(12,
                       br(),
                       h2("Welcome to the Student Exam Dashboard", align = "center"),
                       p("Analyze, visualize, and get recommendations on student performance in just a few clicks!",
                         align = "center", style = "font-size: 16px;"),
                       br(),
                       img(
                         src = "https://res.cloudinary.com/dpdz5uwvf/image/upload/v1745511056/ac2cb122-7c0a-49f7-8465-fccb71377fc0_wsbcxb.png",
                         height = "300px",
                         style = "display:block; margin:auto;"
                       )
                       
                       
                       
                )
              )
      ),
      tabItem(tabName = "login",
              fluidRow(
                column(4, offset = 4,
                       br(),
                       wellPanel(
                         h3("🔐 Login"),
                         textInput("username", "Username"),
                         passwordInput("password", "Password"),
                         actionButton("login_btn", "Login", class = "btn btn-primary w-100"),
                         div(style = "color:red;", textOutput("login_status"))
                       )
                )
              )
      ),
      tabItem(tabName = "dashboard",
              conditionalPanel(
                condition = "output.loggedIn == true",
                fluidRow(column(12, h3("📊 Dashboard Overview", align = "center"))),
                fluidRow(
                  column(6, plotlyOutput("pass_plot")),
                  column(6, plotlyOutput("fail_plot"))
                ),
                fluidRow(
                  column(6, plotlyOutput("grade_plot")),
                  column(6, plotlyOutput("average_plot"))
                )
              )
      ),
      tabItem(tabName = "upload",
              conditionalPanel(
                condition = "output.loggedIn == true",
                fluidRow(
                  column(6, fileInput("file", "Upload CSV File", accept = ".csv"))
                )
              )
      ),
      tabItem(tabName = "prediction",
              conditionalPanel(
                condition = "output.loggedIn == true",
                fluidRow(
                  column(12,
                         h4("🔮 Subject Recommendations"),
                         verbatimTextOutput("prediction"),
                         plotlyOutput("focus_plot")
                  )
                )
              )
      ),
      tabItem(tabName = "logout",
              fluidRow(
                column(12,
                       h4("You have been logged out."),
                       actionLink("go_home", "Return to Home", class = "btn btn-secondary")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  credentials <- list(username = "student", password = "123")
  user_logged_in <- reactiveVal(FALSE)
  
  output$loggedIn <- reactive({ user_logged_in() })
  outputOptions(output, "loggedIn", suspendWhenHidden = FALSE)
  
  observeEvent(input$login_btn, {
    if (input$username == credentials$username && input$password == credentials$password) {
      user_logged_in(TRUE)
      updateTabItems(session, "tabs", selected = "dashboard")
    } else {
      output$login_status <- renderText("❌ Invalid username or password.")
    }
  })
  
  observeEvent(input$logout_btn, {
    user_logged_in(FALSE)
    updateTabItems(session, "tabs", selected = "home")
  })
  
  dataset <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df <- df %>% select(-c(Student_ID, Total))
    return(df)
  })
  
  output$pass_plot <- renderPlotly({
    df <- dataset()
    pass_rate <- colMeans(df >= 40, na.rm = TRUE) * 100
    plot_ly(x = names(pass_rate), y = pass_rate, type = 'bar', name = 'Pass %') %>%
      layout(title = "Pass Percentage per Subject")
  })
  
  output$fail_plot <- renderPlotly({
    df <- dataset()
    fail_rate <- colMeans(df < 40, na.rm = TRUE) * 100
    plot_ly(x = names(fail_rate), y = fail_rate, type = 'bar', name = 'Fail %') %>%
      layout(title = "Failure Rate per Subject")
  })
  
  output$grade_plot <- renderPlotly({
    df <- dataset()
    grades <- cut(rowMeans(df), breaks = c(0, 40, 50, 60, 70, 80, 100), labels = c("F", "D", "C", "B", "A", "A+"))
    grade_data <- table(grades)
    plot_ly(labels = names(grade_data), values = as.numeric(grade_data), type = 'pie') %>%
      layout(title = "Grade Distribution")
  })
  
  output$average_plot <- renderPlotly({
    df <- dataset()
    avg_scores <- colMeans(df, na.rm = TRUE)
    plot_ly(x = names(avg_scores), y = avg_scores, type = 'bar') %>%
      layout(title = "Average Score per Subject")
  })
  
  output$prediction <- renderPrint({
    df <- dataset()
    avg <- colMeans(df, na.rm = TRUE)
    cat("📌 Subjects that need focus (avg < 40):\n")
    print(names(avg[avg < 40]))
    cat("\n🏆 Strong Subjects (avg > 70):\n")
    print(names(avg[avg > 70]))
  })
  
  output$focus_plot <- renderPlotly({
    df <- dataset()
    avg_scores <- colMeans(df, na.rm = TRUE)
    plot_ly(x = names(avg_scores), y = avg_scores, type = 'bar', marker = list(color = avg_scores)) %>%
      layout(title = "📊 Subject Performance Overview")
  })
}

shinyApp(ui = ui, server = server)