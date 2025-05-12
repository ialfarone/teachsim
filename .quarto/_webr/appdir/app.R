library(shiny)
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

ui <- page_sidebar(
  title = "Cox Regression Simulator",
  sidebar = sidebar(
    sliderInput("beta_therapy", "Effect of Therapy (β):", min = -1.5, max = 1.5, value = -0.6, step = 0.1),
    sliderInput("beta_gender",  "Effect of Gender (β):",  min = -1.5, max = 1.5, value =  0.4, step = 0.1),
    sliderInput("baseline",     "Baseline Hazard:",       min =  0.01, max = 0.1, value = 0.03, step = 0.005),
    checkboxInput("adj_gender", "Adjust for Gender", value = TRUE)
  ),
  mainPanel(
    h4("Simulated Dataset (first six rows)"),
    verbatimTextOutput("simData"),
    h4("Cox‑Model Summary"),
    verbatimTextOutput("coxSummary"),
    h4("Kaplan–Meier Survival Curves"),
    plotOutput("kmCurve")
  )
)

server <- function(input, output, session) {

  # Reactive data-generating mechanism
  psych_data <- reactive({
    n <- 300
    gender  <- factor(sample(c("Male", "Female"), n, replace = TRUE))
    therapy <- factor(sample(c("Short", "Long"),  n, replace = TRUE))

    linpred <- input$beta_therapy * (therapy == "Long") +
               input$beta_gender  * (gender  == "Male")

    time            <- rexp(n, rate = input$baseline * exp(linpred))
    censoring_time  <- runif(n, 5, 40)
    status          <- ifelse(time <= censoring_time, 1, 0)
    time            <- pmin(time, censoring_time)
    data.frame(time, status, therapy, gender)
  })

  # Display first six rows of data
  output$simData <- renderPrint({
    head(psych_data())
  })

  # Display Kaplan–Meier curve
  output$kmCurve <- renderPlot({
    data <- psych_data()
    fit_km <- survfit(Surv(time, status) ~ therapy, data = data)
    ggsurvplot(fit_km, data = data, pval = TRUE, conf.int = TRUE,
               legend.title = "Therapy Duration",
               xlab = "Time to relapse (weeks)",
               ylab = "Survival probability",
               ggtheme = theme_minimal())
  })

  # Display Cox model summary
  output$coxSummary <- renderPrint({
    formula <- if (input$adj_gender) {
      Surv(time, status) ~ therapy + gender
    } else {
      Surv(time, status) ~ therapy
    }
    fit_cox <- coxph(formula, data = psych_data())
    summary(fit_cox)
  })
}

shinyApp(ui, server)
