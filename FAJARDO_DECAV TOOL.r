#Install packages
install.packages(“shiny”)
install.packages(“readxl”)
install.packages(“dplyr”)
install.packages(“lubridate”)
install.packages(“ggplot2”)
install.packages(“shinythemes”)
library(shiny)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shinythemes)
# UI
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Dengue Case Analysis and Visualization Tool"),
                sidebarLayout(
                  sidebarPanel(
                    HTML('<a href="https://tinyurl.com/DeCAVTemp" target="_blank">Download template here</a><br><br>'),
                    fileInput("file", "Upload Excel File"),
                    radioButtons("analysis_type", "Select Analysis Type:",
                                 choices = list("Compute Threshold" = "threshold", "Case Analysis" = "case_analysis")),
                    uiOutput("additional_inputs"),actionButton("download_image", "Download Panel as Image")),
                  mainPanel(
                    id = "main_panel",
                    style = "background-color: white;", 
                    uiOutput("main_output"),
                    uiOutput("show_graphs_button"))),
                tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.4.1/html2canvas.min.js"),
                          tags$script(HTML("
      Shiny.addCustomMessageHandler('captureContent', function(message) {
        var element = document.getElementById('main_panel');
        html2canvas(element, {
          backgroundColor: 'white',
          onrendered: function(canvas) {
            var img = canvas.toDataURL('image/png');
            var link = document.createElement('a');
            link.href = img;
            link.download = 'dengue_analysis.png';
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);} });}); "))))

# Server
server <- function(input, output, session) {
  observeEvent(input$analysis_type, {
    output$additional_inputs <- renderUI({
      if (input$analysis_type == "case_analysis") {
        tagList(
          numericInput("population", "Population Size:", value = 400000, min = 1),
          selectInput("plot_type", "Select Plot Type:",
                      choices = c("Incidence Rate", "Age Group Bar Graph", "Gender Pie Chart", "Age Group by Gender Stack Bar Graph", "Total Cases per Barangay")),
          downloadButton("downloadPlot", "Download Plot as Image"))}})})
  data <- reactive({
    req(input$file)
    if (input$analysis_type == "case_analysis") {
      read_excel(input$file$datapath, sheet = "data")
    } else {
      read_excel(input$file$datapath, sheet = "MW")}})
  #Formula for threshold analysis 
  threshold_analysis <- reactive({
    req(input$analysis_type == "threshold", data())
    mw_data <- data()
    mw_data <- mw_data %>%
      mutate(
        Mean = rowMeans(select(mw_data, `2017`, `2018`, `2020`, `2021`, `2022`), na.rm = TRUE),
        SD = apply(select(mw_data, `2017`, `2018`, `2020`, `2021`, `2022`), 1, sd, na.rm = TRUE),
        `2SD` = SD * 1.64,
        Alert = SD + `2SD`,
        Epidemic = Alert + Mean,
        Status = ifelse(`2023` > Epidemic, "Above Threshold", "Below Threshold")
      )
    mw_data
  })
  output$main_output <- renderUI({
    if (input$analysis_type == "threshold") {
      tagList(
        tableOutput("threshold_table"),
        plotOutput("threshold_plot")
      )
    } else {
      tagList(
        plotOutput("plot"),
        fluidRow(
          column(12, align = "center",
                 textOutput("age_statistics"),
                 textOutput("gender_statistics"),
                 textOutput("incidence_statistics"),
                 textOutput("brgy_statistics")
          )
        ),
        plotOutput("brgy_bargraph"),
        tableOutput("brgy_cases")
      )
    }
  })
  output$threshold_table <- renderTable({
    req(threshold_analysis())
    threshold_analysis()
  })
  output$threshold_plot <- renderPlot({
    req(threshold_analysis())
    mw_data <- threshold_analysis()
    ggplot(mw_data, aes(x = Morbidity_Week)) +
      geom_col(aes(y = `2023`), fill = ifelse(mw_data$Status == "Above Threshold", "red", "green")) +
      geom_line(aes(y = Epidemic), color = "blue", size = 1) +
      labs(x = "Morbidity Week", y = "Cases", title = "Threshold Analysis with Epidemic Line") +
      theme_minimal() +
      scale_y_continuous(sec.axis = sec_axis(~ ., name = "Epidemic Threshold"))
  })
  observeEvent(input$show_graphs, {
    showModal(modalDialog(
      title = "Threshold Analysis",
      plotOutput("threshold_plot"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  MIR <- reactive({
    req(data())
    data.frame <- data()
    data.frame <- data.frame %>%
      mutate(Case_Date = dmy(Case_Date)) %>%
      filter(!is.na(Case_Date))
    incidence_df <- data.frame %>%
      group_by(month = floor_date(Case_Date, "month")) %>%
      summarise(Incidence_Rate = n() / input$population * 1000)
    return(incidence_df)
  })
  brgy_cases <- reactive({
    req(data())
    data.frame <- data()
    brgy_case_summary <- data.frame %>%
      group_by(Barangay) %>%
      summarise(Total_Cases = n()) %>%
      arrange(desc(Total_Cases))
    return(brgy_case_summary)
  })
  output$brgy_cases <- renderTable({
    req(input$plot_type == "Total Cases per Barangay")
    brgy_cases()
  })
  output$brgy_bargraph <- renderPlot({
    req(input$plot_type == "Total Cases per Barangay")
    brgy_bargraph(brgy_cases())
  })
  output$age_statistics <- renderText({
    req(input$plot_type)
    if (input$plot_type == "Age Group Bar Graph") {
      age_stats <- table(cut(data()$Age, breaks = c(0, 5, 18, 30, 50, Inf), labels = c("Babies", "Children", "Young Adults", "Middle Age Adults", "Seniors")))
      max_age_group <- names(sort(age_stats, decreasing = TRUE)[1])
      second_max_age_group <- names(sort(age_stats, decreasing = TRUE)[2])
      third_max_age_group <- names(sort(age_stats, decreasing = TRUE)[3])
      min_age_group <- names(sort(age_stats)[1])
      paste("In age group:",
            "Individuals aged", max_age_group, "were mostly affected, followed by",
            second_max_age_group, "next were", third_max_age_group,
            "and", min_age_group, "were the rarely affected by dengue.")
    } else {""}
  })
  output$gender_statistics <- renderText({
    req(input$plot_type)
    if (input$plot_type == "Gender Pie Chart") {
      gender_stats <- table(data()$Gender)
      total_cases <- nrow(data())
      female_percentage <- round((gender_stats["female"] / total_cases) * 100, 2)
      male_percentage <- round((gender_stats["male"] / total_cases) * 100, 2)
      paste("In Gender:", 
            "A total of", total_cases, "cases,",
            female_percentage, "% of them are Female and",
            male_percentage, "% are Male.")
    } else {""}
  })
  output$incidence_statistics <- renderText({
    req(input$plot_type)
    if (input$plot_type == "Incidence Rate") {
      num_cases <- nrow(data())
      paste("There were", num_cases, "cases of dengue cases")
    } else {""}
  })
  output$brgy_statistics <- renderText({
    req(input$plot_type)
    if (input$plot_type == "Total Cases per Barangay") {
      brgy_data <- brgy_cases()
      if (nrow(brgy_data) >= 5) {
        highest_cases_brgy <- brgy_data$Barangay[1]
        highest_cases <- brgy_data$Total_Cases[1]
        highest_percentage <- round((highest_cases / sum(brgy_data$Total_Cases)) * 100, 2)
        second_highest_cases_brgy <- brgy_data$Barangay[2]
        second_highest_cases <- brgy_data$Total_Cases[2]
        second_highest_percentage <- round((second_highest_cases / sum(brgy_data$Total_Cases)) * 100, 2)
        third_highest_cases_brgy <- brgy_data$Barangay[3]
        third_highest_cases <- brgy_data$Total_Cases[3]
        fourth_highest_cases_brgy <- brgy_data$Barangay[4]
        fourth_highest_cases <- brgy_data$Total_Cases[4]
        fifth_highest_cases_brgy <- brgy_data$Barangay[5]
        least_cases_brgy <- brgy_data$Barangay[nrow(brgy_data)]
        least_cases <- brgy_data$Total_Cases[nrow(brgy_data)]
        least_percentage <- round((least_cases / sum(brgy_data$Total_Cases)) * 100, 2)
        paste("The barangay with the highest number of cases is", highest_cases_brgy, "with", highest_cases,
              "cases, accounting for", highest_percentage, "% of the total cases. The second is", second_highest_cases_brgy,
              "with", second_highest_cases, "cases, accounting for", second_highest_percentage, "% of the total cases. The third, fourth, and fifth barangays are",
              third_highest_cases_brgy, ",", fourth_highest_cases_brgy, ", and", fifth_highest_cases_brgy, "respectively. Finally,", least_cases_brgy,
              "has the fewest cases, with", least_cases, "cases, accounting for", least_percentage, "% of the total cases.")
      } else {
        "Not enough data to display barangay statistics."
      }
    } else {""}
  })
  output$plot <- renderPlot({
    req(input$plot_type)
    if (input$plot_type == "Incidence Rate") {
      incidence_rate_plot(MIR())
    } else if (input$plot_type == "Age Group Bar Graph") {
      abargraph(data())
    } else if (input$plot_type == "Gender Pie Chart") {
      gpiechart(data())
    } else if (input$plot_type == "Age Group by Gender Stack Bar Graph") {
      aggraph(data())
    }
  })
  observeEvent(input$download_image, {
    session$sendCustomMessage("captureContent", list())
  })
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot", input$plot_type, ".png", sep = "")
    },
    content = function(file) {
      plot <- switch(input$plot_type,
                     "Incidence Rate" = incidence_rate_plot(MIR()),
                     "Age Group Bar Graph" = abargraph(data()),
                     "Gender Pie Chart" = gpiechart(data()),
                     "Age Group by Gender Stack Bar Graph" = aggraph(data()),
                     "Total Cases per Barangay" = brgy_bargraph(brgy_cases()))
      ggsave(file, plot = plot, bg = "white")
    })}
# Incidence Rate Plot
incidence_rate_plot <- function(MIR) {
  ggplot(MIR, aes(x = month, y = Incidence_Rate)) +
    geom_line(color = "blue") +
    geom_point(color = "red") +
    labs(x = "Month", y = "Incidence Rate per 1000 People", title = "Monthly Incidence Rate of Dengue Cases") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "white"))}
# Bar Graph
abargraph <- function(data) {
  data <- data %>%
    mutate(Age_Group = cut(Age, breaks = c(0, 5, 18, 30, 50, Inf), labels = c("Babies", "Children", "Young Adults", "Middle Age Adults", "Old Adults")))
  ggplot(data, aes(x = Age_Group, fill = Age_Group)) +
    geom_bar() +
    labs(x = "Age Group", y = "Count", title = "Age Group Bar Graph") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "white")) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)}
# Pie Chart gender 
gpiechart <- function(data) {
  gender_counts <- table(data$Gender)
  gender_counts_df <- data.frame(Gender = names(gender_counts), Count = as.numeric(gender_counts))
  gender_counts_df <- gender_counts_df[order(gender_counts_df$Count, decreasing = TRUE), ]  
  pie_chart <- ggplot(gender_counts_df, aes(x = "", y = Count, fill = Gender)) +
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0) +
    labs(title = "Gender Pie Chart") +
    theme_void() +
    geom_text(aes(label = paste0(Gender, ": ", round(Count/sum(Count) * 100, 1), "%")), position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c("skyblue", "pink")) +  
    theme(plot.background = element_rect(fill = "white")) 
  return(pie_chart)}
# Stack Bar Graph age at gender
aggraph <- function(data) {
  data <- data %>%
    mutate(Age_Group = cut(Age, breaks = c(0, 5, 18, 30, 50, Inf), labels = c("Babies", "Children", "Young Adults", "Middle Age Adults", "Old Adults")))
  stacked_bar <- ggplot(data, aes(x = Age_Group, fill = Gender)) +
    geom_bar(position = "stack") +
    labs(x = "Age Group", y = "Count", title = "Age Group by Gender Stack Bar Graph") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), plot.background = element_rect(fill = "white")) +
    geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5))
  return(stacked_bar)}
# Barangay Bar Graph
brgy_bargraph <- function(brgy_cases) {
  ggplot(brgy_cases, aes(x = reorder(Barangay, -Total_Cases), y = Total_Cases, fill = Barangay)) +
    geom_bar(stat = "identity") +
    labs(x = "Barangay", y = "Total Cases", title = "Total Cases per Barangay") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "white")) +
    geom_text(aes(label = Total_Cases), vjust = -0.5)}
shinyApp(ui = ui, server = server)
