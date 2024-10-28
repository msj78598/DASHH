library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(openxlsx)
library(googlesheets4)  # مكتبة الوصول إلى Google Sheets

# تحديد رابط Google Sheets أو معرف الجدول
sheet_url <- "https://docs.google.com/spreadsheets/d/178RNAY73M0uBhFvjQCDAlPUNmhp68rzZ/edit?usp=drive_link"

# قراءة البيانات من Google Sheets
main_data <- read_sheet(sheet_url)

# استخراج الإحداثيات من عمود 'الموقع'
if ("الموقع" %in% colnames(main_data)) {
  main_data <- main_data %>%
    separate(الموقع, into = c("Latitude", "Longitude"), sep = ",", convert = TRUE)
  print("تم استخراج الإحداثيات من عمود 'الموقع'.")
} else {
  stop("عمود 'الموقع' غير موجود في البيانات.")
}

# باقي الكود لتشغيل التطبيق في Shiny
ui <- fluidPage(
  titlePanel("داشبورد تحليل عدادات الإدارة"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_disconnected", "رفع ملف العدادات غير المتصلة:", accept = c(".xlsx")),
      selectInput("status_filter", "اختر حالة الاتصال:", choices = c("كل الحالات", "متصل", "غير متصل"), selected = "كل الحالات"),
      selectInput("category", "اختر الفئة:", choices = c("كل الفئات", unique(main_data$الفئة)), selected = "كل الفئات"),
      selectInput("office", "اختر المكتب:", choices = c("كل المكاتب", unique(main_data$المكتب)), selected = "كل المكاتب"),
      selectInput("cycle", "اختر الدورة:", choices = c("كل الدورات", unique(main_data$الدورة)), selected = "كل الدورات"),
      selectInput("breaker_size", "اختر سعة القاطع:", choices = c("كل السعات", unique(main_data$`سعة القاطع`)), selected = "كل السعات"),
      downloadButton("download_data", "تصدير البيانات")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("إجمالي العدادات والنسبة", plotOutput("total_bar_plot")),
        tabPanel("العدادات والنسبة حسب المكتب", plotOutput("office_bar_plot")),
        tabPanel("العدادات والنسبة حسب الدورة", plotOutput("cycle_bar_plot")),
        tabPanel("البيانات", dataTableOutput("table"), textOutput("data_status"))
      )
    )
  )
)

server <- function(input, output, session) {
  # معالجة ملف العدادات غير المتصلة
  disconnected_meters <- reactive({
    req(input$file_disconnected)
    disconnect_file <- read_excel(input$file_disconnected$datapath)
    disconnect_file$`HES Device Id` <- as.character(disconnect_file$`HES Device Id`)
    return(disconnect_file$`HES Device Id`)
  })
  
  # تحديد حالة الاتصال للعدادات بناءً على الملف المرفوع
  reactive_data <- reactive({
    data_filtered <- main_data
    disconnect_ids <- disconnected_meters()
    
    # تحديد حالة الاتصال: المتصل والغير متصل
    data_filtered$Status <- ifelse(data_filtered$`HES Device Id` %in% disconnect_ids, "غير متصل", "متصل")
    
    # تصفية حسب الفلاتر
    if (input$status_filter != "كل الحالات") {
      data_filtered <- data_filtered %>% filter(Status == input$status_filter)
    }
    if (input$category != "كل الفئات") {
      data_filtered <- data_filtered %>% filter(الفئة == input$category)
    }
    if (input$office != "كل المكاتب") {
      data_filtered <- data_filtered %>% filter(المكتب == input$office)
    }
    if (input$cycle != "كل الدورات") {
      data_filtered <- data_filtered %>% filter(الدورة == input$cycle)
    }
    if (input$breaker_size != "كل السعات") {
      data_filtered <- data_filtered %>% filter(`سعة القاطع` == input$breaker_size)
    }
    
    return(data_filtered)
  })
  
  # عرض البيانات في الجدول مع الفلاتر
  output$table <- renderDataTable({
    data_filtered <- reactive_data()
    datatable(data_filtered, extensions = 'Buttons', options = list(
      pageLength = 10,
      dom = 'Bfrtip',
      buttons = list(
        'copy', 'csv', 'excel', 'pdf', 'print'
      )
    ))
  })
  
  # تصدير البيانات إلى ملف Excel
  output$download_data <- downloadHandler(
    filename = function() {
      paste("تصدير_البيانات_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data_filtered <- reactive_data()
      write.xlsx(data_filtered, file)
    }
  )
}

# تشغيل التطبيق
shinyApp(ui = ui, server = server)
