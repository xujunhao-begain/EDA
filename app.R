# 加载必要的包
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(heatmaply)
library(viridis)

# 模拟数据生成函数
generate_sample_data <- function() {
  set.seed(123)
  
  # 学生基本信息
  n_students <- 500
  student_ids <- paste0("S", 1001:(1000 + n_students))
  
  student_data <- data.frame(
    student_id = student_ids,
    age = sample(6:16, n_students, replace = TRUE, 
                 prob = c(0.05, 0.05, 0.1, 0.1, 0.15, 0.2, 0.15, 0.1, 0.05, 0.03, 0.02)),
    gender = sample(c("男", "女"), n_students, replace = TRUE, prob = c(0.6, 0.4)),
    location = sample(c("北京", "上海", "广州", "深圳", "杭州", "成都", "其他"), 
                      n_students, replace = TRUE, 
                      prob = c(0.25, 0.2, 0.15, 0.15, 0.1, 0.1, 0.05)),
    enrollment_date = sample(seq(as.Date('2022-01-01'), as.Date('2024-12-31'), by="day"), n_students),
    current_course = sample(c("Scratch初级", "Scratch高级", "Python基础", "Python游戏开发", 
                              "Web开发", "机器人编程", "算法入门"), n_students, replace = TRUE)
  )
  
  # 学习行为数据
  learning_data <- data.frame()
  courses <- c("Scratch初级", "Scratch高级", "Python基础", "Python游戏开发", 
               "Web开发", "机器人编程", "算法入门")
  
  for(student in student_ids) {
    n_lessons <- sample(10:50, 1)
    course <- sample(courses, 1)
    
    student_lessons <- data.frame(
      student_id = rep(student, n_lessons),
      course_id = rep(course, n_lessons),
      lesson_date = seq(as.Date('2023-01-01'), by = "week", length.out = n_lessons),
      attendance = sample(c(TRUE, FALSE), n_lessons, replace = TRUE, prob = c(0.85, 0.15)),
      homework_score = round(rnorm(n_lessons, mean = 80, sd = 15)),
      learning_minutes = round(rnorm(n_lessons, mean = 90, sd = 20))
    )
    
    # 确保分数在合理范围内
    student_lessons$homework_score <- pmax(0, pmin(100, student_lessons$homework_score))
    student_lessons$learning_minutes <- pmax(30, student_lessons$learning_minutes)
    
    learning_data <- rbind(learning_data, student_lessons)
  }
  
  # 课程数据
  course_data <- data.frame(
    course_id = courses,
    difficulty = c(1, 2, 2, 3, 3, 2, 4),
    popularity = c(0.9, 0.8, 0.7, 0.6, 0.5, 0.75, 0.4),
    completion_rate = c(0.85, 0.75, 0.7, 0.6, 0.55, 0.8, 0.5),
    monthly_price = c(800, 1000, 1200, 1500, 1800, 1600, 2000)
  )
  
  # 教师数据
  teacher_data <- data.frame(
    teacher_id = paste0("T", 101:110),
    teacher_name = c("张老师", "李老师", "王老师", "赵老师", "刘老师", 
                     "陈老师", "杨老师", "周老师", "吴老师", "黄老师"),
    course_specialty = sample(courses, 10, replace = TRUE),
    student_satisfaction = round(runif(10, 3.5, 5), 1),
    years_experience = sample(1:8, 10, replace = TRUE)
  )
  
  return(list(
    students = student_data,
    learning = learning_data,
    courses = course_data,
    teachers = teacher_data
  ))
}

# 生成数据
app_data <- generate_sample_data()

# UI界面
ui <- dashboardPage(
  skin = "blue",
  
  # 头部
  dashboardHeader(
    title = "少儿编程数据分析平台",
    titleWidth = 300
  ),
  
  # 侧边栏
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("📊 业务概览", tabName = "overview", icon = icon("dashboard")),
      menuItem("👨‍🎓 学生分析", tabName = "students", icon = icon("user-graduate")),
      menuItem("📚 课程分析", tabName = "courses", icon = icon("book")),
      menuItem("👩‍🏫 教师分析", tabName = "teachers", icon = icon("chalkboard-teacher")),
      menuItem("📈 趋势预测", tabName = "forecast", icon = icon("chart-line")),
      
      hr(),
      
      # 筛选器
      dateRangeInput(
        "date_range", 
        "选择时间范围:",
        start = as.Date('2023-01-01'),
        end = as.Date('2024-12-31'),
        language = "zh-CN"
      ),
      
      pickerInput(
        "course_type", 
        "课程类型:",
        choices = unique(app_data$students$current_course),
        selected = unique(app_data$students$current_course),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      
      pickerInput(
        "location_filter", 
        "地区筛选:",
        choices = unique(app_data$students$location),
        selected = unique(app_data$students$location),
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      )
    )
  ),
  
  # 主体
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      
      # 业务概览标签页
      tabItem(
        tabName = "overview",
        fluidRow(
          # KPI指标卡片
          valueBoxOutput("total_students", width = 3),
          valueBoxOutput("active_students", width = 3),
          valueBoxOutput("completion_rate", width = 3),
          valueBoxOutput("monthly_revenue", width = 3)
        ),
        
        fluidRow(
          # 报名趋势
          box(
            title = "学生报名趋势", status = "primary", solidHeader = TRUE,
            width = 8,
            plotlyOutput("enrollment_trend")
          ),
          
          # 学生年龄分布
          box(
            title = "学生年龄分布", status = "primary", solidHeader = TRUE,
            width = 4,
            plotlyOutput("age_distribution")
          )
        ),
        
        fluidRow(
          # 收入构成
          box(
            title = "收入构成分析", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("revenue_breakdown")
          ),
          
          # 地区分布
          box(
            title = "学生地区分布", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("location_distribution")
          )
        )
      ),
      
      # 学生分析标签页
      tabItem(
        tabName = "students",
        fluidRow(
          box(
            title = "学生画像分析", status = "primary", solidHeader = TRUE,
            width = 12,
            plotlyOutput("student_portrait")
          )
        ),
        
        fluidRow(
          box(
            title = "学习行为分析", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("learning_behavior")
          ),
          
          box(
            title = "成绩分布", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("score_distribution")
          )
        ),
        
        fluidRow(
          box(
            title = "学生详细数据", status = "primary", solidHeader = TRUE,
            width = 12,
            DTOutput("student_table")
          )
        )
      ),
      
      # 课程分析标签页
      tabItem(
        tabName = "courses",
        fluidRow(
          box(
            title = "课程受欢迎度", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("course_popularity")
          ),
          
          box(
            title = "课程完成率", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("course_completion")
          )
        ),
        
        fluidRow(
          box(
            title = "课程难度vs完成率", status = "primary", solidHeader = TRUE,
            width = 8,
            plotlyOutput("difficulty_completion")
          ),
          
          box(
            title = "课程筛选器", status = "primary", solidHeader = TRUE,
            width = 4,
            selectInput("course_select", "选择课程:", choices = unique(app_data$students$current_course)),
            sliderInput("difficulty_range", "难度范围:", min = 1, max = 5, value = c(1, 5))
          )
        ),
        
        fluidRow(
          box(
            title = "课程学习进度热力图", status = "primary", solidHeader = TRUE,
            width = 12,
            plotlyOutput("course_heatmap")
          )
        )
      ),
      
      # 教师分析标签页
      tabItem(
        tabName = "teachers",
        fluidRow(
          box(
            title = "教师效能分析", status = "primary", solidHeader = TRUE,
            width = 8,
            plotlyOutput("teacher_performance")
          ),
          
          box(
            title = "教师满意度分布", status = "primary", solidHeader = TRUE,
            width = 4,
            plotlyOutput("satisfaction_distribution")
          )
        ),
        
        fluidRow(
          box(
            title = "教师详细数据", status = "primary", solidHeader = TRUE,
            width = 12,
            DTOutput("teacher_table")
          )
        )
      ),
      
      # 趋势预测标签页
      tabItem(
        tabName = "forecast",
        fluidRow(
          box(
            title = "学生增长预测", status = "primary", solidHeader = TRUE,
            width = 8,
            plotlyOutput("growth_forecast")
          ),
          
          box(
            title = "流失风险预警", status = "primary", solidHeader = TRUE,
            width = 4,
            DTOutput("risk_students")
          )
        ),
        
        fluidRow(
          box(
            title = "课程推荐引擎", status = "primary", solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(6,
                     selectInput("student_select", "选择学生:", choices = NULL)
              ),
              column(6,
                     actionButton("recommend_btn", "生成推荐课程", class = "btn-primary")
              )
            ),
            DTOutput("course_recommendations")
          )
        )
      )
    )
  )
)

# 服务器逻辑
server <- function(input, output, session) {
  
  # 反应式数据过滤
  filtered_data <- reactive({
    # 基于日期和课程类型过滤数据
    students_filtered <- app_data$students %>%
      filter(current_course %in% input$course_type,
             location %in% input$location_filter)
    
    student_ids <- students_filtered$student_id
    
    learning_filtered <- app_data$learning %>%
      filter(student_id %in% student_ids,
             lesson_date >= input$date_range[1],
             lesson_date <= input$date_range[2])
    
    list(
      students = students_filtered,
      learning = learning_filtered,
      courses = app_data$courses,
      teachers = app_data$teachers
    )
  })
  
  # 更新学生选择下拉框
  observe({
    updateSelectInput(session, "student_select", 
                      choices = filtered_data()$students$student_id)
  })
  
  # KPI指标卡片
  output$total_students <- renderValueBox({
    data <- filtered_data()
    n_students <- nrow(data$students)
    
    valueBox(
      value = n_students,
      subtitle = "总学生数量",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$active_students <- renderValueBox({
    data <- filtered_data()
    # 计算最近30天有学习记录的学生
    recent_date <- max(data$learning$lesson_date)
    active_threshold <- recent_date - 30
    
    active_students <- data$learning %>%
      filter(lesson_date >= active_threshold) %>%
      distinct(student_id) %>%
      nrow()
    
    active_rate <- round(active_students / nrow(data$students) * 100, 1)
    
    valueBox(
      value = paste0(active_rate, "%"),
      subtitle = "活跃学生比例",
      icon = icon("user-check"),
      color = "green"
    )
  })
  
  output$completion_rate <- renderValueBox({
    data <- filtered_data()
    # 模拟完成率计算
    avg_completion <- mean(data$courses$completion_rate) * 100
    
    valueBox(
      value = paste0(round(avg_completion, 1), "%"),
      subtitle = "平均课程完成率",
      icon = icon("graduation-cap"),
      color = "yellow"
    )
  })
  
  output$monthly_revenue <- renderValueBox({
    data <- filtered_data()
    # 计算月度收入（基于课程价格和学生数量）
    monthly_rev <- sum(data$courses$monthly_price) * nrow(data$students) / 10
    
    valueBox(
      value = paste0("¥", round(monthly_rev / 1000, 1), "k"),
      subtitle = "估算月度收入",
      icon = icon("yen-sign"),
      color = "red"
    )
  })
  
  # 报名趋势图
  output$enrollment_trend <- renderPlotly({
    data <- filtered_data()
    
    enrollment_trend <- data$students %>%
      mutate(month = floor_date(enrollment_date, "month")) %>%
      count(month) %>%
      complete(month = seq(min(month), max(month), by = "month"), 
               fill = list(n = 0))
    
    p <- ggplot(enrollment_trend, aes(x = month, y = n)) +
      geom_line(color = "#3c8dbc", size = 1) +
      geom_point(color = "#3c8dbc", size = 2) +
      labs(x = "月份", y = "报名人数", title = "") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # 年龄分布图
  output$age_distribution <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data$students, aes(x = age, fill = gender)) +
      geom_histogram(binwidth = 1, alpha = 0.7, position = "dodge") +
      scale_fill_manual(values = c("#3498db", "#e74c3c")) +
      labs(x = "年龄", y = "学生数量", fill = "性别") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 收入构成图
  output$revenue_breakdown <- renderPlotly({
    data <- filtered_data()
    
    revenue_by_course <- data$students %>%
      group_by(current_course) %>%
      summarise(
        student_count = n(),
        estimated_revenue = student_count * mean(data$courses$monthly_price)
      )
    
    p <- plot_ly(revenue_by_course, 
                 labels = ~current_course, 
                 values = ~estimated_revenue,
                 type = 'pie',
                 textinfo = 'label+percent',
                 insidetextorientation = 'radial') %>%
      layout(title = "")
    
    p
  })
  
  # 地区分布图
  output$location_distribution <- renderPlotly({
    data <- filtered_data()
    
    location_count <- data$students %>%
      count(location) %>%
      arrange(desc(n))
    
    p <- ggplot(location_count, aes(x = reorder(location, n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +
      coord_flip() +
      labs(x = "地区", y = "学生数量") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 学生画像分析
  output$student_portrait <- renderPlotly({
    data <- filtered_data()
    
    portrait_data <- data$students %>%
      group_by(age, gender, location) %>%
      summarise(count = n(), .groups = 'drop')
    
    p <- ggplot(portrait_data, aes(x = age, y = location, size = count, color = gender)) +
      geom_point(alpha = 0.7) +
      scale_color_manual(values = c("#3498db", "#e74c3c")) +
      labs(x = "年龄", y = "地区", size = "学生数量", color = "性别") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 学习行为分析
  output$learning_behavior <- renderPlotly({
    data <- filtered_data()
    
    behavior_data <- data$learning %>%
      group_by(course_id) %>%
      summarise(
        avg_attendance = mean(attendance) * 100,
        avg_learning_time = mean(learning_minutes),
        avg_score = mean(homework_score)
      )
    
    p <- ggplot(behavior_data, aes(x = avg_learning_time, y = avg_score, 
                                   size = avg_attendance, color = course_id)) +
      geom_point(alpha = 0.7) +
      labs(x = "平均学习时间(分钟)", y = "平均作业分数", 
           size = "出勤率(%)", color = "课程") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 成绩分布
  output$score_distribution <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data$learning, aes(x = homework_score, fill = course_id)) +
      geom_density(alpha = 0.5) +
      labs(x = "作业分数", y = "密度", fill = "课程") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 学生表格
  output$student_table <- renderDT({
    data <- filtered_data()
    
    summary_data <- data$learning %>%
      group_by(student_id) %>%
      summarise(
        avg_score = round(mean(homework_score), 1),
        attendance_rate = round(mean(attendance) * 100, 1),
        total_lessons = n()
      ) %>%
      left_join(data$students, by = "student_id") %>%
      select(student_id, age, gender, location, current_course, 
             avg_score, attendance_rate, total_lessons)
    
    datatable(
      summary_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'
        )
      ),
      colnames = c('学生ID', '年龄', '性别', '地区', '当前课程', 
                   '平均分数', '出勤率(%)', '总课时')
    )
  })
  
  # 课程受欢迎度
  output$course_popularity <- renderPlotly({
    data <- filtered_data()
    
    popularity_data <- data$students %>%
      count(current_course) %>%
      arrange(desc(n))
    
    p <- ggplot(popularity_data, aes(x = reorder(current_course, n), y = n, fill = n)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +
      coord_flip() +
      labs(x = "课程", y = "学生数量") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 课程完成率
  output$course_completion <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data$courses, aes(x = reorder(course_id, completion_rate), 
                                  y = completion_rate * 100, 
                                  fill = completion_rate)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +
      coord_flip() +
      labs(x = "课程", y = "完成率(%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 课程难度vs完成率
  output$difficulty_completion <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data$courses, aes(x = difficulty, y = completion_rate * 100, 
                                  size = popularity * 100, color = course_id)) +
      geom_point(alpha = 0.7) +
      labs(x = "课程难度", y = "完成率(%)", 
           size = "受欢迎度(%)", color = "课程") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 课程热力图
  output$course_heatmap <- renderPlotly({
    data <- filtered_data()
    
    # 创建学习进度矩阵
    progress_data <- data$learning %>%
      group_by(student_id, course_id) %>%
      summarise(
        progress = min(100, n() * 10),  # 模拟进度计算
        .groups = 'drop'
      ) %>%
      pivot_wider(names_from = course_id, values_from = progress, values_fill = 0)
    
    # 转换为矩阵
    progress_matrix <- as.matrix(progress_data[, -1])
    rownames(progress_matrix) <- progress_data$student_id
    
    # 只显示部分学生以避免性能问题
    if(nrow(progress_matrix) > 50) {
      progress_matrix <- progress_matrix[1:50, ]
    }
    
    heatmaply(
      progress_matrix,
      colors = viridis(n = 256),
      main = "学生学习进度热力图",
      xlab = "课程",
      ylab = "学生",
      showticklabels = c(FALSE, TRUE),
      k_col = 3,
      k_row = 3
    )
  })
  
  # 教师效能分析
  output$teacher_performance <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data$teachers, aes(x = years_experience, y = student_satisfaction, 
                                   size = years_experience, color = course_specialty,
                                   text = teacher_name)) +
      geom_point(alpha = 0.7) +
      labs(x = "教学经验(年)", y = "学生满意度", 
           color = "课程专业", size = "教学经验") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # 教师满意度分布
  output$satisfaction_distribution <- renderPlotly({
    data <- filtered_data()
    
    p <- ggplot(data$teachers, aes(x = student_satisfaction, fill = course_specialty)) +
      geom_histogram(binwidth = 0.2, alpha = 0.7, position = "identity") +
      labs(x = "学生满意度", y = "教师数量", fill = "课程专业") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 教师表格
  output$teacher_table <- renderDT({
    data <- filtered_data()
    
    datatable(
      data$teachers,
      options = list(
        pageLength = 10,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'
        )
      ),
      colnames = c('教师ID', '教师姓名', '专业课程', '学生满意度', '教学经验(年)')
    )
  })
  
  # 增长预测
  output$growth_forecast <- renderPlotly({
    data <- filtered_data()
    
    # 生成预测数据
    months <- 12
    historical <- data$students %>%
      mutate(month = floor_date(enrollment_date, "month")) %>%
      count(month) %>%
      arrange(month)
    
    # 简单线性增长预测
    last_count <- tail(historical$n, 1)
    growth_rate <- 0.08  # 月增长率8%
    
    forecast_data <- data.frame(
      month = seq(max(historical$month) + months(1), by = "month", length.out = months),
      n = last_count * (1 + growth_rate)^(1:months)
    )
    
    p <- ggplot() +
      geom_line(data = historical, aes(x = month, y = n), color = "#3498db", size = 1) +
      geom_line(data = forecast_data, aes(x = month, y = n), color = "#e74c3c", size = 1, linetype = "dashed") +
      geom_point(data = forecast_data, aes(x = month, y = n), color = "#e74c3c") +
      labs(x = "月份", y = "学生数量") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # 流失风险学生
  output$risk_students <- renderDT({
    data <- filtered_data()
    
    risk_data <- data$learning %>%
      group_by(student_id) %>%
      summarise(
        recent_absence = mean(tail(attendance, 5) == FALSE),  # 最近5次课缺勤率
        score_decline = ifelse(n() > 5, 
                               cor(seq_len(n()), homework_score) < -0.3, 
                               FALSE),
        total_lessons = n()
      ) %>%
      filter(recent_absence > 0.3 | score_decline == TRUE) %>%
      left_join(data$students, by = "student_id") %>%
      select(student_id, age, current_course, recent_absence, score_decline) %>%
      mutate(
        risk_level = ifelse(recent_absence > 0.5, "高风险", "中风险")
      )
    
    datatable(
      risk_data,
      options = list(
        pageLength = 5,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'
        )
      ),
      colnames = c('学生ID', '年龄', '课程', '近期缺勤率', '成绩下降', '风险等级')
    )
  })
  
  # 课程推荐
  observeEvent(input$recommend_btn, {
    output$course_recommendations <- renderDT({
      student_id <- input$student_select
      
      if(is.null(student_id) || student_id == "") {
        return(NULL)
      }
      
      # 简单的推荐逻辑 - 基于学生当前课程和课程难度
      student_data <- filtered_data()$students %>%
        filter(student_id == input$student_select)
      
      current_course <- student_data$current_course
      student_age <- student_data$age
      
      # 推荐逻辑
      recommendations <- filtered_data()$courses %>%
        mutate(
          suitability = case_when(
            student_age < 10 & difficulty <= 2 ~ 0.9,
            student_age < 10 & difficulty > 2 ~ 0.3,
            student_age >= 10 & difficulty <= 3 ~ 0.8,
            student_age >= 10 & difficulty > 3 ~ 0.6,
            TRUE ~ 0.5
          ),
          recommendation_score = suitability * popularity
        ) %>%
        arrange(desc(recommendation_score)) %>%
        select(course_id, difficulty, popularity, completion_rate, recommendation_score)
      
      datatable(
        recommendations,
        options = list(
          pageLength = 5,
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Chinese.json'
          )
        ),
        colnames = c('推荐课程', '难度', '受欢迎度', '完成率', '推荐分数')
      )
    })
  })
}

# 运行应用
shinyApp(ui, server)