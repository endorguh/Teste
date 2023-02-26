library(shiny)
library(dplyr)

df <- read.csv2("nome_do_arquivo.csv")

# Create df2 for tabPanel "Project"
df2 <- df %>%
  group_by(Landuse, `FARM NAME`, Planting.year = year(Est_Date)) %>%
  summarise(Area_Ha = sum(Area_Ha, na.rm = T),
            Current.plots = sum(Plots.Current, na.rm = T),
            Previous.plots = sum(Plots.Previous, na.rm = T),
            Volume.6years.current = sum(Volume.6years.current.Area, na.rm = T) / sum(Area_Ha),
            Volume.7years.current = sum(Volume.7years.current.Area, na.rm = T) / sum(Area_Ha),
            Volume.6years.previous = sum(Volume.6years.previous.Area, na.rm = T) / sum(Area_Ha),
            Volume.7years.previous = sum(Volume.7years.previous.Area, na.rm = T) / sum(Area_Ha),
            IMA6.current = sum(IMA6.current.Area, na.rm = T) / sum(Area_Ha),
            IMA7.current = sum(IMA7.current.Area, na.rm = T) / sum(Area_Ha),
            IMA6.previous = sum(IMA6.previous.Area, na.rm = T) / sum(Area_Ha),
            IMA7.previous = sum(IMA7.previous.Area, na.rm = T) / sum(Area_Ha))

# Create df3 for tabPanel "Climatic"
df3 <- df %>%
  group_by(Landuse, Climatic = Climática) %>%
  summarise(Area_Ha = sum(Area_Ha, na.rm = T),
            Current.plots = sum(Plots.Current, na.rm = T),
            Previous.plots = sum(Plots.Previous, na.rm = T),
            Volume.6years.current = sum(Volume.6years.current.Area, na.rm = T) / sum(Area_Ha),
            Volume.7years.current = sum(Volume.7years.current.Area, na.rm = T) / sum(Area_Ha),
            Volume.6years.previous = sum(Volume.6years.previous.Area, na.rm = T) / sum(Area_Ha),
            Volume.7years.previous = sum(Volume.7years.previous.Area, na.rm = T) / sum(Area_Ha),
            IMA6.current = sum(IMA6.current.Area, na.rm = T) / sum(Area_Ha),
            IMA7.current = sum(IMA7.current.Area, na.rm = T) / sum(Area_Ha),
            IMA6.previous = sum(IMA6.previous.Area, na.rm = T) / sum(Area_Ha),
            IMA7.previous = sum(IMA7.previous.Area, na.rm = T) / sum(Area_Ha))

# Create df4 for tabPanel "Planting Year"
df4 <- df %>%
  group_by(Landuse, Planting.year = year(Est_Date)) %>%
  summarise(Area_Ha = sum(Area_Ha, na.rm = T),
            Current.plots = sum(Plots.Current, na.rm = T),
            Previous.plots = sum(Plots.Previous, na.rm = T),
            Volume.6years.current = sum(Volume.6years.current.Area, na.rm = T) / sum(Area_Ha),
            Volume.7years.current = sum(Volume.7years.current.Area, na.rm = T) / sum(Area_Ha),
            Volume.6years.previous = sum(Volume.6years.previous.Area, na.rm = T) / sum(Area_Ha),
            Volume.7years.previous = sum(Volume.7years.previous.Area, na.rm = T) / sum(Area_Ha),
            IMA6.current = sum(IMA6.current.Area, na.rm = T) / sum(Area_Ha),
            IMA7.current = sum(IMA7.current.Area, na.rm = T) / sum(Area_Ha),
            IMA6.previous = sum(IMA6.previous.Area, na.rm = T) / sum(Area_Ha),
            IMA7.previous = sum(IMA7.previous.Area, na.rm = T) / sum(Area_Ha))

# Create df5 for tabPanel "Genetic"
df5 <- df %>%
  group_by(Landuse, Genetic) %>%
  summarise(Area_Ha = sum(Area_Ha, na.rm = T),
            Current.plots = sum(Plots.Current, na.rm = T),
            Previous.plots = sum(Plots.Previous, na.rm = T),
            Volume.6years.current = sum(Volume.6years.current.Area, na.rm = T) / sum(Area_Ha),
            Volume.7years.current = sum(Volume.7years.current.Area, na.rm = T) / sum(Area_Ha),
            Volume.6years.previous = sum(Volume.6years.previous.Area, na.rm = T) / sum(Area_Ha),
            Volume.7years.previous = sum(Volume.7years.previous.Area, na.rm = T) / sum(Area_Ha),
            IMA6.current = sum(IMA6.current.Area, na.rm = T) / sum(Area_Ha),
            IMA7.current = sum(IMA7.current.Area, na.rm = T) / sum(Area_Ha),
            IMA6.previous = sum(IMA6.previous.Area, na.rm = T) / sum(Area_Ha),
            IMA7.previous = sum(IMA7.previous.Area, na.rm = T) / sum(Area_Ha))

# Define UI for shinyapp
ui <- fluidPage(
  navbarPage(
    "ORM - Forestry Operation Review Meeting",
    tabPanel("Project",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "landuse", label = "Landuse", choices = unique(df2$Landuse)),
                 selectInput(inputId = "farmname", label = "FARM NAME", choices = unique(df2$`FARM NAME`)),
                 selectInput(inputId = "plantingyear", label = "Planting Year", choices = unique(df2$Planting.year))
               ),
               mainPanel(
                 plotOutput(outputId = "project_plot"),
                 dataTableOutput(outputId = "project_table")
               )
             )
    ),
    tabPanel("Climatic",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "landuse2", label = "Landuse", choices = unique(df3$Landuse)),
                 selectInput(inputId = "climatic", label = "Climatic", choices = unique(df3$Climatic))
               ),
               mainPanel(
                 plotOutput(outputId = "climatic_plot"),
                 dataTableOutput(outputId = "climatic_table")
               )
             )
    ),
    tabPanel("Planting Year",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "landuse3", label = "Landuse", choices = unique(df4$Landuse)),
                 selectInput(inputId = "plantingyear2", label = "Planting Year", choices = unique(df4$Planting.year))
               ),
               mainPanel(
                 plotOutput(outputId = "plantingyear_plot"),
                 dataTableOutput(outputId = "plantingyear_table")
               )
             )
    ),
    tabPanel("Genetic",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "landuse4", label = "Landuse", choices = unique(df5$Landuse)),
                 selectInput(inputId = "genetic", label = "Genetic", choices = unique(df5$Genetic))
               ),
               mainPanel(
                 plotOutput(outputId = "genetic_plot"),
                 dataTableOutput(outputId = "genetic_table")
               )
             )
    )
  ),
  theme = shinytheme("united"),
  tags$head(tags$style(HTML(".navbar{background-color: #034159;}
                             .navbar-default .navbar-nav > li > a:hover, .navbar-default .navbar-nav > li > a:focus {
                             background-color: #0CF25D;
                             color: #034159;}
                             .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover, .navbar-default .navbar-nav > .active > a:focus {
                             background-color: #0CF25D;
                             color: #034159;}
                             .navbar-default .navbar-brand:hover, .navbar-default .navbar-brand:focus {
                             color: #0CF25D;
                             background-color: #034159;}
                             .navbar-default {
                             border-color: #02735E;}
                             ")),
            tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico"))
  ))
  
  # Define server for shinyapp
  server <- function(input, output) {
    
    # Create reactive subset of df2 based on user inputs
    df2_sub <- reactive({
      df2 %>%
        filter(Landuse == input$landuse,
               `FARM NAME` == input$farmname,
               Planting.year == input$plantingyear)
    })
    
    # Render project plot
    output$project_plot <- renderPlot({
      ggplot(df2_sub(), aes(x = factor(1), y = c(IMA6.current, IMA7.current, IMA6.previous, IMA7.previous))) +
        geom_col(aes(fill = factor(c(IMA6.current, IMA7.current, IMA6.previous, IMA7.previous))), position = "dodge") +
        coord_flip() +
        scale_fill_manual(values = c("#02735E", "#038C3E", "#025951", "#0CF25D")) +
        labs(x = "", y = "Integrated Mean Area (m2/ha)",
             fill = "Year") +
        ggtitle("ORM - Project Analysis")
    })
    
    # Render project table
    output$project_table <- renderDataTable({
      df2_sub() %>%
        select(Landuse, `FARM NAME`, Planting.year, Area_Ha, Current.plots, Previous.plots, Volume.6years.current, Volume.7years.current,
               Volume.6years.previous, Volume.7years.previous, IMA6.current, IMA7.current, IMA6.previous, IMA7.previous) %>%
        rename("Landuse" = Landuse, "Farm Name" = `FARM NAME`, "Planting Year" = Planting.year,
               "Area (Ha)" = Area_Ha, "Current Plots" = Current.plots, "Previous Plots" = Previous.plots,
               "Volume 6 years current (m3/ha)" = Volume.6years.current, "Volume 7 years current (m3/ha)" = Volume.7years.current,
               "Volume 6 years previous (m3/ha)" = Volume.6years.previous, "Volume 7 years previous (m3/ha)" = Volume.7years.previous,
               "IMA6 current (m2/ha)" = IMA6.current, "IMA7 current (m2/ha)" = IMA7.current,
               "IMA6 previous (m2/ha)" = IMA6.previous, "IMA7 previous (m2/ha)" = IMA7.previous) %>%
        datatable(rownames = FALSE,
                  class = "display nowrap compact",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    scrollX = TRUE
                  ))
    })
    
    # Create reactive subset of df3 based on user inputs
    df3_sub <- reactive({
      df3 %>%
        filter(Landuse == input$landuse2,
               Climatic == input$climatic)
    })
    
    # Render climatic plot
    output$climatic_plot <- renderPlot({
      ggplot(df3_sub(), aes(x = factor(1), y = c(IMA6.current, IMA7.current, IMA6.previous, IMA7.previous))) +
        geom_col(aes(fill = factor(c(IMA6.current, IMA7.current, IMA6.previous, IMA7.previous))), position = "dodge") +
        coord_flip() +
        scale_fill_manual(values = c("#02735E", "#038C3E", "#025951", "#0CF25D")) +
        labs(x = "", y = "Integrated Mean Area (m2/ha)",
             fill = "Year") +
        ggtitle("ORM - Climatic Analysis")
    })
    
    # Render climatic table
    output$climatic_table <- renderDataTable({
      df3_sub() %>%
        select(Landuse, Climatic, Area_Ha, Current.plots, Previous.plots, Volume.6years.current, Volume.7years.current,
               Volume.6years.previous, Volume.7years.previous, IMA6.current, IMA7.current, IMA6.previous, IMA7.previous) %>%
        rename("Landuse" = Landuse, "Climatic" = Climatic,
               "Area (Ha)" = Area_Ha, "Current Plots" = Current.plots, "Previous Plots" = Previous.plots,
               "Volume 6 years current (m3/ha)" = Volume.6years.current, "Volume 7 years current (m3/ha)" = Volume.7years.current,
               "Volume 6 years previous (m3/ha)" = Volume.6years.previous, "Volume 7 years previous (m3/ha)" = Volume.7years.previous,
               "IMA6 current (m2/ha)" = IMA6.current, "IMA7 current (m2/ha)" = IMA7.current,
               "IMA6 previous (m2/ha)" = IMA6.previous, "IMA7 previous (m2/ha)" = IMA7.previous) %>%
        datatable(rownames = FALSE,
                  class = "display nowrap compact",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    scrollX = TRUE
                  ))
    })
    
    # Create reactive subset of df4 based on user inputs
    df4_sub <- reactive({
      df4 %>%
        filter(Landuse == input$landuse3,
               Planting.year == input$plantingyear2)
    })
    
    # Render planting year plot
    output$plantingyear_plot <- renderPlot({
      ggplot(df4_sub(), aes(x = factor(1), y = c(IMA6.current, IMA7.current, IMA6.previous, IMA7.previous))) +
        geom_col(aes(fill = factor(c(IMA6.current, IMA7.current, IMA6.previous, IMA7.previous))), position = "dodge") +
        coord_flip() +
        scale_fill_manual(values = c("#02735E", "#038C3E", "#025951", "#0CF25D")) +
        labs(x = "", y = "Integrated Mean Area (m2/ha)",
             fill = "Year") +
        ggtitle("ORM - Planting Year Analysis")
    })
    
    # Render planting year table
    output$plantingyear_table <- renderDataTable({
      df4_sub() %>%
        select(Landuse, Planting.year, Area_Ha, Current.plots, Previous.plots, Volume.6years.current, Volume.7years.current,
               Volume.6years.previous, Volume.7years.previous, IMA6.current, IMA7.current, IMA6.previous, IMA7.previous) %>%
        rename("Landuse" = Landuse, "Planting Year" = Planting.year,
               "Area (Ha)" = Area_Ha, "Current Plots" = Current.plots, "Previous Plots" = Previous.plots,
               "Volume 6 years current (m3/ha)" = Volume.6years.current, "Volume 7 years current (m3/ha)" = Volume.7years.current,
               "Volume 6 years previous (m3/ha)" = Volume.6years.previous, "Volume 7 years previous (m3/ha)" = Volume.7years.previous,
               "IMA6 current (m2/ha)" = IMA6.current, "IMA7 current (m2/ha)" = IMA7.current,
               "IMA6 previous (m2/ha)" = IMA6.previous, "IMA7 previous (m2/ha)" = IMA7.previous) %>%
        datatable(rownames = FALSE,
                  class = "display nowrap compact",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    scrollX = TRUE
                  ))
    })
    
    # Create reactive subset of df5 based on user inputs
    df5_sub <- reactive({
      df5 %>%
        filter(Landuse == input$landuse4,
               Genetic == input$genetic)
    })
    
    # Render genetic plot
    output$genetic_plot <- renderPlot({
      ggplot(df5_sub(), aes(x = factor(1), y = c(IMA6.current, IMA7.current, IMA6.previous, IMA7.previous))) +
        geom_col(aes(fill = factor(c(IMA6.current, IMA7.current, IMA6.previous, IMA7.previous))), position = "dodge") +
        coord_flip() +
        scale_fill_manual(values = c("#02735E", "#038C3E", "#025951", "#0CF25D")) +
        labs(x = "", y = "Integrated Mean Area (m2/ha)",
             fill = "Year") +
        ggtitle("ORM - Genetic Analysis")
    })
    
    # Render genetic table
    output$genetic_table <- renderDataTable({
      df5_sub() %>%
        select(Landuse, Genetic, Area_Ha, Current.plots, Previous.plots, Volume.6years.current, Volume.7years.current,
               Volume.6years.previous, Volume.7years.previous, IMA6.current, IMA7.current, IMA6.previous, IMA7.previous) %>%
        rename("Landuse" = Landuse, "Genetic" = Genetic,
               "Area (Ha)" = Area_Ha, "Current Plots" = Current.plots, "Previous Plots" = Previous.plots,
               "Volume 6 years current (m3/ha)" = Volume.6years.current, "Volume 7 years current (m3/ha)" = Volume.7years.current,
               "Volume 6 years previous (m3/ha)" = Volume.6years.previous, "Volume 7 years previous (m3/ha)" = Volume.7years.previous,
               "IMA6 current (m2/ha)" = IMA6.current, "IMA7 current (m2/ha)" = IMA7.current,
               "IMA6 previous (m2/ha)" = IMA6.previous, "IMA7 previous (m2/ha)" = IMA7.previous) %>%
        datatable(rownames = FALSE,
                  class = "display nowrap compact",
                  extensions = c('Buttons', 'Scroller'),
                  options = list(
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    scrollX = TRUE
                  ))
    })
    
  }
  
  # Run the shinyapp
  shinyApp(ui, server)
  
    