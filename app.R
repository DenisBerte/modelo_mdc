library(car)
library(pacman)
library(haven)
library(dplyr)
library(MASS)
library(lmtest)
library(nortest)
library(ggplot2)
library(openxlsx)
library(gridExtra)

options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Ler a base de dados
bd <- read.xlsx("bd_reg_mee3.xlsx")

# Remover valores ausentes
bd <- na.omit(bd)

# Transformações Box-Cox
bc_emoc <- boxcox(mdc_emoc ~ 1, data = bd, lambda = seq(-2, 2, by = 0.1))
lambda_emoc <- bc_emoc$x[which.max(bc_emoc$y)]
bd$mdc_emoc_trans <- (bd$mdc_emoc^lambda_emoc - 1) / lambda_emoc

bc_cog <- boxcox(mdc_cog ~ 1, data = bd, lambda = seq(-2, 2, by = 0.1))
lambda_cog <- bc_cog$x[which.max(bc_cog$y)]
bd$mdc_cog_trans <- (bd$mdc_cog^lambda_cog - 1) / lambda_cog

bc_comp <- boxcox(mdc_comp ~ 1, data = bd, lambda = seq(-2, 2, by = 0.1))
lambda_comp <- bc_comp$x[which.max(bc_comp$y)]
bd$mdc_comp_trans <- (bd$mdc_comp^lambda_comp - 1) / lambda_comp

# Interface do Usuário (UI)
ui <- fluidPage(
  titlePanel("Modelo de Regressão Linear Múltipla"),
  sidebarLayout(
    sidebarPanel(
      h4("Seleção do Modelo"),
      selectInput("dependent_var", "Escolha a Variável Dependente:", 
                  choices = list("Emocional" = "mdc_emoc_trans", 
                                 "Cognitivo" = "mdc_cog_trans", 
                                 "Comportamental" = "mdc_comp_trans")),
      h4("Entrada das Variáveis"),
      selectInput("vit_bin", "Vitimização (Sim/Não):", 
                  choices = list("Sim" = 0, "Não" = 1), selected = 0),
      selectInput("sex_rec", "Sexo (Feminino/Masculino):", 
                  choices = list("Feminino" = 0, "Masculino" = 1), selected = 0),
      selectInput("cor_rec", "Cor (Não branca/Branca):", 
                  choices = list("Não branca" = 0, "Branca" = 1), selected = 0),
      selectInput("idade_rec", "Idade (Mais Velhos /Mais Novos):", 
                  choices = list("Mais Velhos (41+)" = 0, "Mais Novos (40-)" = 1), selected = 0),
      sliderInput("acp_desor", "Desorganização Social (Zscore):", 
                  min = -2.172, max = 2.379, value = 0, step = 0.01),
      sliderInput("acp_coesao", "Coesão Social (Zscore):", 
                  min = -4.168, max = 1.896, value = 0, step = 0.01),
      sliderInput("acp_exclu", "Auto-percepção de Desprezo (Zscore):", 
                  min = -1.761, max = 4.639, value = 0, step = 0.01),
      sliderInput("acp_preocup", "Saúde e Preocupação (Zscore):", 
                  min = -2.106, max = 4.933, value = 0, step = 0.01),
      sliderInput("mdc_cog_trans", "Cognitivo (Transformado):", 
                  min = 0, max = 4.604, value = 2.093, step = 0.01),
      sliderInput("mdc_comp_trans", "Comportamental (Transformado):", 
                  min = 0, max = 2.25, value = 0.943, step = 0.01),
      actionButton("predict_button", "Prever")
    ),
    mainPanel(
      h4("Resultado da Predição"),
      verbatimTextOutput("prediction_result"),
      h4("Resumo do Modelo e Testes de Pressupostos"),
      verbatimTextOutput("model_summary"),
      plotOutput("residual_plots"),
      h4("Gráficos de Predição"),
      plotOutput("prediction_plots")
    )
  )
)

# Servidor (Server)
server <- function(input, output) {
  # Função de predição
  selected_model <- reactive({
    formula <- as.formula(paste(input$dependent_var, "~ vit_bin + acp_desor + acp_coesao + 
                                 sex_rec + cor_rec + idade_rec + acp_exclu + acp_preocup + 
                                 mdc_emoc_trans + mdc_cog_trans + mdc_comp_trans"))
    stepAIC(lm(formula, data = bd), direction = "both", trace = FALSE)
  })
  
  predict_model <- reactive({
    new_data <- data.frame(
      vit_bin = as.numeric(input$vit_bin),
      acp_desor = input$acp_desor,
      acp_coesao = input$acp_coesao,
      sex_rec = as.numeric(input$sex_rec),
      cor_rec = as.numeric(input$cor_rec),
      idade_rec = as.numeric(input$idade_rec),
      acp_exclu = input$acp_exclu,
      acp_preocup = input$acp_preocup,
      mdc_cog_trans = input$mdc_cog_trans,
      mdc_comp_trans = input$mdc_comp_trans
    )
    predict(selected_model(), newdata = new_data)
  })
  
  # Resultado da predição
  output$prediction_result <- renderText({
    req(input$predict_button)
    paste("Predição:", round(predict_model(), 4))
  })
  
  # Resumo do modelo e testes de pressupostos
  output$model_summary <- renderPrint({
    model <- selected_model()
    cat("Resumo do Modelo:\n")
    print(summary(model))
    cat("\nTeste de Breusch-Pagan:\n")
    print(bptest(model))
    cat("\nTeste de Durbin-Watson:\n")
    print(dwtest(model))
    cat("\nFatores de Inflação da Variância (VIF):\n")
    print(vif(model))
    cat("\nTeste de Shapiro-Francia:\n")
    print(sf.test(residuals(model)))
  })
  
  # Gráficos de resíduos
  output$residual_plots <- renderPlot({
    crPlots(selected_model())
  })
  
  # Gráficos de predição
  output$prediction_plots <- renderPlot({
    model <- selected_model()
    y_pred <- predict(model, newdata = bd)
    bd$y_pred <- y_pred
    y_limits <- range(y_pred)
    
    grafico_exclusao <- ggplot(bd) +
      geom_smooth(aes(x = acp_exclu, y = y_pred), method = "lm", se = TRUE, color = "black", fill = "gray") +
      labs(x = "I.S: Percepção de Exclusão", y = "Preditos") +
      ylim(y_limits) +
      theme_classic()
    
    grafico_preocupacao <- ggplot(bd) +
      geom_smooth(aes(x = acp_preocup, y = y_pred), method = "lm", se = TRUE, color = "black", fill = "gray") +
      labs(x = "I.S: Preocupação com Futuro", y = "Preditos") +
      ylim(y_limits) +
      theme_classic()
    
    grid.arrange(grafico_exclusao, grafico_preocupacao, ncol = 2)
  })
}

# Executar o aplicativo Shiny
shinyApp(ui, server)



