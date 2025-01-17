# Pacotes
# Pacotes necessários
library(shiny)
library(ggplot2)
library(MASS)

# Carregar os dados
bd <- read_sav("bd_reg_mee3.sav")
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

# Modelo de regressão
mod_emoc5_trans <- lm(mdc_emoc_trans ~ vit_bin + acp_desor + acp_coesao + 
                        sex_rec + cor_rec + idade_rec + acp_exclu + acp_preocup + 
                        mdc_cog_trans + mdc_comp_trans, data = bd)

# Interface do Usuário (UI)
ui <- fluidPage(
  titlePanel("Modelo de Regressão Linear Múltipla"),
  sidebarLayout(
    sidebarPanel(
      h4("Entrada das Variáveis"),
      selectInput("vit_bin", "Vitimização (Sim/Não):", 
                  choices = list("Sim" = 0, "Não" = 1), selected = 0),
      sliderInput("acp_desor", "Desorganização Social (Zscore):", 
                  min = -2.172, max = 2.379, value = 0, step = 0.01),
      sliderInput("acp_coesao", "Coesão Social (Zscore):", 
                  min = -4.168, max = 1.896, value = 0, step = 0.01),
      selectInput("sex_rec", "Sexo (Feminino/Masculino):", 
                  choices = list("Feminino" = 0, "Masculino" = 1), selected = 0),
      selectInput("cor_rec", "Cor (Não branca/Branca):", 
                  choices = list("Não branca" = 0, "Branca" = 1), selected = 0),
      selectInput("idade_rec", "Idade (Mais Velhos/Mais Novos):", 
                  choices = list("Mais Velhos" = 0, "Mais Novos" = 1), selected = 0),
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
      h4("Resumo do Modelo"),
      verbatimTextOutput("model_summary"),
      plotOutput("regression_plot")
    )
  )
)

# Servidor (Server)
server <- function(input, output) {
  # Função de predição
  predict_emoc <- reactive({
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
    predict(mod_emoc5_trans, newdata = new_data)
  })
  
  # Resultado da predição
  output$prediction_result <- renderText({
    req(input$predict_button)  # Garante que o botão foi pressionado
    paste("Predição do Emocional (Transformado):", round(predict_emoc(), 4))
  })
  
  # Resumo do modelo
  output$model_summary <- renderPrint({
    summary(mod_emoc5_trans)
  })
  
  # Gráfico de regressão
  output$regression_plot <- renderPlot({
    ggplot(bd, aes(x = mdc_cog_trans, y = mdc_emoc_trans)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, color = "blue") +
      labs(title = "Relação entre Cognitivo e Emocional",
           x = "Cognitivo (Transformado)",
           y = "Emocional (Transformado)") +
      theme_minimal()
  })
}

# Executar o aplicativo Shiny
shinyApp(ui, server)
