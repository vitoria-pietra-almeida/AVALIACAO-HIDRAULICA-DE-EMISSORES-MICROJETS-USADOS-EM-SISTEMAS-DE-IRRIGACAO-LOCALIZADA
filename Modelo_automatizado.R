################################################################################
# SCRIPT DE AVALIAÇÃO HIDRÁULICA DE EMISSORES "MICROJETS" USADOS EM SISTEMAS   #
# DE IRRIGAÇÃO LOCALIZADA - TCC (UFRA) - JULHO/2025                            #
# Autora: Vitoria Pietra Pinto de Almeida                                      #
################################################################################


# Instalação e carregamento das bibliotecas necessárias
if(!require(pastecs)) install.packages("pastecs")
if(!require(psych)) install.packages("psych")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(readxl)) install.packages("readxl")
if(!require(grid)) install.packages("grid")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(showtext)) install.packages("showtext")

library(pastecs)
library(psych)
library(ggplot2)
library(dplyr)
library(readxl)
library(grid)
library(openxlsx)
library(showtext)

# Função para calcular o R²
calcular_R2 <- function(y_obs, y_pred) {
  RSS <- sum((y_obs - y_pred)^2)
  TSS <- sum((y_obs - mean(y_obs))^2)
  R2 <- 1 - (RSS / TSS)
  return(R2)
}

# Função para calcular o CVf
calcular_cvf <- function(dados) {
  cvf <- dados %>%
    group_by(P) %>%
    summarise(
      media_Q = mean(Q),
      desvio_padrao = sd(Q),
      CVf = (sd(Q) / mean(Q)) * 100
    )
  return(cvf)
}

# Função para processar cada microjet
processar_microjet <- function(caminho_excel, pasta_saida, cor_pontos, titulo_grafico, y_breaks, y_limits) {
  
  # Leitura dos dados
  dados <- read_excel(caminho_excel)
  
  # Ajuste do modelo
  modelo <- nls(Q ~ k * P^x, start = list(k = 1, x = 0.5), data = dados)
  
  # Resumo completo do modelo (k, x, erro padrão, significância, etc)
  cat("\n\n===== RESUMO DO MODELO:", titulo_grafico, "=====\n")
  print(summary(modelo))
  
  # Adicionar os valores previstos pelo modelo (Q_pred)
  dados$Q_pred <- predict(modelo)
  
  # Calcular R² e RMSE
  R2 <- calcular_R2(dados$Q, dados$Q_pred)
  RMSE <- sqrt(mean((dados$Q - dados$Q_pred)^2))
  
  # Calcular CVf
  cvf <- calcular_cvf(dados)
  
  # Estatística descritiva por pressão
  estatisticas_pressao <- dados %>%
    group_by(P) %>%
    summarise(
      media_Q = mean(Q),
      desvio_padrao_Q = sd(Q),
      n = n(),
      Q_min = min(Q),
      Q_max = max(Q)
    )
  
  cat("\n===== Estatísticas Descritivas por Pressão:", titulo_grafico, "=====\n")
  print(estatisticas_pressao)
  
  # Exibir R²
  cat("\nR² para", titulo_grafico, "=", round(R2, 4), "\n")
  cat("RMSE para", titulo_grafico, "=", round(RMSE, 4), "\n")
  
  # Exibir CVf
  cat("\nCoeficiente de Variação de Fabricação (CVf) por Pressão:\n")
  print(cvf)
  
  # Gráfico
  grafico <- ggplot(dados, aes(x = P)) +
    geom_point(aes(y = Q, color = "Vazão Observada"), size = 4.5) +
    geom_line(aes(y = Q_pred, color = "Vazão Estimada (Modelo)"), linewidth = 0.9) +
    labs(
      title = titulo_grafico,
      x = "Pressão (m.c.a.)",
      y = expression("Vazão (L·h"^{-1}*")"),
      color = NULL
    ) +
    scale_color_manual(values = c(
      "Vazão Observada" = cor_pontos,
      "Vazão Estimada (Modelo)" = "cyan"
    )) +
    scale_x_continuous(breaks = seq(5, 30, by = 5), limits = c(5, 30), expand = c(0, 0)) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits, expand = c(0, 0)) +
    theme_minimal(base_size = 40, base_family = "Times New Roman") +
    theme(
      plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 40),
      axis.text = element_text(size = 30),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.grid.major = element_line(color = "grey94"),
      panel.grid.minor = element_line(color = "grey97"),
      legend.position = "bottom",
      legend.text = element_text(size = 30),
      legend.key.width = unit(2, "cm")
    )
  
  print(grafico)
  
  # Salvar o gráfico
  nome_grafico <- file.path(pasta_saida, paste0(gsub(" ", "_", tolower(titulo_grafico)), ".png"))
  ggsave(nome_grafico, plot = grafico, width = 16, height = 10, dpi = 600, type = "cairo-png")
  
  # Criar o arquivo Excel com várias abas
  nome_excel <- file.path(pasta_saida, paste0("resultados_", gsub(" ", "_", tolower(titulo_grafico)), ".xlsx"))
  wb <- createWorkbook()
  
  # Aba 1 - Coeficientes do Modelo + R² + RMSE
  tabela_modelo <- as.data.frame(summary(modelo)$coefficients)
  
  linha_em_branco <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(tabela_modelo)))
  colnames(linha_em_branco) <- colnames(tabela_modelo)
  rownames(linha_em_branco) <- ""
  
  tabela_R2 <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(tabela_modelo)))
  colnames(tabela_R2) <- colnames(tabela_modelo)
  rownames(tabela_R2) <- "R2 do Modelo"
  tabela_R2[1, "Estimate"] <- R2
  
  tabela_RMSE <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(tabela_modelo)))
  colnames(tabela_RMSE) <- colnames(tabela_modelo)
  rownames(tabela_RMSE) <- "RMSE do Modelo"
  tabela_RMSE[1, "Estimate"] <- RMSE
  
  tabela_final_modelo <- rbind(tabela_modelo, linha_em_branco, tabela_R2, tabela_RMSE)
  
  addWorksheet(wb, "Modelo_e_R2")
  writeData(wb, "Modelo_e_R2", tabela_final_modelo, rowNames = TRUE)
  
  # Aba 2 - Estatísticas Descritivas
  addWorksheet(wb, "Estatisticas_Por_Pressao")
  writeData(wb, "Estatisticas_Por_Pressao", estatisticas_pressao)
  
  # Aba 3 - CVf
  addWorksheet(wb, "CVf")
  writeData(wb, "CVf", cvf)
  
  # Salvar o arquivo Excel
  saveWorkbook(wb, nome_excel, overwrite = TRUE)
  
  # Criar arquivo de saída .txt com todas as informações
  nome_txt <- file.path(pasta_saida, paste0("relatorio_", gsub(" ", "_", tolower(titulo_grafico)), ".txt"))
  sink(nome_txt)
  
  cat("===== AJUSTE DO MODELO NLS =====\n")
  print(summary(modelo))
  
  cat("\n\n===== R² DO MODELO =====\n")
  print(R2)
  
  cat("\n\n===== RMSE DO MODELO =====\n")
  print(RMSE)
  
  cat("\n\n===== ESTATÍSTICA DESCRITIVA POR PRESSÃO =====\n")
  print(describeBy(dados, dados$P))
  
  cat("\n\n===== CVf POR PRESSÃO =====\n")
  print(cvf)
  
  sink()
  
  # Retorno
  return(list(modelo = modelo, R2 = R2, cvf = cvf, grafico = grafico))
}

##APÓS RODAR UMA VEZ OS COMANDOS ACIMA, CASO VÁ PROCESSAR OUTROS DADOS, 
##BASTA MUDAR O ENDEREÇO DO ARQUIVO COM OS DADOS E A PASTA DE SAÍDA. 
##FEITO ISSO, SÓ PRECISA MARCAR A PARTIR DAQUI PARA RODAR DE NOVO
                                                         #
#######################################                  #
###       PROCESSAR MICROJETs       ###                  #
#######################################                #####
                                                        ###
# Microjet VERMELHO                                      #
result_vermelho <- processar_microjet(
  caminho_excel = "D:/DOCUMENTOS/1_UFRA/2025/TCC/microjet_vermelho2.xlsx",
  pasta_saida = "D:/DOCUMENTOS/1_UFRA/2025/TCC/EMISSORES/VERMELHO TESTE",
  cor_pontos = "red",
  titulo_grafico = "Relação Vazão x Pressão – Microjet Vermelho",
  y_breaks = seq(30, 65, by = 5),
  y_limits = c(30, 65)
)

#REPETIÇÕES
# Microjet PRETO
result_preto <- processar_microjet(
  caminho_excel = "D:/DOCUMENTOS/1_UFRA/2025/TCC/EMISSORES/PRETO/microjet_preto2.xlsx",
  pasta_saida = "D:/DOCUMENTOS/1_UFRA/2025/TCC/EMISSORES/PRETO",
  cor_pontos = "black",
  titulo_grafico = "Relação Vazão x Pressão – Microjet Preto",
  y_breaks = seq(17, 32, by = 5),
  y_limits = c(17, 32)
)

# Microjet AZUL
result_azul <- processar_microjet(
  caminho_excel = "D:/DOCUMENTOS/1_UFRA/2025/TCC/EMISSORES/AZUL/microjet_azul2.xlsx",
  pasta_saida = "D:/DOCUMENTOS/1_UFRA/2025/TCC/EMISSORES/AZUL",
  cor_pontos = "blue",
  titulo_grafico = "Relação Vazão x Pressão – Microjet Azul",
  y_breaks = seq(12.5, 25, by = 2.5),
  y_limits = c(12.5, 25)
)

