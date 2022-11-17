################################################################################
# A seguir serão demonstrados os algorítimos utilizados para desenvolvimento   #
# do TCC do MBA em Data Science and Analytis do MBA USP Esalq - Turma 202      #  
#Uso da técnica de Regressão Logistica Binomial                                #
################################################################################

################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS            #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet", "magick",
             "cowplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

################################################################################
#CARREGAMENTO DA BASE DE DADOS                                                 #
################################################################################
library(readxl)
TCC1 <- read_excel("~/2022/MBA/Supervised ML Modelos Logísticos Binários e Multinomiais  I_II/Aula 1/TCC1.xlsx",
)
View(TCC1)

################################################################################
#OBSERVAÇÃO DA BASE DE DADOS TCC1: y:satisfeito (S/N)                          #
################################################################################
#Visualizando a base de dados TCC1
TCC1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 16)

#Estatísticas Univariadas da Base de Dados
summary(TCC1)

#Tabela de frequências absolutas das variáveis qualitativas
table(TCC1$satisfeito)
table(TCC1$buscar)
table(TCC1$remuneracao)
table(TCC1$beneficios)
table(TCC1$proposito)
table(TCC1$flexibilidade)
table(TCC1$ambiente)
table(TCC1$aprendizado)
table(TCC1$carreira)
table(TCC1$maturidade)
table(TCC1$gestores)
table(TCC1$reputacao)
table(TCC1$presencialfull)
table(TCC1$regiao)
table(TCC1$cargo)
table(TCC1$formatrabalhoatual)

################################################################################
<<<<<<< HEAD
#TRANSFORMANDO VARIÁVEIS EM FACTOR                                             #
=======
TRANSFORMANDO VARIÁVEIS EM FACTOR                                              #
>>>>>>> af3e9ff5efd4674d9b349df6117947582b0b26bd
################################################################################
TCC1$buscar<-as.factor(TCC1$buscar) 
TCC1$satisfeito<-as.factor(TCC1$satisfeito) 
TCC1$remuneracao<-as.factor(TCC1$remuneracao)
TCC1$beneficios<-as.factor(TCC1$beneficios)
TCC1$proposito<-as.factor(TCC1$proposito)
TCC1$flexibilidade<-as.factor(TCC1$flexibilidade)
TCC1$ambiente<-as.factor(TCC1$ambiente)
TCC1$aprendizado<-as.factor(TCC1$aprendizado)
TCC1$carreira<-as.factor(TCC1$carreira)
TCC1$maturidade<-as.factor(TCC1$maturidade)
TCC1$gestores<-as.factor(TCC1$gestores)
TCC1$reputacao<-as.factor(TCC1$reputacao)
TCC1$presencialfull<-as.factor(TCC1$presencialfull)
TCC1$regiao<-as.factor(TCC1$regiao)
TCC1$cargo<-as.factor(TCC1$cargo)
TCC1$formatrabalhoatual<-as.factor(TCC1$formatrabalhoatual)

glimpse(TCC1)


<<<<<<< HEAD
################################################################################
#                    PROCEDIMENTO N-1 DUMMIES                                  #
################################################################################
#Dummizando as variáveis abaixo:# 
=======
##############################################################################
#                    PROCEDIMENTO N-1 DUMMIES                                #
##############################################################################
#Dummizando as variáveis atendimento, sortimento, acessibilidade e preço. O 
#código abaixo, automaticamente, fará: a) a dummização das variáveis originais;
#b)removerá as variáveis dummizadas originais; c) estabelecerá como categorias 
#de referência as categorias de label 1 de cada variável original.
>>>>>>> af3e9ff5efd4674d9b349df6117947582b0b26bd

TCC1_dummies <- dummy_columns(.data = TCC1,
                              select_columns = c("satisfeito",
                                                 "buscar",
                                                 "remuneracao",
                                                 "beneficios",
                                                 "proposito",
                                                 "flexibilidade",
                                                 "ambiente",
                                                 "aprendizado",
                                                 "carreira",
                                                 "maturidade",
                                                 "gestores",
                                                 "reputacao",
                                                 "presencialfull",
                                                 "regiao",
                                                 "cargo",
                                                 "formatrabalhoatual"),
                              remove_selected_columns = T,
                              remove_first_dummy = T)


<<<<<<< HEAD
################################################################################
#                       ESTIMANDO O MODELO                                     #
################################################################################
=======
##############################################################################
#                       ESTIMANDO O MODELO                                   #
##############################################################################
>>>>>>> af3e9ff5efd4674d9b349df6117947582b0b26bd
#Visualizando a base de dados fidelidade_dummies
TCC1_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

modelo_TCC1_dummies <- glm(formula = satisfeito_sim ~ . , 
                           data = TCC1_dummies, 
                           family = "binomial")

#Parâmetros do modelo_TCC1_dummies
summary(modelo_TCC1_dummies)

#Valor do LL do modelo_TCC1_dummies
logLik(modelo_TCC1_dummies)

#Procedimento Stepwise
step_TCC1_dummies <- step(object = modelo_TCC1_dummies,
                          k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#Parâmetros do modelo step_TCC1_dummies
summary(step_TCC1_dummies)

#Outro modo de apresentar os outputs do modelo step_TCC1_dummies
summ(step_TCC1_dummies, confint = T, digits = 3, ci.width = .95)
export_summs(step_TCC1_dummies, scale = F, digits = 6)

#Valor do LL do modelo step_TCC1_dummies
logLik(step_TCC1_dummies)

#Comparando os modelos step_TCC1_dummies e modelo_TCC1_dummies
#função lrtest do pacote lmtest
lrtest(modelo_TCC1_dummies, step_TCC1_dummies)

export_summs(modelo_TCC1_dummies, step_TCC1_dummies, scale = F,
             digits = 4)


################################################################################
#              CONSTRUÇÃO DE UMA MATRIZ DE CONFUSÃO                            #
################################################################################
confusionMatrix(
  table(predict(step_TCC1_dummies, type = "response") >= 0.5, 
        TCC1$satisfeito == "sim")[2:1, 2:1]
)


################################################################################
# IGUALANDO OS CRITÉRIOS DE ESPECIFICIDADE E DE SENSITIVIDADE                  #
################################################################################
#Igualando a probabilidade de acerto daqueles que estão satisfeitos
#(sensitividade) e a probabilidade de acerto daqueles que não estão satisfeitos
#(especificidade).

#ATENÇÃO: o que será feito a seguir possui fins didáticos, apenas. DE NENHUMA
#FORMA o procedimento garante a maximização da acurácia do modelo!

#função prediction do pacote ROCR
predicoes <- prediction(predictions = step_TCC1_dummies$fitted.values, 
                        labels = TCC1$satisfeito) 
#a função prediction, do pacote ROCR, cria um objeto com os dados necessários
#para a futura plotagem da curva ROC.

#função performance do pacote ROCR
dados_curva_roc <- performance(predicoes, measure = "sens") 
#A função peformance(), do pacote ROCR, extraiu do objeto 'predicoes' os 
#dados de sensitividade, de sensibilidade e de especificidade para a plotagem.

#Porém, desejamos os dados da sensitividade, então devemos fazer o seguinte 
#ajuste:
sensitividade <- dados_curva_roc@y.values[[1]] 

#extraindo dados da sensitividade do modelo
especificidade <- performance(predicoes, measure = "spec") 

#extraindo os dados da especificidade, mas também há que se fazer um ajuste 
#para a plotagem:
especificidade <- especificidade@y.values[[1]]

cutoffs <- dados_curva_roc@x.values[[1]] 
#extraindo os cutoffs do objeto 'sensitividade'.

#Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
#e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
#base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
#frame que contém os vetores mencionados.

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

#Visualizando o novo data frame dados_plotagem
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

################################################################################
#                            CONSTRUÇÃO DA CURVA ROC                           #
################################################################################
ROC <- roc(response = TCC1$satisfeito, 
           predictor = step_TCC1_dummies$fitted.values)

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

################################################################################
#                        Análises de probabilidade                             #
################################################################################

## Variáveis: beneficios = sim, flexibilidade = sim; maturidade = sim 

#Cargo: Cientista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 1, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")

#Cargo: Engenheiro de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 1,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Engenheiro de Machine Learning
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 1, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 1,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de BI
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 1, cargo_analbi = 1,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Estatístico
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 1,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Professor
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 1
        ),
        type = "response")    


## Variáveis: beneficios = sim, flexibilidade = não; maturidade = sim 

#Cargo: Cientista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 1, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")

#Cargo: Engenheiro de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 1,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Engenheiro de Machine Learning
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 1, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 1,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de BI
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 1,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Estatístico
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 1,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Professor
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 1
        ),
        type = "response")    

## Variáveis: beneficios = sim, flexibilidade = sim; maturidade = não 

#Cargo: Cientista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 1, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")

#Cargo: Engenheiro de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 1,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Engenheiro de Machine Learning
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 1, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 1,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de BI
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 1,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Estatístico
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 1,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Professor
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 1
        ),
        type = "response")    

## Variáveis: beneficios = não, flexibilidade = sim; maturidade = sim 

#Cargo: Cientista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 1, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")

#Cargo: Engenheiro de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 1,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Engenheiro de Machine Learning
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 1, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 1,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de BI
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 1,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Estatístico
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 1,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Professor
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 1
        ),
        type = "response")   

## Variáveis: beneficios = não, flexibilidade = sim; maturidade = não 

#Cargo: Cientista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 1, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")

#Cargo: Engenheiro de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 1,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Engenheiro de Machine Learning
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 1, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 1,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de BI
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 1,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Estatístico
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 1,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Professor
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 1,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 1
        ),
        type = "response")   

## Variáveis: beneficios = não, flexibilidade = não; maturidade = sim 

#Cargo: Cientista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 1, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")

#Cargo: Engenheiro de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 1,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Engenheiro de Machine Learning
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 1, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 1,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de BI
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 1,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Estatístico
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 1,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Professor
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 0, flexibilidade_sim = 0,
                   maturidade_sim = 1, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 1
        ),
        type = "response")   

## Variáveis: beneficios = sim, flexibilidade = não; maturidade = não 

#Cargo: Cientista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 1, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")

#Cargo: Engenheiro de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 1,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Engenheiro de Machine Learning
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 1, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de Dados
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 1,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Analista de BI
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 0, cargo_analbi = 1,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Estatístico
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 1,cargo_outro = 0,cargo_professor = 0
        ),
        type = "response")    

#Cargo: Professor
predict(object = step_TCC1_dummies,
        data.frame(beneficios_sim = 1, flexibilidade_sim = 0,
                   maturidade_sim = 0, cargo_analbi = 0,cargo_analdados = 0,
                   cargo_analint = 0, cargo_analneg = 0, cargo_analsist = 0,
                   cargo_cientista = 0, cargo_dev = 0, cargo_engdados = 0,
                   cargo_engml = 0, cargo_engoutras = 0, 
                   cargo_estatistico = 0,cargo_outro = 0,cargo_professor = 1
        ),
        type = "response")   




