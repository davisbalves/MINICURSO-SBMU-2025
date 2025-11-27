###############################################################################
#                       MINICURSO: INTRODUÇÃO A ESTUDOS                       #
#                 EPIDEMIOLÓGICOS E MÉTODOS DE ANÁLISE NO R                   #
#                                 BLOCO PRÁTICO                               #
###############################################################################

###############################################################################
#                             ANÁLISE ECOLÓGICA NO R                          #
#                 Mapas, Descrição, Correlação e Comparações                  #
###############################################################################

# Instalando pacotes necessários ----------------------------------------------
pacotes <- c("geobr","patchwork","sf","ggplot2","dplyr","dunn.test","broom",
  "readr","NHANES","summarytools","rqlm","survival","survminer")

# Instalar apenas os que não estão instalados
instalar <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]

if(length(instalar) > 0){
  install.packages(instalar, dependencies = TRUE)
} else {
  message("Todos os pacotes já estão instalados.")
}

# Carregando pacotes ----------------------------------------------------------

library(geobr)        # Para baixar mapas geográficos do Brasil
library(patchwork)    # Para combinar múltiplos gráficos
library(sf)           # Para manipulação de objetos espaciais (shapefiles)
library(ggplot2)      # Para visualização (inclui geom_sf)
library(dplyr)        # Para manipulação de dados
library(dunn.test)    # Para pós-teste do Kruskal-Wallis
library(broom)        # Para organizar resultados de testes estatísticos
library(readr)        # Para leitura de arquivos CSV
library(NHANES)       # Banco de dados de estudo seccional
library(summarytools) # Ferramentas de análise descritiva
library(rqlm)         # Para ajustar modelo de poisson com variância robusta
library(survival)     # Análise de sobrevida
library(survminer)    # Gráficos bonitos na Análise de sobrevida

###############################################################################
# 1. Importação e preparação dos dados
###############################################################################

# Lendo arquivo com os dados agregados dos municípios do RJ -------------------

ecologico <- read_delim(
  "https://raw.githubusercontent.com/davisbalves/MINICURSO-SBMU-2025/main/dados_exemplo_ecologico.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  trim_ws = TRUE
)

# Baixando o shapefile dos municípios do RJ -----------------------------------
rj <- geobr::read_municipality(code_muni = "RJ")

# Criando variável para chave de ligação (apenas os 6 primeiros dígitos) -------
rj$code_muni1 <- as.numeric(substr(rj$code_muni,1,6))

# Unindo a base do mapa com a base ecológica ----------------------------------
rj_mapa <- left_join(rj, ecologico, by = c("code_muni1"="code_muni"))


###############################################################################
# 2. Resumos descritivos das variáveis
###############################################################################

# Resumo da variável TAXA ------------------------------------------------------
summary(rj_mapa$TAXA)

# Municípios com menores valores de TAXA ---------------------------------------
rj_mapa %>% 
  select(name_muni, TAXA) %>% 
  arrange(TAXA) %>% 
  head()

# Municípios com maiores valores de TAXA ---------------------------------------
rj_mapa %>% 
  select(name_muni, TAXA) %>% 
  arrange(TAXA) %>% 
  tail()

# Resumo da variável PROP_IDOSO ------------------------------------------------
summary(rj_mapa$PROP_IDOSO)

# Menores valores de PROP_IDOSO ------------------------------------------------
rj_mapa %>% 
  select(name_muni, PROP_IDOSO) %>% 
  arrange(PROP_IDOSO) %>% 
  head()

# Maiores valores de PROP_IDOSO ------------------------------------------------
rj_mapa %>% 
  select(name_muni, PROP_IDOSO) %>% 
  arrange(PROP_IDOSO) %>% 
  tail()


###############################################################################
# 3. Criação das Categorias por Quartis (manualmente pelos resumos)
###############################################################################

# Visualizando os quartis da TAXA ----------------------------------------------
summary(rj_mapa$TAXA)

# Criando categoria de quartis da TAXA -----------------------------------------
rj_mapa$TAXA_CAT <- NA
rj_mapa$TAXA_CAT[rj_mapa$TAXA < summary(rj_mapa$TAXA)[2]] <- "Menor que 6,452"
rj_mapa$TAXA_CAT[rj_mapa$TAXA >= summary(rj_mapa$TAXA)[2] & 
                   rj_mapa$TAXA <= summary(rj_mapa$TAXA)[3]] <- "De 6,452 a 7,910"
rj_mapa$TAXA_CAT[rj_mapa$TAXA > summary(rj_mapa$TAXA)[3] & 
                   rj_mapa$TAXA <= summary(rj_mapa$TAXA)[5]] <- "De 7,910 a 10,568"
rj_mapa$TAXA_CAT[rj_mapa$TAXA > summary(rj_mapa$TAXA)[5]] <- "Maior que 10,568"

# Ordenando os níveis -----------------------------------------------------------
rj_mapa$TAXA_CAT <- factor(rj_mapa$TAXA_CAT,
                           levels = c("Menor que 6,452",
                                      "De 6,452 a 7,910",
                                      "De 7,910 a 10,568",
                                      "Maior que 10,568"))


# Criando categorias para PROP_IDOSO -------------------------------------------
summary(rj_mapa$PROP_IDOSO)

rj_mapa$PROP_IDOSO_CAT <- NA
rj_mapa$PROP_IDOSO_CAT[rj_mapa$PROP_IDOSO < summary(rj_mapa$PROP_IDOSO)[2]] <- "Menor que 17,20"
rj_mapa$PROP_IDOSO_CAT[rj_mapa$PROP_IDOSO >= summary(rj_mapa$PROP_IDOSO)[2] &
                         rj_mapa$PROP_IDOSO <= summary(rj_mapa$PROP_IDOSO)[3]] <- "De 17,20 a 19,20"
rj_mapa$PROP_IDOSO_CAT[rj_mapa$PROP_IDOSO > summary(rj_mapa$PROP_IDOSO)[3] &
                         rj_mapa$PROP_IDOSO <= summary(rj_mapa$PROP_IDOSO)[5]] <- "De 19,20 a 21,11"
rj_mapa$PROP_IDOSO_CAT[rj_mapa$PROP_IDOSO > summary(rj_mapa$PROP_IDOSO)[5]] <- "Maior que 21,11"

rj_mapa$PROP_IDOSO_CAT <- factor(rj_mapa$PROP_IDOSO_CAT,
                                 levels = c("Menor que 17,20",
                                            "De 17,20 a 19,20",
                                            "De 19,20 a 21,11",
                                            "Maior que 21,11"))


###############################################################################
# 4. Construção dos Mapas Temáticos (choropleth)
###############################################################################

# Mapa por quartis da TAXA -----------------------------------------------------
ggplot(rj_mapa) +
  geom_sf(aes(fill = TAXA_CAT), color = NA) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "grey90") +
  theme_minimal() +
  labs(fill = "Taxa",
       title = "Mapa da Taxa por Quartis – RJ") -> mapa_taxa

mapa_taxa

# Mapa por quartis da Proporção de Idosos --------------------------------------
ggplot(rj_mapa) +
  geom_sf(aes(fill = PROP_IDOSO_CAT), color = NA) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "grey90") +
  theme_minimal() +
  labs(fill = "Proporção de Idosos",
       title = "Mapa da Proporção de Idosos por Quartis – RJ") -> mapa_prop

# Exibindo os dois mapas lado a lado -------------------------------------------
mapa_taxa / mapa_prop


###############################################################################
# 5. Testes de Normalidade
###############################################################################

# Teste de normalidade da TAXA --------------------------------------------------
shapiro.test(rj_mapa$TAXA)

# Teste de normalidade da PROP_IDOSO -------------------------------------------
shapiro.test(rj_mapa$PROP_IDOSO)


###############################################################################
# 6. Correlação entre variáveis contínuas
###############################################################################

# Gráfico de dispersão entre PROP_IDOSO e TAXA ---------------------------------
plot(rj_mapa$PROP_IDOSO,
     rj_mapa$TAXA,
     xlab = "Proporção de Idosos",
     ylab = "Taxa de Mortalidade por IAM",
     main = "")

# Teste de correlação (Spearman é mais robusto) --------------------------------
cor.test(rj_mapa$PROP_IDOSO, rj_mapa$TAXA, method = "spearman")


###############################################################################
# 7. Comparação de grupos (quartis de idosos)
###############################################################################

# Normalidade por grupo ---------------------------------------------------------
data.frame(rj_mapa) %>%
  group_by(PROP_IDOSO_CAT) %>%
  summarise(
    n = n(),
    shapiro_p = shapiro.test(TAXA)$p.value
  )

# Boxplot comparando TAXA entre categorias de idosos ----------------------------
boxplot(rj_mapa$TAXA ~ rj_mapa$PROP_IDOSO_CAT,
        xlab = "",
        ylab = "Taxa de Mortalidade")

# Teste de Kruskal-Wallis -------------------------------------------------------
kruskal.test(rj_mapa$TAXA ~ rj_mapa$PROP_IDOSO_CAT)

# Teste Post-Hoc de Dunn --------------------------------------------------------
dunn.test(rj_mapa$TAXA, rj_mapa$PROP_IDOSO_CAT, altp = TRUE)

###############################################################################
#                  ANÁLISE DE DADOS DE ESTUDO SECCIONAL                       #
#       Associação, Comparações por Grupos e Razão de Prevalência (RP)        #
###############################################################################

# -------------------------------------------------------------------
# 1) CARREGAR O BANCO E EXPLORAR AS PRIMEIRAS LINHAS
# -------------------------------------------------------------------

data("NHANES")   # Carrega o banco NHANES incluído no pacote
head(NHANES)     # Visualiza as 6 primeiras linhas do banco

# -------------------------------------------------------------------
# 2) FILTRAR O CICLO 2011-2012 E APENAS ADULTOS (>=20 anos)
# -------------------------------------------------------------------

NHANES %>%
  filter(SurveyYr=="2011_12" & Age>=20) -> bd_11_12

# -------------------------------------------------------------------
# 3) FREQUÊNCIAS DE VARIÁVEIS CATEGÓRICAS
# -------------------------------------------------------------------
# O objetivo é descrever a amostra utilizada

freq(bd_11_12$Diabetes)     # Frequência de diabetes (sim/não)
freq(bd_11_12$Race1)        # Raça/etnia
freq(bd_11_12$PhysActive)   # Atividade física
descr(bd_11_12$Age,)         # idades

# -------------------------------------------------------------------
# -------------------------------------------------------------------
# 4) TABELAS DE CONTINGÊNCIA COM TESTE QUI-QUADRADO
# -------------------------------------------------------------------
# Investigamos associações entre características e diabetes

ctable(bd_11_12$Race1, bd_11_12$Diabetes,
       prop = "r", chisq = TRUE, useNA = "no")

ctable(bd_11_12$PhysActive, bd_11_12$Diabetes,
       prop = "r", chisq = TRUE, useNA = "no")

ctable(bd_11_12$AgeDecade2, bd_11_12$Diabetes,
       prop = "r", chisq = TRUE, useNA = "no")

# -------------------------------------------------------------------
# 5) COMPARAÇÃO DE IDADE SEGUNDO DIABETES
# -------------------------------------------------------------------

boxplot(bd_11_12$Age ~ bd_11_12$Diabetes,
        xlab = "",
        ylab = "Idade")

# Teste de normalidade da idade por grupo (Yes/No)
bd_11_12 %>% 
  group_by(Diabetes) %>% 
  summarise(pvalor = shapiro.test(Age)$p.value)

# Como não há normalidade → usar teste não paramétrico
wilcox.test(bd_11_12$Age ~ bd_11_12$Diabetes)

# -------------------------------------------------------------------
# 6) PREPARAÇÃO PARA MODELOS DE PREVALÊNCIA (POISSON ROBUSTO)
# -------------------------------------------------------------------
# Criar variável binária de diabetes (0/1)

bd_11_12 %>%
  mutate(
    Diabetes2 = ifelse(Diabetes=="Yes",1,
                       ifelse(Diabetes=="No",0,NA)),
    Race1 = factor(Race1,
                   levels = c("White","Hispanic","Mexican","Black","Other")),
    PhysActive = factor(PhysActive,
                        levels = c("Yes","No"))
  ) -> bd_11_12

# -------------------------------------------------------------------
# 7) MODELOS DE POISSON ROBUSTO (Razão de Prevalência)
# -------------------------------------------------------------------
# O objetivo agora é estimar razões de prevalência (PR) e IC95%

# PR para diabetes segundo raça/etnia
rqlm(Diabetes2 ~ Race1, data = bd_11_12,
     eform = TRUE, family = "poisson")

# PR para diabetes segundo atividade física
rqlm(Diabetes2 ~ PhysActive, data = bd_11_12,
     eform = TRUE, family = "poisson")

# PR para diabetes segundo idade contínua
rqlm(Diabetes2 ~ Age, data = bd_11_12,
     eform = TRUE, family = "poisson")

###############################################################################
#                ANÁLISE DE DADOS DE ESTUDO CASO-CONTROLE                     #
#       Associação, Comparações por Grupos e Razão de Prevalência (RP)        #
###############################################################################

# -------------------------------------------------------------------
# 1) CARREGAMENTO DO BANCO
# -------------------------------------------------------------------

data(infert)     # Carrega o banco infert, disponível no R base
head(infert)     # Visualiza as primeiras linhas

?infert          # Abre a documentação do banco (importante!)

# -------------------------------------------------------------------
# 2) PREPARAÇÃO DAS VARIÁVEIS
# -------------------------------------------------------------------
# Criar fator "infertilidade" para representar casos e controles
# Criar fator para abortos espontâneos

infert %>% 
  mutate(
    infertilidade = factor(case, labels = c("No", "Yes")),
    abort_spont   = factor(spontaneous)
  ) -> infert

# -------------------------------------------------------------------
# 3) DISTRIBUIÇÃO DE FREQUÊNCIA (CARACTERIZAÇÃO DA AMOSTRA)
# -------------------------------------------------------------------

freq(infert$infertilidade)   # Frequência de casos e controles
freq(infert$education)       # Escolaridade
freq(infert$abort_spont)     # Abortos espontâneos prévios
descr(infert$Age)            # Descrição da idade

# -------------------------------------------------------------------
# 4) ASSOCIAÇÃO ENTRE VARIÁVEIS CATEGÓRICAS
# -------------------------------------------------------------------
# Escolaridade × infertilidade

ctable(infert$education, infert$infertilidade,
       prop = "r", chisq = TRUE, useNA = "no")

# Como há células com baixa frequência, usar Fisher
fisher.test(table(infert$education, infert$infertilidade))

# Abortos espontâneos × infertilidade
ctable(infert$abort_spont, infert$infertilidade,
       prop = "r", chisq = TRUE, useNA = "no")

# -------------------------------------------------------------------
# 5) COMPARAÇÃO DE IDADE ENTRE CASOS E CONTROLES
# -------------------------------------------------------------------

boxplot(infert$age ~ infert$infertilidade,
        xlab = "",
        ylab = "Idade")

# Teste de normalidade por grupo (Yes vs No)
infert %>% 
  group_by(infertilidade) %>% 
  summarise(pvalor = shapiro.test(age)$p.value)

# Um dos grupos tem normalidade. Como exemplo, vamos usar teste paramétrico

# Teste de homogeneidade de variâncias
var.test(infert$age ~ infert$infertilidade)

# Teste t para média da idade entre casos e controles
t.test(infert$age ~ infert$infertilidade, var.equal = TRUE)

# -------------------------------------------------------------------
# 6) REGRESSÃO LOGÍSTICA (MEDIDA DE ASSOCIAÇÃO: ODDS RATIO)
# -------------------------------------------------------------------

summary(glm(infertilidade ~ age, 
            data = infert, 
            family = "binomial"))

summary(glm(infertilidade ~ education, 
            data = infert, 
            family = "binomial"))

summary(glm(infertilidade ~ abort_spont, 
            data = infert, 
            family = "binomial"))

###############################################################################
#                ANÁLISE DE DADOS DE ESTUDO DE COORTE                         #
#          Associação, Comparações por Grupos e Risco Relatico (RR)           #
###############################################################################

# -------------------------------------------------------------------
# 1) CARREGAR O BANCO E EXPLORAR ESTRUTURA
# -------------------------------------------------------------------

data(cancer)    # Carrega o banco 'cancer' (pacote survival)
head(cancer)    # Visualiza as primeiras linhas

?cancer         # Abre a documentação do banco (descrição das variáveis)

# -------------------------------------------------------------------
# 2) VERIFICAR A ESCALA DE UMA DAS VARIÁVEIS CLÍNICAS (PH.ECOG)
# -------------------------------------------------------------------

table(cancer$ph.ecog)   # Distribuição da escala de performance (status funcional)

# -------------------------------------------------------------------
# 3) CRIAÇÃO DE VARIÁVEIS DERIVADAS E RECODIFICAÇÕES
# -------------------------------------------------------------------
# status2   → fator com rótulos "Censura" e "Óbito"
# sex2      → fator com rótulos "Masculino" e "Feminino"
# ph.ecog2  → agrupa a categoria 3 em 2 (reduzindo níveis) e rotula categorias

cancer %>% 
  mutate(
    status2 = factor(status, labels = c("Censura","Óbito")),
    sex2    = factor(sex, labels = c("Masculino","Feminino")),
    ph.ecog2 = ifelse(ph.ecog == 3, 2, ph.ecog),
    ph.ecog2 = factor(
      ph.ecog2,
      labels = c("Assintomático",
                 "Sintomático Ambulatorial",
                 "No leito")
    )
  ) -> cancer

head(cancer)   # Conferir se as recodificações ficaram corretas

# -------------------------------------------------------------------
# 4) TABELAS DE CONTINGÊNCIA: FATORES ASSOCIADOS AO ÓBITO
# -------------------------------------------------------------------
# Associação entre sexo e status (óbito/censura)

ctable(cancer$sex2, cancer$status2,
       prop = "r", chisq = TRUE, useNA = "no")

# Associação entre condição funcional (ph.ecog2) e status (óbito/censura)

ctable(cancer$ph.ecog2, cancer$status2,
       prop = "r", chisq = TRUE, useNA = "no")

# -------------------------------------------------------------------
# 5) COMPARAÇÃO DA IDADE ENTRE ÓBITO E CENSURA
# -------------------------------------------------------------------

boxplot(cancer$age ~ cancer$status2,
        xlab = "",
        ylab = "Idade")

# Teste de normalidade da idade em cada grupo de status
cancer %>% 
  group_by(status2) %>% 
  summarise(p_valor = shapiro.test(age)$p.value)

# Teste de homogeneidade de variâncias entre os grupos
var.test(cancer$age ~ cancer$status2)

# Teste t para comparar idade média entre censura e óbito
t.test(cancer$age ~ cancer$status2, var.equal = TRUE)

# -------------------------------------------------------------------
# 6) MODELOS DE POISSON COM VARIÂNCIA ROBUSTA (RQLM)
# -------------------------------------------------------------------
# Aqui usamos o status numérico como desfecho para estimar razões
# de ocorrência de óbito segundo diferentes exposições.

# Definir "Feminino" como categoria de referência em sex2
cancer$sex2 <- relevel(cancer$sex2, ref = "Feminino")

# Modelo 1: associação entre sexo e status (óbito/censura)
rqlm(status ~ sex2, data = cancer, eform = TRUE, family = "poisson")

# Modelo 2: associação entre condição funcional (ph.ecog2) e status
rqlm(status ~ ph.ecog2, data = cancer, eform = TRUE, family = "poisson")

# Modelo 3: associação entre idade (contínua) e status
rqlm(status ~ age, data = cancer, eform = TRUE, family = "poisson")

###############################################################################
#                  ANÁLISE DE DADOS DE ESTUDO DE COORTE                       #
#                         Análise de Sobrevivência                            #
###############################################################################

# -------------------------------------------------------------------
# 1) Criar o objeto de sobrevida (Surv)
# -------------------------------------------------------------------
# Surv(tempo, status) → estrutura básica da análise

surv_obj <- Surv(time = cancer$time, event = cancer$status)

surv_obj  # visualização

# -------------------------------------------------------------------
# 2) Curva Kaplan-Meier geral
# -------------------------------------------------------------------

km_geral <- survfit(Surv(time, status) ~ 1, data = cancer)

ggsurvplot(
  km_geral,
  conf.int = TRUE,
  xlab = "Dias de Seguimento",
  ylab = "Probabilidade de Sobrevivência",
  ggtheme = theme_minimal(),
  surv.median.line = "hv"   # linha da mediana de sobrevida
)

# -------------------------------------------------------------------
# 3) Kaplan-Meier comparando SEXO
# -------------------------------------------------------------------

km_sexo <- survfit(surv_obj ~ sex2, data = cancer)

ggsurvplot(
  km_sexo,
  data = cancer,
  pval = TRUE,                   # mostra p-valor da log-rank
  conf.int = TRUE,
  xlab = "Dias",
  ylab = "Probabilidade de Sobrevivência",
  legend.title = "Sexo",
  ggtheme = theme_minimal(),
  surv.median.line = "hv"
)

# -------------------------------------------------------------------
# 4) Kaplan-Meier por Performance Status (ECOG)
# -------------------------------------------------------------------

km_ecog <- survfit(surv_obj ~ ph.ecog2, data = cancer)

ggsurvplot(
  km_ecog,
  data = cancer,
  pval = TRUE,
  conf.int = FALSE,
  legend.title = "ECOG",
  xlab = "Dias",
  ylab = "Probabilidade de Sobrevivência",
  surv.median.line = "hv",
  ggtheme = theme_minimal()
)

# -------------------------------------------------------------------
# 6) Modelo de Cox – fatores associados ao risco de morte
# -------------------------------------------------------------------

cox1 <- coxph(Surv(time, status) ~ age, data = cancer)
summary(cox1)

cox2 <- coxph(Surv(time, status) ~ sex2, data = cancer)
summary(cox2)

cox3 <- coxph(Surv(time, status) ~ ph.ecog2, data = cancer)
summary(cox3)

# Modelo múltiplo
cox_multiplo <- coxph(Surv(time, status) ~ age + sex2 + ph.ecog2, data = cancer)
summary(cox_multiplo)

cox_multiplo2 <- coxph(Surv(time, status) ~ sex2 + ph.ecog2, data = cancer)
summary(cox_multiplo2)
