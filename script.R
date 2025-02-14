# Base de Dados - Interrupções de Energia ANEEL
# Marco Antonio Faganello - marcofaga@gmail.com - http://github.com/marcofaga
# Data de início: 01 de dezembro de 2023
# Codificação: Banco em ASCII e Script em UTF-8
# Descrição do Script: Script de montagem da base de dados dos candidatos

# AREA DO A4 na ABNT (com 5 cm de margens):
# Full page: 160x247mm em in 6.3x9.7 em px com 300 dpi 1890x2917
# Meia Página: 160X124mm / 6.3x4.9in / 1890x1470 (300 dpi)
# Áurea menor: 160x094mm / 6.3x3.7in / 1890x1110 (300 dpi)
# Áurea maior: 160X153mm / 6.3x6.0in / 1890X1800 (300 dpi)

# colorBrewer Divergente (4 cores): #e66101, #fdb863, #b2abd2, #5e3c99
# colorBrewer Sequencial (4 cores): #fef0d9, #fdcc8a, #fc8d59, #d7301f

# libraries ====================================================================

library(tidyverse)
library(beepr)
library(janitor)
library(sf)
library(stringi)
library(geobr)
library(lubridate)


# options=======================================================================
options(stringsAsFactors = F)
options(knitr.kable.NA = '-')
options(timeout=10000)

# functions=====================================================================
#apl <- list.files("../../../../R/functions/", full.names = T)
#lapply(apl, source)
#remove(apl)
stop()

# script =======================================================================
# Dados de interrupção de energia elétrica disponíveis em:
#https://dadosabertos.aneel.gov.br/dataset/interrupcoes-de-energia-eletrica-nas-redes-de-distribuicao
#ap <- read_csv2("raw/interrupcoes-energia-eletrica-2024.csv", locale = locale(encoding = "latin1")) # dados até 10 de novembro de 2024 apenas ATUALIZAR
#ap <- ap |> filter(!is.na(DatGeracaoConjuntoDados))
#saveRDS(ap, "raw/interrupcoes-energia-eletrica-2024.rds")
#ap <- read_csv2("raw/interrupcoes-energia-eletrica-2023.csv", locale = locale(encoding = "latin1"))
#saveRDS(ap, "raw/interrupcoes-energia-eletrica-2023.rds")
#ap <- read_csv2("raw/interrupcoes-energia-eletrica-2022.csv", locale = locale(encoding = "latin1"))
#saveRDS(ap, "raw/interrupcoes-energia-eletrica-2022.rds")
#ap <- read_csv2("raw/interrupcoes-energia-eletrica-2021.csv", locale = locale(encoding = "latin1"))
#saveRDS(ap, "raw/interrupcoes-energia-eletrica-2021.rds")
#ap <- read_csv2("raw/interrupcoes-energia-eletrica-2020.csv", locale = locale(encoding = "latin1"))
#saveRDS(ap, "raw/interrupcoes-energia-eletrica-2020.rds")

# Dados geograficos das distribuidoras em: https://dadosabertos-aneel.opendata.arcgis.com/search?tags=dist
camadas <- st_layers("raw/Enel_SP_390_2023-12-31_V11_20240503-1028.gdb.zip")
conjuntos <- read_sf("raw/Enel_SP_390_2023-12-31_V11_20240503-1028.gdb.zip", layer="SUB")
interr <- readRDS("raw/interrupcoes-energia-eletrica-2024.rds")
apid <- unique(conjuntos$COD_ID)
interr <- interr |> filter(NomAgenteRegulado == "ELETROPAULO METROPOLITANA ELETRICIDADE DE SAO PAULO S.A.")
apun <-
  interr |> 
  select(DscConjuntoUnidadeConsumidora) |>
  unique() |>
  mutate(sub = stri_trans_general(tolower(DscConjuntoUnidadeConsumidora), "Latin-ASCII"))

apun2 <-
  conjuntos |> 
  select(NOME, COD_ID) |>
  unique() |>
  mutate(sub = stri_trans_general(tolower(NOME), "Latin-ASCII")) |>
  st_drop_geometry()

apun3 <-
  apun |>
  left_join(apun2, by=c("sub"), keep = TRUE) |>
  arrange(sub.y, sub.x)

apun3$sub.y[is.na(apun3$sub.y)] <- c("alexandre gusmao", "bandeirantes reticul", "jardim da gloria", "jucelino kubitschek", "miguel reale reticul", "bandeirantes", "sao bernardo do camp", "tabao da serra", "vargem grande paulis")
apun3$COD_ID[is.na(apun3$NOME)] <- apun2$COD_ID[match(apun3$sub.y[is.na(apun3$NOME)], apun2$sub)]
apun3 <- apun3 |> select(sub.x, COD_ID)
interr$DscConjuntoUnidadeConsumidora <- tolower(stri_trans_general(interr$DscConjuntoUnidadeConsumidora, "Latin-ASCII"))

interr <- interr |> left_join(apun3, by = c("DscConjuntoUnidadeConsumidora" = "sub.x"))
interr <- interr |> filter(DscTipoInterrupcao == "Não Programada")
agreg <- interr |> group_by(COD_ID) |> summarise(n = n())
conjuntos$interr <- agreg$n[match(conjuntos$COD_ID, agreg$COD_ID)]

mapasp <- 
ggplot()+geom_sf(data=conjuntos, aes(fill=interr))

#classificando os tipos de interrupcao
ap <- interr$DscFatoGeradorInterrupcao
ap <- strsplit(ap, ";")
apclass1 <- unlist(map(ap, ~.[3]))
apclass2 <- unlist(map(ap, ~.[4]))
interr$class1 <- apclass1
interr$class2 <- apclass2

View(interr |> group_by(class2) |> summarise(n = n()) |> arrange(n))

## bd01_interr_2024 ============================================================

conjuntos <- read_sf("raw/Enel_SP_390_2023-12-31_V11_20240503-1028.gdb.zip", layer="CONJ")  # Carregar camada CONJ
interr <- readRDS("raw/interrupcoes-energia-eletrica-2024.rds")  # Ler dados de interrupções em RDS

apid <- unique(conjuntos$COD_ID)  # Extrair IDs únicos dos conjuntos
interr$NomAgenteRegulado <- trimws(interr$NomAgenteRegulado)  # Remover espaços em branco
interr$NomAgenteRegulado  <- stri_trans_general(toupper(interr$NomAgenteRegulado), "Latin-ASCII")  # Normalizar texto (maiúsculas e acentos)

# Definindo os conjuntos que ficam em São Paulo
mapasp <- read_municipality(3550308)  # Carregar mapa de São Paulo (código IBGE)
apw <- st_intersects(conjuntos, mapasp)  # Verificar intersecção geográfica
apw <- map(apw, ~ifelse(length(.) == 0, NA, .))  # Tratar resultados vazios
apw <- unlist(apw)  # Converter lista para vetor
apw <- which(!is.na(apw))  # Identificar índices com intersecção
conjuntos$sp <- NA  # Criar coluna de flag
conjuntos$sp[apw] <- TRUE  # Marcar conjuntos em SP
apo <- conjuntos |> filter(sp)  # Filtrar apenas conjuntos marcados

# Calculando áreas de intersecção (>50%)
interseccao <- st_intersection(conjuntos, mapasp)  # Calcular geometrias intersectadas
area_interseccao <- st_area(interseccao)  # Calcular área de intersecção
area2 <- st_area(apo)  # Calcular área original dos conjuntos
aporc <- area_interseccao/area2  # Calcular porcentagem de sobreposição
aporc <- as.numeric(aporc) > 0.5  # Criar flag para >50% de sobreposição
apo$sp <- aporc  # Atualizar coluna de flag
apo$sp[apo$NOME == "VARGINHA"] <- TRUE # Incluindo varginha
apo <- apo |> filter(sp)  # Filtrar conjuntos com >50% em SP

ggplot()+geom_sf(data=apo, fill="blue")

interr <- interr |> filter(IdeConjuntoUnidadeConsumidora %in% apo$COD_ID)  # Filtrar interrupções em SP
interr$inicio_ts <- ymd_hms(interr$DatInicioInterrupcao, tz = "UTC")  # Converter data/hora início
interr <- interr |> filter(!is.na(inicio_ts))  # Remover registros sem data válida
interr$inicio_ts <- as.numeric(interr$inicio_ts)  # Converter para timestamp numérico

interr$fim_ts <- ymd_hms(interr$DatFimInterrupcao, tz = "UTC")  # Converter data/hora fim
interr <- interr |> filter(!is.na(fim_ts))  # Remover registros sem data válida
interr$fim_ts <- as.numeric(interr$fim_ts)  # Converter para timestamp numérico

interr$total <- interr$fim_ts-interr$inicio_ts  # Calcular duração em segundos
interr$total_horas <- interr$total/3600  # Converter duração para horas
interr$mes <- month(ymd_hms(interr$DatInicioInterrupcao))  # Extrair mês da interrupção

apdsc <- interr$DscFatoGeradorInterrupcao  # Separar causas concatenadas
apdsc <- strsplit(apdsc, ";")  # Dividir string por ;
apdsc1 <- unlist(map(apdsc, ~.[1]))  # Extrair 1ª causa
apdsc2 <- unlist(map(apdsc, ~.[2]))  # Extrair 2ª causa
apdsc3 <- unlist(map(apdsc, ~.[3]))  # Extrair 3ª causa
apdsc4 <- unlist(map(apdsc, ~.[4]))  # Extrair 4ª causa

interr$causa_01 <- apdsc1  # Atribuir causa primária
interr$causa_02 <- apdsc2  # Atribuir causa secundária
interr$causa_03 <- apdsc3  # Atribuir causa terciária
interr$causa_04 <- apdsc4  # Atribuir causa quaternária

interr <- interr |> filter(causa_01 == "INTERNA")  # Filtrar causas internas
interr <- interr |> filter(causa_03 != "NAO CLASSIFICADA")  # Remover não classificados

interr <- interr |> filter(DscFatoGeradorInterrupcao != "INTERNA;PROGRAMADA;ALTERACAO;PARA MELHORIA")  # Excluir manutenções programadas

interr$mes_inicio_interr <- month(interr$DatInicioInterrupcao)  # Criar coluna de mês inicial
interr$data_inicio <- as_date(interr$DatInicioInterrupcao)
interr$data_fim <- as_date(interr$DatFimInterrupcao)

write_rds(interr, "bases/bd01_interr_2024.rds")  # Salvar base processada

## bd02_interr_out24 ===========================================================

interr <- read_rds("bases/bd01_interr_2024.rds")
interr <- interr |> filter(mes_inicio_interr == 10)
write_rds(interr, "bases/bd02_interr_out24.rds")

## A01 - Descritivo dos dados de apagão ========================================

### 01 - incidência de apagão por dia outubro 24 ===============================

base <- read_rds("bases/bd02_interr_out24.rds")

tabsum <- 
  base |>
  group_by(data_inicio) |>
  summarise(n = n())

ggplot(data=tabsum, aes(x=data_inicio, y=n))+
  geom_line() +
  geom_vline(xintercept = ymd("2024-10-11"), linetype = "dotdash", color="#af8dc3", size=0.6)+
  geom_vline(xintercept = ymd("2024-10-06"), linetype = "longdash", color = "#008837")+
  geom_vline(xintercept = ymd("2024-10-27"), linetype = "longdash", color = "#008837")+
  #geom_vline(xintercept = ymd("2024-10-24"), linetype = "dotdash", color="#af8dc3", size=0.6)+
  xlab("dia")+
  ylab("total de interrupções")+
  scale_y_continuous(limits = c(400, 2400), breaks=seq(400, 2300, 100))+
  geom_text(x = ymd("2024-10-11"), y = 2400, label = "11 de outubro")+
  #geom_text(x = ymd("2024-10-24"), y = 14500, label = "24/10")+
  geom_text(x = ymd("2024-10-06"), y = 2400, label = "1º turno")+
  geom_text(x = ymd("2024-10-27"), y = 2400, label = "2º turno")+
  theme_minimal()

ggsave(filename = "img/a01_01.png", width = 2931, height = 1566, units = "px")

### 02 - incidência de apagão por dia no ano de 2024 ===========================

base <- read_rds("bases/bd01_interr_2024.rds")

tabsum <- 
  base |>
  group_by(data_inicio) |>
  summarise(n = n()) |>
  filter(data_inicio <= ymd("2024-11-30"))

ggplot(data=tabsum, aes(x=data_inicio, y=n))+
  geom_line() +
  #geom_vline(xintercept = ymd("2024-10-11"), linetype = "dotdash", color="#af8dc3", size=0.6)+
  #geom_vline(xintercept = ymd("2024-10-06"), linetype = "longdash", color = "#008837")+
  #geom_vline(xintercept = ymd("2024-10-27"), linetype = "longdash", color = "#008837")+
  #geom_vline(xintercept = ymd("2024-10-24"), linetype = "dotdash", color="#af8dc3", size=0.6)+
  xlab("dia")+
  ylab("total de interrupções")+
  scale_y_continuous(limits = c(200, 2400), breaks=seq(200, 2400, 100))+
  geom_text(x = ymd("2024-10-11"), y = 2400, label = "11/10")+
  #geom_text(x = ymd("2024-10-06"), y = 14500, label = "1º turno")+
  #geom_text(x = ymd("2024-10-27"), y = 14500, label = "2º turno")+
  theme_minimal()

ggsave(filename = "img/a01_02.png", width = 2931, height = 1566, units = "px")

### 03 - incidência de apagão por hora no dia 11 de outubro de 2024 ============

base <- read_rds("bases/bd02_interr_out24.rds")
base <-
  base |>
  filter(data_inicio == ymd("2024-10-11"))

base$hora <- hour(base$DatInicioInterrupcao)

tabsum <- 
  base |>
  group_by(hora) |>
  summarise(n = n())

ggplot(data=tabsum, aes(x=hora, y=n))+
  geom_line() +
  #geom_vline(xintercept = ymd("2024-10-11"), linetype = "dotdash", color="#af8dc3", size=0.6)+
  #geom_vline(xintercept = ymd("2024-10-06"), linetype = "longdash", color = "#008837")+
  #geom_vline(xintercept = ymd("2024-10-27"), linetype = "longdash", color = "#008837")+
  #geom_vline(xintercept = ymd("2024-10-24"), linetype = "dotdash", color="#af8dc3", size=0.6)+
  xlab("11 de outubro 2024 - hora")+
  ylab("total de interrupções")+
  scale_y_continuous(limits = c(5, 1300), breaks=seq(0, 1200, 100))+
  scale_x_continuous(limits = c(0, 23), breaks=seq(0, 23, 1))+
  #geom_text(x = ymd("2024-10-11"), y = 2400, label = "11 de outubro")+
  #geom_text(x = ymd("2024-10-24"), y = 14500, label = "24/10")+
  #geom_text(x = ymd("2024-10-06"), y = 2400, label = "1º turno")+
  #geom_text(x = ymd("2024-10-27"), y = 2400, label = "2º turno")+
  theme_minimal()

ggsave(filename = "img/a01_03.png", width = 2931, height = 1566, units = "px")

### 04 - Número de uc atingidas ================================================

base <- read_rds("bases/bd01_interr_2024.rds")

tabsum <- 
  base |>
  group_by(data_inicio) |>
  summarise(uc = sum(NumUnidadeConsumidora)) |>
  filter(data_inicio <= ymd("2024-11-30")) |>
  mutate(uc = uc/1000)

ggplot(data=tabsum, aes(x=data_inicio, y=uc))+
  geom_line() +
  #geom_vline(xintercept = ymd("2024-10-11"), linetype = "dotdash", color="#af8dc3", size=0.6)+
  #geom_vline(xintercept = ymd("2024-10-06"), linetype = "longdash", color = "#008837")+
  #geom_vline(xintercept = ymd("2024-10-27"), linetype = "longdash", color = "#008837")+
  #geom_vline(xintercept = ymd("2024-10-24"), linetype = "dotdash", color="#af8dc3", size=0.6)+
  xlab("dia")+
  ylab("total de unidades consumidoras atingidas (por mil)")+
  scale_y_continuous(limits = c(0, 1700), breaks=seq(0, 1600, 100))+
  geom_text(x = ymd("2024-10-11"), y = 1650, label = "11/10")+
  #geom_text(x = ymd("2024-10-06"), y = 14500, label = "1º turno")+
  #geom_text(x = ymd("2024-10-27"), y = 14500, label = "2º turno")+
  theme_minimal()

ggsave(filename = "img/a01_04.png", width = 2931, height = 1566, units = "px")


## A02 - Mapa ==================================================================

# ATENCAO TEm POR PONTO... EXCELENTE

base <- read_rds("bases/bd01_interr_2024.rds")

conjuntos <- read_sf("raw/Enel_SP_390_2023-12-31_V11_20240503-1028.gdb.zip", layer="CONJ")

base2 <- base |> group_by(IdeConjuntoUnidadeConsumidora) |> summarise(n = n_distinct(NumOrdemInterrupcao))

basearr <- tibble(base = sort(unique(base$DscConjuntoUnidadeConsumidora)))
basearr$conj <- conjuntos$NOME[match(basearr$base, conjuntos$NOME)]
basearr$conj[is.na(basearr$conj)] <- c("ALEXANDRE GUSMAO", "BANDEIRANTES RETICUL", "JARDIM DA GLORIA", "JUCELINO KUBITSCHEK", "MIGUEL REALE RETICUL", "TABAO DA SERRA")
basearr <- basearr |> filter(conj %in%  c("ALEXANDRE GUSMAO", "BANDEIRANTES RETICUL", "JARDIM DA GLORIA", "JUCELINO KUBITSCHEK", "MIGUEL REALE RETICUL", "TABAO DA SERRA"))

conjuntos$NOME[conjuntos$NOME %in% basearr$conj] <- basearr$base[match(conjuntos$NOME[conjuntos$NOME %in% basearr$conj], basearr$conj)]
conjuntos <- conjuntos |> filter(NOME %in% base$DscConjuntoUnidadeConsumidora)

conj <- conjuntos |> left_join(base, by=c("NOME"="DscConjuntoUnidadeConsumidora"))

conj$p <- conj$n/sum(conj$n)*100

ggplot()+geom_sf(data=conj, aes(fill=p))+geom_sf_text(data=conj, aes(label=NOME), size=1)



## A01 - Análise Nunes 1t e 2t apagão 2024 =====================================

# VER AQUI O TS DO MOMENTO DA TEMPESTADE. 
# UDPATAE: 11 de novembro
# OS DADOS DO DIA 11 de outubro da ENEL NAO FORAM ATUALIZADOS AINDA DADOS ABERTOS
# ùltimo dado de interrupção é do dia 30-09-2024 ....
# Data da ultima atualiação no site: 10 de novembro de 2024, 06:30 (UTC-03:00)

apdir <- tempdir()
apsec24 <- unzip("G:\\Meu Drive\\basesRef\\secoes2024/votacao_secao_2024_SP.zip", exdir = apdir)
apsec24 <- read_csv2(apsec24[1], locale = locale(encoding = "ISO-8859-1"))
apsec24 <-
  apsec24 |>
  filter(NM_MUNICIPIO == "SÃO PAULO",
         DS_CARGO == "Prefeito",
         NR_TURNO == 1)
apsec24$CD_MUNICIPIO <- as.character(apsec24$CD_MUNICIPIO)
apsec24$CD_MUNICIPIO <- str_pad(apsec24$CD_MUNICIPIO, 5, "left", "0")
apsec24$z_sec <- paste(apsec24$NR_ZONA, apsec24$NR_SECAO, sep="_")
apsec24$tipo <- "nominal"
apsec24$tipo[apsec24$NR_VOTAVEL %in% c("95", "96")] <- "branco_nulo"
apsec24$id_lv <- paste(apsec24$NM_LOCAL_VOTACAO, apsec24$DS_LOCAL_VOTACAO_ENDERECO, sep="_")

# base com os locais de votação geolocalizados pelo TSE
# tem que juntar os NAS das bases que vem do banco de secoes vot.. Aliás, é mais importante lá que do eleitorado local...
# Refazer.. PAROU AQUI
apunz <- unzip("G://Meu Drive//basesRef//secoes2024//eleitorado_local_votacao_2024.zip", exdir=apdir)
aplocal <- read_csv2(apunz[1], locale=locale(encoding = "Latin1"))
aplocal <- aplocal |> filter(CD_MUNICIPIO == "71072")
aplocal <- clean_names(aplocal)
aplocal <-
  aplocal |>
  select(nm_local_votacao,
         ds_endereco,
         nm_bairro,
         nr_latitude,
         nr_longitude
  ) |>
  unique()
aplocal$id_lv <- paste(aplocal$nm_local_votacao, aplocal$ds_endereco, sep="_")
aplocal <- aplocal |> select(-c(nm_local_votacao, ds_endereco))

# arrumando aqui as latitudes e longitudes
apcenter <- 
  st_centroid(mapasp) |>
  st_coordinates() |>
  as.numeric()

apflor <- as.character(abs(floor(apcenter[1])))
apnchar <- nchar(apflor)
apneg <- apcenter[1] < 1
apfator <- ifelse(apnchar == 2, 8, 7)

aplocal$nr_longitude <- gsub("\\.", "", aplocal$nr_longitude)
aplocal$nr_longitude[aplocal$nr_longitude == "-1"] <- NA
aplocal$nr_longitude <- str_pad(aplocal$nr_longitude, 11, "right", "0")
aplocal$nr_longitude <- abs(as.numeric(aplocal$nr_longitude))/10^apfator

if(apneg) {
  
  aplocal$nr_longitude <- aplocal$nr_longitude*-1
  
}

apflor2 <- as.character(abs(floor(apcenter[2])))
apnchar2 <- nchar(apflor2)
apneg2 <- apcenter[2] < 1
apfator2 <- ifelse(apnchar2 == 2, 8, 9)

aplocal$nr_latitude <- gsub("\\.", "", aplocal$nr_latitude)
aplocal$nr_latitude[aplocal$nr_latitude == "-1"] <- NA
aplocal$nr_latitude <- str_pad(aplocal$nr_latitude, 11, "right", "0")
aplocal$nr_latitude <- abs(as.numeric(aplocal$nr_latitude))/10^apfator2

if(apneg2) {
  
  aplocal$nr_latitude <- aplocal$nr_latitude*-1
  
}

apcoordsna <- list(c(-23.693942584580103, -46.676653970347246),
                 c(-23.559143875502585, -46.6453469422516),
                 c(-23.528346937018394, -46.655488272877825),
                 c(-23.550355732314088, -46.6535551269517),
                 c(-23.63273874126896, -46.72670312961399),
                 c(-23.533597029598653, -46.74586404830128),
                 c(-23.569050149833764, -46.59312904242965),
                 c(-23.685940732004806, -46.673345171294024),
                 c(-23.561530819474026, -46.63517784299858),
                 c(-23.558337656014643, -46.637517676022924),
                 c(-23.54758929068275, -46.65112431619971),
                 c(-23.527802119651046, -46.59313276154439),
                 c(-23.530788097088223, -46.74723595265413),
                 c(-23.634683300115743, -46.734766010139815),
                 c(-23.693920389901407, -46.65548111626802),
                 c(-23.473483816561338, -46.73601470016941),
                 c(-23.52501302280346, -46.73695691508486),
                 c(-23.463480864977864, -46.620771214706686),
                 c(-23.53259281133494, -46.745888984675226),
                 c(-23.545337335164408, -46.462410159195805),
                 c(-23.548675227770996, -46.616296978848844),
                 c(-23.527479073566603, -46.59429032501811),
                 c(-23.532889559556217, -46.74561339702295),
                 c(-23.53341428038487, -46.74657466497347),
                 c(-23.530483347303875, -46.59366080054025),
                 c(-23.58913892610149, -46.59069067342951),
                 c(-23.57025181311587, -46.58520726230839))

aplat <- unlist(map(apcoordsna, ~.[1]))
aplon <- unlist(map(apcoordsna, ~.[2]))

aplocal$nr_latitude[is.na(aplocal$nr_latitude)] <- aplat
aplocal$nr_longitude[is.na(aplocal$nr_longitude)] <- aplon

apsec24$lat <- aplocal$nr_latitude[match(apsec24$id_lv, aplocal$id_lv)]
apsec24$lon <- aplocal$nr_longitude[match(apsec24$id_lv, aplocal$id_lv)]



apx <- 
  interr |> group_by(IdeConjuntoUnidadeConsumidora, DscConjuntoUnidadeConsumidora) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(p = n/sum(n)*100)


# PAREI AQUIIIIIII


# save =========================================================================
clsap()
dateXXX <- Sys.time()
save.image()
