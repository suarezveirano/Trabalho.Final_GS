# Trabalho.Final 


A primeira etapa foi organizar o projeto e criar as pastas que contêm as informações.
A seguinte estrutura foi seguida:
dados (banco de dados); script (códigos); figuras (plot); doc (documentação); R (funções).

## Atenção
Uma explicação sobre o banco de dados e as diferentes variáveis é apresentada no arquivo de metadados.

## Cargo librerias que utilizará durante el análisis

```{r}
library(tidyverse)
library(lubridate)
```

# 1) Leio todos os arquivos (.csv) juntos e deixo-os unificados em um único arquivo .rds. 

Uso a função "read_multiple.R" (ver pasta R)

```{r }
DF_tidy<-tbl_with_sources <-
  list.files( path = "data/datos_origin",
              pattern = "*.csv", 
              full.names = T) %>% 
  map_df(~read_multiple(.)) ## aplico PURRR
```

Todas as colunas estão em formato de caracter agora


# 2) Ajusto la base de datos

Visualizo y realizo un analisis descriptivo de mi dataframe.

```{r}
str(DF_tidy)
summary(DF_tidy)
```

# 3) Transformo de acuerdo a lo que considero deben ser mis variables finales (Date, Factor, Character, Num).

```{r}
DF <- DF_tidy %>% 
  mutate (fecha = dmy(fecha), 
          hora = parse_time(hora), 
          CondicionA = parse_factor(adultas),
          CondicionB = parse_factor(ad_no_teleog),
          CondicionC = parse_factor(inmaduras),
          grupo = parse_factor(grupo),
          peso = parse_number(peso),
          .keep = "unused") 

str(DF)
```

(Variables Finales = ide, fecha, hora, CondicionA, CondicionB, CondicionC, grupo, peso)

# 4) Guardo mi DF en un single R object en una carpeta específica 

```{r}
saveRDS(object = DF, file = "data/DF.rds")
```

# 5) Estudo das diferentes variables

```{r}
DF_trabalho <- readr::read_rds("DF.rds")
View(DF_trabalho)
```

## Motivação: ver variacion del fechas en los distintos meses (aplico lubridate) e saber a diferenca de horas entre dois datas registradas

```{r}
DF_trabalho %>% 
  mutate(
    año = year(fecha),
    mes = month(fecha),
    dia_semana = wday(fecha,label=T, abbr=T)
  )

as.double(difftime(lubridate::ymd("2020-10-15"),
                   lubridate::ymd("2020-09-09"),
                   units="days"))
```

## Motivação: ver variacion del peso en los distintos meses en cada grupo por separado

```{r}
levels(DF_trabalho$grupo)

DF_trabalho %>%
  mutate(mes = lubridate::month(fecha)) %>% 
  group_by(grupo, mes) %>% 
  summarise(peso_medio = mean(peso, na.rm = TRUE)) %>% 
  ggplot(aes(x=mes, y= peso_medio)) +
  geom_line() +
  facet_wrap(~grupo)
```

## Motivação: ver las colunnas que contienen NA y en que cantidad faltan datos

```{r}
DF_trabalho %>%
  summarise(
    across(
      .cols = everything(),
      ~sum(is.na(.x))
    )) %>%
  View()
```

## Motivação: Agrupar las variables Condicion A, B y C en una única valiable,ver la candidad de diagnosticos por grupo e identificar la cantidad que no presentan grupo

```{r}
DF_trabalho %>%
  tidyr::pivot_longer(
    cols = starts_with("Condicion"), 
    names_to ="Diagnostico",
    values_to ="Presencia_Ausencia") %>% 
  mutate(grupo=fct_explicit_na(grupo, "sin grupo")) %>% 
  group_by(grupo) %>% 
  summarise(n_Diagnostico = n()) %>%
  ggplot(aes(grupo, n_Diagnostico))+
  geom_col()
```

## Motivação: Revertir el orden de los niveles del grupo

```{r}
DF_trabalho %>% 
  forcats::fct_rev(grupo)
```

## Motivação: otorgar un significado de cada parte de los nuneros de identificacion y buscar determinado numero de trabajo

```{r}
stringr::str_length(DF_trabalho$ide[1])

DF_numero <- DF_trabalho %>%
  mutate(
    CodigoISOdeUruguay = stringr::str_sub(ide, start = 1, end = 3),
    Numerounico = stringr::str_sub(ide, start = 4, end = 12),
    Numerodetrabajo = stringr::str_sub(ide, start = 13, end = 16))

stringr::str_subset(DF_numero$Numerodetrabajo, pattern="7921")
stringr::str_which(DF_numero$Numerodetrabajo, pattern="7921")  
```

Fim da primeira etapa exploratória.

Desta forma aplicamos os conhecimentos adquiridos e utilizamos todos os pacotes estudados.

A continuidade do estudo passaria pela aplicação da análise estatística dos dados e a apresentação dos resultados em figuras e tabelas.
