#
# Este algoritmo agrega tabelas do Censo de 2010 com o propósito de criar uma 
# População Sintética brasileira por individuo.
#

library(sqldf)
etapas <- 10

set.seed(1)

# Converte um valor numérico da tabela do censo em tipo numérico compatível com o R
conv_nro <- function(valor){
  valor <- gsub("[ ]", "", valor)
  valor <- gsub("[-]", 0, valor)
  return (as.numeric(valor))
}

# A partir do nome de um estado a função retorna a respectiva sigla
conv_codeState <- function(nome_do_estado){
  cod_estado <- c('ac', 	'al', 	'ap', 	'am', 	'ba', 	'ce', 	'df', 	'es', 	'go', 	'ma', 	'mt', 	'ms', 	'mg', 	'pa', 	'pb', 	'pr', 	'pe', 	'pi', 	'rj', 	'rn', 	'rs', 	'ro', 	'rr', 	'sc', 	'sp', 	'se', 	'to') 
  descr_estado <- c('acre', 	'alagoas', 	'amapá', 	'amazonas', 	'bahia', 	'ceará', 	'distrito federal', 	'espírito santo', 	'goiás', 	'maranhão', 	'mato grosso', 	'mato grosso do sul', 	'minas gerais', 	'pará', 	'paraíba', 	'paraná', 	'pernambuco', 	'piauí', 	'rio de janeiro', 	'rio grande do norte', 	'rio grande do sul', 	'rondônia', 	'roraima', 	'santa catarina', 	'são paulo', 	'sergipe', 	'tocantins')
  tab_state <- data.frame(descr_estado, cod_estado)
  id <- which(tab_state$descr_estado == tolower(nome_do_estado))
  return (tab_state$cod_estado[id])
}

# Retorna o nome do estado a partir do nome do arquivo
conv_estado <- function(nome_do_arquivo){
  pt <- 0
  v01 <- ''
  for(i in 1:nchar(nome_do_arquivo)){
    letra <- substr(nome_do_arquivo,i,i)
    if(letra == '.'){
      pt <- pt + 1
    } else {
      if(pt == 1){
        v01 <- paste(v01,letra,sep = '')
      }
    }
  }
  v01 <- as.numeric(v01)
  nro_estado <- c(   2, 	14, 	6, 	3, 	16, 	10, 	27, 	18, 	26, 	8, 	25, 	24, 	17, 	5, 	12, 	21, 	13, 	9, 	19, 	11, 	23, 	1, 	4, 	22, 	20, 	15, 	7) 
  cod_estado <- c('ac', 	'al', 	'ap', 	'am', 	'ba', 	'ce', 	'df', 	'es', 	'go', 	'ma', 	'mt', 	'ms', 	'mg', 	'pa', 	'pb', 	'pr', 	'pe', 	'pi', 	'rj', 	'rn', 	'rs', 	'ro', 	'rr', 	'sc', 	'sp', 	'se', 	'to') 
  descr_estado <- c('Acre', 	'Alagoas', 	'Amapá', 	'Amazonas', 	'Bahia', 	'Ceará', 	'Distrito Federal', 	'Espírito Santo', 	'Goiás', 	'Maranhão', 	'Mato Grosso', 	'Mato Grosso do Sul', 	'Minas Gerais', 	'Pará', 	'Paraíba', 	'Paraná', 	'Pernambuco', 	'Piauí', 	'Rio de Janeiro', 	'Rio Grande do Norte', 	'Rio Grande do Sul', 	'Rondônia', 	'Roraima', 	'Santa Catarina', 	'São Paulo', 	'Sergipe', 	'Tocantins')
  id <- which(v01==nro_estado)
  return(cod_estado[id])
}

#
# Tabela de índices [localização, sexo, raça]
#

tab_indices <- data.frame(
  variavel  = c('domicílio', 'domicílio',      'sexo',     'sexo',   'raça',  'raça',    'raça',  'raça',     'raça',           'raça'),
  codigo    = c(          1,           2,           1,          2,        1,       2,         3,       4,          5,                6),
  descricao = c(   'urbana',     'rural', 'masculino', 'feminino', 'branca', 'negra', 'amarela', 'parda', 'indígena', 'sem declaração'),
  item      = rep('', 10)
)

#
# 01 - Agregar [Código do municipio, domicilio, sexo] - (Tabelas 4.*.1.1.csv)
#      Arquivos: Tabela 4.[*].1.1 - População residente, por situação do domicílio e sexo, segundo as mesorregiões, as microrregiões, os municípios e os distritos - [estado] - 2010
#       

estados <- vector()
cod_mun <- vector()
nom_mun <- vector()
pop_tot <- vector()
domi_ru <- vector()
domi_tt <- vector()
sexo_mf <- vector()
sexo_tt <- vector()

endereco <- 'C:\\CSV\\domicilio_sexo\\CSV'
arquivos <- dir(endereco)
tam <- length(arquivos)
ctrl <- 0
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)
  estado <- tolower(substr(arquivos[i],1,2))
  
  nro_lin <- nrow(tab_tmp)
  for(j in 1:nro_lin){
    if(nchar(tab_tmp[j,11])==7){
      for(l in 1:4){
        
        ctrl <- ctrl + 1
        estados[ctrl] <- estado
        cod_mun[ctrl] <- tab_tmp[j,11]
        nom_mun[ctrl] <- tolower(tab_tmp[j,1])
        pop_tot[ctrl] <- conv_nro(tab_tmp[j,2])
        
        if(l==1){
          domi_ru[ctrl] <- 1
          domi_tt[ctrl] <- conv_nro(tab_tmp[j,5])
          sexo_mf[ctrl] <- 1
          sexo_tt[ctrl] <- conv_nro(tab_tmp[j,6])
        }
        
        if(l==2){
          domi_ru[ctrl] <- 1
          domi_tt[ctrl] <- conv_nro(tab_tmp[j,5])
          sexo_mf[ctrl] <- 2
          sexo_tt[ctrl] <- conv_nro(tab_tmp[j,7])
        }
        
        if(l==3){
          domi_ru[ctrl] <- 2
          domi_tt[ctrl] <- conv_nro(tab_tmp[j,8])
          sexo_mf[ctrl] <- 1
          sexo_tt[ctrl] <- conv_nro(tab_tmp[j,9])
        }
        
        if(l==4){
          domi_ru[ctrl] <- 2
          domi_tt[ctrl] <- conv_nro(tab_tmp[j,8])
          sexo_mf[ctrl] <- 2
          sexo_tt[ctrl] <- conv_nro(tab_tmp[j,10])
        }
        
      }
    }
  }
}
cat('\nEtapas: 01/',etapas,' - ok\n')
tabela <- data.frame(estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, sexo_mf, sexo_tt)
rm(estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, sexo_mf, sexo_tt)
tabela <- unique.data.frame(tabela)

# Nome das cidades: Há no IBGE 04 cidades com nomes diferentes entre as tabelas (motivo: erros ou alterações reais de nomes);
id <- which(tabela$nom_mun=='embu')
if(!identical(id,integer(0))){ tabela$nom_mun[id] <- 'embu das artes' }
id <- which(tabela$nom_mun=='urbana santos')
if(!identical(id,integer(0))){ tabela$nom_mun[id] <- 'urbano santos'  }
id <- which(tabela$nom_mun=='manoel urbana')
if(!identical(id,integer(0))){ tabela$nom_mun[id] <- 'manoel urbano'  }
id <- which(tabela$nom_mun=='santarém' & tabela$estados=='pb')
if(!identical(id,integer(0))){ tabela$nom_mun[id] <- 'joca claudino'  }

tab01   <- unique.data.frame(tabela[,1:4])
ori_pop <- sum(tab01$pop_tot)
  tab01 <- unique.data.frame(tabela[,1:6])
ori_dom <- aggregate(tab01$domi_tt, by=list(tab01$domi_ru),FUN=sum)
ori_sex <- aggregate(tabela$sexo_tt, by=list(tabela$sexo_mf), FUN=sum)

cat('\n População brasileira: ', ori_pop, 
    '\n     população urbana: ', ori_dom$x[ which(ori_dom$Group.1==1) ],
    '\n      população rural: ', ori_dom$x[ which(ori_dom$Group.1==2) ],
    '\n  população masculina: ', ori_sex$x[ which(ori_sex$Group.1==1) ],
    '\n   população feminina: ', ori_sex$x[ which(ori_sex$Group.1==2) ], '\n\n')

rm(tab_tmp, tab01, ori_dom, ori_sex, ori_pop)

#
# Tabela de índices [localização, sexo, raça]+[municípios]
#

t01 <- unique.data.frame(tabela[,1:3])
variavel  <- rep('cod_mun', nrow(t01))
codigo    <- t01$cod_mun
descricao <- t01$nom_mun
item      <- t01$estados

tab_tmp <- data.frame(variavel, codigo, descricao, item)
tab_indices <- rbind.data.frame(tab_indices, tab_tmp)
rm(variavel, codigo, descricao, tab_tmp)
rm(ctrl, endereco, i, perc, tam, arquivos, t01, item, estado)

#
# 02 - Agrega [Código do municipio, domicilio, sexo]+[raca] - (Tabelas 4.*.csv)
#      Arquivos: Tabela 4.[*] - População residente, por situação do domicílio e cor ou raça, segundo os municípios - [estado] - 2010
#       

cod_mun <- vector()
pop_tot <- vector()
domi_ru <- vector()
domi_tt <- vector()
branca  <- vector()
negra   <- vector()
amarela <- vector()
parda   <- vector()
indigena<- vector()
semdecl <- vector()

endereco <- 'C:\\CSV\\domicilio_raca\\CSV'
arquivos <- dir(endereco)
tam <- length(arquivos)
ctrl <- 0
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)

  nro_lin <- nrow(tab_tmp)
  for(j in 1:nro_lin){
    if(nchar(tab_tmp[j,23]) == 7){
      for(ii in 1:2){
        
        ctrl <- ctrl + 1
        cod_mun[ctrl] <- tab_tmp[j,23]
        pop_tot[ctrl] <- conv_nro(tab_tmp[j,2])
        
        if(ii == 1){
          domi_ru[ctrl] <- 1
          domi_tt[ctrl] <- conv_nro(tab_tmp[j,9])
          branca[ctrl]  <- conv_nro(tab_tmp[j,10])
          negra[ctrl]   <- conv_nro(tab_tmp[j,11])
          amarela[ctrl] <- conv_nro(tab_tmp[j,12])
          parda[ctrl]   <- conv_nro(tab_tmp[j,13])
          indigena[ctrl]<- conv_nro(tab_tmp[j,14])
          semdecl[ctrl] <- conv_nro(tab_tmp[j,15])
        }
        
        if(ii == 2){
          domi_ru[ctrl] <- 2
          domi_tt[ctrl] <- conv_nro(tab_tmp[j,16])
          branca[ctrl]  <- conv_nro(tab_tmp[j,17])
          negra[ctrl]   <- conv_nro(tab_tmp[j,18])
          amarela[ctrl] <- conv_nro(tab_tmp[j,19])
          parda[ctrl]   <- conv_nro(tab_tmp[j,20])
          indigena[ctrl]<- conv_nro(tab_tmp[j,21])
          semdecl[ctrl] <- conv_nro(tab_tmp[j,22])
        }
      }
    }
  }
}    
tab_01 <- data.frame(cod_mun, pop_tot, domi_ru, domi_tt, branca,  negra, amarela, parda, indigena, semdecl)
rm(cod_mun, pop_tot, domi_ru, domi_tt, branca,  negra, amarela, parda, indigena, semdecl)

tab01 <- tab_01[,5:10]
ori_rac <- colSums(tab01)

cat('\n              Brancos: ', ori_rac[1], 
    '\n               Negros: ', ori_rac[2],
    '\n             Amarelos: ', ori_rac[3],
    '\n               Pardos: ', ori_rac[4],
    '\n            Indígenas: ', ori_rac[5],
    '\n       Sem declaração: ', ori_rac[6], '\n\n')

tab_01$branca   <-  ifelse(tab_01$domi_tt==0,0,tab_01$branca   / tab_01$domi_tt)
tab_01$negra    <-  ifelse(tab_01$domi_tt==0,0,tab_01$negra    / tab_01$domi_tt)
tab_01$amarela  <-  ifelse(tab_01$domi_tt==0,0,tab_01$amarela  / tab_01$domi_tt)
tab_01$parda    <-  ifelse(tab_01$domi_tt==0,0,tab_01$parda    / tab_01$domi_tt)
tab_01$indigena <-  ifelse(tab_01$domi_tt==0,0,tab_01$indigena / tab_01$domi_tt)
tab_01$semdecl  <-  ifelse(tab_01$domi_tt==0,0,tab_01$semdecl  / tab_01$domi_tt)

tab_02 <- fn$sqldf('select a.estados, a.cod_mun, a.nom_mun, a.pop_tot, 
                           a.domi_ru, a.domi_tt, a.sexo_mf, a.sexo_tt, 
                           b.branca,  b.negra,    b.amarela,  b.parda, 
                           b.indigena, b.semdecl
                      from tabela a
                           left join tab_01 b
                             on a.cod_mun = b.cod_mun and
                                a.domi_ru = b.domi_ru  ')

estados <- vector()
cod_mun <- vector()
nom_mun <- vector()
pop_tot <- vector()
domi_ru <- vector()
domi_tt <- vector()
sexo_mf <- vector()
sexo_tt <- vector()
raca_ds <- vector()
raca_tt <- vector()

ctrl <- 0
tam <- nrow(tab_02)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  for(ii in 1:6){
    
    ctrl <- ctrl + 1
    estados[ctrl] <- tab_02$estados[i]
    cod_mun[ctrl] <- tab_02$cod_mun[i]
    nom_mun[ctrl] <- tab_02$nom_mun[i]
    pop_tot[ctrl] <- tab_02$pop_tot[i]
    domi_ru[ctrl] <- tab_02$domi_ru[i]
    domi_tt[ctrl] <- tab_02$domi_tt[i]
    sexo_mf[ctrl] <- tab_02$sexo_mf[i]
    sexo_tt[ctrl] <- tab_02$sexo_tt[i]
    
    if(ii == 1){
      raca_ds[ctrl] <- 1
      raca_tt[ctrl] <- round(tab_02$sexo_tt[i]*tab_02$branca[i],0)
    }
    
    if(ii == 2){
      raca_ds[ctrl] <- 2
      raca_tt[ctrl] <- round(tab_02$sexo_tt[i]*tab_02$negra[i],0)
    }
    
    if(ii == 3){
      raca_ds[ctrl] <- 3
      raca_tt[ctrl] <- round(tab_02$sexo_tt[i]*tab_02$amarela[i],0)
    }
    
    if(ii == 4){
      raca_ds[ctrl] <- 4
      raca_tt[ctrl] <- round(tab_02$sexo_tt[i]*tab_02$parda[i],0)
    }
    
    if(ii == 5){
      raca_ds[ctrl] <- 5
      raca_tt[ctrl] <- round(tab_02$sexo_tt[i]*tab_02$indigena[i],0)
    }
    
    if(ii == 6){
      raca_ds[ctrl] <- 6
      raca_tt[ctrl] <- round(tab_02$sexo_tt[i]*tab_02$semdecl[i],0)
    }
  }
  
}
tabela <- data.frame(estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, sexo_mf, sexo_tt, raca_ds, raca_tt)
rm(estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, sexo_mf, sexo_tt, raca_ds, raca_tt)
rm(tab01, tab_01, tab_02, tab_tmp)

tab01 <- fn$sqldf('select estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, 
                          sexo_mf, sexo_tt, sum(raca_tt) raca, (sexo_tt-sum(raca_tt)) diferenca
                     from tabela
                 group by estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, 
                          sexo_mf, sexo_tt
                 having diferenca <> 0')

tam <- nrow(tab01)
for(i in 1:tam) {
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  ids <- which(tab01$estados[i] == tabela$estados & 
               tab01$cod_mun[i] == tabela$cod_mun &  
               tab01$nom_mun[i] == tabela$nom_mun &  
               tab01$pop_tot[i] == tabela$pop_tot & 
               tab01$domi_ru[i] == tabela$domi_ru &  
               tab01$domi_tt[i] == tabela$domi_tt &  
               tab01$sexo_mf[i] == tabela$sexo_mf & 
               tab01$sexo_tt[i] == tabela$sexo_tt &
               abs(tab01$diferenca[i]) <= tabela$raca_tt )
  
  if(!identical(ids,integer(0))){
    percentual <- ifelse( tabela$raca_tt[ids]==0,0, tabela$raca_tt[ids] / tab01$sexo_tt[i] )
    posicao <- sample(1:length(ids),1, prob = percentual)
    tabela$raca_tt[ids[posicao]] <- tabela$raca_tt[ids[posicao]] + tab01$diferenca[i]
  }
  
}

# Chegacagem
tab01 <- fn$sqldf('select estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, 
                          sexo_mf, sexo_tt, sum(raca_tt) raca, (sexo_tt-sum(raca_tt)) diferenca
                     from tabela
                 group by estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, 
                          sexo_mf, sexo_tt
                 having diferenca <> 0')

status <- ifelse(nrow(tab01)==0,'ok','Há diferenças na etapa 02.')
cat('\nEtapas: 02/',etapas,' - ', status, '\n')
rm(tab01)

#
# 03 - Agrega [Código do municipio, domicilio, sexo] + [idade] - (BR_TAB71.csv)
#      Arquivo: Tabela 7.1 - População residente em domicílios particulares permanentes, por grupos de idade, segundo as Unidades da Federação, os municípios e a situação do domicílio - 2010
#

estados <- vector()
nom_mun <- vector()
domi_ru <- vector()
domi_tt <- vector()
a00a04s	<- vector()
a05a14s	<- vector()
a15a17s	<- vector()
a18a19s	<- vector()
a20a39s	<- vector()
a40a59s <- vector()
a60plus <- vector()

ctrl <- 0
arquivos <- 'C:\\CSV\\domicilio_idade\\csv\\BR_TAB71.csv'
tab_tmp <- read.csv2(arquivos, header = FALSE)
tam <- nrow(tab_tmp)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  if(tab_tmp$V3[i] == 'Urbana' | tab_tmp$V3[i] == 'Rural'){
    
    ctrl <- ctrl + 1
    estados[ctrl] <- conv_codeState(tolower(tab_tmp$V1[i]))
    nom_mun[ctrl] <- tolower(tab_tmp$V2[i])
    domi_ru[ctrl] <- ifelse(tab_tmp$V3[i]=='Urbana',1,2)
    domi_tt[ctrl] <- conv_nro(tab_tmp$V4[i])  
    a00a04s[ctrl]	<- conv_nro(tab_tmp$V5[i])
    a05a14s[ctrl]	<- conv_nro(tab_tmp$V6[i])
    a15a17s[ctrl]	<- conv_nro(tab_tmp$V7[i])
    a18a19s[ctrl]	<- conv_nro(tab_tmp$V8[i])
    a20a39s[ctrl]	<- conv_nro(tab_tmp$V9[i])
    a40a59s[ctrl] <- conv_nro(tab_tmp$V10[i])
    a60plus[ctrl] <- conv_nro(tab_tmp$V11[i])
  }
}
tab_01 <- data.frame(estados, nom_mun, domi_ru, domi_tt, a00a04s, a05a14s, a15a17s, a18a19s, a20a39s, a40a59s, a60plus)

id <- which(tab_01$nom_mun=='embu')
if(!identical(id,integer(0))){ tab_01$nom_mun[id] <- 'embu das artes' }
id <- which(tab_01$nom_mun=='urbana santos')
if(!identical(id,integer(0))){ tab_01$nom_mun[id] <- 'urbano santos'  }
id <- which(tab_01$nom_mun=='manoel urbana')
if(!identical(id,integer(0))){ tab_01$nom_mun[id] <- 'manoel urbano'  }
id <- which(tab_01$nom_mun=='santarém' & tab_01$estados=='pb')
if(!identical(id,integer(0))){ tab_01$nom_mun[id] <- 'joca claudino'  }

# Incluir codigo do municipio 
tab_tmp <- unique.data.frame(tabela[,1:3])
tab_01 <- fn$sqldf('select a.estados, b.cod_mun, a.nom_mun, a.domi_ru, a.domi_tt, 
                           a.a00a04s, a.a05a14s, a.a15a17s, a.a18a19s, a.a20a39s, 
                           a.a40a59s, a.a60plus
                      from tab_01 a
                           left join tab_tmp b
                             on a.estados = b.estados and 
                                a.nom_mun = b.nom_mun   ')

tab_01$a00a04s <- ifelse( tab_01$a00a04s == 0, 0, tab_01$a00a04s / tab_01$domi_tt )
tab_01$a05a14s <- ifelse( tab_01$a05a14s == 0, 0, tab_01$a05a14s / tab_01$domi_tt )
tab_01$a15a17s <- ifelse( tab_01$a15a17s == 0, 0, tab_01$a15a17s / tab_01$domi_tt )
tab_01$a18a19s <- ifelse( tab_01$a18a19s == 0, 0, tab_01$a18a19s / tab_01$domi_tt )
tab_01$a20a39s <- ifelse( tab_01$a20a39s == 0, 0, tab_01$a20a39s / tab_01$domi_tt )
tab_01$a40a59s <- ifelse( tab_01$a40a59s == 0, 0, tab_01$a40a59s / tab_01$domi_tt )
tab_01$a60plus <- ifelse( tab_01$a60plus == 0, 0, tab_01$a60plus / tab_01$domi_tt )

ids <- which(tabela$raca_tt==0)
tabela <- tabela[-ids,]

tab_tmp <- fn$sqldf('select a.estados, a.cod_mun, a.nom_mun, a.pop_tot, 
                            a.domi_ru, a.domi_tt, a.sexo_mf, a.sexo_tt, 
                            a.raca_ds, a.raca_tt,
                            b.a00a04s, b.a05a14s, b.a15a17s, b.a18a19s, 
                            b.a20a39s, b.a40a59s, b.a60plus
                      from tabela a
                           left join tab_01 b
                             on a.estados = b.estados and
                                a.cod_mun = b.cod_mun and 
                                a.domi_ru = b.domi_ru   ')

tab_tmp$a00a04s <- round(tab_tmp$a00a04s * tab_tmp$raca_tt,0)
tab_tmp$a05a14s <- round(tab_tmp$a05a14s * tab_tmp$raca_tt,0)
tab_tmp$a15a17s <- round(tab_tmp$a15a17s * tab_tmp$raca_tt,0)
tab_tmp$a18a19s <- round(tab_tmp$a18a19s * tab_tmp$raca_tt,0)
tab_tmp$a20a39s <- round(tab_tmp$a20a39s * tab_tmp$raca_tt,0)
tab_tmp$a40a59s <- round(tab_tmp$a40a59s * tab_tmp$raca_tt,0)
tab_tmp$a60plus <- round(tab_tmp$a60plus * tab_tmp$raca_tt,0)

tam <- nrow(tab_tmp)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  total <- sum(tab_tmp[i,11:17])
  diferenca <- tab_tmp$raca_tt[i] - total
  
  if(diferenca != 0){
    if(diferenca > 0){
      
      if( total != 0 ){
        percentual <- tab_tmp[i, 11:17] / total
        posicao <- sample(0:6,1, prob = percentual)
      } else {
        posicao <- sample(0:6,1)
      }
      id <- 11 + posicao
      tab_tmp[i,id] <- tab_tmp[i,id] + diferenca
      
    } else {
      
      vetor <- which(tab_tmp[i,11:17] != 0)
      if(!identical(vetor,integer(0))){
        tot_perc <- sum( tab_tmp[i,11:17][vetor] )
        percentual <- tab_tmp[i,11:17][vetor] / tot_perc
        posicao <- sample(1:length(vetor),1, prob = percentual)
        id <- 11 + vetor[posicao] - 1
        tab_tmp[i,id] <- tab_tmp[i,id] + diferenca
      }
      
    }
  }
}

# Checagem
a <- tab_tmp$raca_tt
b <- rowSums(tab_tmp[,11:17]) 
c <- a - b

status <- ifelse(sum(c)!=0, 'Há erro na etapa 03', 'ok')
cat('\nEtapas: 03/',etapas,' - ', status, '\n')
rm(a,b,c)

tabela <- tab_tmp
rm(estados, nom_mun, domi_ru, domi_tt, a00a04s, a05a14s, a15a17s, a18a19s, a20a39s, a40a59s, a60plus)
rm(tab_tmp, tab_01, percentual, vetor, ori_rac, ids, total, tot_perc, posicao, perc, nro_lin, tam, i, ii, id, j, l, diferenca, arquivos, endereco)

# Resultados gerais
t01 <- unique.data.frame(tabela[,1:4])
v01 <- sum(t01$pop_tot)
t01 <- unique.data.frame(tabela[,1:6])
v02 <- sum(t01$domi_tt)
t01 <- unique.data.frame(tabela[,1:8])
v03 <- sum(t01$sexo_tt)
t01 <- unique.data.frame(tabela[,1:10])
v04 <- sum(t01$raca_tt)
v05 <- sum(tabela[,11:17])

status <- ifelse((v02/v01) + (v03/v01) + (v04/v01) + (v05/v01)==4,'ok', 'Há erro entre os totais.')
cat('\n Dados Gerais: ', status, '\n\n')
rm(t01, v01, v02, v03, v04, v05)

#
# Dados sobre Educação
#

#
# 04 - Agrega situação educacional (já estudou, estuda e nunca estudou) - (Tabela 2.*.5.1.csv)
#      Arquivos: Tabela 2.[*].5.1 - População residente, por frequência a escola ou creche e rede de ensino que frequentavam, segundo os municípios - [estado] - 2010
#

cod_mun   <- vector()
total_att <- vector()
never_att <- vector()
still_att <- vector()
has_att   <- vector()

ctrl <- 0
endereco <- 'C:\\CSV\\escolaridade_idade\\frequencias'
arquivos <- dir(endereco)
tam <- length(arquivos)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)
  
  for(ii in 1:nrow(tab_tmp)){
    if(nchar(tab_tmp[ii,8])==7){
      ctrl <- ctrl + 1
      cod_mun[ctrl]   <- tab_tmp[ii,8]
      total_att[ctrl] <- conv_nro(tab_tmp[ii,2])
      never_att[ctrl] <- conv_nro(tab_tmp[ii,7])
      has_att[ctrl]   <- conv_nro(tab_tmp[ii,6])
      still_att[ctrl] <- conv_nro(tab_tmp[ii,3])
    }
  }
}

tab_01 <- data.frame(cod_mun, total_att, never_att, has_att, still_att)
rm(cod_mun, total_att, never_att, has_att, still_att)

#ajuste: O total por município não corresponte à soma de 'alunos', 'ex' e 'nunca foi a escola'
tam <- nrow(tab_01)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  total <- sum(tab_01[i,3:5])
  diferenca <- tab_01$total_att[i] - total
  
  if(diferenca != 0){
    if(diferenca > 0){
      
      while(diferenca > 0){
        if( total != 0 ){
          percentual <- tab_01[i,3:5] / total
          posicao <- sample(3:5,1, prob = percentual)
        } else {
          posicao <- sample(3:5,1)
        }
        tab_01[i,posicao] <- tab_01[i,posicao] + 1
        diferenca <- diferenca - 1
      }
      
    } else {
      
      while(diferenca != 0){
        vetor <- which(tab_01[i,3:5] > 0 )
        if(!identical(vetor,integer(0))){
          tot_perc <- sum( tab_01[i,3:5][vetor] )
          percentual <- tab_01[i,3:5][vetor] / tot_perc
          posicao <- sample(1:length(vetor),1, prob = percentual)
          id <- 2 + vetor[posicao]
          tab_01[i,id] <- tab_01[i,id] - 1
          diferenca <- diferenca + 1
        } else {
          diferenca <- 0
        }
      }
      
    }
  }
}

#  Checagem
library(sqldf)
tab01 <- fn$sqldf('select total_att, (never_att + has_att + still_att) total,
                     total_att - (never_att + has_att + still_att) diferenca
                    from tab_01')

status <- ifelse(identical(which(tab01$diferenca != 0),integer(0)), 'ok', 'Há erro na etapa 04')
cat('\nEtapas: 04 /',etapas,' - ', status, '\n')

rm(percentual, tab_tmp, tab01, arquivos, endereco, vetor, total, tot_perc, posicao, i, ii)

#
# Tabela de índices [localização, sexo, raça, municípios] + [situação escolar]
#

variavel  <- c('situação', 'situação', 'situação')
codigo    <- c(1, 2, 3)
descricao <- c('frequentam', 'já frequentaram', 'nunca frequentaram')
item      <- rep('', 3)

tab_tmp <- data.frame(variavel, codigo, descricao, item)
tab_indices <- rbind.data.frame(tab_indices, tab_tmp)
rm(variavel, codigo, descricao, tab_tmp, item)

#
# 05 - Agrega os atuais estudantes - [idade] - (Tabela 2.*.5.2.csv)
#      Arquivos: Tabela 2.[*].5.2 - População residente, total e que frequentavam escola ou creche, por grupos de idade, segundo os municípios - [estados] - 2010
#

cod_mun <- vector()
tot_mun <- vector()
frq_esc <- vector()
pop_tot <- vector()
a00a03s <- vector()
a04ou5s <- vector() 
a00006s <- vector() 
a07a14s <- vector()	
a15a17s <- vector() 
a18ou19 <- vector() 
a20a24s <- vector()	
a25plus <- vector()

ctrl <- 0
endereco <- 'C:\\CSV\\escolaridade_idade\\idades'
arquivos <- dir(endereco)
tam <- length(arquivos)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)
  
  for(ii in 1:nrow(tab_tmp)){
    if(nchar(tab_tmp[ii,21])==7){
      
      codigo <- tab_tmp[ii,21]
      id <- which(codigo==tab_01$cod_mun)
      
      ctrl <- ctrl + 1
      cod_mun[ctrl] <- codigo
      frq_esc[ctrl] <- 99 #total
      tot_mun[ctrl] <- conv_nro(tab_tmp[ii,2])
      var01         <- conv_nro(tab_tmp[ii,2])
      pop_tot[ctrl] <- conv_nro(tab_01$total_att[id])
      a00a03s[ctrl] <- conv_nro(tab_tmp[ii,3])
      a04ou5s[ctrl] <- conv_nro(tab_tmp[ii,4]) 
      a00006s[ctrl] <- conv_nro(tab_tmp[ii,5]) 
      a07a14s[ctrl] <- conv_nro(tab_tmp[ii,6])	
      a15a17s[ctrl] <- conv_nro(tab_tmp[ii,7]) 
      a18ou19[ctrl] <- conv_nro(tab_tmp[ii,8]) 
      a20a24s[ctrl] <- conv_nro(tab_tmp[ii,9])	
      a25plus[ctrl] <- conv_nro(tab_tmp[ii,10])
      
      ctrl <- ctrl + 1
      cod_mun[ctrl] <- codigo
      frq_esc[ctrl] <- 1
      tot_mun[ctrl] <- var01
      pop_tot[ctrl] <- conv_nro(tab_01$still_att[id])
      a00a03s[ctrl] <- conv_nro(tab_tmp[ii,13])
      a04ou5s[ctrl] <- conv_nro(tab_tmp[ii,14]) 
      a00006s[ctrl] <- conv_nro(tab_tmp[ii,15]) 
      a07a14s[ctrl] <- conv_nro(tab_tmp[ii,16])	
      a15a17s[ctrl] <- conv_nro(tab_tmp[ii,17]) 
      a18ou19[ctrl] <- conv_nro(tab_tmp[ii,18]) 
      a20a24s[ctrl] <- conv_nro(tab_tmp[ii,19])	
      a25plus[ctrl] <- conv_nro(tab_tmp[ii,20])
      
      ctrl <- ctrl + 1
      cod_mun[ctrl] <- tab_tmp[ii,21]
      frq_esc[ctrl] <- 2
      tot_mun[ctrl] <- var01
      pop_tot[ctrl] <- conv_nro(tab_01$has_att[id])
      a00a03s[ctrl] <- 0
      a04ou5s[ctrl] <- 0 
      a00006s[ctrl] <- 0
      a07a14s[ctrl] <- 0
      a15a17s[ctrl] <- 0
      a18ou19[ctrl] <- 0 
      a20a24s[ctrl] <- 0	
      a25plus[ctrl] <- 0
      
      ctrl <- ctrl + 1
      cod_mun[ctrl] <- tab_tmp[ii,21]
      frq_esc[ctrl] <- 3
      tot_mun[ctrl] <- var01
      pop_tot[ctrl] <- conv_nro(tab_01$never_att[id])
      a00a03s[ctrl] <- 0
      a04ou5s[ctrl] <- 0 
      a00006s[ctrl] <- 0
      a07a14s[ctrl] <- 0
      a15a17s[ctrl] <- 0
      a18ou19[ctrl] <- 0 
      a20a24s[ctrl] <- 0	
      a25plus[ctrl] <- 0
      
    }
  }
}
tab_02 <- data.frame(cod_mun, tot_mun, frq_esc, pop_tot, a00a03s, a04ou5s, a00006s, a07a14s, a15a17s, a18ou19, a20a24s, a25plus)
rm(cod_mun, tot_mun, frq_esc, pop_tot, a00a03s, a04ou5s, a00006s, a07a14s, a15a17s, a18ou19, a20a24s, a25plus)

#Ajuste: Há diferenças entre os totais de alunos e as somas das idades 
tam <- nrow(tab_02)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  if(tab_02$frq_esc[i] == 1 | tab_02$frq_esc[i] == 99){
    
    total <- sum(tab_02[i,5:12])
    diferenca <- tab_02$pop_tot[i] - total
    
    if(diferenca != 0){
      
      if(diferenca > 0){
        
        while(diferenca > 0){
          if( total != 0 ){
            percentual <- tab_02[i,5:12] / total
            posicao <- sample(5:12,1, prob = percentual)
          } else {
            posicao <- sample(5:12,1)
          }
          tab_02[i,posicao] <- tab_02[i,posicao] + 1
          diferenca <- diferenca - 1
        }
        
      } else {
        
        while(diferenca != 0){
          vetor <- which(tab_02[i,5:12] > 0 )
          if(!identical(vetor,integer(0))){
            tot_perc <- sum( tab_02[i,5:12][vetor] )
            percentual <- tab_02[i,5:12][vetor] / tot_perc
            posicao <- sample(1:length(vetor),1, prob = percentual)
            id <- 4 + vetor[posicao]
            tab_02[i,id] <- tab_02[i,id] - 1
            diferenca <- diferenca + 1
          } else {
            diferenca <- 0
          }
        }
        
      }
    }
  }
}

# Checagem
tab_tmp <- fn$sqldf('select (pop_tot - (a00a03s + a04ou5s + a00006s + a07a14s +
                            a15a17s + a18ou19 + a20a24s + a25plus)) diferenca
                       from tab_02
                      where frq_esc = 1 OR frq_esc = 99')
status <- ifelse(identical(which(tab_tmp != 0),integer(0)), 'ok', 'Há erro na etapa 05')
cat('\nEtapas: 05 /',etapas,' - ', status, '\n')
rm(tab_tmp, i, id, ii, perc, posicao, endereco, diferenca, tam, total, tot_perc, var01, vetor, codigo, ctrl)

#
# 06 - Agrega os atuais estudantes - [graduação] -  (Tabela 2.*.5.3.csv)
#      Arquivos: Tabela 2.[*.5.3 - Pessoas que frequentavam escola ou creche, por curso que frequentavam, segundo os municípios - [estados] - 2010
#

# Variáveis de graduação:
# crecres - Creche;	
# pre_esc - Pré-escolar;	
# cla_alf - Classe de alfabetização;	
# alf_j_a - Alfabetização de jovens e adultos;	
# e_funda - Fundamental;	
# e_medio - Médio;	
# e_super - Superior de graduação; 	
# posgrad - Especialização de nível superior, mestrado ou doutorado.

tam <- nrow(tab_02)
crecres <- rep(0,tam)
pre_esc <- rep(0,tam)
cla_alf <- rep(0,tam)
alf_j_a <- rep(0,tam)
e_funda <- rep(0,tam)
e_medio <- rep(0,tam)
e_super <- rep(0,tam)
posgrad <- rep(0,tam)

tab_02 <- cbind(tab_02, crecres, pre_esc, cla_alf, alf_j_a, e_funda, e_medio, e_super, posgrad)
rm(crecres, pre_esc, cla_alf, alf_j_a, e_funda, e_medio, e_super, posgrad)

cod_mun <- vector()
still_att <- vector()
st_creche <- vector()
st_pr_esc <- vector()
st_cl_alf <- vector()
st_ed_j_a <- vector()
st_fundam <- vector()
st_medios <- vector()
st_gradua <- vector()
st_posgra <- vector()

ctrl <- 0
endereco <- 'C:\\CSV\\escolaridade_idade\\niveis'
arquivos <- dir(endereco)
tam <- length(arquivos)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)
  
  for(ii in 1:nrow(tab_tmp)){
    if(nchar(tab_tmp[ii,11])==7){
      ctrl <- ctrl + 1
      cod_mun[ctrl]   <- tab_tmp[ii,11]
      id <- which(tab_02$cod_mun==cod_mun[ctrl] & tab_02$frq_esc==1)
      still_att[ctrl] <- conv_nro(tab_02$pop_tot[id])
      st_creche[ctrl] <- conv_nro(tab_tmp[ii,3])
      st_pr_esc[ctrl] <- conv_nro(tab_tmp[ii,4])
      st_cl_alf[ctrl] <- conv_nro(tab_tmp[ii,5])
      st_ed_j_a[ctrl] <- conv_nro(tab_tmp[ii,6])
      st_fundam[ctrl] <- conv_nro(tab_tmp[ii,7])
      st_medios[ctrl] <- conv_nro(tab_tmp[ii,8])
      st_gradua[ctrl] <- conv_nro(tab_tmp[ii,9])
      st_posgra[ctrl] <- conv_nro(tab_tmp[ii,10])
    }
  }
}

tab_01 <- data.frame(cod_mun, still_att, st_creche, st_pr_esc, st_cl_alf, st_ed_j_a, st_fundam, st_medios, st_gradua, st_posgra)
rm(cod_mun, still_att, st_creche, st_pr_esc, st_cl_alf, st_ed_j_a, st_fundam, st_medios, st_gradua, st_posgra)

# Ajuste: Há diferença entre os totais de estudantes e totais por graduação
tam <- nrow(tab_01)
for(i in 1:tam){

  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')

  total <- sum(tab_01[i,3:10])
  diferenca <- tab_01$still_att[i] - total

  if(diferenca != 0){

    if(diferenca > 0){

      while(diferenca > 0){
        if( total != 0 ){
          percentual <- tab_01[i,3:10] / total
          posicao <- sample(3:10,1, prob = percentual)
        } else {
          posicao <- sample(3:10,1)
        }
        tab_01[i,posicao] <- tab_01[i,posicao] + 1
        diferenca <- diferenca - 1
      }

    } else {

      while(diferenca != 0){
        vetor <- which(tab_01[i,3:10] > 0 )
        if(!identical(vetor,integer(0))){
          tot_perc <- sum( tab_01[i,3:10][vetor] )
          percentual <- tab_01[i,3:10][vetor] / tot_perc
          posicao <- sample(1:length(vetor),1, prob = percentual)
          id <- 2 + vetor[posicao]
          tab_01[i,id] <- tab_01[i,id] - 1
          diferenca <- diferenca + 1
        } else {
          diferenca <- 0
        }
      }

    }
  }

}

# Checagem
tab_tmp <- tab_01$still_att - rowSums(tab_01[,3:10])
status <- ifelse(identical(which(tab_tmp!=0), integer(0))==TRUE,'OK', 'Erro L948')
cat('\n',status,'\n\n')

# Atualizar as graduações de quem frequenta 
for(i in 1:nrow(tab_02)){
  
  if(tab_02$frq_esc[i]==1){
    
    id <- which(tab_01$cod_mun==tab_02$cod_mun[i])
    
    if(!identical(id,integer(0))){
      tab_02$crecres[i] <- tab_01$st_creche[id]
      tab_02$pre_esc[i] <- tab_01$st_pr_esc[id]
      tab_02$cla_alf[i] <- tab_01$st_cl_alf[id]
      tab_02$alf_j_a[i] <- tab_01$st_ed_j_a[id]
      tab_02$e_funda[i] <- tab_01$st_fundam[id]
      tab_02$e_medio[i] <- tab_01$st_medios[id]
      tab_02$e_super[i] <- tab_01$st_gradua[id]
      tab_02$posgrad[i] <- tab_01$st_posgra[id]
    }
  }
}

#
# 07 - Agrega os ex-estudantes - [graduação] -  (Tabela 2.*.5.4.csv)
#      Arquivos: Tabela 2.[*].5.4 - Pessoas de 10 anos ou mais de idade, por nível de instrução, segundo os municípios - [estado] - 2010
#

# Observação: A tabela 2.*.5.4 tem estudantes e ex-estudantes:
#             - Extrair % relativos a ex-estudantes
#             - Há uma regra dos dados que foi generalizada, estudantes com 10 anos ou mais;

cod_mun <- vector()
all_total <- vector()
all_f_inc <- vector()
all_m_inc <- vector()
all_s_inc <- vector()
all_s_com <- vector()
all_nao_d <- vector()

ctrl <- 0
endereco <- 'C:\\CSV\\escolaridade_idade\\all_levels'
arquivos <- dir(endereco)

tam <- length(arquivos)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)
  
  for(ii in 1:nrow(tab_tmp)){
    if(nchar(tab_tmp[ii,8])==7){
      ctrl <- ctrl + 1
      cod_mun[ctrl]   <- tab_tmp[ii,8]
      all_total[ctrl] <- conv_nro(tab_tmp[ii,2])
      all_f_inc[ctrl] <- conv_nro(tab_tmp[ii,3])
      all_m_inc[ctrl] <- conv_nro(tab_tmp[ii,4])
      all_s_inc[ctrl] <- conv_nro(tab_tmp[ii,5])
      all_s_com[ctrl] <- conv_nro(tab_tmp[ii,6])
      all_nao_d[ctrl] <- conv_nro(tab_tmp[ii,7])
    }
  }
}

tab_01 <- data.frame(cod_mun, all_total, all_f_inc, all_m_inc, all_s_inc, all_s_com, all_nao_d)
rm(cod_mun, all_total, all_f_inc, all_m_inc, all_s_inc, all_s_com, all_nao_d)

tab_01$all_f_inc <- ifelse( tab_01$all_f_inc == 0, 0, tab_01$all_f_inc / tab_01$all_total )
tab_01$all_m_inc <- ifelse( tab_01$all_m_inc == 0, 0, tab_01$all_m_inc / tab_01$all_total )
tab_01$all_s_inc <- ifelse( tab_01$all_s_inc == 0, 0, tab_01$all_s_inc / tab_01$all_total )
tab_01$all_s_com <- ifelse( tab_01$all_s_com == 0, 0, tab_01$all_s_com / tab_01$all_total )
tab_01$all_nao_d <- ifelse( tab_01$all_nao_d == 0, 0, tab_01$all_nao_d / tab_01$all_total )

seminst <- rep(0,nrow(tab_02))
naodete <- rep(0,nrow(tab_02))
tab_02 <- cbind(tab_02, seminst, naodete)

# Ajusta dados dos ex-estudantes
for(i in 1:nrow(tab_02)){
  if(tab_02$frq_esc[i]==2){
    id <- which(tab_01$cod_mun==tab_02$cod_mun[i])
    if(!identical(id,integer(0))){
      tab_02$seminst[i] <- round(tab_01$all_f_inc[id] * tab_02$pop_tot[i],0)
      tab_02$e_funda[i] <- round(tab_01$all_m_inc[id] * tab_02$pop_tot[i],0)
      tab_02$e_medio[i] <- round(tab_01$all_s_inc[id] * tab_02$pop_tot[i],0)
      tab_02$e_super[i] <- round(tab_01$all_s_com[id] * tab_02$pop_tot[i],0)
      tab_02$naodete[i] <- round(tab_01$all_nao_d[id] * tab_02$pop_tot[i],0)
    }
  }
}

# Ajustar dados dos que já e os que nunca estudaram 
codigos <- unique(tab_02$cod_mun)
tam <- length(codigos)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  id99 <- which(tab_02$cod_mun == codigos[i] & tab_02$frq_esc==99)
  
  if(!identical(id99,integer(0))){
    id01 <- which(tab_02$cod_mun == codigos[i] & tab_02$frq_esc==1)
    id02 <- which(tab_02$cod_mun == codigos[i] & tab_02$frq_esc==2)
    id03 <- which(tab_02$cod_mun == codigos[i] & tab_02$frq_esc==3)
    
    total01 <- tab_02$pop_tot[id99] - tab_02$pop_tot[id01]
    
    per02 <- tab_02$pop_tot[id02] / total01
    per03 <- tab_02$pop_tot[id03] / total01    
    for(ii in 5:12){
      saldo <- tab_02[id99,ii] - tab_02[id01,ii]
      if(saldo < 0){
        tab_02[id01,ii] <- tab_02[id99,ii]
        saldo <- 0
      }
      tab_02[id02,ii] <- round(saldo * per02,0)
      tab_02[id03,ii] <- round(saldo * per03,0)
    }
  }
}

# Ajuste
codigos <- unique(tab_02$cod_mun)
tam <- length(codigos)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  id99 <- which(tab_02$cod_mun == codigos[i] & tab_02$frq_esc==99)
  
  if(!identical(id99,integer(0))){
    for(ii in 1:3){
      id <- which(tab_02$cod_mun == codigos[i] & tab_02$frq_esc==ii)
      total <- sum(tab_02[id,5:12])
      saldo <- tab_02$pop_tot[id] - total
      
      if(saldo != 0){
        if(saldo > 0){
          probabilidade <- tab_02[id,5:12] / total
          id01 <- sample(5:12,1,prob=probabilidade)
          tab_02[id, id01] <- tab_02[id, id01] + saldo
        }else{
          ids <- which( tab_02[id,5:12] >= abs(saldo) )  
          if(!identical(ids,integer(0))){
            total <- sum(tab_02[id,5:12][ids])
            probabilidade <- tab_02[id,5:12][ids] / total
            id01 <- sample(ids,1,prob = probabilidade)
            tab_02[id,5:12][id01] <- tab_02[id,5:12][id01] + saldo
          }
        }
      }
    }
  }
}
rm(tab_tmp, percentual, probabilidade, naodete, per02, per03, perc, seminst)

#
# Tabela de índices [localização, sexo, raça, municípios, situação escolar] + 
#                   [escolaridade]
#

# Escolaridade
# 1 - 'creche';      
# 2 - 'pré-escolar'; 
# 3 - 'fundamental'; 
# 4 - 'médio';       
# 5 - 'classe de alfabetização';
# 6 - 'alfabetização de jovens e adultos'; 
# 7 - 'superior de graduação';  
# 8 - 'especialização de nível superior, mestrado ou doutorado';  

variavel  <- rep('escolaridade', 8)
codigo    <- 1:8
descricao <- c('creche', 'pré-escolar', 'fundamental', 'médio', 'classe de alfabetização', 'alfabetização de jovens e adultos', 'superior de graduação', 'especialização de nível superior, mestrado ou doutorado')
item      <- rep('', 8)

tab_tmp <- data.frame(variavel, codigo, descricao, item)
tab_indices <- rbind.data.frame(tab_indices, tab_tmp)
rm(variavel, codigo, descricao, tab_tmp, item)

# Relacionando a idade à educação dos alunos que frequentam a escola

cod_mun <- vector()
tot_mun <- vector()
frq_esc <- vector()
pop_tot <- vector()
degrees <- vector()
pop_deg <- vector()
a00a03s <- vector()
a04ou5s <- vector()
a00006s <- vector()
a07a14s <- vector()
a15a17s <- vector()
a18ou19 <- vector()
a20a24s <- vector()
a25plus <- vector()
t00a03s <- vector()
t04ou5s <- vector()
t00006s <- vector()
t07a14s <- vector()
t15a17s <- vector()
t18ou19 <- vector()
t20a24s <- vector()
t25plus <- vector()

ctrl <- 0
tam <- nrow(tab_02)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  if( tab_02$frq_esc[i] == 1 ){
    
    for(ii in 1:8){
      
      if(ii == 1) {var01 <- 1; var02 <- tab_02$crecres[i]}
      if(ii == 2) {var01 <- 2; var02 <- tab_02$pre_esc[i]}
      if(ii == 3) {var01 <- 3; var02 <- tab_02$e_funda[i]}
      if(ii == 4) {var01 <- 4; var02 <- tab_02$e_medio[i]}
      if(ii == 5) {var01 <- 5; var02 <- tab_02$cla_alf[i]}
      if(ii == 6) {var01 <- 6; var02 <- tab_02$alf_j_a[i]}
      if(ii == 7) {var01 <- 7; var02 <- tab_02$e_super[i]}
      if(ii == 8) {var01 <- 8; var02 <- tab_02$posgrad[i]}
      
      ctrl <- ctrl + 1
      cod_mun[ctrl] <- tab_02$cod_mun[i]
      tot_mun[ctrl] <- tab_02$tot_mun[i]
      frq_esc[ctrl] <- tab_02$frq_esc[i]
      pop_tot[ctrl] <- tab_02$pop_tot[i]
      degrees[ctrl] <- var01
      pop_deg[ctrl] <- var02
      a00a03s[ctrl] <- 0
      a04ou5s[ctrl] <- 0
      a00006s[ctrl] <- 0
      a07a14s[ctrl] <- 0
      a15a17s[ctrl] <- 0
      a18ou19[ctrl] <- 0
      a20a24s[ctrl] <- 0
      a25plus[ctrl] <- 0    
      t00a03s[ctrl] <- tab_02$a00a03s[i]
      t04ou5s[ctrl] <- tab_02$a04ou5s[i]
      t00006s[ctrl] <- tab_02$a00006s[i]
      t07a14s[ctrl] <- tab_02$a07a14s[i]
      t15a17s[ctrl] <- tab_02$a15a17s[i]
      t18ou19[ctrl] <- tab_02$a18ou19[i]
      t20a24s[ctrl] <- tab_02$a20a24s[i]
      t25plus[ctrl] <- tab_02$a25plus[i]
      
    }
  }
}
tab_01 <- data.frame(cod_mun, tot_mun, frq_esc, pop_tot, degrees, pop_deg, a00a03s, a04ou5s, a00006s, a07a14s, a15a17s, a18ou19, a20a24s, a25plus, t00a03s, t04ou5s, t00006s, t07a14s, t15a17s, t18ou19, t20a24s, t25plus)
rm(cod_mun, tot_mun, frq_esc, pop_tot, degrees, pop_deg, a00a03s, a04ou5s, a00006s, a07a14s, a15a17s, a18ou19, a20a24s, a25plus, t00a03s, t04ou5s, t00006s, t07a14s, t15a17s, t18ou19, t20a24s, t25plus)

cod_mun <- unique(tab_02$cod_mun)
tam <- length(cod_mun)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  saldo <- 0
  
  id <- which(cod_mun[i] == tab_01$cod_mun)
  
  # Creche
  idc <- which(tab_01$cod_mun == cod_mun[i] & tab_01$degrees == 1)
  
  if(tab_01$t00a03s[idc] >= tab_01$pop_deg[idc]){
    # If students >= degree then balance students and degree
    saldo <- tab_01$t00a03s[idc] - tab_01$pop_deg[idc]
    tab_01$a00a03s[idc] <- tab_01$pop_deg[idc]
    tab_01$t00a03s[id]  <- saldo
    saldo <- 0
  } else {
    # If students < degree then distribute by years
    saldo <- tab_01$pop_deg[idc] - tab_01$t00a03s[idc]
    tab_01$a00a03s[idc] <- tab_01$t00a03s[idc]
    tab_01$t00a03s[id]  <- 0
    tab_01$a04ou5s[idc] <- saldo
    tab_01$t04ou5s[id]  <- tab_01$t04ou5s[id] - saldo
    saldo <- 0
  }
  
  # Pré-escolar
  idc <- which(tab_01$cod_mun == cod_mun[i] & tab_01$degrees == 2)
  if(tab_01$t00a03s[idc] >= tab_01$pop_deg[idc]){
    # If students >= degree then balance students and degree
    saldo <- tab_01$t00a03s[idc] - tab_01$pop_deg[idc]
    tab_01$a00a03s[idc] <- tab_01$pop_deg[idc]
    tab_01$t00a03s[id]  <- saldo
    saldo <- 0
  } else {
    # If students < degree then distribute by years
    saldo <- tab_01$pop_deg[idc] - tab_01$t00a03s[idc]
    tab_01$a00a03s[idc] <- tab_01$t00a03s[idc]
    tab_01$t00a03s[id]  <- 0
    
    if(tab_01$t04ou5s[idc] >= saldo){
      tab_01$a04ou5s[idc] <- saldo
      tab_01$t04ou5s[id]  <- tab_01$t04ou5s[id] - saldo
    } else {
      saldo <- saldo - tab_01$t04ou5s[idc]
      tab_01$a04ou5s[idc] <- tab_01$t04ou5s[idc]
      tab_01$t04ou5s[id]  <- 0
      
      if(saldo > 0){
        if(tab_01$t00006s[idc] >= saldo) {
          tab_01$a00006s[idc] <- saldo
          tab_01$t00006s[id]  <- tab_01$t00006s[id] - saldo
          saldo <- 0
        } else {
          saldo <- saldo - tab_01$t00006s[idc]
          tab_01$a00006s[idc] <- tab_01$t00006s[idc]
          tab_01$t00006s[id]  <- 0
        }
      }
      
      if(saldo > 0){
        if(tab_01$t07a14s[idc] >= saldo){
          tab_01$a07a14s[idc] <- saldo
          tab_01$t07a14s[id]  <- tab_01$t07a14s[id] - saldo
          saldo <- 0
        } else {
          saldo <- saldo - tab_01$t07a14s[idc]
          tab_01$a07a14s[idc] <- tab_01$t07a14s[idc]
          tab_01$t07a14s[id]  <- 0
          tab_01$a15a17s[idc] <- saldo
          tab_01$t15a17s[id]  <- tab_01$t15a17s[id] - saldo
          saldo <- 0
        }
      }
      
    }
    
  }
  
  # Fundamental
  idc <- which(tab_01$cod_mun == cod_mun[i] & tab_01$degrees == 3)
  
  if(tab_01$t04ou5s[idc] >= tab_01$pop_deg[idc]){
    # If students >= degree then balance students and degree
    saldo <- tab_01$t04ou5s[idc] - tab_01$pop_deg[idc]
    tab_01$a04ou5s[idc] <- tab_01$pop_deg[idc]
    tab_01$t04ou5s[id]  <- saldo
    saldo <- 0
  } else {
    
    # If students < degree then distribute by years
    if(tab_01$t04ou5s[idc]==0){
      saldo <- tab_01$pop_deg[idc]
    }else{
      saldo <- tab_01$pop_deg[idc] - tab_01$t04ou5s[idc]
    }
    
    tab_01$a04ou5s[idc] <- tab_01$t04ou5s[idc]
    tab_01$t04ou5s[id]  <- 0
    
    if(saldo > 0){
      if(tab_01$t00006s[idc] >= saldo) {
        tab_01$a00006s[idc] <- saldo
        tab_01$t00006s[id]  <- tab_01$t00006s[id] - saldo
        saldo <- 0
      } else {
        saldo <- saldo - tab_01$t00006s[idc]
        tab_01$a00006s[idc] <- tab_01$t00006s[idc]
        tab_01$t00006s[id]  <- 0
      }
    }
    
    if(saldo > 0){
      if(tab_01$t07a14s[idc] >= saldo){
        tab_01$a07a14s[idc] <- saldo
        tab_01$t07a14s[id]  <- tab_01$t07a14s[id] - saldo
        saldo <- 0
      } else {
        saldo <- saldo - tab_01$t07a14s[idc]
        tab_01$a07a14s[idc] <- tab_01$t07a14s[idc]
        tab_01$t07a14s[id]  <- 0
      }
    }
    
    if(saldo > 0){
      if(tab_01$t15a17s[idc] >= saldo){
        tab_01$a15a17s[idc] <- saldo
        tab_01$t15a17s[id]  <- tab_01$t15a17s[id] - saldo
        saldo <- 0
      } else {
        saldo <- saldo - tab_01$t15a17s[idc]
        tab_01$a15a17s[idc] <- tab_01$t15a17s[idc]
        tab_01$t15a17s[id]  <- 0
      }
    }
    
    if(saldo > 0){
      
      if(tab_01$t18ou19[idc] >= saldo){
        tab_01$a18ou19[idc] <- saldo
        tab_01$t18ou19[id]  <- tab_01$t18ou19[id] - saldo
        saldo <- 0
      } else {
        saldo <- saldo - tab_01$t18ou19[idc]
        tab_01$a18ou19[idc] <- tab_01$t18ou19[idc]
        tab_01$t18ou19[id]  <- 0
        
        if(saldo > 0){
          if(tab_01$t20a24s[idc] >= saldo){
            tab_01$a20a24s[idc] <- saldo
            tab_01$t20a24s[id]  <- tab_01$t20a24s[id] - saldo
            saldo <- 0
          }else{
            saldo <- saldo - tab_01$t20a24s[idc]
            tab_01$a20a24s[idc] <- tab_01$t20a24s[idc]
            tab_01$t20a24s[id]  <- 0
            
            tab_01$a25plus[idc] <- saldo
            tab_01$t25plus[id] <- tab_01$t25plus[id] - saldo

            saldo <- 0
          }
        }
      }
    }
    
  }
  
  #Ensino Médio
  
  idc <- which(tab_01$cod_mun == cod_mun[i] & tab_01$degrees == 4)
  
  if(tab_01$t07a14s[idc] >= tab_01$pop_deg[idc]){
    # If students >= degree then balance students and degree
    saldo <- tab_01$t07a14s[idc] - tab_01$pop_deg[idc]
    tab_01$a07a14s[idc] <- tab_01$pop_deg[idc]
    tab_01$t07a14s[id]  <- saldo
  } else {
    
    # If students < degree then distribute by years
    if(tab_01$t07a14s[idc] == 0){
      saldo <- tab_01$pop_deg[idc]
    }else{
      saldo <- tab_01$pop_deg[idc] - tab_01$t07a14s[idc]
    }
    
    tab_01$a07a14s[idc] <- tab_01$t07a14s[idc]
    tab_01$t07a14s[id]  <- 0
    
    if(tab_01$t15a17s[idc] >= saldo) {
      tab_01$a15a17s[idc] <- saldo
      tab_01$t15a17s[id]  <- tab_01$t15a17s[id] - saldo
      saldo <- 0
    } else {
      saldo <- saldo - tab_01$t15a17s[idc]
      tab_01$a15a17s[idc] <- tab_01$t15a17s[idc]
      tab_01$t15a17s[id]  <- 0
    }
    
    if(saldo > 0){
      
      if(tab_01$t18ou19[idc] >= saldo){
        tab_01$a18ou19[idc] <- saldo
        tab_01$t18ou19[id]  <- tab_01$t18ou19[id] - saldo
        saldo <- 0
      } else {
        saldo <- saldo - tab_01$t18ou19[idc]
        tab_01$a18ou19[idc] <- tab_01$t18ou19[idc]
        tab_01$t18ou19[id]  <- 0
        
        if(saldo > 0){
          if(tab_01$t20a24s[idc] >= saldo){
            tab_01$a20a24s[idc] <- saldo
            tab_01$t20a24s[id]  <- tab_01$t20a24s[id] - saldo
            saldo <- 0
          }else{
            saldo <- saldo - tab_01$t20a24s[idc]
            tab_01$a20a24s[idc] <- tab_01$t20a24s[idc]
            tab_01$t20a24s[id]  <- 0
            
            tab_01$a25plus[idc] <- saldo
            tab_01$t25plus[id] <- tab_01$t25plus[id] - saldo

            saldo <- 0
          }
        }
      }
    }
    
  }
  
  # Classe de alfabetização
  
  idc <- which(tab_01$cod_mun == cod_mun[i] & tab_01$degrees == 5 )
  
  if(tab_01$t07a14s[idc] >= tab_01$pop_deg[idc]){
    # If students >= degree then balance students and degree
    saldo <- tab_01$t07a14s[idc] - tab_01$pop_deg[idc]
    tab_01$a07a14s[idc] <- tab_01$pop_deg[idc]
    tab_01$t07a14s[id]  <- saldo
  } else {
    
    # If students < degree then distribute by years
    if(tab_01$t07a14s[idc] == 0){
      saldo <- tab_01$pop_deg[idc]
    }else{
      saldo <- tab_01$pop_deg[idc] - tab_01$t07a14s[idc]
    }
    
    tab_01$a07a14s[idc] <- tab_01$t07a14s[idc]
    tab_01$t07a14s[id]  <- 0
    
    if(tab_01$t15a17s[idc] >= saldo) {
      tab_01$a15a17s[idc] <- saldo
      tab_01$t15a17s[id]  <- tab_01$t15a17s[id] - saldo
      saldo <- 0
    } else {
      saldo <- saldo - tab_01$t15a17s[idc]
      tab_01$a15a17s[idc] <- tab_01$t15a17s[idc]
      tab_01$t15a17s[id]  <- 0
    }
    
    if(saldo > 0){
      
      if(tab_01$t18ou19[idc] >= saldo){
        tab_01$a18ou19[idc] <- saldo
        tab_01$t18ou19[id]  <- tab_01$t18ou19[id] - saldo
        saldo <- 0
      } else {
        saldo <- saldo - tab_01$t18ou19[idc]
        tab_01$a18ou19[idc] <- tab_01$t18ou19[idc]
        tab_01$t18ou19[id]  <- 0
        
        if(saldo > 0){
          if(tab_01$t20a24s[idc] >= saldo){
            tab_01$a20a24s[idc] <- saldo
            tab_01$t20a24s[id]  <- tab_01$t20a24s[id] - saldo
            saldo <- 0
          }else{
            saldo <- saldo - tab_01$t20a24s[idc]
            tab_01$a20a24s[idc] <- tab_01$t20a24s[idc]
            tab_01$t20a24s[id]  <- 0
            
            tab_01$a25plus[idc] <- saldo
            tab_01$t25plus[id] <- tab_01$t25plus[id] - saldo
            saldo <- 0
          }
        }
      }
    }
    
  }
  
  # Alfabetização de jovens e adultos
  
  idc <- which(tab_01$cod_mun == cod_mun[i] & tab_01$degrees == 6)
  
  if(tab_01$t07a14s[idc] >= tab_01$pop_deg[idc]){
    # If students >= degree then balance students and degree
    saldo <- tab_01$t07a14s[idc] - tab_01$pop_deg[idc]
    tab_01$a07a14s[idc] <- tab_01$pop_deg[idc]
    tab_01$t07a14s[id]  <- saldo
  } else {
    
    # If students < degree then distribute by years
    if(tab_01$t07a14s[idc] == 0){
      saldo <- tab_01$pop_deg[idc]
    }else{
      saldo <- tab_01$pop_deg[idc] - tab_01$t07a14s[idc]
    }
    
    tab_01$a07a14s[idc] <- tab_01$t07a14s[idc]
    tab_01$t07a14s[id]  <- 0
    
    if(tab_01$t15a17s[idc] >= saldo) {
      tab_01$a15a17s[idc] <- saldo
      tab_01$t15a17s[id]  <- tab_01$t15a17s[id] - saldo
      saldo <- 0
    } else {
      saldo <- saldo - tab_01$t15a17s[idc]
      tab_01$a15a17s[idc] <- tab_01$t15a17s[idc]
      tab_01$t15a17s[id]  <- 0
    }
    
    if(saldo > 0){
      
      if(tab_01$t18ou19[idc] >= saldo){
        tab_01$a18ou19[idc] <- saldo
        tab_01$t18ou19[id]  <- tab_01$t18ou19[id] - saldo
        saldo <- 0
      } else {
        saldo <- saldo - tab_01$t18ou19[idc]
        tab_01$a18ou19[idc] <- tab_01$t18ou19[idc]
        tab_01$t18ou19[id]  <- 0
        
        if(saldo > 0){
          if(tab_01$t20a24s[idc] >= saldo){
            tab_01$a20a24s[idc] <- saldo
            tab_01$t20a24s[id]  <- tab_01$t20a24s[id] - saldo
            saldo <- 0
          }else{
            saldo <- saldo - tab_01$t20a24s[idc]
            tab_01$a20a24s[idc] <- tab_01$t20a24s[idc]
            tab_01$t20a24s[id]  <- 0
            
            tab_01$a25plus[idc] <- saldo
            tab_01$t25plus[id] <- tab_01$t25plus[id] - saldo
            saldo <- 0
          }
        }
      }
    }
  }
  
  # Superior de graduação 
  
  idc <- which(tab_01$cod_mun == cod_mun[i] & tab_01$degrees == 7)
  
  if(tab_01$t18ou19[idc] >= tab_01$pop_deg[idc]){
    # If students >= degree then balance students and degree
    saldo <- tab_01$t18ou19[idc] - tab_01$pop_deg[idc]
    tab_01$a18ou19[idc] <- tab_01$pop_deg[idc]
    tab_01$t18ou19[id]  <- saldo
  } else {
    
    # If students < degree then distribute by years
    if(tab_01$t18ou19[idc] == 0){
      saldo <- tab_01$pop_deg[idc]
    }else{
      saldo <- tab_01$pop_deg[idc] - tab_01$t18ou19[idc]
    }
    
    tab_01$a18ou19[idc] <- tab_01$t18ou19[idc]
    tab_01$t18ou19[id]  <- 0
    
    if(tab_01$t20a24s[idc] >= saldo) {
      tab_01$a20a24s[idc] <- saldo
      tab_01$t20a24s[id]  <- tab_01$t20a24s[id] - saldo
      saldo <- 0
    } else {
      saldo <- saldo - tab_01$t20a24s[idc]
      tab_01$a20a24s[idc] <- tab_01$t20a24s[idc]
      tab_01$t20a24s[id]  <- 0
    }
    
    if(saldo > 0){
      
      if(tab_01$t25plus[idc] >= saldo){
        tab_01$a25plus[idc] <- saldo
        tab_01$t25plus[id]  <- tab_01$t25plus[id] - saldo
        saldo <- 0
      } else {
        saldo <- saldo - tab_01$t25plus[idc]
        tab_01$a25plus[idc] <- tab_01$t25plus[idc]
        tab_01$t25plus[id]  <- 0
        
      }
    }
    
  }
  
  # Especialização de nível superior, mestrado ou doutorado
  
  idc <- which(tab_01$cod_mun == cod_mun[i] & tab_01$degrees == 8)
  if(tab_01$t20a24s[idc] >= tab_01$pop_deg[idc]){
    # If students >= degree then balance students and degree
    saldo <- tab_01$t20a24s[idc] - tab_01$pop_deg[idc]
    tab_01$a20a24s[idc] <- tab_01$pop_deg[idc]
    tab_01$t20a24s[id]  <- saldo
  } else {
    
    # If students < degree then distribute by years
    if(tab_01$t20a24s[idc] == 0){
      saldo <- tab_01$pop_deg[idc]
    }else{
      saldo <- tab_01$pop_deg[idc] - tab_01$t20a24s[idc]
    }
    
    tab_01$a20a24s[idc] <- tab_01$t20a24s[idc]
    tab_01$t20a24s[id]  <- 0
    
    if(tab_01$t25plus[idc] >= saldo) {
      tab_01$a25plus[idc] <- saldo
      tab_01$t25plus[id]  <- tab_01$t25plus[id] - saldo
      saldo <- 0
    } else {
      saldo <- saldo - tab_01$t25plus[idc]
      tab_01$a25plus[idc] <- tab_01$t25plus[idc]
      tab_01$t25plus[id]  <- 0
    }
  }
  
}

# Ajuste
colunas <- 15:22
vetor <- colSums(tab_01[,colunas])
nro <- which(vetor > 0)
idc <- colunas[nro][1]
idb <- as.numeric(6 + nro[1])

ids <- which(tab_01[,idc]>0)
cod_mun <- unique(tab_01$cod_mun[ids])

for(i in 1:length(cod_mun)){
  ids <- which(tab_01$cod_mun == cod_mun[i])
  for(ii in 1:length(ids)){
    id <- ids[ii]
    total <- sum(tab_01[id,7:14])
    if(tab_01$pop_deg[id] > total){
      diferenca <- tab_01$pop_deg[id] - total
      tab_01[ids,idc] <- tab_01[ids,idc] - diferenca
      tab_01[id, idb] <- tab_01[id, idb] + diferenca
    }
  }
}

# Incluindo pessoas que não foram a escola

# Escolaridade
# 9 - 'sem instrução escolar'

variavel  <- rep('escolaridade')
codigo    <- 9
descricao <- c('sem instrução escolar')
item <- ''

tab_tmp <- data.frame(variavel, codigo, descricao, item)
tab_indices <- rbind.data.frame(tab_indices, tab_tmp)
rm(variavel, codigo, descricao, item, tab_tmp)

tab_tmp <- tab_01[,1:14]
tab_01 <- tab_tmp

cod_mun <- vector() 
tot_mun <- vector()
frq_esc <- vector() 
pop_tot <- vector() 
degrees <- vector() 
pop_deg <- vector() 
a00a03s <- vector() 
a04ou5s <- vector() 
a00006s <- vector() 
a07a14s <- vector() 
a15a17s <- vector() 
a18ou19 <- vector() 
a20a24s <- vector() 
a25plus <- vector()

ctrl <- 0
tam <- nrow(tab_02)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  if(tab_02$frq_esc[i] == 3){
    ctrl <- ctrl + 1
    cod_mun[ctrl] <- tab_02$cod_mun[i]
    tot_mun[ctrl] <- tab_02$tot_mun[i]
    frq_esc[ctrl] <- tab_02$frq_esc[i]
    pop_tot[ctrl] <- tab_02$pop_tot[i]
    degrees[ctrl] <- 9
    pop_deg[ctrl] <- tab_02$pop_tot[i]
    a00a03s[ctrl] <- tab_02$a00a03s[i]
    a04ou5s[ctrl] <- tab_02$a04ou5s[i]
    a00006s[ctrl] <- tab_02$a00006s[i]
    a07a14s[ctrl] <- tab_02$a07a14s[i]
    a15a17s[ctrl] <- tab_02$a15a17s[i]
    a18ou19[ctrl] <- tab_02$a18ou19[i]
    a20a24s[ctrl] <- tab_02$a20a24s[i]
    a25plus[ctrl] <- tab_02$a25plus[i]
  }
}

tab_tmp <- data.frame(cod_mun, tot_mun, frq_esc, pop_tot, degrees, pop_deg, a00a03s, a04ou5s, a00006s, a07a14s, a15a17s, a18ou19, a20a24s, a25plus)
rm(cod_mun, tot_mun, frq_esc, pop_tot, degrees, pop_deg, a00a03s, a04ou5s, a00006s, a07a14s, a15a17s, a18ou19, a20a24s, a25plus)

tab_tmp <- rbind(tab_01, tab_tmp)
id <- order(tab_tmp$cod_mun, decreasing = TRUE)
tab_tmp <- tab_tmp[id,]
tab_01 <- tab_tmp

# Incluindo pessoas que já frequentaram a escola

# Escolaridade
# 10 -'fase pré-escolar', 
# 11 - 'não determinado', 
# 12 - 'fundamental incompleto', 
# 13 - 'médio incompleto', 
# 14 - 'superior incompleto', 
# 15 - 'superior completo'

variavel  <- c('escolaridade', 'escolaridade', 'escolaridade', 'escolaridade', 'escolaridade', 'escolaridade')
codigo    <- c(10, 11, 12, 13, 14, 15)
descricao <- c('fase pré-escolar', 'não determinado', 'fundamental incompleto', 'médio incompleto', 'superior incompleto', 'superior completo')
item      <- rep('', 6)

tab_tmp <- data.frame(variavel, codigo, descricao, item)
tab_indices <- rbind.data.frame(tab_indices, tab_tmp)
rm(variavel, codigo, descricao, item, tab_tmp)

# Ajuste de proporção em relação aos ex-aluno 
id <-  which(tab_02$frq_esc == 2)
tab_tmp <- tab_02[id,]

tam <- nrow(tab_tmp)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  total01 <- with(tab_tmp, sum(e_funda[i], e_medio[i], e_super[i], seminst[i], naodete[i]))
  
  tab_tmp$e_funda[i] <- ifelse( tab_tmp$e_funda[i] == 0 ,0 ,tab_tmp$e_funda[i] / total01 )
  tab_tmp$e_medio[i] <- ifelse( tab_tmp$e_medio[i] == 0 ,0 ,tab_tmp$e_medio[i] / total01 )
  tab_tmp$e_super[i] <- ifelse( tab_tmp$e_super[i] == 0 ,0 ,tab_tmp$e_super[i] / total01 )
  tab_tmp$seminst[i] <- ifelse( tab_tmp$seminst[i] == 0 ,0 ,tab_tmp$seminst[i] / total01 )
  tab_tmp$naodete[i] <- ifelse( tab_tmp$naodete[i] == 0 ,0 ,tab_tmp$naodete[i] / total01 )
}

cod_mun <- vector() 
tot_mun <- vector()
frq_esc <- vector() 
pop_tot <- vector() 
degrees <- vector() 
pop_deg <- vector() 
a00a03s <- vector() 
a04ou5s <- vector() 
a00006s <- vector() 
a07a14s <- vector() 
a15a17s <- vector() 
a18ou19 <- vector() 
a20a24s <- vector() 
a25plus <- vector()

tam <- nrow(tab_tmp)
ctrl <- 0
for(i in 1:tam){
  
  vetor <- vector()
  
  for(ii in 1:6){
    
    ctrl <- ctrl + 1
    vetor[ii] <- ctrl
    
    cod_mun[ctrl] <- tab_tmp$cod_mun[i] 
    tot_mun[ctrl] <- tab_tmp$tot_mun[i]
    frq_esc[ctrl] <- tab_tmp$frq_esc[i] 
    pop_tot[ctrl] <- tab_tmp$pop_tot[i] 
    
    if(ii == 1) {var01 <- 10; var02 <- 1}
    if(ii == 2) {var01 <- 11; var02 <- tab_tmp$naodete[i]}
    if(ii == 3) {var01 <- 12; var02 <- tab_tmp$seminst[i]} # retirei o termo 'Sem instrução' porque já há um item para sem instrução
    if(ii == 4) {var01 <- 13; var02 <- tab_tmp$e_funda[i]} # retirei o termo 'Fundamental completo e ' porque média incompleto presupõe fund. compl.
    if(ii == 5) {var01 <- 14; var02 <- tab_tmp$e_medio[i]}
    if(ii == 6) {var01 <- 15; var02 <- tab_tmp$e_super[i]}
    
    degrees[ctrl] <- var01 
    
    if(ii == 1) {
      
      a00a03s[ctrl] <- tab_tmp$a00a03s[i]
      a04ou5s[ctrl] <- tab_tmp$a04ou5s[i]
      a00006s[ctrl] <- tab_tmp$a00006s[i]
      a07a14s[ctrl] <- 0
      a15a17s[ctrl] <- 0
      a18ou19[ctrl] <- 0
      a20a24s[ctrl] <- 0
      a25plus[ctrl] <- 0
      
      pop_deg[ctrl] <- a00a03s[ctrl] + a04ou5s[ctrl] + a00006s[ctrl]
      
    } else  {
      
      if (ii == 6){
        
        a00a03s[ctrl] <- 0
        a04ou5s[ctrl] <- 0
        a00006s[ctrl] <- 0
        a07a14s[ctrl] <- 0
        a15a17s[ctrl] <- 0
        a18ou19[ctrl] <- 0
        a20a24s[ctrl] <- round(tab_tmp$a20a24s[i] * var02, 0)
        a25plus[ctrl] <- round(tab_tmp$a25plus[i] * var02, 0)
        
        pop_deg[ctrl] <- a00a03s[ctrl] + a04ou5s[ctrl] + a00006s[ctrl] + a07a14s[ctrl] + a15a17s[ctrl] + a18ou19[ctrl] + a20a24s[ctrl] + a25plus[ctrl]
        
        #ajuste de saldo gerado pelo ensino superior
        saldo <- 0
        a07a14s[vetor[2]]  <- a07a14s[vetor[2]] + tab_tmp$a07a14s[i] - sum(a07a14s[vetor])
        a15a17s[vetor[2]]  <- a15a17s[vetor[2]] + tab_tmp$a15a17s[i] - sum(a15a17s[vetor])
        a18ou19[vetor[2]]  <- a18ou19[vetor[2]] + tab_tmp$a18ou19[i] - sum(a18ou19[vetor])
        a20a24s[vetor[2]]  <- a20a24s[vetor[2]] + tab_tmp$a20a24s[i] - sum(a20a24s[vetor])
        a25plus[vetor[2]]  <- a25plus[vetor[2]] + tab_tmp$a25plus[i] - sum(a25plus[vetor])
        
        if(a07a14s[vetor[2]]<0) { a07a14s[vetor[2]]<-0 }
        if(a15a17s[vetor[2]]<0) { a15a17s[vetor[2]]<-0 }
        if(a18ou19[vetor[2]]<0) { a18ou19[vetor[2]]<-0 }
        if(a20a24s[vetor[2]]<0) { a20a24s[vetor[2]]<-0 }
        if(a25plus[vetor[2]]<0) { a25plus[vetor[2]]<-0 }
        
      } else {
        
        a00a03s[ctrl] <- 0
        a04ou5s[ctrl] <- 0
        a00006s[ctrl] <- 0
        a07a14s[ctrl] <- round(tab_tmp$a07a14s[i] * var02, 0)
        a15a17s[ctrl] <- round(tab_tmp$a15a17s[i] * var02, 0)
        a18ou19[ctrl] <- round(tab_tmp$a18ou19[i] * var02, 0)
        a20a24s[ctrl] <- round(tab_tmp$a20a24s[i] * var02, 0)
        a25plus[ctrl] <- round(tab_tmp$a25plus[i] * var02, 0)
        
        pop_deg[ctrl] <- a00a03s[ctrl] + a04ou5s[ctrl] + a00006s[ctrl] + a07a14s[ctrl] + a15a17s[ctrl] + a18ou19[ctrl] + a20a24s[ctrl] + a25plus[ctrl]
      }
    }
  }
}
pop_deg <- a00a03s + a04ou5s + a00006s + a07a14s + a15a17s + a18ou19 + a20a24s + a25plus

tab_tmp <- data.frame(cod_mun, tot_mun, frq_esc, pop_tot, degrees, pop_deg, a00a03s, a04ou5s, a00006s, a07a14s, a15a17s, a18ou19, a20a24s, a25plus)
rm(cod_mun, tot_mun, frq_esc, pop_tot, degrees, pop_deg, a00a03s, a04ou5s, a00006s, a07a14s, a15a17s, a18ou19, a20a24s, a25plus)

tab_tmp <- rbind.data.frame(tab_tmp,tab_01)
tab_01 <- tab_tmp

t02 <- fn$sqldf('select cod_mun,	tot_mun,	frq_esc,	pop_tot,	
                        sum(pop_deg) pop_deg, (pop_tot - sum(pop_deg)) diferenca
                   from tab_01 
                   group by cod_mun,	tot_mun,	frq_esc,	pop_tot	 ')

# Ajusta total entre situações escolares e graduação
tam <- nrow(t02)
for(i in 1:tam) {
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  ids <- which(   t02$cod_mun[i] == tab_01$cod_mun &
                  t02$frq_esc[i] == tab_01$frq_esc &
                abs(t02$diferenca[i]) <= tab_01$pop_deg )
  
  if(!identical(ids,integer(0))){
    
    if(t02$diferenca[i]>0){
      
      percentual <- ifelse( tab_01$pop_deg[ids] == 0, 0, tab_01$pop_deg[ids] / tab_01$pop_tot[ids] )
      posicao <- sample(1:length(percentual),1, prob = percentual)
      id <- ids[posicao]
      
    }else{
      
      itens <- which(tab_01$pop_deg[ids] >= abs(t02$diferenca[i]))
      
      if(!identical(itens,integer(0))){
        
        percentual <- ifelse( tab_01$pop_deg[ids][itens] == 0, 0, tab_01$pop_deg[ids][itens] / tab_01$pop_tot[ids][itens] )
        
        posicao <- sample(1:length(percentual),1, prob = percentual)
        id <- ids[itens[posicao]]
      }
      
    }
    tab_01$pop_deg[id] <- tab_01$pop_deg[id] + t02$diferenca[i]
  }
}

t01 <- fn$sqldf('select cod_mun,	tot_mun,	frq_esc,	pop_tot,	degrees,	pop_deg,
                        (pop_deg - (a00a03s	+ a04ou5s	+ a00006s	+ a07a14s	+ a15a17s	+ a18ou19	+ a20a24s	+ a25plus)) diferenca
                   from tab_01
               group by cod_mun,	tot_mun,	frq_esc,	pop_tot,	degrees,	pop_deg ')

# Ajusta a idade
tam <- nrow(t01)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  ids <- which( t01$cod_mun[i] == tab_01$cod_mun &
                t01$frq_esc[i] == tab_01$frq_esc &
                t01$degrees[i] == tab_01$degrees )
  
  if(!identical(ids,integer(0))){ 
    
    vetor <- which(tab_01[ids,7:14] > 0)

    if(!identical(vetor,integer(0))){
      
      total <- sum(tab_01[ids,7:14][vetor])
      idades <- tab_01[ids,7:14][vetor]
      
      percentual <- idades / total
      
      saldo <- abs(t01$diferenca[i])
      
      while(saldo > 0){
        posicao <- sample(1:length(percentual),1,prob = percentual)
        posicao <- vetor[posicao] + 6
        if(tab_01[ids,posicao] > 0){
          
          if(t01$diferenca[i] < 0){
            var01 <- (-1)
          } else {
            var01 <- (+1)
          }
          tab_01[ids,posicao] <- tab_01[ids,posicao] + var01
          saldo <- saldo - 1
        }
      }

    } else {
      if( sum(tab_01[ids,7:14]) == 0 ){
        posicao <- sample(7:14,1)
        tab_01[ids,posicao] <- t01$diferenca[i]
      }
    }
  }
  
}

#
# 08 - Agrega os ex-estudantes - [graduação] -  (Tabela 3.[*].2.2.csv)
#      Arquivos: Tabela 3.[*].2.2 - Pessoas de 15 anos ou mais de idade, por grupos de idade e sexo, segundo o  nível de instrução e a cor ou raça - [estado] - 2010
# [valores estaduais]

fn_raca <- function(raca){
  raca <- tolower(raca)
  valor <- 6
  if(raca == 'branca'  ) {valor <- 1}
  if(raca == 'preta'   ) {valor <- 2}
  if(raca == 'amarela' ) {valor <- 3}
  if(raca == 'parda'   ) {valor <- 4}
  if(raca == 'indígena') {valor <- 5}
  return (valor)
}

estados    <- vector()
sexo       <- vector()
raca       <- vector()
educ       <- vector()
idade      <- vector()
total      <- vector()
populacao  <- vector()
percentual <- vector()

ctrl <- 0
endereco <- 'C:\\CSV\\escolaridade_idade\\estados'
arquivos <- dir(endereco)

tam <- length(arquivos)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)
  
  estado <- conv_estado(arquivos[i])
  
  total01 <- conv_nro(tab_tmp[9,2])
  
  for(ii in 16:50){
    
    if(ii %in% c(16,23,30,37,44)){
      
      var01 <- ''
      if(ii == 16) {var01 <- 12; total01 <- conv_nro(tab_tmp[16,5]); total02 <- conv_nro(tab_tmp[16,8])}
      if(ii == 23) {var01 <- 13; total01 <- conv_nro(tab_tmp[23,5]); total02 <- conv_nro(tab_tmp[23,8])}
      if(ii == 30) {var01 <- 14; total01 <- conv_nro(tab_tmp[30,5]); total02 <- conv_nro(tab_tmp[30,8])}
      if(ii == 37) {var01 <- 15; total01 <- conv_nro(tab_tmp[37,5]); total02 <- conv_nro(tab_tmp[37,8])}
      if(ii == 44) {var01 <- 11; total01 <- conv_nro(tab_tmp[44,5]); total02 <- conv_nro(tab_tmp[44,8])}
      
    } else {
      
      ctrl <- ctrl + 1
      estados[ctrl] <- estado
      sexo[ctrl] <- 1
      educ[ctrl] <- var01
      raca[ctrl] <- fn_raca(tab_tmp[ii,1])
      idade[ctrl] <- '15 a 24 anos'
      total[ctrl] <- total01
      populacao[ctrl] <- conv_nro(tab_tmp[ii,6])
      percentual[ctrl] <- populacao[ctrl] / total[ctrl]
      
      ctrl <- ctrl + 1
      estados[ctrl] <- estado
      sexo[ctrl] <- 2
      educ[ctrl] <- var01
      raca[ctrl] <- fn_raca(tab_tmp[ii,1])
      idade[ctrl] <- '15 a 24 anos'
      total[ctrl] <- total01
      populacao[ctrl] <- conv_nro(tab_tmp[ii,7])
      percentual[ctrl] <- populacao[ctrl] / total[ctrl]
      
      ctrl <- ctrl + 1
      estados[ctrl] <- estado
      sexo[ctrl] <- 1
      educ[ctrl] <- var01
      raca[ctrl] <- fn_raca(tab_tmp[ii,1])
      idade[ctrl] <- '25 anos ou mais'
      total[ctrl] <- total02
      populacao[ctrl] <- conv_nro(tab_tmp[ii,9])
      percentual[ctrl] <- populacao[ctrl] / total[ctrl]
      
      ctrl <- ctrl + 1
      estados[ctrl] <- estado
      sexo[ctrl] <- 2
      educ[ctrl] <- var01
      raca[ctrl] <- fn_raca(tab_tmp[ii,1])
      idade[ctrl] <- '25 anos ou mais'
      total[ctrl] <- total02
      populacao[ctrl] <- conv_nro(tab_tmp[ii,10])
      percentual[ctrl] <- populacao[ctrl] / total[ctrl]
      
    }
  }
}

tab_02 <- data.frame(estados, sexo, raca, educ, idade, total, populacao, percentual)
rm(estados, sexo, raca, educ, idade, total, populacao, percentual)

#
# Joining state education on cities education
#

tab_tmp <- unique.data.frame(tabela[,1:2])

tab_01  <- fn$sqldf('select b.estados, a.cod_mun, a.tot_mun, a.frq_esc, a.pop_tot, a.degrees, 
                            a.pop_deg, a.a00a03s, a.a04ou5s, a.a00006s, a.a07a14s, 
                            a.a15a17s, a.a18ou19, a.a20a24s, a.a25plus
                       from tab_01 a
                  left join tab_tmp b
                         on a.cod_mun = b.cod_mun ')

estados <- vector()
cod_mun <- vector()
frq_esc <- vector()
pop_tot <- vector()
degrees <- vector()
pop_deg <- vector()
a00a03s <- vector()
a04ou5s <- vector()
a00006s <- vector()
a07a14s <- vector()
a15a17s <- vector()
a18ou19 <- vector()
a20a24s <- vector()
a25plus <- vector()
sexo    <- vector()
raca    <- vector()
pop_sr  <- vector()
percentual <- vector()

ctrl <- 0

tam <- nrow(tab_01)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  var01 <- ''
  if(tab_01$degrees[i] == 10) {var01 <- 12}
  if(tab_01$degrees[i] == 11) {var01 <- 11}
  if(tab_01$degrees[i] == 12) {var01 <- 12}
  if(tab_01$degrees[i] == 13) {var01 <- 13}
  if(tab_01$degrees[i] == 14) {var01 <- 14}
  if(tab_01$degrees[i] == 15) {var01 <- 15}
  if(tab_01$degrees[i] ==  1) {var01 <- 12}
  if(tab_01$degrees[i] ==  2) {var01 <- 12}
  if(tab_01$degrees[i] ==  3) {var01 <- 12}
  if(tab_01$degrees[i] ==  4) {var01 <- 13}
  if(tab_01$degrees[i] ==  7) {var01 <- 14}
  if(tab_01$degrees[i] ==  8) {var01 <- 15}
  if(tab_01$degrees[i] ==  5) {var01 <- 12}
  if(tab_01$degrees[i] ==  6) {var01 <- 13}
  if(tab_01$degrees[i] ==  9) {var01 <- 12}
  
  ids <- which(tab_02$estados == tab_01$estados[i] &
                 tab_02$idade == '15 a 24 anos' &
                 tab_02$educ == var01)
  
  tab_tmp <- tab_02[ids,]
  
  for(ii in 1:nrow(tab_tmp)){
    
    ctrl <- ctrl + 1
    
    estados[ctrl] <- tab_01$estados[i]
    cod_mun[ctrl] <- tab_01$cod_mun[i]
    frq_esc[ctrl] <- tab_01$frq_esc[i]
    pop_tot[ctrl] <- tab_01$pop_tot[i]
    degrees[ctrl] <- tab_01$degrees[i]
    pop_deg[ctrl] <- tab_01$pop_deg[i]
    
    sexo[ctrl] <- tab_tmp$sexo[ii]
    raca[ctrl] <- tab_tmp$raca[ii]
    
    a00a03s[ctrl] <- round(tab_01$a00a03s[i] * tab_tmp$percentual[ii],0)
    a04ou5s[ctrl] <- round(tab_01$a04ou5s[i] * tab_tmp$percentual[ii],0)
    a00006s[ctrl] <- round(tab_01$a00006s[i] * tab_tmp$percentual[ii],0)
    a07a14s[ctrl] <- round(tab_01$a07a14s[i] * tab_tmp$percentual[ii],0)
    a15a17s[ctrl] <- round(tab_01$a15a17s[i] * tab_tmp$percentual[ii],0)
    a18ou19[ctrl] <- round(tab_01$a18ou19[i] * tab_tmp$percentual[ii],0)
    a20a24s[ctrl] <- round(tab_01$a20a24s[i] * tab_tmp$percentual[ii],0)
    
    id <- which(tab_02$estados == tab_tmp$estados[ii] &
                  tab_02$sexo    == tab_tmp$sexo[ii] &
                  tab_02$raca    == tab_tmp$raca[ii] &
                  tab_02$educ    == tab_tmp$educ[ii] &
                  tab_02$idade   == '25 anos ou mais')
    
    if(tab_01$a25plus[i] == 0){
      pop_sr[ctrl] <- round(tab_01$pop_deg[i] * tab_tmp$percentual[ii],0)
    } else {
      pop_sr[ctrl] <- round( (tab_01$pop_deg[i]-tab_01$a25plus[i]) * tab_tmp$percentual[ii], 0)
      pop_sr[ctrl] <- pop_sr[ctrl] + round(tab_01$a25plus[i] * tab_02$percentual[id],0)
    }
    a25plus[ctrl] <- round(tab_01$a25plus[i] * tab_02$percentual[id],0)
  }
}

tab_tmp <- cbind.data.frame(estados, cod_mun, frq_esc, pop_tot, degrees, pop_deg, sexo, raca, pop_sr, a00a03s, a04ou5s, a00006s, a07a14s, a15a17s, a18ou19, a20a24s, a25plus)
tab_tmp$pop_sr <- a00a03s + a04ou5s + a00006s + a07a14s + a15a17s + a18ou19 + a20a24s + a25plus

ids <- order(tab_tmp$estados, tab_tmp$cod_mun, tab_tmp$frq_esc, tab_tmp$pop_tot, tab_tmp$degrees, tab_tmp$pop_deg)
tab_tmp <- tab_tmp[ids,]

rm(estados, cod_mun, frq_esc, pop_tot, degrees, pop_deg, sexo, raca, pop_sr, a00a03s, a04ou5s, a00006s, a07a14s, a15a17s, a18ou19, a20a24s, a25plus)

# Ajusta diferencas no pop_sr
t01 <- fn$sqldf(' select estados, cod_mun, frq_esc, pop_tot, degrees, 
                         pop_deg, sum(pop_sr) pop_sr, (pop_deg - sum(pop_sr)) diferenca
                   from tab_tmp 
                group by estados, cod_mun, frq_esc, pop_tot, degrees, pop_deg
                having (pop_deg - sum(pop_sr)) <> 0')

tam <- nrow(t01)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  if(t01$pop_sr[i] == 0){
    
    ids <- which( t01$cod_mun[i] == tab_tmp$cod_mun &
                  t01$frq_esc[i] == tab_tmp$frq_esc &
                  t01$degrees[i] == tab_tmp$degrees &
                  tab_tmp$pop_sr == 0 )

    if(!identical(ids,integer(0))){
      saldo <- abs(t01$diferenca[i])
      while(saldo > 0){
        posicao <- sample(1:length(ids),1)
        id <- ids[posicao]
        tab_tmp$pop_sr[id] <- tab_tmp$pop_sr[id] + 1
        saldo <- saldo - 1
      }
    }
    
  } else {
    ids <- which( t01$cod_mun[i] == tab_tmp$cod_mun &
                  t01$frq_esc[i] == tab_tmp$frq_esc &
                  t01$degrees[i] == tab_tmp$degrees &
                  tab_tmp$pop_sr > 0 )
    
    if(!identical(ids,integer(0))){ 
      
      saldo <- abs(t01$diferenca[i])
      
      if(t01$diferenca[i] < 0){
        var01 <- (-1)
      } else {
        var01 <- (+1)
      }
      
      while(saldo > 0){
        
        percentual <- tab_tmp$pop_sr[ids] / tab_tmp$pop_deg[ids]
        
        posicao <- sample(1:length(percentual),1, prob = percentual)
        id <- ids[posicao]
        
        if(tab_tmp$pop_sr[id] > 0){
          tab_tmp$pop_sr[id] <- tab_tmp$pop_sr[id] + var01
          saldo <- saldo - 1
        }
      }
    }
  }
  
}

# Ajusta as idades em relação o total 
tam <- nrow(tab_tmp)
for(i in 1:tam){
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  var01 <- (tab_tmp$a00a03s[i] + tab_tmp$a04ou5s[i] + tab_tmp$a00006s[i] + tab_tmp$a07a14s[i] + 
            tab_tmp$a15a17s[i] + tab_tmp$a18ou19[i] + tab_tmp$a20a24s[i] + tab_tmp$a25plus[i])
  
  if(tab_tmp$pop_sr[i] != var01){
    
    saldo <- abs(tab_tmp$pop_sr[i] - var01)
    diferenca <- tab_tmp$pop_sr[i] - var01
    
    if(var01 == 0) {
      
      var02 <- 0
      if( tab_tmp$degrees[i]%in%c( 9, 11, 12) ){
        var02 <- 10:17
      }
      if( tab_tmp$degrees[i] == 1 ){
        var02 <- 10
      }
      if( tab_tmp$degrees[i]%in%c(10, 2) ){
        var02 <- 10:12
      }
      if( tab_tmp$degrees[i] == 3 ){
        var02 <- 13
      }
      if( tab_tmp$degrees[i] == 4 ){
        var02 <- 14:15
      }
      if( tab_tmp$degrees[i]%in%c(6, 5) ){
        var02 <- 14:17
      }
      if( tab_tmp$degrees[i] == 13 ){
        var02 <- 14:17
      }
      if( tab_tmp$degrees[i]%in%c(7, 15, 8, 14) ){
        var02 <- 16:17
      }
      
      while(saldo > 0){
        posicao <- sample(1:length(var02),1)
        id <- var02[posicao]
        tab_tmp[i,id] <- tab_tmp[i,id] + 1
        saldo <- saldo - 1
      }
      
      
    } else {

      if(diferenca < 0){
        var02 <- (-1)
      } else {
        var02 <- (+1)
      }
      
      ids <- which(tab_tmp[i,10:17]>0)
      
      idades <- tab_tmp[i,10:17][ids]
      total <- sum(tab_tmp[i,10:17][ids])
      percentual <- idades / total
      
      while(saldo > 0){
        posicao <- sample(1:length(ids),1, prob = percentual)
        id <- ids[posicao] + 9
        if(tab_tmp[i,id]>0){
          tab_tmp[i,id] <- tab_tmp[i,id] + var02
          saldo <- saldo - 1
        }
      }
    }
  }
}

# Ajustar as idades - ok
rm(vetor, itens, colunas, percentual, idades, arquivos, t01, t02)

#
# Juntar os dados gerais aos dados educacionais do censo
#

tab01 <- fn$sqldf('select a.estados, a.cod_mun, a.nom_mun, a.pop_tot, a.domi_ru, 
                          a.domi_tt, a.sexo_mf, a.sexo_tt, a.raca_ds, a.raca_tt, 
                          b.frq_esc, b.pop_tot tot_frq, 
                          b.degrees, b.pop_deg, b.pop_sr,
                          a.a00a04s, a.a05a14s, a.a15a17s, a.a18a19s, a.a20a39s, 
                          a.a40a59s, a.a60plus,
                          b.a00a03s b00a03s, b.a04ou5s b04ou5s, b.a00006s b00006s, 
                          b.a07a14s b07a14s, b.a15a17s b15a17s, b.a18ou19 b18ou19, 
                          b.a20a24s b20a24s, b.a25plus b25plus
                     from tabela a
                left join tab_tmp b 
                       on 
                          a.cod_mun = b.cod_mun and
                          a.sexo_mf = b.sexo    and
                          a.raca_ds = b.raca
                          ')

frq_tot <- rep(0,nrow(tab01))
tab02 <- cbind.data.frame(tab01[,1:11],frq_tot,tab01[,12:30])

# ajusta proporções da frq_esc
tab02$frq_tot <- round( (ifelse(tab02$pop_tot==0, 0, tab02$tot_frq/tab02$pop_tot) )*tab02$raca_tt,0)

t01 <- unique.data.frame(tab02[,1:12])
t02 <- fn$sqldf('select estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, 
                        sexo_mf, sexo_tt, raca_ds, raca_tt, 
                        sum(frq_tot) frq_tot, (raca_tt - sum(frq_tot)) diferenca
                   from t01
               group by estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, 
                        sexo_mf, sexo_tt, raca_ds, raca_tt
                 having (raca_tt - sum(frq_tot)) <>0')

tam <- nrow(t02)
for(i in 1:tam){
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')

  ids <- which(t02$cod_mun[i] == tab02$cod_mun &
               t02$domi_ru[i] == tab02$domi_ru &
               t02$sexo_mf[i] == tab02$sexo_mf &
               t02$raca_ds[i] == tab02$raca_ds )
  # View(tab02[ids,])
  
  if(!identical(ids,integer(0))){
    
    t03 <- unique.data.frame(tab02[ids,1:12])
    percentual <- t03$frq_tot / t03$raca_tt
    
    if(sum(percentual)==0){
      posicao <- sample(1:length(percentual),1)
    }else{
      posicao <- sample(1:length(percentual),1, prob = percentual)
    }
    
    var01 <- t03$frq_esc[posicao]
    
    ids01 <- which(t02$cod_mun[i] == tab02$cod_mun &
                   t02$domi_ru[i] == tab02$domi_ru &
                   t02$sexo_mf[i] == tab02$sexo_mf &
                   t02$raca_ds[i] == tab02$raca_ds &
                            var01 == tab02$frq_esc)
    # View(tab02[ids01,])
    if(!identical(ids01,integer(0))){
      tab02$frq_tot[ids01] <- (tab02$frq_tot[ids01] + t02$diferenca[i])
    }
  }
}

deg_tot <- round( ifelse(tab02$tot_frq==0,0,tab02$pop_deg / tab02$tot_frq) * tab02$frq_tot, 0)
tab01 <- cbind.data.frame(tab02[,1:14], deg_tot, tab02[,15:31])

#ajusta degree
t01 <- tab01[,1:15]
t02 <- fn$sqldf('select estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, 
                        sexo_mf, sexo_tt, raca_ds, raca_tt, frq_esc, frq_tot, 
                        tot_frq, sum(deg_tot) deg_tot, 
                        (frq_tot-sum(deg_tot)) diferenca
                   from t01
                  group by estados, cod_mun, nom_mun, pop_tot, domi_ru, domi_tt, 
                        sexo_mf, sexo_tt, raca_ds, raca_tt, frq_esc, frq_tot, 
                        tot_frq
                having (frq_tot-sum(deg_tot))<>0 ')

tam <- nrow(t02)
for(i in 1:tam){
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  ids <- which( t02$cod_mun[i] == tab01$cod_mun &
                t02$domi_ru[i] == tab01$domi_ru &
                t02$sexo_mf[i] == tab01$sexo_mf &
                t02$raca_ds[i] == tab01$raca_ds &
                t02$frq_esc[i] == tab01$frq_esc &
         abs(t02$diferenca[i]) <= tab01$deg_tot )
  # View(tab01[ids,])
  
  if(!identical(ids,integer(0))){
    
    percentual <- tab01$deg_tot[ids] / tab01$frq_tot[ids]
    
    if(sum(percentual)==0){
      posicao <- sample(1:length(percentual),1)
    }else{
      posicao <- sample(1:length(percentual),1, prob = percentual)
    }
    
    var01 <- tab01$degrees[ids][posicao]
    
    ids01 <- which( t02$cod_mun[i] == tab01$cod_mun &
                    t02$domi_ru[i] == tab01$domi_ru &
                    t02$sexo_mf[i] == tab01$sexo_mf &
                    t02$raca_ds[i] == tab01$raca_ds &
                    t02$frq_esc[i] == tab01$frq_esc &
                             var01 == tab01$degrees )
    # View(tab01[ids01,])
    if(!identical(ids01,integer(0))){
      tab01$deg_tot[ids01] <- (tab01$deg_tot[ids01] + t02$diferenca[i])
    }
    
  } else {
    
    ids <- which( t02$cod_mun[i] == tab01$cod_mun &
                  t02$domi_ru[i] == tab01$domi_ru &
                  t02$sexo_mf[i] == tab01$sexo_mf &
                  t02$raca_ds[i] == tab01$raca_ds &
                  t02$frq_esc[i] == tab01$frq_esc  )
    
    if(!identical(ids,integer(0))){
      
      posicao <- which(tab01$pop_sr[ids]>0)
      
      if(!identical(posicao,integer(0))){
        total <- sum(tab01$pop_sr[ids][posicao])
        percentual <- tab01$pop_sr[ids][posicao] / total
      } else {
        percentual <- rep(0, length(ids))
      }

      if(sum(percentual)==0){
        posicao <- sample(1:length(percentual),1)
      }else{
        posicao <- sample(1:length(percentual),1, prob = percentual)
      }
      
      var01 <- tab01$degrees[ids][posicao]
      
      ids01 <- which( t02$cod_mun[i] == tab01$cod_mun &
                      t02$domi_ru[i] == tab01$domi_ru &
                      t02$sexo_mf[i] == tab01$sexo_mf &
                      t02$raca_ds[i] == tab01$raca_ds &
                      t02$frq_esc[i] == tab01$frq_esc &
                               var01 == tab01$degrees )
      # View(tab01[ids01,])
      if(!identical(ids01,integer(0))){
        tab01$deg_tot[ids01] <- (tab01$deg_tot[ids01] + t02$diferenca[i])
      }
      
    }
    
  }
}

tab01 <- tab01[,-c(13,16)]

# ajuste de idades 

tab01$b00a03s <- ifelse( tab01$b00a03s == 0, 0, tab01$b00a03s / tab01$pop_sr )
tab01$b04ou5s <- ifelse( tab01$b04ou5s == 0, 0, tab01$b04ou5s / tab01$pop_sr )
tab01$b00006s <- ifelse( tab01$b00006s == 0, 0, tab01$b00006s / tab01$pop_sr )
tab01$b07a14s <- ifelse( tab01$b07a14s == 0, 0, tab01$b07a14s / tab01$pop_sr )
tab01$b15a17s <- ifelse( tab01$b15a17s == 0, 0, tab01$b15a17s / tab01$pop_sr )
tab01$b18ou19 <- ifelse( tab01$b18ou19 == 0, 0, tab01$b18ou19 / tab01$pop_sr )
tab01$b20a24s <- ifelse( tab01$b20a24s == 0, 0, tab01$b20a24s / tab01$pop_sr )
tab01$b25plus <- ifelse( tab01$b25plus == 0, 0, tab01$b25plus / tab01$pop_sr )

tab01$b00a03s <- ifelse( tab01$b00a03s == 0, 0, round( tab01$deg_tot * tab01$b00a03s, 0) )
tab01$b04ou5s <- ifelse( tab01$b04ou5s == 0, 0, round( tab01$deg_tot * tab01$b04ou5s, 0) )
tab01$b00006s <- ifelse( tab01$b00006s == 0, 0, round( tab01$deg_tot * tab01$b00006s, 0) )
tab01$b07a14s <- ifelse( tab01$b07a14s == 0, 0, round( tab01$deg_tot * tab01$b07a14s, 0) )
tab01$b15a17s <- ifelse( tab01$b15a17s == 0, 0, round( tab01$deg_tot * tab01$b15a17s, 0) )
tab01$b18ou19 <- ifelse( tab01$b18ou19 == 0, 0, round( tab01$deg_tot * tab01$b18ou19, 0) )
tab01$b20a24s <- ifelse( tab01$b20a24s == 0, 0, round( tab01$deg_tot * tab01$b20a24s, 0) )
tab01$b25plus <- ifelse( tab01$b25plus == 0, 0, round( tab01$deg_tot * tab01$b25plus, 0) )

tab01$pop_sr <- rowSums(tab01[,23:30])
diferenca <- tab01$deg_tot - tab01$pop_sr
tab01 <- cbind.data.frame(tab01, diferenca)

tab01$pop_sr <- rowSums(tab01[,23:30])
tab01$diferenca <- tab01$deg_tot - tab01$pop_sr
id01 <- which(tab01$diferenca!=0)

tam <- length(id01)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  id <- id01[i]

  if(tab01$diferenca[id] < 0){
    
    ids <- which( tab01[id,23:30] >= abs(tab01$diferenca[id]) )
    
    if(!identical(ids,integer(0))){
      percentual <- tab01[id,23:30][ids] / tab01$deg_tot[id]
      posicao <- sample(1:length(percentual),1, prob = percentual)
      tab01[id,23:30][ids][posicao] <- tab01[id,23:30][ids][posicao] + tab01$diferenca[id]
    } else {
      
      if( tab01$pop_sr[id] > abs( tab01$diferenca[id] ) ){
        saldo <- abs( tab01$diferenca[id] )
        for(ii in 23:30){
          if( tab01[id,ii] > 0){
            saldo <- saldo - 1
            tab01[id,ii] <- tab01[id,ii] - 1
          }
          if(saldo ==0){
            break
          }
        }
      }
      
    }

  } else {
    percentual <- tab01[id,23:30] / tab01$deg_tot[id]
    if(sum(percentual)>0){
      posicao <- sample(1:length(percentual),1, prob = percentual)
      tab01[id,23:30][posicao] <- tab01[id,23:30][posicao] + tab01$diferenca[id]
    } else {
      
      if(tab01$pop_sr[id]==0){
        
        if( tab01$diferenca[id] > 0 ){
          
          if(tab01$degrees[id] == 12 | 
             tab01$degrees[id] ==  7 |
             tab01$degrees[id] ==  8 ) {
            tab01[id,30] <- tab01[id,30] + tab01$diferenca[id]
          }
          
          if(tab01$degrees[id] ==  1 | 
             tab01$degrees[id] ==  2 |
             tab01$degrees[id] == 10 ) {
            tab01[id,23] <- tab01[id,23] + tab01$diferenca[id]
          }
          
          if(tab01$degrees[id] ==  6 | 
             tab01$degrees[id] ==  5 |
             tab01$degrees[id] == 13 |
             tab01$degrees[id] == 14 ) {
            posicao <- sample(27:30, 1)
            tab01[id,posicao] <- tab01[id,posicao] + tab01$diferenca[id]
          }
          
          if(tab01$degrees[id] ==  9 | 
             tab01$degrees[id] == 12 |
             tab01$degrees[id] == 11 ) {
            posicao <- sample(23:30, 1)
            tab01[id,posicao] <- tab01[id,posicao] + tab01$diferenca[id]
          }
          
          if(tab01$degrees[id] == 3 | 
             tab01$degrees[id] == 4 ) {
            posicao <- sample(26:28, 1)
            tab01[id,posicao] <- tab01[id,posicao] + tab01$diferenca[id]
          }
          
        }
      }
      
    }
  }
  
}

sum( colSums(tab01[,23:30]) )

# Ajustar idade - ok
id <- which(colnames(tab01)=='diferenca')
colnames(tab01)[id] <- 'total'

tab01$total <- tab01$a20a39s + tab01$a40a59s + tab01$a60plus

tab01$a20a39s <- ifelse( tab01$a20a39s == 0, 0, tab01$a20a39s / tab01$total )
tab01$a40a59s <- ifelse( tab01$a40a59s == 0, 0, tab01$a40a59s / tab01$total )
tab01$a60plus <- ifelse( tab01$a60plus == 0, 0, tab01$a60plus / tab01$total )

b25a39s <- round( tab01$b25plus * tab01$a20a39s, 0 )
b40a59s <- round( tab01$b25plus * tab01$a40a59s, 0 )
b60plus <- round( tab01$b25plus * tab01$a60plus, 0 )

tab01$total <- b25a39s + b40a59s + b60plus

tab02 <- cbind.data.frame(tab01, b25a39s, b40a59s, b60plus)

tab02 <- tab02[,-c(15:22)]

diferenca <- tab02$b25plus - tab02$total
tab02 <- cbind.data.frame(tab02, diferenca)

tab02$total <- tab02$b25a39s + tab02$b40a59s + tab02$b60plus
tab02$diferenca <- tab02$b25plus - tab02$total
ids <- which(tab02$diferenca != 0)

tam <- length(ids)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  id <- ids[i]
  
  if(tab02$total[id] == 0 & tab02$diferenca[id] > 0){
    posicao <- sample(24:26,1)
    tab02[id,posicao] <- tab02[id,posicao] + tab02$diferenca[id]
  } else {
    
    if(tab02$diferenca[id] > 0){
      posicao <- sample(24:26,1)
      tab02[id,posicao] <- tab02[id,posicao] + tab02$diferenca[id]
    } else {
      posicao <- which( tab02[id,24:26] >= abs(tab02$diferenca[id]))
      if(!identical(posicao, integer(0))){
        id02 <- sample(1:length(posicao),1)
        tab02[id,24:26][posicao][id02] <- tab02[id,24:26][posicao][id02] + tab02$diferenca[id]
      }
    }
    
  }
}

tab02$degrees <- tolower(tab02$degrees)
tab02 <- tab02[,-c(22,23,27)]
sum( colSums(tab02[,15:24]) )

# Checagem
t01 <- unique.data.frame(tab02[,1:4])
cat('População brasileira: ',sum(t01$pop_tot))
t01 <- unique.data.frame(tab02[,1:6])
cat('Total por domicílios: ',sum(t01$domi_tt))
t01 <- unique.data.frame(tab02[,1:8])
cat('Total por sexo: ',sum(t01$sexo_tt))
t01 <- unique.data.frame(tab02[,1:10])
cat('Total por raça: ',sum(t01$raca_tt))
t01 <- unique.data.frame(tab02[,1:12])
cat('Total por situação escolar: ',sum(t01$frq_tot)) 
t01 <- unique.data.frame(tab02[,1:14])
cat('Total por graduação: ',sum(t01$deg_tot))

cat('Total por intervalos de idade: ', sum(tab02[,15:24]))

#ajuste deg_tot

colunas <- 15:24
tam <- nrow(tab02)
for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  total <- sum(tab02[i,colunas])
  diferenca <- tab02$deg_tot[i] - total
  if(diferenca != 0){
    
    if(tab02$frq_esc[i] ==  1){ intervalo <- 15:16}
    if(tab02$frq_esc[i] ==  2){ intervalo <- 16:17}
    if(tab02$frq_esc[i] ==  3){ intervalo <- 18:24}
    if(tab02$frq_esc[i] ==  4){ intervalo <- 19:24}
    if(tab02$frq_esc[i] ==  5){ intervalo <- 19:24}
    if(tab02$frq_esc[i] ==  6){ intervalo <- 19:24}
    if(tab02$frq_esc[i] ==  7){ intervalo <- 20:24}
    if(tab02$frq_esc[i] ==  8){ intervalo <- 21:24}
    if(tab02$frq_esc[i] ==  9){ intervalo <- 15:24}
    if(tab02$frq_esc[i] == 10){ intervalo <- 15:17}
    if(tab02$frq_esc[i] == 11){ intervalo <- 15:24}
    if(tab02$frq_esc[i] == 12){ intervalo <- 18:24}
    if(tab02$frq_esc[i] == 13){ intervalo <- 19:24}
    if(tab02$frq_esc[i] == 14){ intervalo <- 20:24}
    if(tab02$frq_esc[i] == 15){ intervalo <- 21:24}
    
    saldo <- abs(diferenca)
    
    if(diferenca > 0){
      while(saldo!=0){
        id <- sample(intervalo,1)
        tab02[i,id] <- tab02[i,id] + 1
        saldo <- saldo - 1
      }
    } else {
      while(saldo!=0){
        posicao <- which(tab02[i,colunas]>0)
        if(identical(posicao, integer(0))){
          saldo <- 0
        }
        id <- sample(posicao,1)
        id <- colunas[id]
        tab02[i,id] <- tab02[i,id] - 1
        saldo <- saldo - 1
      }
    }

  }
}


# Extração dos dados
colnames(tab02) <- c("estados", "codigo_municipio", "nome", "populacao_municipio", 
                     "localizacao", "populacao_localizacao", "sexo", "total_sexo", 
                     "raca", "total_raca", "situacao_escolar", "total_por_situacao",
                     "graduacao", "total_graduacao", "i00a03", "i04a05",
                     "i06a06", "i07a14", "i15a17", "i18a19", "i20a24", "i25a39", 
                     "i40a59", "i60mais") 

rm(t01, t02, t03, tab_tmp, tab_01, tab_02, tab01)
rm(b25a39s, b40a59s, b60plus, deg_tot, frq_tot, id01)
rm(colunas, ctrl, diferenca, endereco, estado, i, id,id02, id03, id99, idb, idc, ids, ids01, ii, intervalo, nro, perc, posicao)
rm(saldo, tam, tot_perc, total, total01, total02, var01, var02)

#
# 3.8 - Agregar Estado Civil 
#       (Tabela 2.*.3.2.csv)
#  Tabela 2.1.3.2 - Pessoas de 10 anos ou mais de idade, por estado civil, segundo os municípios 
#

cod_mun <- vector()
stcivil_casado <- vector() # Casado(a)
stcivil_diquit <- vector() # Desquitado(a) ou separado(a) judicialmente
stcivil_divorc <- vector() # Divorciado(a)
stcivil_viuvos <- vector() # Viúvo(a)
stcivil_soltei <- vector() # Solteiro(a)

ctrl <- 0
endereco <- 'C:\\CSV\\estado_civil'
arquivos <- dir(endereco)

for(i in 1:length(arquivos)){
  
  # bar process
  perc <- round(i / 27 * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)
  
  for(ii in 1:nrow(tab_tmp)){
    
    comprimento <- nchar(tab_tmp[ii,8])
    
    if(comprimento == 7){
      ctrl <- ctrl + 1
      cod_mun[ctrl] <- tab_tmp[ii, 8]
      stcivil_casado[ctrl] <- conv_nro( tab_tmp[ii, 3] )
      stcivil_diquit[ctrl] <- conv_nro( tab_tmp[ii, 4] )
      stcivil_divorc[ctrl] <- conv_nro( tab_tmp[ii, 5] )
      stcivil_viuvos[ctrl] <- conv_nro( tab_tmp[ii, 6] )
      stcivil_soltei[ctrl] <- conv_nro( tab_tmp[ii, 7] )
    }
  }
}

tab01 <- data.frame(cod_mun, stcivil_casado, stcivil_diquit, stcivil_divorc, stcivil_viuvos, stcivil_soltei)
rm(stcivil_casado, stcivil_diquit, stcivil_divorc, stcivil_viuvos, stcivil_soltei)


t01 <- unique.data.frame(tab02[,1:4])

library(sqldf)

t02 <- fn$sqldf('select a.estados, a.codigo_municipio, a.nome,  a.populacao_municipio, 
                        b.stcivil_casado, b.stcivil_diquit, b.stcivil_divorc, 
                        b.stcivil_viuvos, b.stcivil_soltei
                       from t01 a
                    left join tab01 b
                    on a.codigo_municipio == b.cod_mun')

# Inferindo que pessoas menor que 10 anos são solteiras

total <- (t02$stcivil_casado + t02$stcivil_diquit + t02$stcivil_divorc + t02$stcivil_viuvos + t02$stcivil_soltei)
saldo <- t02$populacao_municipio-total
t02$stcivil_soltei <- t02$stcivil_soltei + saldo

tab_tmp <- fn$sqldf('select a.estados, a.codigo_municipio, a.nome, 
                            a.populacao_municipio, a.localizacao, a.populacao_localizacao,
                            a.sexo, a.total_sexo, a.raca, 
                            a.total_raca, a.situacao_escolar, a.total_por_situacao, 
                            a.graduacao, a.total_graduacao, a.i00a03, 
                            a.i04a05, a.i06a06, a.i07a14, a.i15a17, a.i18a19, a.i20a24, 
                            a.i25a39, a.i40a59, a.i60mais, 
                            b.stcivil_casado, b.stcivil_diquit, b.stcivil_divorc, 
                            b.stcivil_viuvos, b.stcivil_soltei
                    from tab02 a
                    left join t02 b
                    on a.codigo_municipio == b.codigo_municipio')

tabela <- tab_tmp

#
# 3.9 - Agregar Rendimentos Urbanos e Rurais
#       (Tabela 4.*.7.3.csv)
#
# Tabela 4.*.7.3 - Valor do rendimento nominal mediano mensal das pessoas de 10 
#       anos ou mais de idade, total e com rendimento, por situação do 
#       domicilio, segundo as mesorregiões, microrregiões, os municípios e 
#        os distritos - * - 2010 
#

# Converte os números no Censo em um tipo numérico compatível ao R.
conv_nro <- function(valor){
  valor <- gsub("[ ]", "", valor)
  valor <- gsub("[,]", ".", valor)
  valor <- gsub("[-]", 0, valor)
  return (as.numeric(valor))
}

cod_mun <- vector()
local <- vector()
redloc <- vector()

ctrl <- 0
endereco <- 'C:\\CSV\\rendimentos\\localizacao'
arquivos <- dir(endereco)
tam <- length(arquivos)

for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)
  
  for(ii in 1:nrow(tab_tmp)){
    comprimento <- nchar(tab_tmp[ii, 8])
    if(comprimento == 7){
      
      var01 <- tab_tmp[ii, 8]
      
      ctrl <- ctrl + 1
      cod_mun[ctrl] <- var01
      local[ctrl] <- 1
      redloc[ctrl]   <- conv_nro(tab_tmp[ii, 5])
      
      ctrl <- ctrl + 1
      cod_mun[ctrl] <- var01
      local[ctrl] <- 2
      redloc[ctrl]   <- conv_nro(tab_tmp[ii, 7]) 
    }
  }
}

tab01 <- unique.data.frame( cbind.data.frame(cod_mun, local, redloc) )
rm(cod_mun, local, redloc)

#

#
# 3.10 - Agregar Rendimentos por gênero 
#
# Tabela 2.*.9.2 - Pessoas de 10 anos ou mais de idade, com rendimento, e valor 
#                 do rendimento nominal médio e mediano mensal das pessoas de 10 
#                  anos ou mais de idade, com rendimento, por sexo, segundo os 
#                  municípios - * - 2010
# 

cod_mun <- vector()
sexo <- vector()
media <- vector()
mediana <- vector()

ctrl <- 0
endereco <- 'C:\\CSV\\rendimentos\\sexo'
arquivos <- dir(endereco)
tam <- length(arquivos)

for(i in 1:tam){
  
  # bar process
  perc <- round(i / 27 * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)
  
  for(ii in 1:nrow(tab_tmp)){
    comprimento <- nchar(tab_tmp[ii, 11])
    if(comprimento == 7){
      
      var01 <- tab_tmp[ii, 11]
      
      ctrl          <- ctrl + 1
      cod_mun[ctrl] <- var01
      sexo[ctrl]    <- 1
      media[ctrl]   <- conv_nro(tab_tmp[ii, 6])
      mediana[ctrl] <- conv_nro(tab_tmp[ii, 9])
      
      ctrl          <- ctrl + 1
      cod_mun[ctrl] <- var01
      sexo[ctrl]    <- 2
      media[ctrl]   <- conv_nro(tab_tmp[ii, 7])
      mediana[ctrl] <- conv_nro(tab_tmp[ii,10])

    }
  }
}

tab02 <- data.frame(cod_mun, sexo, media, mediana)
rm(cod_mun, sexo, media, mediana)


#
# 3.11 - Agregar Rendimentos por gênero e raça
#
# Tabela 3.*.2.4 - Pessoas residentes em domicílios particulares, por cor ou raça, 
#                  segundo o sexo e as classes de rendimento nominal mensal 
#                  domiciliar per capita - Rondônia - 2010
# 
#

# A partir do nome de um estado a funcao retorna a respectiva sigla.
conv_codeState <- function(nome_do_estado){
  cod_estado <- c('ac', 	'al', 	'ap', 	'am', 	'ba', 	'ce', 	'df', 	'es', 	'go', 	'ma', 	'mt', 	'ms', 	'mg', 	'pa', 	'pb', 	'pr', 	'pe', 	'pi', 	'rj', 	'rn', 	'rs', 	'ro', 	'rr', 	'sc', 	'sp', 	'se', 	'to') 
  descr_estado <- c('acre', 	'alagoas', 	'amapá', 	'amazonas', 	'bahia', 	'ceará', 	'distrito federal', 	'espírito santo', 	'goiás', 	'maranhão', 	'mato grosso', 	'mato grosso do sul', 	'minas gerais', 	'pará', 	'paraíba', 	'paraná', 	'pernambuco', 	'piauí', 	'rio de janeiro', 	'rio grande do norte', 	'rio grande do sul', 	'rondônia', 	'roraima', 	'santa catarina', 	'são paulo', 	'sergipe', 	'tocantins')
  tab_state <- data.frame(descr_estado, cod_estado)
  id <- which(tab_state$descr_estado == tolower(nome_do_estado))
  return (tab_state$cod_estado[id])
}

estado <- vector()
raca   <- vector()
sexo   <- vector()

rend_1p8  <- vector()
rend_1p4  <- vector()
rend_1p2  <- vector()
rend_1    <- vector()
rend_2    <- vector()
rend_3    <- vector()
rend_5    <- vector()
rend_10   <- vector()
rend_mais <- vector()
rend_sem  <- vector()

ctrl <- 0
endereco <- 'C:\\CSV\\rendimentos\\raca'
arquivos <- dir(endereco)
tam <- length(arquivos)

for(i in 1:tam){
  
  # bar process
  perc <- round(i / tam * 100,2)
  cat(paste0(perc, '% processados.'))
  cat('                              ')
  cat('\r')
  
  tab_tmp <- read.csv2(paste0(endereco,'\\',arquivos[i]),header = FALSE)
  
  var01 <- strsplit(tab_tmp[2,1],'-')[[1]][3]
  var01 <- substr(var01, 2, nchar(var01)-1 )
  var01 <- conv_codeState(var01)
  
  # Branca
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 1
  sexo[ctrl]      <- 1
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[19, 3])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[20, 3])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[21, 3])
  rend_1[ctrl]    <- conv_nro(tab_tmp[22, 3])
  rend_2[ctrl]    <- conv_nro(tab_tmp[23, 3])
  rend_3[ctrl]    <- conv_nro(tab_tmp[24, 3])
  rend_5[ctrl]    <- conv_nro(tab_tmp[25, 3])
  rend_10[ctrl]   <- conv_nro(tab_tmp[26, 3])
  rend_mais[ctrl] <- conv_nro(tab_tmp[27, 3])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[28, 3])
  
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 1
  sexo[ctrl]      <- 2
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[30, 3])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[31, 3])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[32, 3])
  rend_1[ctrl]    <- conv_nro(tab_tmp[33, 3])
  rend_2[ctrl]    <- conv_nro(tab_tmp[34, 3])
  rend_3[ctrl]    <- conv_nro(tab_tmp[35, 3])
  rend_5[ctrl]    <- conv_nro(tab_tmp[36, 3])
  rend_10[ctrl]   <- conv_nro(tab_tmp[37, 3])
  rend_mais[ctrl] <- conv_nro(tab_tmp[38, 3])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[39, 3])
  
  #Negra
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 2
  sexo[ctrl]      <- 1 
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[19, 4])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[20, 4])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[21, 4])
  rend_1[ctrl]    <- conv_nro(tab_tmp[22, 4])
  rend_2[ctrl]    <- conv_nro(tab_tmp[23, 4])
  rend_3[ctrl]    <- conv_nro(tab_tmp[24, 4])
  rend_5[ctrl]    <- conv_nro(tab_tmp[25, 4])
  rend_10[ctrl]   <- conv_nro(tab_tmp[26, 4])
  rend_mais[ctrl] <- conv_nro(tab_tmp[27, 4])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[28, 4])
  
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 2
  sexo[ctrl]      <- 2
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[30, 4])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[31, 4])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[32, 4])
  rend_1[ctrl]    <- conv_nro(tab_tmp[33, 4])
  rend_2[ctrl]    <- conv_nro(tab_tmp[34, 4])
  rend_3[ctrl]    <- conv_nro(tab_tmp[35, 4])
  rend_5[ctrl]    <- conv_nro(tab_tmp[36, 4])
  rend_10[ctrl]   <- conv_nro(tab_tmp[37, 4])
  rend_mais[ctrl] <- conv_nro(tab_tmp[38, 4])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[39, 4])
  
  #Asiática
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 3
  sexo[ctrl]      <- 1 
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[19, 5])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[20, 5])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[21, 5])
  rend_1[ctrl]    <- conv_nro(tab_tmp[22, 5])
  rend_2[ctrl]    <- conv_nro(tab_tmp[23, 5])
  rend_3[ctrl]    <- conv_nro(tab_tmp[24, 5])
  rend_5[ctrl]    <- conv_nro(tab_tmp[25, 5])
  rend_10[ctrl]   <- conv_nro(tab_tmp[26, 5])
  rend_mais[ctrl] <- conv_nro(tab_tmp[27, 5])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[28, 5])
  
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 3
  sexo[ctrl]      <- 2
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[30, 5])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[31, 5])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[32, 5])
  rend_1[ctrl]    <- conv_nro(tab_tmp[33, 5])
  rend_2[ctrl]    <- conv_nro(tab_tmp[34, 5])
  rend_3[ctrl]    <- conv_nro(tab_tmp[35, 5])
  rend_5[ctrl]    <- conv_nro(tab_tmp[36, 5])
  rend_10[ctrl]   <- conv_nro(tab_tmp[37, 5])
  rend_mais[ctrl] <- conv_nro(tab_tmp[38, 5])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[39, 5]) 
  
  # Parda
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 4
  sexo[ctrl]      <- 1 
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[19, 6])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[20, 6])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[21, 6])
  rend_1[ctrl]    <- conv_nro(tab_tmp[22, 6])
  rend_2[ctrl]    <- conv_nro(tab_tmp[23, 6])
  rend_3[ctrl]    <- conv_nro(tab_tmp[24, 6])
  rend_5[ctrl]    <- conv_nro(tab_tmp[25, 6])
  rend_10[ctrl]   <- conv_nro(tab_tmp[26, 6])
  rend_mais[ctrl] <- conv_nro(tab_tmp[27, 6])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[28, 6])
  
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 4
  sexo[ctrl]      <- 2
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[30, 6])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[31, 6])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[32, 6])
  rend_1[ctrl]    <- conv_nro(tab_tmp[33, 6])
  rend_2[ctrl]    <- conv_nro(tab_tmp[34, 6])
  rend_3[ctrl]    <- conv_nro(tab_tmp[35, 6])
  rend_5[ctrl]    <- conv_nro(tab_tmp[36, 6])
  rend_10[ctrl]   <- conv_nro(tab_tmp[37, 6])
  rend_mais[ctrl] <- conv_nro(tab_tmp[38, 6])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[39, 6])   
  
  # Indígena
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 5
  sexo[ctrl]      <- 1 
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[19, 7])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[20, 7])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[21, 7])
  rend_1[ctrl]    <- conv_nro(tab_tmp[22, 7])
  rend_2[ctrl]    <- conv_nro(tab_tmp[23, 7])
  rend_3[ctrl]    <- conv_nro(tab_tmp[24, 7])
  rend_5[ctrl]    <- conv_nro(tab_tmp[25, 7])
  rend_10[ctrl]   <- conv_nro(tab_tmp[26, 7])
  rend_mais[ctrl] <- conv_nro(tab_tmp[27, 7])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[28, 7])
  
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 5
  sexo[ctrl]      <- 2
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[30, 7])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[31, 7])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[32, 7])
  rend_1[ctrl]    <- conv_nro(tab_tmp[33, 7])
  rend_2[ctrl]    <- conv_nro(tab_tmp[34, 7])
  rend_3[ctrl]    <- conv_nro(tab_tmp[35, 7])
  rend_5[ctrl]    <- conv_nro(tab_tmp[36, 7])
  rend_10[ctrl]   <- conv_nro(tab_tmp[37, 7])
  rend_mais[ctrl] <- conv_nro(tab_tmp[38, 7])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[39, 7])
  
  # Sem declaração
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 6
  sexo[ctrl]      <- 1 
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[19, 8])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[20, 8])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[21, 8])
  rend_1[ctrl]    <- conv_nro(tab_tmp[22, 8])
  rend_2[ctrl]    <- conv_nro(tab_tmp[23, 8])
  rend_3[ctrl]    <- conv_nro(tab_tmp[24, 8])
  rend_5[ctrl]    <- conv_nro(tab_tmp[25, 8])
  rend_10[ctrl]   <- conv_nro(tab_tmp[26, 8])
  rend_mais[ctrl] <- conv_nro(tab_tmp[27, 8])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[28, 8])
  
  ctrl <- ctrl + 1
  estado[ctrl]    <- var01
  raca[ctrl]      <- 6
  sexo[ctrl]      <- 2
  rend_1p8[ctrl]  <- conv_nro(tab_tmp[30, 8])
  rend_1p4[ctrl]  <- conv_nro(tab_tmp[31, 8])
  rend_1p2[ctrl]  <- conv_nro(tab_tmp[32, 8])
  rend_1[ctrl]    <- conv_nro(tab_tmp[33, 8])
  rend_2[ctrl]    <- conv_nro(tab_tmp[34, 8])
  rend_3[ctrl]    <- conv_nro(tab_tmp[35, 8])
  rend_5[ctrl]    <- conv_nro(tab_tmp[36, 8])
  rend_10[ctrl]   <- conv_nro(tab_tmp[37, 8])
  rend_mais[ctrl] <- conv_nro(tab_tmp[38, 8])
  rend_sem[ctrl]  <- conv_nro(tab_tmp[39, 8]) 

}

tab03 <- data.frame(estado, raca, sexo, rend_1p8, rend_1p4, rend_1p2, rend_1, rend_2, rend_3, rend_5, rend_10, rend_mais, rend_sem)
rm(estado, raca, sexo, rend_1p8, rend_1p4, rend_1p2, rend_1, rend_2, rend_3, rend_5, rend_10, rend_mais, rend_sem)

# Juntar dados de rendimento
tab_tmp <- unique.data.frame(tabela[,1:10])

t01 <- fn$sqldf('select  a.estados, a.codigo_municipio,  
                         a.localizacao, a.sexo, a.raca, b.redloc, c.media, c.mediana,
                         d.rend_1p8, d.rend_1p4, d.rend_1p2, d.rend_1, d.rend_2, d.rend_3, d.rend_5, d.rend_10, d.rend_mais, d.rend_sem
                    from tab_tmp a
               left join tab01 b
                      on a.codigo_municipio = b.cod_mun and
                         a.localizacao = b.local 
               left join tab02 c
                      on a.codigo_municipio = c.cod_mun and
                         a.sexo = c.sexo
               left join tab03 d
                      on a.estados = d.estado and
                         a.sexo    = d.sexo and
                         a.raca    = d.raca
                ')
rm(tab01, tab02, tab03)

# Inserir dados de rendimento
tab_tmp <- fn$sqldf('select a.estados,              a.codigo_municipio,              
                            a.populacao_municipio,  a.localizacao,          a.populacao_localizacao, 
                            a.sexo,                 a.total_sexo,           a.raca,                 
                            a.total_raca,           a.situacao_escolar,     a.total_por_situacao,   
                            a.graduacao,            a.total_graduacao,      a.i00a03,               
                            a.i04a05,               a.i06a06,               a.i07a14,               
                            a.i15a17,               a.i18a19,               a.i20a24,               
                            a.i25a39,               a.i40a59,               a.i60mais,              
                            a.stcivil_casado,       a.stcivil_diquit,       a.stcivil_divorc,       
                            a.stcivil_viuvos,       a.stcivil_soltei,
                            b.redloc,          b.media,           b.mediana,         
                            b.rend_1p8,        b.rend_1p4,        b.rend_1p2,        b.rend_1,          
                            b.rend_2,          b.rend_3,          b.rend_5,          b.rend_10,         
                            b.rend_mais,       b.rend_sem
                       from tabela a
                  left join t01 b
                         on  a.codigo_municipio = b.codigo_municipio and
                             a.localizacao = b.localizacao and
                             a.sexo = b.sexo and
                             a.raca = b.raca      ')

tabela <- tab_tmp
rm(tab_tmp, saldo, total)

ids <- which(tabela$total_graduacao==0)
tabela <- tabela[-ids,]

#Salvar população agregada do censo 2010
write.csv2(tabela,'Dados_Agregados_Censo2010.csv', row.names = FALSE)
write.csv2(tab_indices, 'Tabela_de_indices_Dados_Sintéticos.csv', row.names = FALSE)
