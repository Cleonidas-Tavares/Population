#
# Creating synthetic population from:.
#  - 2010 Brazilian census - microdata;
#  - 2010 Brazilian census - agregate data;
#

library(nanoparquet)
library(rJava)
library(rChoiceDialogs)
library(svDialogs)
library(sqldf)

set.seed(1)

id_ori <- c('2010 Brazilian census - Microdata', '2010 Brazilian census - Agregated data')
origem  <- jselect.list(id_ori, title='Select an item:', multiple=FALSE);

if( identical(origem,id_ori[1]) | identical(origem, id_ori[2]) ){
  
  if(identical(origem,id_ori[1])){
    
    cat('\n\n\ Enter the file with 2010 Brazilian census - microdata.\n\n')
    arquivo <- jchoose.files(caption = "Select the parquet file - 2010 Brazilian census - microdata") 
    
    cat('\n wait a minute.             \r')
    tab_tmp <- nanoparquet::read_parquet(arquivo)

    t01 <- fn$sqldf('select cod_mun, domicil, sexo, raca, frq_esc, est_civ, 
                            rend_bt, idade, count(*) total, sum(peso) peso
                       from tab_tmp
                   group by cod_mun, domicil, sexo, raca, frq_esc, est_civ,
                            rend_bt, idade ')
    cod_mun <- unique(t01$cod_mun)
    cat('                              ')
    cat('\r')
    
  } else {
    
    semente <- dlg_input("Enter a sample seed for the data:")$res
    if( identical(grepl("^[0-9]{1,}$", semente), TRUE) ){
      semente <- as.numeric(semente)
    } else {
      semente <- 1 
    }
    
    set.seed(semente)
    cat('\nSeed: ', semente, '\n')
    
    cat('\n\n\t Enter the parquet file with 2010 Brazilian census - Agregated Data.\n\n')
    arquivo <- jchoose.files(caption = "Select the 2010 Brazilian census - agregated data") 
    
    cat('\n wait a minute.             \r')
    t01 <- nanoparquet::read_parquet(arquivo[1])
    colnames(t01)[2] <- 'cod_mun'
    cod_mun <- unique(t01$cod_mun)
    cat('                              ')
    
    # Rendimentos: Calculo da probabilidade
    total <- t01$rend_1p8 + t01$rend_1p4 + t01$rend_1p2 + t01$rend_1 + t01$rend_2 + t01$rend_3 + t01$rend_5 + t01$rend_10 + t01$rend_mais + t01$rend_sem 
    
    t01$rend_1p8  <- ifelse(t01$rend_1p8  == 0, 0, t01$rend_1p8  / total)
    t01$rend_1p4  <- ifelse(t01$rend_1p4  == 0, 0, t01$rend_1p4  / total)
    t01$rend_1p2  <- ifelse(t01$rend_1p2  == 0, 0, t01$rend_1p2  / total)
    t01$rend_1    <- ifelse(t01$rend_1    == 0, 0, t01$rend_1    / total)
    t01$rend_2    <- ifelse(t01$rend_2    == 0, 0, t01$rend_2    / total)
    t01$rend_3    <- ifelse(t01$rend_3    == 0, 0, t01$rend_3    / total)
    t01$rend_5    <- ifelse(t01$rend_5    == 0, 0, t01$rend_5    / total)
    t01$rend_10   <- ifelse(t01$rend_10   == 0, 0, t01$rend_10   / total) 
    t01$rend_mais <- ifelse(t01$rend_mais == 0, 0, t01$rend_mais / total) 
    t01$rend_sem  <- ifelse(t01$rend_sem  == 0, 0, t01$rend_sem  / total)
    rm(total)
  }
  
  sair <- 0
  while(sair==0){
    
    cat("\014") 
    
    itens <- c('City', 'State')
    resp  <- jselect.list(itens, title='Creating a synthetic population for:', multiple=FALSE);
    
    if( identical(resp,itens[1]) | identical(resp, itens[2]) ){
      
      erro <- 0
      
      if( identical(resp,itens[1]) ){
        codigo <- dlg_input("Enter a city-code:")$res
        codigo <- as.numeric(codigo)
        if( codigo %in% cod_mun ){
          ids_codes <- codigo
        } else {
          dlg_message('Invalid code.')$res
          erro <- 1
        }
      }
      
      if( identical(resp,itens[2]) ){
        
        cod_est <- c( 11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53) 
        nom_est <- c( 'rondônia', 'acre', 'amazonas', 'roraima', 'pará', 'amapá', 'tocantins', 'maranhão', 'piauí', 'ceará', 'rio grande do norte', 'paraíba', 'pernambuco', 'alagoas', 'sergipe', 'bahia', 'minas gerais', 'espírito santo', 'rio de janeiro', 'são paulo', 'paraná', 'santa catarina', 'rio grande do sul', 'mato grosso do sul', 'mato grosso', 'goiás', 'distrito federal' )
        
        state_item  <- jselect.list(nom_est, title='Creating a synthetic population for:', multiple=FALSE);
        
        id <- which(state_item == nom_est)
        codigo <- cod_est[id]
        
        if( codigo %in% cod_est ){
          var02 <- unique(t01$cod_mun)
          var01 <- as.numeric( substr(var02,1,2) )
          ids_codes <- which(var01 == codigo)
          ids_codes <- var02[ids_codes]
        } else {
          dlg_message('Invalid state-code.')$res
          erro <- 1
        }
      }
      if(erro == 0){
        for(ii in ids_codes){
        
        ids <- which(t01$cod_mun == ii)
        
        if(identical(origem,id_ori[1])){
          var01   <- round(sum(t01$peso[ids]), 0)
        } else {
          var01   <- sum(t01$total_graduacao[ids])
        }
        
        ident   <- vector(mode = "numeric", length = var01)
        codmun  <- vector(mode = "numeric", length = var01)
        domicil <- vector(mode = "numeric", length = var01)
        sexo    <- vector(mode = "numeric", length = var01)
        raca    <- vector(mode = "numeric", length = var01)
        frq_esc <- vector(mode = "numeric", length = var01)
        est_civ <- vector(mode = "numeric", length = var01)
        renda   <- vector(mode = "numeric", length = var01)
        idade   <- vector(mode = "numeric", length = var01)
        
        ctrl <- 0
        
        if(identical(origem,id_ori[1]) & erro == 0){
          #MD
          dt_ini <- date()
          t02 <- t01[ids,]
          nro <- 0
          tam <- nrow(t02)
          for(i in 1:tam){
            
            # bar process
            perc <- round(i / tam * 100,2)
            cat(paste0(perc, '% processing. '))
            cat('                              ')
            cat('\r')
            
            saldo <- round(t02$peso[i], 0)
            
            while (saldo > 0) {
              nro <- nro + 1
              
              ident[nro]   <- nro       
              codmun[nro]  <- t02$cod_mun[i]
              domicil[nro] <- t02$domicil[i]
              sexo[nro]    <- t02$sexo[i]  
              raca[nro]    <- t02$raca[i]  
              frq_esc[nro] <- t02$frq_esc[i]
              est_civ[nro] <- t02$est_civ[i]
              renda[nro]   <- t02$rend_bt[i]
              idade[nro]   <- t02$idade[i]
              
              saldo <- saldo - 1
            }
            
          }
          
          tab_tmp <- data.frame(ident, codmun, domicil, sexo, raca, frq_esc, est_civ, renda, idade)
          colnames(tab_tmp) <- c('id', 'city_code', 'domicile', 'gender', 'race', 'school_situation', 'marital_status', 'income', 'age')
          rm(ident, codmun, domicil, sexo, raca, frq_esc, est_civ, renda, idade)

          if( identical(resp,itens[1]) ){
            nome <- paste(ii, '- synthetic population from microdata - 2010 census.csv')
            write.csv2(tab_tmp, nome, row.names = FALSE)
            cat('\n Started in: ', dt_ini, '\n Ended in: ', date(),'\n\n')
            texto <- paste('Population successfully generated. The file \'', nome, '\' was written to ', getwd(), sep = '')
            dlg_message(texto)$res
            rm(texto)
          } else {
            nome <- paste(ii, '- synthetic population from microdata - 2010 census.parquet')
            nanoparquet::write_parquet(tab_tmp, nome)
            cat('City: ', ii,' - ok \n\n')
          }
          
        } 
        
        if(identical(origem,id_ori[2]) & erro == 0){
          #DS
          t02 <- t01[ids,]
          colnames(t02)[23] <- 'i60a99'
          
          ctrl <- 0
          tam <- nrow(t02)
          for(i in 1:tam){
            
            # bar process
            perc <- round(i / tam * 100,2)
            cat(paste0(perc, '% processing. '))
            cat('                              ')
            cat('\r')
            
            prob_stcivil <- t02[i,24:28] / sum(t02[i,24:28])
            
            for(j in 14:23){
              if(t02[i,j]>0){
                saldo <- t02[i,j]
                ini <- as.numeric( substr(colnames(t02)[j],2,3) )
                fim <- as.numeric( substr(colnames(t02)[j],5,6) )
                intervalos <- ini:fim
                
                while (saldo > 0) {
                  saldo <- saldo - 1
                  ctrl <- ctrl + 1
                  ident[ctrl]   <- ctrl
                  codmun[ctrl]  <- t02$cod_mun[i]
                  domicil[ctrl] <- t02$localizacao[i]
                  sexo[ctrl]    <- t02$sexo[i]
                  raca[ctrl]    <- t02$raca[i]
                  frq_esc[ctrl] <- t02$situacao_escolar[i]
                  idade[ctrl]   <- sample(intervalos,1)
                  
                  if(idade[ctrl]<10){
                    var04 <- 5
                    sal_minimo <- 0
                    var03 <- 0
                  } else {
                    var04 <- sample(1:5,1,prob=prob_stcivil)
                    
                    if(t02$redloc[i] > 0 & t02$mediana[i] >0){
                      sal_minimo <- (t02$redloc[i] + t02$mediana[i]) / 2
                    } else {
                      if(t02$redloc[i] > 0){
                        sal_minimo <- t02$redloc[i]
                      } else {
                        sal_minimo <- t02$mediana[i]
                      }
                    }
                    
                    probabilidade <- t01[j,32:41]
                    id <- sample(1:10, 1,prob = probabilidade)
                    
                    var01 <- switch (id, (1/100), (1/8), (1/4), (1/2), 1, 2, 3,  5,  10, 0) + 0.0005
                    var02 <- switch (id,   (1/8), (1/4), (1/2),     1, 2, 3, 5, 10, 100, 0)
                    var03 <- ifelse(id==10, 0, runif(1,var01, var02) )
                    
                  }
                  est_civ[ctrl] <- var04
                  
                  renda[ctrl] <- round(var03 * sal_minimo, 02)
                  
                }
              }
            }
          }

          tab_tmp <- data.frame(ident, codmun, domicil, sexo, raca, frq_esc, est_civ, renda, idade)
          colnames(tab_tmp) <- c('id', 'city_code', 'domicile', 'gender', 'race', 'school_situation', 'marital_status', 'income', 'age')
          rm(ident, codmun, domicil, sexo, raca, frq_esc, est_civ, renda, idade)
          
          if( identical(resp,itens[1]) ){
            nome <- paste(codigo, '- synthetic population from agregated_data - 2010 census.csv')
            write.csv2(tab_tmp, nome, row.names = FALSE)      
            texto <- paste('Population successfully generated. The file \'', nome, '\'  was written to  ', getwd(), sep = '')
            dlg_message(texto)$res
            rm(texto, nome)
          } else {
            nome <- paste(ii, '- synthetic population from Agregated Data - 2010 census.parquet')
            nanoparquet::write_parquet(tab_tmp, nome)
            cat('City: ', ii,' - ok \n\n')
            rm(nome)
          }

        }
      }
      }
    } else {
      dlg_message('End of process.')$res
      sair <- 1
    }
  }
  
  rm(t01, t02, tab_tmp, cod_mun, ids, sair)
  
} else {
  dlg_message('No options were selected.')$res
}

cat("\n\n End of process\n\n")
