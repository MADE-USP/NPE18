# Pacotes -----------------------------------------------------------------

# Abrir arquivos de Stata .dta
library(haven)

# Quantil ponderado por peso 
library(modi)

# Gini com pesos
library(acid)

# Renda POF + IRPF --------------------------------------------------------

### Abrir base com rendimento POF + IRPF ### 
base_final <- read_dta("base final.dta")

# Ordenar observacoes conforme renda POF + IRPF
base_final <- base_final[order(base_final$renda_irpfepof),]

# Criar identificao unica de moradores
base_final$id <- paste(base_final$cod_upa,
                       base_final$num_dom,
                       base_final$num_uc,
                       base_final$cod_informante, sep = "-")

### Abrir base com rendimento POF ### 
base <- read_dta("saidera.dta")

# Ordenar observacoes conforme renda POF + IRPF
base <- base[order(base$renda_monetaria),]

# Criar identificao unica de moradores
base$id <- paste(base$cod_upa,
                 base$num_dom,
                 base$num_uc,
                 base$cod_informante, sep = "-")

###  Abrir base morador ### 
morador <- readRDS("MORADOR.rds")

# V0404 - Sexo
# V0405 - Cor ou raça

# Criar identificao unica de moradores
morador$id <- paste(morador$COD_UPA,
                    morador$NUM_DOM,
                    morador$NUM_UC,
                    morador$COD_INFORMANTE, sep = "-")

# Base raça, genero e rendas
baserr <- merge(base_final[c("id", "renda_irpfepof", "peso_final")],
                base[c("id", "renda_monetaria")],
                by = "id")

baserr <- merge(baserr,
                morador[c("id", "V0404", "V0405", "ANOS_ESTUDO", "V0425")], 
                all.x = T, 
                by = "id")


# Criar variaveis ---------------------------------------------------------

# Educacao como categorica
baserr$Educ <- ifelse(baserr$V0425 %in% c(1:8), "Fund/Alfa",
                      ifelse(baserr$V0425 %in% c(9:11), "Medio",
                             ifelse(baserr$V0425 %in% c(12:15), "Superior", NA)))

# Raca
baserr$Negros <- baserr$V0405 %in% c(2,4)

# Genero
baserr$Mulheres <- baserr$V0404 == 2

# Análise -------------------------------------------

# Gerar lista com minimo e maximo de cada decil + 1%
decis <- lapply(lapply(as.list(c(paste0(sapply(seq(from = 0, to = .9, by = .1), 
                                               function(quantil) weighted.quantile(baserr$renda_monetaria,
                                                                                   baserr$peso_final,
                                                                                   prob = quantil)),
                                        "-",
                                        c(sapply(seq(0.1,.9,.1), 
                                               function(quantil) weighted.quantile(baserr$renda_monetaria,
                                                                                   baserr$peso_final,
                                                                                   prob = quantil)),
                                 max(baserr$renda_monetaria))))),
                       function(decil) strsplit(decil, "-")), 
                function(elemento) as.numeric(elemento[[1]]))

# Nomear os decis
names(decis) <- paste0(seq(0,.9,.1), "-", seq(.1,1,.1))

# Vetor de identificacao da variavel V0404
genero <- c("Homem" = 1, "Mulher" = 2)

# Vetor de identificacao da variavel V0405
raca <- list("B&A" = c(1,3), "N" = c(2,4))

### Apropriacao da renda ###

apropriacao <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis) {
      
      apropriacao <- rbind(apropriacao,
                           data.frame(Genero = gen,
                                      Raca = paste(rc, collapse = "-"),
                                      Decil = paste(dec, collapse = "-"),
                                      Apro = sum(baserr$renda_irpfepof[baserr$renda_monetaria >= dec[1] &
                                                                         baserr$renda_monetaria <= dec[2] &
                                                                         baserr$V0404 == gen & 
                                                                         (baserr$V0405 %in% rc)]*
                                                   baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                           baserr$renda_monetaria <= dec[2] &
                                                                           baserr$V0404 == gen & 
                                                                           (baserr$V0405 %in% rc)])/
                                        sum(baserr$peso_final*baserr$renda_irpfepof)))
      
    }
    
  }
  
}

# Atributir o nome
apropriacao$Genero <- c(rep("H",nrow(apropriacao)/2), rep("M",nrow(apropriacao)/2))
apropriacao$Raca <- c(rep(c(rep("B",(nrow(apropriacao)/4)), rep("N",(nrow(apropriacao)/4))),2))
apropriacao$Decil <- names(decis)
apropriacao$Grupo <- paste0(apropriacao$Genero,"-", apropriacao$Raca)

### Rendimento medio por decil por grupo ###

rendimento <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis) {
      
      rendimento <- rbind(rendimento,
                          data.frame(Genero = gen,
                                     Raca = paste(rc, collapse = "-"),
                                     Decil = paste(dec, collapse = "-"),
                                     RendMed = weighted.mean(x = baserr$renda_irpfepof[baserr$renda_monetaria >= dec[1] &
                                                                          baserr$renda_monetaria <= dec[2] &
                                                                          baserr$V0404 == gen & 
                                                                          (baserr$V0405 %in% rc)],
                                                    w = baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                        baserr$renda_monetaria <= dec[2] &
                                                                        baserr$V0404 == gen & 
                                                                        (baserr$V0405 %in% rc)], na.rm = T),
                                     EducMed = weighted.mean(x = baserr$ANOS_ESTUDO[baserr$renda_monetaria >= dec[1] &
                                                                                         baserr$renda_monetaria <= dec[2] &
                                                                                         baserr$V0404 == gen & 
                                                                                         (baserr$V0405 %in% rc)],
                                                             w = baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                                     baserr$renda_monetaria <= dec[2] &
                                                                                     baserr$V0404 == gen & 
                                                                                     (baserr$V0405 %in% rc)], na.rm = T),
                                     PropFund = weighted.mean(x = baserr$Educ[baserr$renda_monetaria >= dec[1] &
                                                                                       baserr$renda_monetaria <= dec[2] &
                                                                                       baserr$V0404 == gen & 
                                                                                       (baserr$V0405 %in% rc)] == "Fund/Alfa",
                                                              w = baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                                      baserr$renda_monetaria <= dec[2] &
                                                                                      baserr$V0404 == gen & 
                                                                                      (baserr$V0405 %in% rc)], na.rm = T),
                                     PropMedi = weighted.mean(x = baserr$Educ[baserr$renda_monetaria >= dec[1] &
                                                                                baserr$renda_monetaria <= dec[2] &
                                                                                baserr$V0404 == gen & 
                                                                                (baserr$V0405 %in% rc)] == "Medio",
                                                              w = baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                                      baserr$renda_monetaria <= dec[2] &
                                                                                      baserr$V0404 == gen & 
                                                                                      (baserr$V0405 %in% rc)], na.rm = T),
                                     PropSupe = weighted.mean(x = baserr$Educ[baserr$renda_monetaria >= dec[1] &
                                                                                baserr$renda_monetaria <= dec[2] &
                                                                                baserr$V0404 == gen & 
                                                                                (baserr$V0405 %in% rc)] == "Superior",
                                                              w = baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                                      baserr$renda_monetaria <= dec[2] &
                                                                                      baserr$V0404 == gen & 
                                                                                      (baserr$V0405 %in% rc)], na.rm = T)))
                           
      
    }
    
  }
  
}

# Atributir o nome
rendimento$Genero <- c(rep("H",20), rep("M",20))
rendimento$Raca <- c(rep(c(rep("B",10), rep("N",10)),2))
rendimento$Decil <- names(decis)
rendimento$Grupo <- paste0(rendimento$Genero,"-", rendimento$Raca)

### Rendimento médio por decil

rendimento.med <- data.frame()

for(dec in decis) {
  
  rendimento.med <- rbind(rendimento.med,
                          data.frame(Decil = paste(dec, collapse = "-"),
                                     RendMed = weighted.mean(x = baserr$renda_irpfepof[baserr$renda_monetaria >= dec[1] &
                                                                                         baserr$renda_monetaria <= dec[2]],
                                                             w = baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                                     baserr$renda_monetaria <= dec[2]], na.rm = T),
                                     EducMed = weighted.mean(x = baserr$ANOS_ESTUDO[baserr$renda_monetaria >= dec[1] &
                                                                                      baserr$renda_monetaria <= dec[2]],
                                                             w = baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                                     baserr$renda_monetaria <= dec[2]], na.rm = T),
                                     PropFund = weighted.mean(x = baserr$Educ[baserr$renda_monetaria >= dec[1] &
                                                                                baserr$renda_monetaria <= dec[2]] == "Fund/Alfa",
                                                              w = baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                                      baserr$renda_monetaria <= dec[2]], na.rm = T),
                                     PropMedi = weighted.mean(x = baserr$Educ[baserr$renda_monetaria >= dec[1] &
                                                                                baserr$renda_monetaria <= dec[2]] == "Medio",
                                                              w = baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                                      baserr$renda_monetaria <= dec[2]], na.rm = T),
                                     PropSupe = weighted.mean(x = baserr$Educ[baserr$renda_monetaria >= dec[1] &
                                                                                baserr$renda_monetaria <= dec[2]] == "Superior",
                                                              w = baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                                      baserr$renda_monetaria <= dec[2]], na.rm = T)))
  
}

# Atributir o nome
rendimento.med$Decil <- names(decis)

### Participacao grupo por decil ###

participacao <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis) {
      
      participacao <- rbind(participacao,
                            data.frame(Genero = gen,
                                       Raca = paste(rc, collapse = "-"),
                                       Decil = paste(dec, collapse = "-"),
                                       Part = sum(baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                      baserr$renda_monetaria <= dec[2] &
                                                                      baserr$V0404 == gen & 
                                                                      (baserr$V0405 %in% rc)])/sum(baserr$peso_final[baserr$renda_monetaria >= dec[1] &
                                                                                                                       baserr$renda_monetaria <= dec[2]])))
      
    }
    
  }
  
}

# Atributir o nome
participacao$Genero <- c(rep("H",20), rep("M",20))
participacao$Raca <- c(rep(c(rep("B",10), rep("N",10)),2))
participacao$Decil <- names(decis)
participacao$Grupo <- paste0(participacao$Genero,"-", participacao$Raca)

# Composiçao demográfica média população

participacao.med <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    participacao.med <- rbind(participacao.med,
                              data.frame(Genero = gen,
                                         Raca = paste(rc, collapse = "-"),
                                         Part = sum(baserr$peso_final[baserr$V0404 == gen & 
                                                                        (baserr$V0405 %in% rc)])/sum(baserr$peso_final)))
    
  }
  
}

### Composicao demografica do topo ###

# Gerar lista com minimo e maximo dos quantis 90%, 99% e 99,9%
decis.topo <- lapply(lapply(as.list(c(paste0(sapply(c(.9, .99, .999), 
                                                    function(quantil) weighted.quantile(x = baserr$renda_irpfepof,
                                                                                        w = baserr$peso_final,
                                                                                        prob = quantil)),
                                             "-",
                                             rep(max(baserr$renda_irpfepof),3)))),
                            function(decil) strsplit(decil, "-")), 
                     function(elemento) as.numeric(elemento[[1]]))

names(decis.topo) <- c("10%", "1%", "0,1%")

### Participacao grupo por decil ###

participacao.topo <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis.topo) {
      
      participacao.topo <- rbind(participacao.topo,
                                 data.frame(Genero = gen,
                                            Raca = paste(rc, collapse = "-"),
                                            Decil = paste(dec, collapse = "-"),
                                            Part = sum(baserr$peso_final[baserr$renda_irpfepof >= dec[1] &
                                                                           baserr$renda_irpfepof <= dec[2] &
                                                                           baserr$V0404 == gen & 
                                                                           (baserr$V0405 %in% rc)])/
                                              sum(baserr$peso_final[baserr$renda_irpfepof >= dec[1] &
                                                                      baserr$renda_irpfepof <= dec[2]])))
                            
      
    }
    
  }
  
}

# Atributir o nome
participacao.topo$Genero <- c(rep("H",6), rep("M",6))
participacao.topo$Raca <- c(rep(c(rep("B",3), rep("N",3)),2))
participacao.topo$Decil <- names(decis.topo)
participacao.topo$Grupo <- paste0(participacao.topo$Genero,"-", participacao.topo$Raca)

# Gerar lista com minimo e maximo dos quantis 90%, 99% e 99,9% por grupo 
decis.topo <- lapply(lapply(as.list(c(paste0(sapply(c(.9, .99, .999), 
                                                    function(quantil) weighted.quantile(x = baserr$renda_irpfepof,
                                                                                        w = baserr$peso_final,
                                                                                        prob = quantil)),
                                             "-",
                                             rep(max(baserr$renda_irpfepof),3)))),
                            function(decil) strsplit(decil, "-")), 
                     function(elemento) as.numeric(elemento[[1]]))

names(decis.topo) <- c("10%", "1%", "0,1%")

### Participacao do 1% do topo no total da renda do grupo ###

# Gerar lista com minimo e maximo dos quantis 90%, 99% e 99,9% por grupo 
decis.topo.grupo <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in c(.9, .99, .999)) {
      
      decis.topo.grupo <- rbind(decis.topo.grupo,
                                data.frame(Genero = gen,
                                           Raca = paste(rc, collapse = "-"),
                                           Decil = dec,
                                           Renda = weighted.quantile(x = baserr$renda_irpfepof[baserr$V0404 == gen & (baserr$V0405 %in% rc)],
                                                                     w = baserr$peso_final[baserr$V0404 == gen & (baserr$V0405 %in% rc)],
                                                                     prob = dec)))
      
    }
    
  }
  
}

# Atributir o nome
decis.topo.grupo$Genero <- c(rep("H",6), rep("M",6))
decis.topo.grupo$Raca <- c(rep(c(rep("B",3), rep("N",3)),2))
decis.topo.grupo$Decil <- names(decis.topo)
decis.topo.grupo$Grupo <- paste0(decis.topo.grupo$Genero,"-", decis.topo.grupo$Raca)

# Calcular a concentracao dentro do grupo

criterios <- list("HB" = list("Genero" = genero[1],
                                "Raca" = raca[[1]],
                                "Quantis" = decis.topo.grupo$Renda[1:3]),
                  "HN" = list("Genero" = genero[1],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.topo.grupo$Renda[4:6]),
                  "MB" = list("Genero" = genero[2],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.topo.grupo$Renda[7:9]),
                  "MN" = list("Genero" = genero[2],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.topo.grupo$Renda[10:12]))

apropriacao.topo.grupo <- data.frame()

for (grupo in criterios) {
  
  for (decil in grupo$Quantis) {
  
    apropriacao.topo.grupo <- rbind(apropriacao.topo.grupo,
                                   data.frame(Genero = grupo$Genero,
                                              Raca = paste0(grupo$Raca, collapse = "-"),
                                              Quantil = decil,
                                              Part = sum(baserr$renda_irpfepof[baserr$renda_irpfepof >= decil &
                                                                                 baserr$renda_irpfepof <= max(baserr$renda_irpfepof) &
                                                                                 baserr$V0404 == grupo$Genero & 
                                                                                 (baserr$V0405 %in% grupo$Raca)]*
                                                           baserr$peso_final[baserr$renda_irpfepof >= decil &
                                                                               baserr$renda_irpfepof <= max(baserr$renda_irpfepof) &
                                                                               baserr$V0404 == grupo$Genero & 
                                                                               (baserr$V0405 %in% grupo$Raca)])/
                                                sum(baserr$renda_irpfepof[baserr$V0404 == grupo$Genero & 
                                                                            (baserr$V0405 %in% grupo$Raca)]*
                                                      baserr$peso_final[baserr$V0404 == grupo$Genero & 
                                                                          (baserr$V0405 %in% grupo$Raca)])))
  
  }
  
}

# Atributir o nome
apropriacao.topo.grupo$Genero <- c(rep("H",6), rep("M",6))
apropriacao.topo.grupo$Raca <- c(rep(c(rep("B",3), rep("N",3)),2))
apropriacao.topo.grupo$Quantil <- names(decis.topo)
apropriacao.topo.grupo$Grupo <- paste0(apropriacao.topo.grupo$Genero,"-", apropriacao.topo.grupo$Raca)

### Apropriacao da renda pelo topo por grupo ###

apropriacao.topo <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in decis.topo) {
      
      apropriacao.topo <- rbind(apropriacao.topo,
                                data.frame(Genero = gen,
                                           Raca = paste(rc, collapse = "-"),
                                           Decil = paste(dec, collapse = "-"),
                                           Apro = sum(baserr$renda_irpfepof[baserr$renda_irpfepof >= dec[1] &
                                                                              baserr$V0404 == gen & 
                                                                              (baserr$V0405 %in% rc)]*
                                                        baserr$peso_final[baserr$renda_irpfepof >= dec[1] &
                                                                            baserr$V0404 == gen & 
                                                                            (baserr$V0405 %in% rc)])/
                                             sum(baserr$peso_final*baserr$renda_irpfepof)))
                           
      
    }
    
  }
  
}

# Atributir o nome
apropriacao.topo$Genero <- c(rep("H",nrow(apropriacao.topo)/2), rep("M",nrow(apropriacao.topo)/2))
apropriacao.topo$Raca <- c(rep(c(rep("B",(nrow(apropriacao.topo)/4)), rep("N",(nrow(apropriacao.topo)/4))),2))
apropriacao.topo$Decil <- names(decis.topo)
apropriacao.topo$Grupo <- paste0(apropriacao.topo$Genero,"-", apropriacao.topo$Raca)

# Gerar lista com minimo e maximo dos decis por grupo
decis.grupo <- data.frame()

for (gen in genero) {
  
  for(rc in raca) {
    
    for(dec in seq(0, .9, .1)) {
      
      decis.grupo <- rbind(decis.grupo,
                                data.frame(Genero = gen,
                                           Raca = paste(rc, collapse = "-"),
                                           Decil = dec,
                                           Renda = weighted.quantile(x = baserr$renda_irpfepof[baserr$V0404 == gen & (baserr$V0405 %in% rc)],
                                                                     w = baserr$peso_final[baserr$V0404 == gen & (baserr$V0405 %in% rc)],
                                                                     prob = dec)))
      
    }
    
    decis.grupo <- rbind(decis.grupo,
                         data.frame(Genero = gen,
                                    Raca = paste(rc, collapse = "-"),
                                    Decil = "Max",
                                    Renda = max(baserr$renda_irpfepof[baserr$V0404 == gen & (baserr$V0405 %in% rc)])))
    
  }
  
}

# Atributir o nome
decis.grupo$Genero <- c(rep("H",22), rep("M",22))
decis.grupo$Raca <- c(rep(c(rep("B",11), rep("N",11)),2))
decis.grupo$Decil <- c(paste(seq(0, .9, .1)), "Max")
decis.grupo$Grupo <- paste0(decis.grupo$Genero,"-", decis.grupo$Raca)

# Calcular a concentracao dentro do grupo

criterios <- list("HB" = list("Genero" = genero[1],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.grupo$Renda[1:11]),
                  "HN" = list("Genero" = genero[1],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.grupo$Renda[12:22]),
                  "MB" = list("Genero" = genero[2],
                              "Raca" = raca[[1]],
                              "Quantis" = decis.grupo$Renda[23:33]),
                  "MN" = list("Genero" = genero[2],
                              "Raca" = raca[[2]],
                              "Quantis" = decis.grupo$Renda[34:44]))

apropriacao.grupo <- data.frame()

for (grupo in criterios) {
  
  for (decil in seq_along(grupo$Quantis)[-11]) {
    
    apropriacao.grupo <- rbind(apropriacao.grupo,
                                    data.frame(Genero = grupo$Genero,
                                               Raca = paste0(grupo$Raca, collapse = "-"),
                                               Quantil = decil,
                                               Apro = sum(baserr$renda_irpfepof[baserr$renda_irpfepof >= grupo$Quantis[decil] &
                                                                                  baserr$renda_irpfepof <= grupo$Quantis[decil+1] &
                                                                                  baserr$V0404 == grupo$Genero & 
                                                                                  (baserr$V0405 %in% grupo$Raca)]*
                                                            baserr$peso_final[baserr$renda_irpfepof >= grupo$Quantis[decil] &
                                                                                baserr$renda_irpfepof <= grupo$Quantis[decil+1] &
                                                                                baserr$V0404 == grupo$Genero & 
                                                                                (baserr$V0405 %in% grupo$Raca)])/
                                                 sum(baserr$renda_irpfepof[baserr$V0404 == grupo$Genero & 
                                                                             (baserr$V0405 %in% grupo$Raca)]*
                                                       baserr$peso_final[baserr$V0404 == grupo$Genero & 
                                                                           (baserr$V0405 %in% grupo$Raca)])))
    
  }
  
}

# Atributir o nome
apropriacao.grupo$Genero <- c(rep("H",20), rep("M",20))
apropriacao.grupo$Raca <- c(rep(c(rep("B",10), rep("N",10)),2))
apropriacao.grupo$Grupo <- paste0(apropriacao.grupo$Genero,"-", apropriacao.grupo$Raca)

# Gráficos ----------------------------------------------------------------

### Participacao de cada grupo por decil de renda ###

# Salvar o grafico a ser feito a seguir
png("CompDemoDecil.png", width = 4800, height = 3200, res = 300)

# Ajuste das margens para caber a legenda
par(mar=c(5, 5, 5, 11), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- matrix(participacao$Part[c(1:10,21:30,11:20,31:40)], nrow = 4, ncol = 10, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       1:10)) * 100

info.grafico <- cbind("Pop." = participacao.med$Part[c(1,3,2,4)]*100, info.grafico)

barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,100),
        space = 0.04,
        font.axis = 2, 
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Decil e total população",
        ylab = "Composição demográfica dos decis de renda (em %)")

legend("right", 
       inset=c(-.17, 0), 
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()

### Participacao de cada grupo dentro dos quantis do topo ###

png("CompDemoTopo.png", width = 4800, height = 3200, res = 300)

par(mar=c(5, 5, 5, 11), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- matrix(participacao.topo$Part[c(1:3,7:9,4:6,10:12)], nrow = 4, ncol = 3, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       c("10%", "1%", "0.1%"))) * 100

info.grafico <- cbind("Pop." = participacao.med$Part[c(1,3,2,4)]*100, info.grafico)

barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,100),
        space = 0.1,
        font.axis = 2,
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Quantis",
        ylab = "Composição demográfica dos quantis de renda (em %)")

legend("right", 
       inset=c(-.17, 0),
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()

# # Acrescentar cores na tabela de participacao.med
# 
# participacao.med$Cor <- c("#45ff66","#3366FF","#EB52FF","#FEFF41")
# participacao.med$PartAcum <- cumsum(participacao.med$Part)
# 
# for (i in 1:3) abline(h = participacao.med$PartAcum[i]*100,
#                       col = participacao.med$Cor[-1][i], 
#                       lwd = 3)

### Apropriacao da renda ###

png("ApropDecil.png", width = 4800, height = 3200, res = 300)

# Ajuste das margens para caber a legenda
par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- matrix(apropriacao$Apro[c(1:10,21:30,11:20,31:40)], nrow = 4, ncol = 10, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       1:10)) * 100

barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,60),
        space = 0.04,
        font.axis = 2,
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Decil",
        ylab = "Apropriação da renda total (em %)")

legend("topleft", 
       inset=c(.1, 0),
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()

### Apropriacao da renda pelo topo ###

png("ApropTopo.png", width = 4800, height = 3200, res = 300)

par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(1,1))

info.grafico <- cbind("0 - 90%" = rowSums(info.grafico[,-ncol(info.grafico)]),
                      matrix(apropriacao.topo$Apro[c(1:3,7:9,4:6,10:12)], nrow = 4, ncol = 3, byrow = T,
                             dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                               "Homens negros", "Mulheres negras"),
                                             c("10%", "1%", "0.1%"))) * 100)
                       

barplot(info.grafico,
        col = c("#45ff66","#EB52FF","#3366FF","#FEFF41"),
        border = "white",
        ylim = c(0,60),
        space = 0.04,
        font.axis = 2,
        cex.axis = 1.5, 
        cex.names = 1.5,
        cex.lab=1.5,
        xlab = "Quantil",
        ylab = "Apropriação da renda total pelo topo (em %)")

legend("topright", 
       inset=c(.1, 0),
       cex = 1.5,
       legend = c("Mulheres negras", "Homens negros", "Mulheres brancas", "Homens brancos"),
       fill = rev(c("#45ff66","#EB52FF","#3366FF","#FEFF41")), 
       bty = "n")

dev.off()

### Apropriacao da renda no topo de cada grupo ###

png("ApropGrupoTopo.png", width = 4800, height = 3200, res = 300)

par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(2,2))

info.grafico <- matrix(apropriacao.topo.grupo$Part[c(1:3,7:9,4:6,10:12)], nrow = 4, ncol = 3, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       c("10%", "1%", "0.1%"))) * 100

cores <- c("#45ff66","#EB52FF","#3366FF","#FEFF41")
legenda <- c("Homens brancos", "Mulheres brancas", "Homens negros", "Mulheres negras")

for(i in 1:4) {
  
  barplot(info.grafico[i,],
          col = cores[i],
          border = "white",
          ylim = c(0,70),
          beside = T,
          font.axis = 2,
          cex.axis = 1.5, 
          cex.names = 1.5,
          cex.lab=1.5,
          xlab = "Quantis do topo",
          ylab = "%")
  
  text(x = seq(.7, 3.1, length.out = 3),
       y = info.grafico[i,]+3,
       labels = round(info.grafico[i,], digits = 0), font = 2)

  text(x = 2.9,
       y = 60,
       labels = legenda[i], font = 2, cex = 1.5)
  
}

dev.off()

### Apropriacao da renda de cada grupo demográfico por decil ###

png("ApropGrupoDecil.png", width = 4800, height = 3200, res = 300)

par(mar=c(5, 5, 5, 5), xpd=TRUE)
par(mfrow = c(2,2))

info.grafico <- matrix(apropriacao.grupo$Apro[c(1:10,21:30,11:20,31:40)], nrow = 4, ncol = 10, byrow = T,
                       dimnames = list(c("Homens brancos", "Mulheres brancas", 
                                         "Homens negros", "Mulheres negras"),
                                       1:10)) * 100

cores <- c("#45ff66","#EB52FF","#3366FF","#FEFF41")
legenda <- c("Homens brancos", "Mulheres brancas", "Homens negros", "Mulheres negras")

for(i in 1:4) {
  
  barplot(info.grafico[i,],
          col = cores[i],
          border = "white",
          ylim = c(0,65),
          beside = T,
          font.axis = 2,
          cex.axis = 1.5, 
          cex.names = 1.5,
          cex.lab=1.5,
          xlab = "Decis",
          ylab = "%")
  
  text(x = seq(.7, 11.5, length.out = 10),
       y = info.grafico[i,]+3,
       labels = round(info.grafico[i,], digits = 0), font = 2)
  
  text(x = 1.9,
       y = 40,
       labels = legenda[i], font = 2, cex = 1.5)
  
}

dev.off()

# Gini --------------------------------------------------------------------

### Gini por grupo ###

gini.grupo <- aggregate(x = baserr$renda_irpfepof,
                        by = list("Raca" = baserr$Negros, "Genero" = baserr$Mulheres),
                        FUN = weighted.gini, w = baserr$peso_final)

weighted.gini(x = baserr$renda_irpfepof[!baserr$Negros], w = baserr$peso_final[!baserr$Negros])
