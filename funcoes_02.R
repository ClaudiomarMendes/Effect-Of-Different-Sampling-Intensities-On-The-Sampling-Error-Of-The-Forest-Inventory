# carrega os pacotes necessarios ####

library(dplyr)
library(kimisc)
library(stringr)

# diminui a intensidade amotral de arvores normais ####

reducao.ia.arvores.normais <-
  function(data.frame, IA.Desejada, n.Minimo) {
    if (missing(data.frame))
      stop("Fornecer data frame a ser amostrado")
    if (missing(IA.Desejada))
      stop("Fornecer Intensidade Amostral (ha) desejada")
    if (missing(n.Minimo))
      stop("Fornecer o numero de minimo de arvores por parcela")
    
    normais.medidas <-
      subset(na.omit(data.frame), Categoria != "Dominante")
    
    hds <- subset(data.frame, Categoria == "Dominante")
    
    normais.n.medidas <-
      subset(data.frame, Categoria != "Dominante" &
               is.na(Ht.Observada))
    
    
    normais.medidas <- normais.medidas %>%
      group_by(Regional, Fazenda, Talhao, Parcela) %>%
      summarise(
        IA.Original  = n(),
        IA.Desejada = ceiling(IA.Desejada),
        n.Remover   = as.numeric(ifelse(
          IA.Desejada >= IA.Original,
          0,
          ifelse(
            IA.Desejada < n.Minimo,
            IA.Original - n.Minimo,
            IA.Original - IA.Desejada
          )
        ))
      ) %>% inner_join(normais.medidas)
    
    
    Arvore.NA <-
      function(data) {
        dados.reorganizados <-
          data[rev(order(data$Arvore)), ]
        
        dados.reorganizados$Ht.Observada[1:dados.reorganizados$n.Remover[1]] <-
          NA
        
        if (mean(data$n.Remover) == 0)
          dados.reorganizados <- data
        
        return(dados.reorganizados)
      }
    
    normais.medidas <-
      do.call(rbind, as.list(by(
        normais.medidas, normais.medidas[, c("Regional", "Fazenda", "Talhao", "Parcela")], Arvore.NA
      )))
    
    x <-
      normais.medidas %>% group_by(Regional, Fazenda, Talhao, Parcela) %>%
      summarise(
        IA.Original = mean(IA.Original),
        n.Remover = mean(n.Remover),
        IA.Desejada = mean(IA.Desejada)
      )
    
    hds <- x %>% inner_join(hds)
    
    normais.n.medidas <- x %>% inner_join(normais.n.medidas)
    
    y <- bind_rows(normais.medidas, normais.n.medidas, hds)
    
    
    return(y)
  }

# diminui a intensidade amotral de arvores dominantes ####

reducao.ia.arvores.dominantes <-
  function(data.frame, IA.Desejada, n.Minimo) {
    if (missing(data.frame))
      stop("Fornecer data frame a ser amostrado")
    if (missing(IA.Desejada))
      stop("Fornecer Intensidade Amostral (ha) desejada")
    if (missing(n.Minimo))
      stop("Fornecer o numero de minimo de arvores por parcela")
    
    hds <-
      subset(na.omit(data.frame), Categoria == "Dominante")
    
    normais <-
      subset(data.frame, Categoria != "Dominante")
    
    hds <- hds %>%
      group_by(Regional, Fazenda, Talhao, Parcela) %>%
      summarise(
        IA.Original  = n(),
        IA.Desejada = ceiling(IA.Desejada),
        n.Remover   = as.numeric(ifelse(
          IA.Desejada >= IA.Original,
          0,
          ifelse(
            IA.Desejada < n.Minimo,
            IA.Original - n.Minimo,
            IA.Original - IA.Desejada
          )
        ))
      ) %>% inner_join(hds)
    
    
    Arvore.NA <-
      function(data) {
        dados.reorganizados <-
          data[rev(order(data$Arvore)), ]
        
        dados.reorganizados$Ht.Observada[1:dados.reorganizados$n.Remover[1]] <-
          NA
        
        dados.reorganizados$Categoria[1:dados.reorganizados$n.Remover[1]] <-
          "Normal"
        
        if (mean(data$n.Remover) == 0)
          dados.reorganizados <- data
        
        return(dados.reorganizados)
      }
    
    hds <-
      do.call(rbind, as.list(by(hds, hds[, c("Regional", "Fazenda", "Talhao", "Parcela")], Arvore.NA)))
    
    x <-
      hds %>% group_by(Regional, Fazenda, Talhao, Parcela) %>%
      summarise(
        IA.Original = mean(IA.Original),
        n.Remover = mean(n.Remover),
        IA.Desejada = mean(IA.Desejada)
      )
    
    normais <- x %>% inner_join(normais)
    
    y <- bind_rows(hds, normais)
    
    return(y)
  }

# diminui a intensidade amotral de parcelas ####

reducao.ia.parcelas <-
  function(data.frame, IA.Desejada, n.Minimo) {
    if (missing(data.frame))
      stop("Fornecer data frame a ser amostrado")
    if (missing(IA.Desejada))
      stop("Fornecer Intensidade Amostral (ha) desejada")
    if (missing(n.Minimo))
      stop("Fornecer o numero de minimo de arvores por parcela")
    
    data.frame <- data.frame %>%
      group_by(Regional, Fazenda, Talhao) %>%
      summarise(
        IA.Original  = nlevels(factor(Parcela)),
        Area.Talhao = mean(Area.Talhao),
        IA.Desejada = round(Area.Talhao / IA.Desejada),
        n.Remover   = as.numeric(ifelse(
          IA.Desejada >= IA.Original,
          0,
          ifelse(
            IA.Desejada < n.Minimo,
            IA.Original - n.Minimo,
            IA.Original - IA.Desejada
          )
        ))
      ) %>% inner_join(data.frame)
    
    
    sem.reducao <- data.frame %>%
      mutate(Cod.Remover = "",
             Filtro.Remover.Parcela = FALSE)
    
    if (mean(data.frame$n.Remover) == 0)
      stop(return(sem.reducao))
    
    b <-
      subset(data.frame, n.Remover == 0) %>% mutate(Cod.Remover = "",
                                                    Filtro.Remover.Parcela = FALSE)
    
    c <- subset(data.frame, n.Remover != 0) %>%
      group_by(Regional, Fazenda, Talhao, Parcela) %>%
      summarise_at(vars(IA.Desejada, IA.Original, n.Remover), mean, na.rm =
                     T) %>%
      mutate(Parcela.Filtro = paste("\"", Parcela, "\"")) %>%
      group_by(Regional, Fazenda, Talhao) %>%
      do(sample.rows(., mean(.$n.Remover), replace = FALSE)) %>%
      summarise(
        IA.Original = mean(IA.Original),
        n.Remover   = mean(n.Remover),
        IA.Desejada = mean(IA.Desejada),
        Cod.Remover = paste(Parcela.Filtro, collapse = ",")
      ) %>%
      left_join(subset(data.frame, n.Remover != 0), .) %>%
      mutate(Parcela.Filtro = paste("\"", Parcela, "\"")) %>%
      group_by(Regional, Fazenda, Talhao) %>%
      mutate(Filtro.Remover.Parcela  = (!str_detect(
        Cod.Remover, as.character(Parcela.Filtro)
      ))) %>%
      filter(Filtro.Remover.Parcela)
    
    c$Parcela.Filtro <- NULL
    
    y <- bind_rows(b, c)
    
    return(y)
    
  }

# diminui o tamanho das parcelas ####

reducao.tamanho.parcela <-
  function(data.frame, Percentual.Retirada) {
    if (missing(data.frame))
      stop("Fornecer data frame a ser amostrado")
    if (missing(Percentual.Retirada))
      stop("Fornecer o percentual de retirada de área das parcelas")
    
    data.frame <- data.frame %>%
      group_by(Regional, Fazenda, Talhao, Parcela) %>%
      mutate(
        n.Filas = max(Fila),
        n.Covas = nlevels(factor(Arvore)),
        Area.Parcela.Original = Area.Parcela,
        Area.Cova = Area.Parcela.Original / n.Covas,
        Percentual.Area.Cova = 100 / n.Covas,
        n.Covas.Removiveis = round(Percentual.Retirada / Percentual.Area.Cova),
        n.Remover = n.Covas.Removiveis + n.Filas * (
          round(
            n.Covas.Removiveis / n.Filas - floor(n.Covas.Removiveis / n.Filas)
          ) - (
            n.Covas.Removiveis / n.Filas - floor(n.Covas.Removiveis / n.Filas)
          )
        ),
        Area.Parcela = Area.Parcela.Original - (n.Remover * Area.Cova)
      )
    
    if (mean(data.frame$n.Remover) == 0)
      stop(return(data.frame))
    
    data.frame.n.remover <-
      bind_rows(subset(data.frame, n.Remover == 0),
                subset(na.omit(subset(
                  data.frame, n.Remover != 0
                )))) %>%
      mutate(Cod.Remover = "",
             Filtro.Remover.Arvore = FALSE)
    
    data.frame.remover <-
      subset(subset(data.frame, n.Remover != 0),
             is.na(Cap) | is.na(Ht.Observada))
    
    data.frame.removidas <- data.frame.remover %>%
      group_by(Regional, Fazenda, Talhao, Parcela, Arvore) %>%
      summarise_at(vars(n.Remover), mean, na.rm =
                     T) %>%
      mutate(Arvore.Filtro = paste("\"", Arvore, "\"")) %>%
      group_by(Regional, Fazenda, Talhao, Parcela) %>%
      do(sample.rows(., mean(.$n.Remover), replace = FALSE)) %>%
      summarise(
        n.Remover   = mean(n.Remover),
        Cod.Remover = paste(Arvore.Filtro, collapse = ",")
      ) %>%
      left_join(data.frame.remover, .) %>%
      mutate(Arvore.Filtro = paste("\"", Arvore, "\"")) %>%
      group_by(Regional, Fazenda, Talhao, Parcela) %>%
      mutate(Filtro.Remover.Arvore  = (!str_detect(
        Cod.Remover, as.character(Arvore.Filtro)
      ))) %>%
      filter(Filtro.Remover.Arvore)
    
    data.frame.removidas$Arvore.Filtro <- NULL
    
    y <- bind_rows(data.frame.removidas, data.frame.n.remover)
    
    return(y)
    
  }

# estima a altura e o volume para o conjunto de dados em questao ####

f.ht.vol <- function(data.frame) {
  data.frame <- merge(
    data.frame,
    na.omit(data.frame) %>%
      group_by(Regional, Fazenda, Talhao, Parcela) %>%
      summarise(Hd = mean(
        subset(Ht.Observada, Categoria == "Dominante")
      )),
    by = c("Regional", "Fazenda", "Talhao", "Parcela")
  ) %>% mutate(
    Ln.Ht.Observada = log(Ht.Observada),
    Ln.Hd = log(Hd),
    Inv.Dap = 1 / Dap
  )
  
  data.frame <- merge(
    data.frame,
    na.omit(data.frame) %>%
      group_by(Regional, Fazenda, Talhao) %>%
      do(reg = lm(Ln.Ht.Observada ~ Inv.Dap + Ln.Hd, data = .)) %>%
      mutate(
        b0.ht = coef(reg)[1],
        b1.ht = coef(reg)[2],
        b2.ht = coef(reg)[3],
        r2.ht = summary(reg)[[9]],
        e.ht  = summary(reg)[[6]]
      ) %>%
      select(-reg),
    by = c("Regional", "Fazenda", "Talhao")
  )
  
  ht.estimada <-
    round(
      exp(
        data.frame$b0.ht + data.frame$b1.ht * data.frame$Inv.Dap + data.frame$b2.ht * data.frame$Ln.Hd
      ),
      digits = 2
    )
  
  data.frame <- data.frame %>% mutate(Ht =
                                        as.numeric(ifelse(
                                          !is.na(Ht.Observada), Ht.Observada, ht.estimada
                                        )),
                                      
                                      Volume =
                                        exp(b0.v + b1.v * log(Dap) + b2.v * log(Ht)))
  
  return(data.frame)
}

# calcula o erro amostral ####

e.amostral <- function(data.frame, Tipo.Populacao, alpha) {
  if (missing(data.frame))
    stop("Fornecer data frame a ser amostrado")
  if (missing(Tipo.Populacao))
    stop("Informar o tipo de população em questão")
  if (missing(alpha))
    stop("Fornecer o alpha para o calculo do erro amostral")
  
  data.frame <- subset(data.frame, Cap != "NA")
  
  data.frame <- data.frame %>%
    group_by(Regional, Fazenda, Talhao) %>%
    summarise(Area.Talhao = mean(Area.Talhao)) %>%
    group_by(Regional, Fazenda) %>%
    summarise(Area.Fazenda = sum(Area.Talhao)) %>%
    inner_join(data.frame)
  
  data.frame <- data.frame %>%
    group_by(Regional, Fazenda, Talhao, Parcela) %>%
    summarise(
      Volume.m3.Parcela = sum(Volume),
      Area.Parcela = mean(Area.Parcela),
      Volume.ha.Parcela = Volume.m3.Parcela * 10000 / Area.Parcela,
      Area.Fazenda = mean(Area.Fazenda)
    ) %>%
    group_by(Regional, Fazenda) %>%
    summarise(
      Volume.Medio.m3.Fazenda = mean(Volume.m3.Parcela),
      Volume.Medio.ha.Fazenda = mean(Volume.ha.Parcela),
      Volume.Total.ha.Fazenda = Volume.Medio.ha.Fazenda * mean(Area.Fazenda),
      n = n(),
      N = mean(Area.Fazenda) * 10000 / mean(Area.Parcela),
      S2 = var(Volume.ha.Parcela, na.rm = T),
      S = sqrt(S2),
      Fator.Correcao.Pop.Fin.Inf =  as.numeric(ifelse(
        Tipo.Populacao == "Populacao Finita",
        sqrt(1 - n /
               N),
        ifelse(Tipo.Populacao == "Populacao Infinita", 1, stop(
          "Use Apenas Populacao Finita e Populacao Infinita para este argumento"
        ))
      )),
      SY = S / sqrt(n) * Fator.Correcao.Pop.Fin.Inf,
      t = qt(alpha / 2, df = n - 1, lower.tail = FALSE),
      E.m3.ha = SY * t,
      E.Percentual = 100 * (E.m3.ha / Volume.Medio.ha.Fazenda),
      LC.Inf.Volume.Medio.ha.Fazenda = Volume.Medio.ha.Fazenda * (1 - (E.Percentual /
                                                                         100)),
      LC.Sup.Volume.Medio.ha.Fazenda = Volume.Medio.ha.Fazenda * (1 + (E.Percentual /
                                                                         100)),
      LC.Inf.Volume.Total.ha.Fazenda = Volume.Total.ha.Fazenda * (1 - (E.Percentual /
                                                                         100)),
      LC.Sup.Volume.Total.ha.Fazenda = Volume.Total.ha.Fazenda * (1 + (E.Percentual /
                                                                         100))
    ) %>% inner_join(data.frame)
  
  
  return(data.frame)
  
}

# calcula o numero de parcelas original e o numero apos a reducao da intensidade ####

n.original.atual.parcelas <- function(data.frame) {
  data.frame <- data.frame %>%
    group_by(Regional, Fazenda, Talhao) %>%
    summarise(n.parcelas.original = mean(IA.Original)) %>%
    group_by(Regional, Fazenda) %>%
    summarise(n.original = sum(n.parcelas.original)) %>%
    inner_join(data.frame)
  
  data.frame <- data.frame %>%
    group_by(Regional, Fazenda, Talhao) %>%
    summarise(n.parcelas.atual = nlevels(factor(Parcela))) %>%
    group_by(Regional, Fazenda) %>%
    summarise(n.atual = sum(n.parcelas.atual)) %>%
    inner_join(data.frame)
  
  return(data.frame)
}

# calcula o numero de arvores dominantes original e o numero apos a reducao da intensidade ####

n.original.atual.dominantes <- function(data.frame) {
  data.frame <- data.frame %>%
    group_by(Regional, Fazenda, Talhao, Parcela) %>%
    summarise(n.dominantes.original = mean(IA.Original)) %>%
    group_by(Regional, Fazenda) %>%
    summarise(n.original = sum(n.dominantes.original)) %>%
    inner_join(data.frame)
  
  data.frame <-
    subset(na.omit(data.frame), Categoria == "Dominante") %>%
    group_by(Regional, Fazenda, Talhao, Parcela) %>%
    summarise(n.dominantes.atual = n()) %>%
    group_by(Regional, Fazenda) %>%
    summarise(n.atual = sum(n.dominantes.atual)) %>%
    inner_join(data.frame)
  
  return(data.frame)
}

# calcula o numero de arvores normais original e o numero apos a reducao da intensidade ####

n.original.atual.normais <- function(data.frame) {
  data.frame <- data.frame %>%
    group_by(Regional, Fazenda, Talhao, Parcela) %>%
    summarise(n.normais.original = mean(IA.Original)) %>%
    group_by(Regional, Fazenda) %>%
    summarise(n.original = sum(n.normais.original)) %>%
    inner_join(data.frame)
  
  data.frame <-
    subset(na.omit(data.frame), Categoria != "Dominante") %>%
    group_by(Regional, Fazenda, Talhao, Parcela) %>%
    summarise(n.normais.atual = n()) %>%
    group_by(Regional, Fazenda) %>%
    summarise(n.atual = sum(n.normais.atual)) %>%
    inner_join(data.frame)
  
  return(data.frame)
}

# emite o resultado final da reducao de arvores ou parcelas para uma intensidade amostral especifica ####

f.geral.reducao.ia <-
  function(n,
           funcao.reducao,
           funcao.n,
           data.frame,
           IA,
           n.Min,
           Tipo.Populacao,
           alpha) {
    data.frame.repeticoes <- vector("list", n)
    
    ifelse(
      funcao.reducao == "Reducao de Arvores Normais",
      funcao.reducao <- reducao.ia.arvores.normais,
      ifelse(
        funcao.reducao == "Reducao de Arvores Dominantes",
        funcao.reducao <- reducao.ia.arvores.dominantes,
        ifelse(
          funcao.reducao == "Reducao de Parcelas",
          funcao.reducao <-
            reducao.ia.parcelas,
          stop(
            "Use apenas 'Reducao de Arvores Normais', 'Reducao de Arvores Dominantes' ou 'Reducao de Parcelas' para este argumento"
          )
        )
      )
    )
    
    ifelse(
      funcao.n == "Numero de Arvores Normais",
      funcao.n <- n.original.atual.normais,
      ifelse(
        funcao.n == "Numero de Arvores Dominantes",
        funcao.n <- n.original.atual.dominantes,
        ifelse(
          funcao.n == "Numero de Parcelas",
          funcao.n <-
            n.original.atual.parcelas,
          stop(
            "Use apenas 'Numero de Arvores Normais', 'Numero de Arvores Dominantes' ou 'Numero de Parcelas' para este argumento"
          )
        )
      )
    )
    
    for (i in 1:n)
    {
      data.frame.repeticoes[[i]] <-
        suppressMessages(e.amostral(
          f.ht.vol(funcao.n(
            funcao.reducao(data.frame, IA, n.Min)
          )) %>%
            mutate(Amostra = paste("Amostra", i, "IA", IA, sep = "_")),
          Tipo.Populacao,
          alpha
        ))
    }
    
    data.frame.repeticoes <- bind_rows(data.frame.repeticoes)
    
    u <- data.frame.repeticoes %>%
      group_by(Amostra, Regional, Fazenda, Talhao, Parcela) %>%
      summarise(
        E.Percentual = mean(E.Percentual),
        LC.Inferior.Medio.ha = mean(LC.Inf.Volume.Medio.ha.Fazenda),
        Volume.Medio.ha.Fazenda = mean(Volume.Medio.ha.Fazenda),
        LC.Superior.Medio.ha = mean(LC.Sup.Volume.Medio.ha.Fazenda),
        LC.Inferior.Total.ha = mean(LC.Inf.Volume.Total.ha.Fazenda),
        Volume.Total.ha.Fazenda = mean(Volume.Total.ha.Fazenda),
        LC.Superior.Total.ha = mean(LC.Sup.Volume.Total.ha.Fazenda),
        n.Original = mean(n.original),
        n.Atual = mean(n.atual)
      ) %>%
      group_by(Regional, Fazenda) %>%
      summarise(
        IA = IA,
        E.Percentual = mean(E.Percentual),
        LC.Inferior.Medio.ha = mean(LC.Inferior.Medio.ha),
        Volume.Medio.ha = mean(Volume.Medio.ha.Fazenda),
        LC.Superior.Medio.ha = mean(LC.Superior.Medio.ha),
        LC.Inferior.Total.ha = mean(LC.Inferior.Total.ha),
        Volume.Total.ha = mean(Volume.Total.ha.Fazenda),
        LC.Superior.Total.ha = mean(LC.Superior.Total.ha),
        n.Original = mean(n.Original),
        n.Atual = mean(n.Atual)
      )
    
    return(u)
    
  }

# emite o resultado final da reducao de tamanho de parcelas para uma porcentagem de reducao especifica ####

f.geral.reducao.tamanho.parcela <-
  function(n,
           data.frame,
           Percentual.Retirada,
           Tipo.Populacao,
           alpha) {
    data.frame.repeticoes <- vector("list", n)
    
    for (i in 1:n)
    {
      data.frame.repeticoes[[i]] <-
        suppressMessages(e.amostral(
          f.ht.vol(
            reducao.tamanho.parcela(data.frame, Percentual.Retirada)
          ) %>%
            mutate(
              Amostra = paste(
                "Amostra",
                i,
                "Percentual retirado",
                Percentual.Retirada,
                sep = "_"
              )
            ),
          Tipo.Populacao,
          alpha
        ))
    }
    
    data.frame.repeticoes <- bind_rows(data.frame.repeticoes)
    
    u <- data.frame.repeticoes %>%
      group_by(Amostra, Regional, Fazenda, Talhao, Parcela) %>%
      summarise(
        E.Percentual = mean(E.Percentual),
        LC.Inferior.Medio.ha = mean(LC.Inf.Volume.Medio.ha.Fazenda),
        Volume.Medio.ha.Fazenda = mean(Volume.Medio.ha.Fazenda),
        LC.Superior.Medio.ha = mean(LC.Sup.Volume.Medio.ha.Fazenda),
        LC.Inferior.Total.ha = mean(LC.Inf.Volume.Total.ha.Fazenda),
        Volume.Total.ha.Fazenda = mean(Volume.Total.ha.Fazenda),
        LC.Superior.Total.ha = mean(LC.Sup.Volume.Total.ha.Fazenda),
        Area.Parcela.Original = mean(Area.Parcela.Original),
        Area.Parcela.Atual = mean(Area.Parcela)
      ) %>%
      group_by(Regional, Fazenda) %>%
      summarise(
        Percentual.Retirado = Percentual.Retirada,
        E.Percentual = mean(E.Percentual),
        LC.Inferior.Medio.ha = mean(LC.Inferior.Medio.ha),
        Volume.Medio.ha = mean(Volume.Medio.ha.Fazenda),
        LC.Superior.Medio.ha = mean(LC.Superior.Medio.ha),
        LC.Inferior.Total.ha = mean(LC.Inferior.Total.ha),
        Volume.Total.ha = mean(Volume.Total.ha.Fazenda),
        LC.Superior.Total.ha = mean(LC.Superior.Total.ha),
        Area.Amostrada.Original.m2 = sum(Area.Parcela.Original),
        Area.Amostrada.Atual.m2 = sum(Area.Parcela.Atual),
        Reducao.m2 = Area.Amostrada.Original.m2 - Area.Amostrada.Atual.m2
      )
    
    return(u)
    
  }

# emite o resultado final da reducao de tamanho de parcelas para varias porcentagens de reducao ####

resultados.reducao.tamanho.parcelas <-
  function(n.repeticoes,
           data,
           percentual.retirada.inicial,
           percentual.retirada.final,
           Tipo.Populacao,
           alpha.do.e.amostral) {
    if (percentual.retirada.inicial / 5 - floor(percentual.retirada.inicial /
                                                5) != percentual.retirada.inicial / 5 - percentual.retirada.inicial / 5)
      stop("Insira apenas valores divisíveis por 5")
    
    if (percentual.retirada.final / 5 - floor(percentual.retirada.final /
                                              5) != percentual.retirada.final / 5 - percentual.retirada.final / 5)
      stop("Insira apenas valores divisíveis por 5")
    
    percentual.retirada.inicial <- percentual.retirada.inicial - 5
    
    data.resultados <- vector("list", percentual.retirada.final)
    
    contador.final <- percentual.retirada.final
    contador.inicial <- percentual.retirada.inicial
    
    for (i in 1:((contador.final - contador.inicial) / 5))
    {
      percentual.retirada.inicial <- percentual.retirada.inicial + 5
      
      data.resultados[[i]] <-
        suppressMessages(
          f.geral.reducao.tamanho.parcela(
            n.repeticoes,
            data,
            percentual.retirada.inicial,
            Tipo.Populacao,
            alpha.do.e.amostral
          )
        )
    }
    
    data.resultados <- bind_rows(data.resultados)
    
    return(data.resultados)
  }

# emite o resultado final da reducao de arvores para varias intensidades amostrais ####

resultados.arvores <-
  function(funcao.reducao,
           funcao.n,
           data,
           ia.atual,
           ia.final,
           n.minimo.arvores.na.parcela,
           Tipo.Populacao,
           alpha.do.e.amostral) {
    data.resultados <- vector("list", ia.atual)
    
    ia.atual <- ia.atual + 1
    
    contador.final <- ia.final
    contador.atual <- ia.atual
    
    for (i in (contador.final + 1):contador.atual)
    {
      ia.atual <- ia.atual - 1
      data.resultados[[i]] <-
        suppressMessages(
          f.geral.reducao.ia(
            1,
            funcao.reducao,
            funcao.n,
            data,
            ia.atual,
            n.minimo.arvores.na.parcela,
            Tipo.Populacao,
            alpha.do.e.amostral
          )
        )
    }
    
    data.resultados <- bind_rows(data.resultados)
    
    return(data.resultados)
  }

# emite o resultado final da reducao de parcelas para varias intensidades amostrais ####

resultados.parcelas <-
  function(n.repeticoes,
           data,
           ia.atual,
           ia.final,
           n.minimo.parcelas.no.talhao,
           Tipo.Populacao,
           alpha.do.e.amostral) {
    data.resultados <- vector("list", ia.final)
    
    ia.atual <- ia.atual - 0.5
    
    contador.final <- ia.final
    contador.atual <- ia.atual
    
    for (i in 1:(contador.final * 2 - (contador.atual) * 2))
    {
      ia.atual <- ia.atual + 0.5
      data.resultados[[i]] <-
        suppressMessages(
          f.geral.reducao.ia(
            n.repeticoes,
            "Reducao de Parcelas",
            "Numero de Parcelas",
            data,
            ia.atual,
            n.minimo.parcelas.no.talhao,
            Tipo.Populacao,
            alpha.do.e.amostral
          )
        )
    }
    
    data.resultados <- bind_rows(data.resultados)
    
    return(data.resultados)
  }

# gera graficos de residuos do volume medio por ha a nivel de talhao ####

grafico.de.residuos <-
  function(data.frame,
           funcao.reducao,
           ia.desejada,
           n.minimo,
           porcentagem.reducao.tamanho.parcela,
           titulo,
           legenda,
           ylim) {
    ifelse(
      funcao.reducao == "Reducao de Arvores Normais",
      funcao.reducao <- reducao.ia.arvores.normais,
      ifelse(
        funcao.reducao == "Reducao de Arvores Dominantes",
        funcao.reducao <- reducao.ia.arvores.dominantes,
        ifelse(
          funcao.reducao == "Reducao de Parcelas",
          funcao.reducao <-
            reducao.ia.parcelas,
          stop(
            "Use apenas 'Reducao de Arvores Normais', 'Reducao de Arvores Dominantes' ou 'Reducao de Parcelas' para este argumento"
          )
        )
      )
    )
    
    ifelse(legenda == "Com Legenda", 
           legenda <- TRUE,
           ifelse(legenda == "Sem Legenda", 
                  legenda <- FALSE,
                  stop("Use Apenas 'Com Legenda' ou 'Sem Legenda' neste argumento")
           )
    )
    
    data.frame.reducao <-
      reducao.tamanho.parcela(data.frame, porcentagem.reducao.tamanho.parcela) %>%
      select(-n.Remover)
    
    data.frame.reducao <-
      f.ht.vol(funcao.reducao(data.frame.reducao, ia.desejada, n.minimo))
    
    data.frame <- f.ht.vol(data.frame)
    
    volume <- subset(data.frame, !is.na(Cap)) %>%
      group_by(Regional, Fazenda, Talhao, Parcela) %>%
      summarise(Vol.Referencia.ha = sum(Volume) * 10000 / mean(Area.Parcela)) %>%
      group_by(Regional, Fazenda, Talhao) %>%
      summarise(Vol.Referencia.ha = mean(Vol.Referencia.ha)) %>%
      inner_join(
        subset(data.frame.reducao, !is.na(Cap)) %>%
          
          group_by(Regional, Fazenda, Talhao, Parcela) %>%
          summarise(Vol.Reducao.ha = sum(Volume) *
                      10000 / mean(Area.Parcela)) %>%
          group_by(Regional, Fazenda, Talhao) %>%
          summarise(Vol.Reducao.ha = mean(Vol.Reducao.ha))
      ) %>%
      mutate(Residuos = ((Vol.Reducao.ha - Vol.Referencia.ha) / Vol.Referencia.ha) * 100)
    
    
    grafico <- ggplot(volume,
                      aes(x = Vol.Referencia.ha, y = Residuos)) +
      geom_point() +
      facet_grid(. ~ Regional) +
      geom_smooth(method = "lm", color = "black") +
      coord_cartesian(ylim = ylim) +
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold",
          vjust = 1
        ),
        axis.title = element_text(size = 12)
      ) +
      labs(
        x = "Volume (m3/ha)",
        y = "Residuos (%)",
        colour = "Fazenda",
        title = titulo
      ) +
      geom_point(
        size = 2.5,
        shape = 18,
        aes(color = Fazenda),
        show.legend = legenda
      )
    return(grafico)
    
  }

# gera estatisticas de ajuste hipsométrico por talhao ####

f.r2.e <-
  function(data.frame,
           funcao.reducao,
           ia.desejada,
           n.minimo,
           porcentagem.reducao.tamanho.parcela) {
    ifelse(
      funcao.reducao == "Reducao de Arvores Normais",
      funcao.reducao <- reducao.ia.arvores.normais,
      ifelse(
        funcao.reducao == "Reducao de Arvores Dominantes",
        funcao.reducao <- reducao.ia.arvores.dominantes,
        ifelse(
          funcao.reducao == "Reducao de Parcelas",
          funcao.reducao <-
            reducao.ia.parcelas,
          stop(
            "Use apenas 'Reducao de Arvores Normais', 'Reducao de Arvores Dominantes' ou 'Reducao de Parcelas' para este argumento"
          )
        )
      )
    )
    
    data.frame.reducao <-
      reducao.tamanho.parcela(data.frame, porcentagem.reducao.tamanho.parcela) %>%
      select(-n.Remover)
    
    data.frame.reducao <-
      f.ht.vol(funcao.reducao(data.frame.reducao, ia.desejada, n.minimo))
    
    data.frame <- f.ht.vol(data.frame)
    
    r2.e <- data.frame %>%
      group_by(Regional, Fazenda, Rotacao, Talhao) %>%
      summarise(r2.obs = mean(r2.ht),
                e.obs = mean(e.ht)) %>%
      inner_join(
        data.frame.reducao %>%
          group_by(Regional, Fazenda, Rotacao, Talhao) %>%
          summarise(r2.est = mean(r2.ht),
                    e.est = mean(e.ht))
      )
    
  }