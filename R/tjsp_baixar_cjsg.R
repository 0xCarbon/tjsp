#' Baixa consulta jurisprudencial do TJSP
#'
#' @param livre palavra ou texto a ser buscado nas ementas e nos acórdãos
#' @param ementa palavra ou texto a ser buscado apenas nas ementas
#' @param processo Número do processo
#' @param aspas lógico. Colocar a expressão entre aspas?
#' @param classe Código da classe processual
#' @param assunto Código do assunto
#' @param orgao_julgador Código do órgão julgador
#' @param inicio  data inicial julgamento
#' @param fim  Data final julgamento
#' @param inicio_pb data inicial registro/publicação
#' @param fim_pb    data final registr/publicacao
#' @param comarca Código da comarca. Uma por vez.
#' @param sg  "T" para origem segundo grau
#' @param cr  "R" para origem colégio recursal
#' @param tipo "A" Para acórdãos, "D" para decisões monocráticas
#' @param n Número de páginas
#' @param diretorio Diretório onde serão armazenadas as páginas html
#' @keywords tjsp,acórdãos
#'
#' @return baixa os htmls das decisões de segunda instância
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_baixar_cjsg(livre = "Lei Maria da Penha")
#' }
#'
tjsp_baixar_cjsg <-
  function(livre = "",
           ementa = "",
           processo = "",
           aspas = FALSE,
           classe = "",
           assunto = "",
           orgao_julgador = "",
           inicio = "",
           fim = "",
           inicio_pb = "",
           fim_pb = "",
           comarca = "",
           sg = "T",
           cr = "",
           tipo = "A",
           n = NULL,
           diretorio = ".") {

  data_errada <-   verificar_datas(inicio, fim, inicio_pb, fim_pb)

  if (data_errada){
    stop("Verifique se:
         Colocou as datas no formato esperado;
         informou somente publica\u00E7\u00E3o ou julgamento;
         colocou uma data inferior ou igual \u00E0 data de hoje.")

  }

    if(inicio != "" && fim != ""){

      datas <- agrupar_datas(inicio, fim,"anual")

      purrr::walk2(datas$data_inicial, datas$data_final, ~{
        tjsp_baixar_cjsg1(livre = livre,
                          ementa = ementa,
                          processo = processo,
                          aspas = aspas,
                          classe = classe,
                          assunto = assunto,
                          orgao_julgador = orgao_julgador,
                          inicio = .x,
                          fim = .y,
                          inicio_pb = inicio_pb,
                          fim_pb = fim_pb,
                          comarca = comarca,
                          sg = sg,
                          cr = cr,
                          tipo = tipo,
                          n = n,
                          diretorio = diretorio
        )
      })

    } else if(inicio_pb != "" && fim_pb != ""){

      datas <- agrupar_datas(inicio_pb, fim_pb)

      purrr::walk2(datas$data_inicial, datas$data_final, ~{

        tjsp_baixar_cjsg1(livre,
                          ementa,
                          processo,
                          aspas,
                          classe,
                          assunto,
                          orgao_julgador,
                          inicio = inicio,
                          fim = fim,
                          inicio_pb = .x,
                          fim_pb = .x,
                          sg = sg,
                          cr = cr,
                          tipo,
                          n,
                          diretorio
        )


      })

    } else {

      tjsp_baixar_cjsg1(livre = livre,
                        ementa = ementa,
                        processo = processo,
                        aspas  = aspas,
                        classe,
                        assunto,
                        orgao_julgador,
                        inicio = inicio,
                        fim = fim,
                        inicio_pb = inicio_pb,
                        fim_pb = fim_pb,
                        comarca = comarca,
                        sg = sg,
                        cr = cr,
                        tipo,
                        n,
                        diretorio)

    }

  message(paste0("Processo de busca jurisprudencial TJSP concluído com sucesso"))
}



#' Função para criar o nome do arquivo
#'
#' @param inicio  data inicial julgamento
#' @param fim  Data final julgamento
#' @param inicio_pb data inicial registro/publicação
#' @param fim_pb    data final registr/publicacao
#' @param tipo tipo de busca
#' @param pagina página
#' @param diretorio diretorio
#'
#' @return Arquivo
#'
formatar_arquivo <- function(inicio,
                            fim,
                            inicio_pb,
                            fim_pb,
                            tipo,
                            pagina,
                            diretorio){


  hora <- stringr::str_replace_all(Sys.time(), "\\D", "_")


  if (inicio != "" & fim != ""){

    i <- lubridate::dmy(inicio) %>%
      stringr::str_replace_all("\\D","_")

    f <- lubridate::dmy(fim) %>%
      stringr::str_replace_all("\\D","_")

    arquivo <- file.path(diretorio,paste0(hora,"_inicio_",i,"_fim_",f,"_pagina_",pagina,"_tipo_",tipo,".html"))



  } else  if (inicio_pb != "" & fim_pb != "") {

    i <- lubridate::dmy(inicio_pb) %>%
      stringr::str_replace_all("\\D","_")

    f <- lubridate::dmy(fim_pb) %>%
      stringr::str_replace_all("\\D","_")


    arquivo <- file.path(diretorio,paste0(hora,"_inicio_pb_",i,"_fim_pb_",f,"_pagina_",pagina,"_tipo_",tipo,".html"))

  } else {

    arquivo <- file.path(diretorio,paste0(hora,"_pagina_",pagina,"_tipo_",tipo,".html"))

  }

  return(arquivo)
}



#' Verifica se está tudo ok com as datas.
#'
#' @param inicio Data inicial de julgamento
#' @param fim Data final de julgamento
#' @param inicio_pb Data inicial de publicação
#' @param fim_pb Data final de publicação
#'
#' @return Retorna TRUE se tem algo errado.
#' @export
#'
verificar_datas <- function(inicio, fim, inicio_pb, fim_pb){

  ### Verifica se nenhuma data é seperior à data atual

  datas <- lubridate::dmy(inicio, fim, inicio_pb, fim_pb)

  x <- any(datas > Sys.Date(), na.rm = TRUE)

  ### Verifica se o usuário colocou data de publicação e data de julgamento.

  y <-  all(inicio != "", fim != "", inicio_pb != "", fim_pb != "")


  ### verifica se o usuário escreveu a data em formato errado.

  z <- any(c(inicio, fim, inicio_pb, fim_pb) |> stringr::str_detect("(\\d{2}/\\d{2}/\\d{4}|^$)", negate =T ))

  any(x,y,z)

  }


#' Qualquer data
#'
#' @inheritParams tjsp_baixar_cjsg
#'
#' @return htmls
#'

tjsp_baixar_cjsg1 <- function (livre = "", ementa = "", processo = "", classe = "",
          assunto = "", orgao_julgador = "", inicio = "", fim = "",
          inicio_pb = "", fim_pb = "",comarca = "", sg = "T", cr = "", tipo = "A",
          n = NULL, diretorio = ".", aspas = FALSE) {

  pattern <- paste0("[", ifelse(inicio != "", inicio, inicio_pb), "-", tipo, "] ")
  
  httr::set_config(httr::config(ssl_verifypeer = FALSE, accept_encoding = "latin1"))
  if (aspas == TRUE) livre <- deparse(livre)

  link_cjsg <- "https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do"

  body <- list(
    dados.buscaInteiroTeor = livre,
    dados.pesquisarComSinonimos = "S",
    dados.pesquisarComSinonimos = "S",
    dados.buscaEmenta = ementa,
    dados.nuProcOrigem = processo,
    dados.nuRegistro = "",
    agenteSelectedEntitiesList = "",
    contadoragente = "0",
    contadorMaioragente = "0",
    codigoCr = "",
    codigoTr = "",
    nmAgente = "",
    juizProlatorSelectedEntitiesList = "",
    contadorjuizProlator = "0",
    contadorMaiorjuizProlator = "0",
    codigoJuizCr = "",
    codigoJuizTr = "",
    nmJuiz = "",
    classesTreeSelection.values = classe,
    classesTreeSelection.text = "",
    assuntosTreeSelection.values = assunto,
    assuntosTreeSelection.text = "",
    comarcaSelectedEntitiesList = "",
    contadorcomarca = "1",
    contadorMaiorcomarca = "1",
    cdComarca = comarca[1],
    nmComarca = "",
    secoesTreeSelection.values = orgao_julgador,
    secoesTreeSelection.text = "",
    dados.dtJulgamentoInicio = inicio,
    dados.dtJulgamentoFim = fim,
    dados.dtRegistroInicio = inicio_pb,
    dados.dtRegistroFim = fim_pb,
    dados.origensSelecionadas = sg,
    dados.origensSelecionadas = cr,
    tipoDecisaoSelecionados = tipo,
    dados.ordenacao = "dtPublicacao"
  )

  # if (any(comarca != "")){
  #
  #   body$contadorcomarca <- length(comarca)
  #   body$contadorMaiorcomarca <- length(comarca)
  #
  #   for(i in 1:length(comarca)){
  #
  #     body <- append(body, comarca[i])
  #
  #     names(body)[length(body)]<- glue::glue("dadosComarca[{i}].cdComarca")
  #
  #   }

  #}

  response <- httr::POST(link_cjsg, encode = "form", body = body,
                         httr::accept("text/html; charset=latin1;"))

  if (response$status_code != 200) {
    stop(paste0(pattern, "Erro na requisição inicial. Status: ", response$status_code))
  }

  r1 <- httr::GET(paste0("https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=", tipo, "&pagina=1"),
                  httr::set_cookies(unlist(response$cookies)), httr::accept("text/html; charset=latin1;")
                  )

  if (r1$status_code != 200) {
    stop(paste0(pattern, "Erro ao acessar a primeira página. Status: ", r1$status_code))
  }

  # Check if no results were found
  no_results <- r1 |>
    httr::content() |>
    xml2::xml_text() |>
    stringr::str_detect("Não foi encontrado nenhum resultado correspondente à busca realizada.")
  
  if (no_results) {
    message(paste0(pattern, "Não foram encontrados resultados para a busca realizada."))
    return(invisible(NULL))
  }
  
  if (!is.null(n)) {
    paginas <- 1:n
    message(paste0(pattern, "Número de páginas definido pelo usuário: ", n))
  } else {
    max_pag <- r1 |>
      httr::content() |>
      xml2::xml_find_all(xpath = "//td[contains(., 'Resultados')]") |>
      xml2::xml_text(trim = T) |>
      stringr::str_extract("\\d+$") |>
      as.numeric() |>
      magrittr::divide_by(20) |>
      ceiling()

    paginas <- 1:max_pag
    message(paste0(pattern, "Número de páginas detectado: ", max_pag))
  }
  
  # Contador para o progresso
  total_paginas <- length(paginas)
  contador <- 0
  
  # Initialize vector to track failed pages
  failed_pages <- c()
  
  purrr::walk(paginas, ~{
    contador <<- contador + 1
    arquivo <- formatar_arquivo(inicio, fim, inicio_pb,
                                fim_pb, tipo, pagina = .x, diretorio)
    
    if (contador %% 5 == 0 || contador == 1 || contador == total_paginas) {
      message(paste0(pattern, "Baixando página ", .x, " de ", total_paginas, 
                     " (", round(contador/total_paginas*100), "%)"))
    }
    
    # Implement retry logic with progressive backoff
    success <- FALSE
    attempts <- 0
    max_attempts <- 3
    
    while (!success && attempts < max_attempts) {
      attempts <- attempts + 1
      backoff_time <- 2^attempts  # Progressive backoff: 2, 4, 8 seconds
      
      tryCatch({
        Sys.sleep(if(attempts == 1) 1 else backoff_time)
        response_pg <- httr::GET(paste0("https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=", tipo, "&pagina=", .x),
                  httr::set_cookies(unlist(response$cookies)), httr::write_disk(arquivo,
                                                                               overwrite = TRUE))
        
        if (response_pg$status_code == 200) {
          success <- TRUE
        } else {
          message(paste0(pattern, "Tentativa ", attempts, " falhou para página ", .x, ": status ", response_pg$status_code))
        }
      }, error = function(e) {
        message(paste0(pattern, "Erro na tentativa ", attempts, " para página ", .x, ": ", e$message))
      })
      
      if (!success && attempts < max_attempts) {
        message(paste0(pattern, "Tentando novamente em ", backoff_time, " segundos..."))
      }
    }
    
    if (!success) {
      message(paste0(pattern, "Falha ao baixar página ", .x, " após ", max_attempts, " tentativas. Continuando para a próxima página."))
      failed_pages <<- c(failed_pages, .x)
    }
  }, .progress = TRUE)
  
  message(paste0(pattern, "Download concluído. ", total_paginas - length(failed_pages), " de ", total_paginas, 
                " páginas baixadas para o diretório: ", diretorio))
  
  if (length(failed_pages) > 0) {
    message(paste0(pattern, "As seguintes páginas falharam: ", paste(failed_pages, collapse = ", ")))
  }
  
  # Return failed pages
  return(failed_pages)
}
