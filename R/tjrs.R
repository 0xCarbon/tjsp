#' Buscar jurisprudência no Tribunal de Justiça do RS
#'
#' Esta função permite buscar jurisprudência no Portal de Jurisprudência do Tribunal de Justiça do Estado do Rio Grande do Sul (TJRS) com base no período de julgamento.
#'
#' @param julgamento_inicial Data de julgamento inicial das decisões a serem buscadas (no formato "DD/MM/YYYY"). O padrão é "" (vazio), indicando que não há restrição de data inicial.
#' @param julgamento_final Data de julgamento final das decisões a serem buscadas (no formato "DD/MM/YYYY"). O padrão é "" (vazio), indicando que não há restrição de data final.
#' @param delay Tempo de espera em segundos entre as requisições. O padrão é 5 segundos.
#' @param timeout_seconds Tempo máximo em segundos para esperar por uma resposta da requisição. O padrão é 60 segundos.
#' @param proxy_config Lista opcional contendo os detalhes da configuração do proxy. Deve conter os seguintes elementos nomeados: \code{hostname} (string), \code{port} (inteiro), \code{username} (string, opcional), \code{password} (string, opcional). O padrão é \code{NULL} (sem proxy).
#' @param max_retries Número máximo de tentativas para cada requisição em caso de falha. O padrão é 3 tentativas.
#' @param diretorio Diretório onde serão salvos os arquivos JSON. O padrão é "."
#' @return Invisível. Os resultados serão salvos em arquivos JSON no diretório especificado.
#'
#' @importFrom glue glue
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom purrr map list_flatten rate_delay slowly
#' @importFrom RCurl postForm curlOptions getURL getCurlHandle curlSetOpt curlEscape
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Buscar jurisprudência tjrs
#' tjrs_jurisprudencia(julgamento_inicial = "01/01/2023", julgamento_final = "31/03/2023")
#'
#' # Caso ele não encontre nada, mostrará e um aviso e retornará um valor NULL
#' tjrs_jurisprudencia(julgamento_inicial = "01/01/2023", julgamento_final = "31/02/2023")
#'
#' # Exemplo com proxy (substitua pelos seus detalhes)
#' proxy_details <- list(
#'   hostname = "your_proxy_host",
#'   port = 8080,
#'   username = "your_proxy_user", # Opcional
#'   password = "your_proxy_password" # Opcional
#' )
#' tjrs_with_proxy <- tjrs_jurisprudencia(
#'   julgamento_inicial = "01/01/2023",
#'   julgamento_final = "31/01/2023",
#'   proxy_config = proxy_details,
#'   diretorio = "tjrs_results"
#' )
#' }
tjrs_jurisprudencia <- function(julgamento_inicial = "", julgamento_final = "", delay = 5, timeout_seconds = 60, proxy_config = NULL, max_retries = 3, diretorio = ".") {
  # Create log pattern
  pattern <- glue::glue("[{julgamento_inicial}-{julgamento_final}] ")

  message(paste0(pattern, "Iniciando busca de jurisprudência no TJRS...")) # Log start
  
  # Verificar e criar diretório se não existir
  if (!dir.exists(diretorio)) {
    dir.create(diretorio, recursive = TRUE)
    message(paste0(pattern, "Diretório criado: ", diretorio))
  }

  # --- Helper function to format file names ---
  formatar_arquivo_tjrs <- function(pagina) {
    hora <- stringr::str_replace_all(Sys.time(), "\\D", "_")
    
    if (julgamento_inicial != "" && julgamento_final != "") {
      i <- stringr::str_replace_all(julgamento_inicial, "\\D", "_")
      f <- stringr::str_replace_all(julgamento_final, "\\D", "_")
      arquivo <- file.path(diretorio, paste0(hora, "_tjrs_inicio_", i, "_fim_", f, "_pagina_", pagina, ".json"))
    } else {
      arquivo <- file.path(diretorio, paste0(hora, "_tjrs_pagina_", pagina, ".json"))
    }
    
    return(arquivo)
  }

  # --- Função para processar e salvar uma página ---
  processar_e_salvar_pagina <- function(pagina_atual, conteudo_pagina) {
    if (!is.null(conteudo_pagina) && !is.null(conteudo_pagina$response) && !is.null(conteudo_pagina$response$docs)) {
      # Create filename
      arquivo <- formatar_arquivo_tjrs(pagina_atual)
      
      # Save docs to file
      jsonlite::write_json(
        conteudo_pagina$response$docs, 
        arquivo, 
        auto_unbox = TRUE,
        pretty = TRUE
      )
      
      return(arquivo)
    }
    return(NULL)
  }

  # --- Initialize Proxy Variables ---
  proxy_hostname <- NULL
  proxy_port     <- NULL
  proxy_username <- NULL
  proxy_password <- NULL
  use_proxy_config <- FALSE

  # --- Check for Proxy Config Parameter ---
  if (!is.null(proxy_config)) {
    if (is.list(proxy_config) &&
        !is.null(proxy_config$hostname) && nzchar(proxy_config$hostname) &&
        !is.null(proxy_config$port) && is.numeric(proxy_config$port) && proxy_config$port > 0) {

      proxy_hostname <- proxy_config$hostname
      proxy_port     <- as.integer(proxy_config$port)
      # Use $username and $password directly, they will be NULL if not present in the list
      proxy_username <- proxy_config$username
      proxy_password <- proxy_config$password

      use_proxy_config <- TRUE
      message(paste0(pattern, "Usando proxy da configuração: ", proxy_hostname, ":", proxy_port))

    } else {
      warning(paste0(pattern, "Configuração de proxy fornecida está incompleta ou inválida. Deve ser uma lista com 'hostname' (string não vazia) e 'port' (inteiro > 0). Procedendo sem proxy."))
    }
  } else {
      message(paste0(pattern, "Nenhuma configuração de proxy fornecida. Procedendo sem proxy."))
  }

  message(paste0(pattern, "Usando proxy da configuração: ", proxy_hostname, ":", proxy_port))
  # --- End Proxy Configuration ---

  url <- "https://www.tjrs.jus.br/buscas/jurisprudencia/ajax.php"

  pagina <- 1
  dt_julgamento_de <- RCurl::curlEscape(julgamento_inicial)
  dt_julgamento_ate <- RCurl::curlEscape(julgamento_final)

  parametros <- list(
    "action" = "consultas_solr_ajax",
    "metodo" = "buscar_resultados",
    "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual=1&q_palavra_chave=&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={dt_julgamento_de}&data_julgamento_ate={dt_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
  )

  # Create curl options
  curl_options <- RCurl::curlOptions(
    ssl.verifypeer = FALSE,
    timeout = as.integer(timeout_seconds),
    useragent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0",
    followlocation = TRUE,
    encoding = "latin1",
    cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
    httpheader = c(
      "Accept" = "application/json",
      "Content-Type" = "application/x-www-form-urlencoded"
    )
  )
  
  # Add proxy configuration if needed
  if (use_proxy_config) {
    proxy_url <- paste0(proxy_hostname, ":", proxy_port)
    # Add proxy to curl options
    curl_options$proxy <- proxy_url
    
    # Handle authentication if credentials are provided
    if (!is.null(proxy_username) && !is.null(proxy_password) && 
        nzchar(proxy_username) && nzchar(proxy_password)) {
      curl_options$proxyuserpwd <- paste0(proxy_username, ":", proxy_password)
    }
  }

  # Helper function for requests with retry and exponential backoff
  fazer_requisicao_com_retry <- function(url, parametros, opcoes, tentativa = 1) {
    if (tentativa > max_retries) {
      message(paste0(pattern, "Excedido o número máximo de tentativas (", max_retries, "). Desistindo."))
      return(NULL)
    }
    
    if (tentativa > 1) {
      backoff_delay <- delay * 2^(tentativa - 2)  # Exponential backoff (delay, delay*2, delay*4, ...)
      message(paste0(pattern, "Tentativa ", tentativa, " de ", max_retries, ". Aguardando ", backoff_delay, " segundos..."))
      Sys.sleep(backoff_delay)
    }
    
    tryCatch({
      RCurl::postForm(
        uri = url,
        .params = parametros,
        .opts = opcoes,
        style = "post"
      )
    }, error = function(e) {
      message(paste0(pattern, "Erro na tentativa ", tentativa, ": ", e$message))
      fazer_requisicao_com_retry(url, parametros, opcoes, tentativa + 1)
    })
  }

  # Wrap the POST request with retry logic
  res <- fazer_requisicao_com_retry(url, parametros, curl_options)
  
  # Check if the request returned NULL (error occurred)
  if(is.null(res)) {
    message(paste0(pattern, "Falha na conexão com o portal de jurisprudência do TJRS após ", max_retries, " tentativas."))
    return(invisible(NULL))
  }

  # Parse JSON response directly (remove hex decoding)
  conteudo <- tryCatch({
    # Convert raw bytes to character string if needed
    if(is.raw(res)) {
      res <- rawToChar(res)
    }
    jsonlite::fromJSON(res)
  }, error = function(e) {
    message(paste0(pattern, "Erro ao analisar a resposta JSON: ", e$message))
    # For debugging
    message(paste0(pattern, "Primeiros 100 caracteres da resposta: ", substr(as.character(res), 1, 100)))
    return(NULL)
  })
  
  if(is.null(conteudo)) {
    message(paste0(pattern, "Falha ao processar a resposta do portal de jurisprudência do TJRS."))
    return(invisible(NULL))
  }

  # Check if the response itself contains an error field
  if (!is.null(conteudo$error)) {
    message(paste0(pattern, "Erro retornado pela API do TJRS na consulta inicial. Detalhes: ", conteudo$error)) # Log API error
    return(invisible(NULL))
  }

  # Check for response structure validity
  if (is.null(conteudo$response) || is.null(conteudo$response$numFound)) {
     message(paste0(pattern, "Estrutura de resposta inesperada da API do TJRS na consulta inicial."))
     return(invisible(NULL))
  }

  total_resultados <- conteudo$response$numFound
  if (is.null(total_resultados) || total_resultados == 0) {
    message(paste0(pattern, "Nenhuma decisao encontrada para os critérios informados."))
    return(jsonlite::toJSON(list(), auto_unbox = TRUE))
  }

  n_paginas <- ceiling(total_resultados / 10)
  message(paste0(pattern, "Total de resultados encontrados: ", total_resultados))
  message(paste0(pattern, "Número total de páginas a serem baixadas: ", n_paginas))

  # Process and save first page results
  primeiro_arquivo <- processar_e_salvar_pagina(1, conteudo)
  message(paste0(pattern, "Página 1 salva em: ", primeiro_arquivo))
  
  arquivos_salvos <- c(primeiro_arquivo)
  
  # If there's only one page, return
  if (n_paginas <= 1) {
    message(paste0(pattern, "Busca de jurisprudência no TJRS concluída."))
    return(invisible(arquivos_salvos))
  }
  
  # Initialize vector to track failed pages
  failed_pages <- c()
  
  # Process remaining pages
  resultados_paginas <- purrr::map(2:n_paginas, purrr::slowly(~ {
    pagina_atual <- .x
    message(paste0(pattern, "Baixando página ", pagina_atual, " de ", n_paginas, "..."))

    parametros_pagina <- list(
      "action" = "consultas_solr_ajax",
      "metodo" = "buscar_resultados",
      "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual={pagina_atual}&q_palavra_chave=&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={dt_julgamento_de}&data_julgamento_ate={dt_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
    )

    # Use the retry function for page requests
    res_pagina <- fazer_requisicao_com_retry(url, parametros_pagina, curl_options)

    if(is.null(res_pagina)) {
      message(paste0(pattern, "Aviso: Falha ao buscar página ", pagina_atual, " após ", max_retries, " tentativas. Pulando esta página."))
      failed_pages <<- c(failed_pages, pagina_atual)
      return(NULL)
    }

    # Parse JSON response directly
    conteudo_pagina <- tryCatch({
      # Convert raw bytes to character string if needed
      if(is.raw(res_pagina)) {
        res_pagina <- rawToChar(res_pagina)
      }
      jsonlite::fromJSON(res_pagina)
    }, error = function(e) {
      message(paste0(pattern, "Erro ao analisar a resposta JSON: ", e$message))
      failed_pages <<- c(failed_pages, pagina_atual)
      return(NULL)
    })
      
    if(is.null(conteudo_pagina)) {
      message(paste0(pattern, "Falha ao processar a resposta do portal de jurisprudência do TJRS."))
      failed_pages <<- c(failed_pages, pagina_atual)
      return(NULL)
    }

    # Check if the page response contains an error field
    if (!is.null(conteudo_pagina$error)) {
      message(paste0(pattern, "Aviso: Erro retornado pela API do TJRS ao buscar página ", pagina_atual, ". Detalhes: ", conteudo_pagina$error,". Pulando esta página."))
      failed_pages <<- c(failed_pages, pagina_atual)
      return(NULL)
    }

    # Check for expected response structure on the page
    if (is.null(conteudo_pagina$response) || is.null(conteudo_pagina$response$docs)) {
      message(paste0(pattern, "Aviso: Estrutura de resposta inesperada da API do TJRS na página ", pagina_atual, ". Pulando esta página."))
      failed_pages <<- c(failed_pages, pagina_atual)
      return(NULL)
    }

    # Save to file and return the file path
    arquivo <- processar_e_salvar_pagina(pagina_atual, conteudo_pagina)
    arquivos_salvos <<- c(arquivos_salvos, arquivo)
    return(arquivo)
  }, purrr::rate_delay(as.integer(delay))))
  
  # Summary message
  message(paste0(pattern, "Download concluído. ", length(arquivos_salvos), " de ", n_paginas, 
                " páginas salvas no diretório: ", diretorio))
  
  if (length(failed_pages) > 0) {
    message(paste0(pattern, "As seguintes páginas falharam: ", paste(failed_pages, collapse = ", ")))
  }
  
  message(paste0(pattern, "Busca de jurisprudência no TJRS concluída."))
  return(invisible(arquivos_salvos))
}