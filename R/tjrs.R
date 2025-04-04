#' Buscar jurisprudência no Tribunal de Justiça do RS
#'
#' Esta função permite buscar jurisprudência no Portal de Jurisprudência do Tribunal de Justiça do Estado do Rio Grande do Sul (TJRS) com base no período de julgamento.
#'
#' @param julgamento_inicial Data de julgamento inicial das decisões a serem buscadas (no formato "DD/MM/YYYY"). O padrão é "" (vazio), indicando que não há restrição de data inicial.
#' @param julgamento_final Data de julgamento final das decisões a serem buscadas (no formato "DD/MM/YYYY"). O padrão é "" (vazio), indicando que não há restrição de data final.
#' @return Uma string JSON contendo um array com as informações sobre as decisões encontradas (originalmente no campo 'docs' da resposta da API).
#'
#' @importFrom glue glue
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom purrr map list_flatten rate_delay slowly
#' @importFrom httr POST content user_agent config
#' @importFrom curl curl_escape
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Buscar jurisprudência tjrs
#' tjrs <- tjrs_jurisprudencia(julgamento_inicial = "01/01/2023", julgamento_final = "31/03/2023")
#' tjrs |> head(5)
#'
#' # Caso ele não encontre nada, mostrará e um aviso e retornará um valor NULL
#' tjrs_jurisprudencia(julgamento_inicial = "01/01/2023", julgamento_final = "31/02/2023")
#' }
tjrs_jurisprudencia <- function(julgamento_inicial = "", julgamento_final = "") {
  # Create log pattern
  pattern <- glue::glue("[{julgamento_inicial}-{julgamento_final}] ")

  message(paste0(pattern, "Iniciando busca de jurisprudência no TJRS...")) # Log start

  url <- "https://www.tjrs.jus.br/buscas/jurisprudencia/ajax.php"

  pagina <- 1
  dt_julgamento_de <- curl::curl_escape(julgamento_inicial)
  dt_julgamento_ate <- curl::curl_escape(julgamento_final)

  parametros <- list(
    "action" = "consultas_solr_ajax",
    "metodo" = "buscar_resultados",
    "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual=1&q_palavra_chave=&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={dt_julgamento_de}&data_julgamento_ate={dt_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
  )

  message(paste0(pattern, "Realizando consulta inicial para obter o número de páginas..."))
  res <- httr::POST(
    url = url,
    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0"),
    body = parametros,
    httr::config(ssl_verifypeer = FALSE)
  )

  if(res$status_code != 200){
    message(paste0(pattern, "Erro ", res$status_code, " ao acessar o portal de jurisprudencia do TJRS na consulta inicial.")) # Log error with status code
    return(NULL)
  }

  conteudo <- httr::content(res, as = "text") |> jsonlite::fromJSON()

  total_resultados <- conteudo$response$numFound
  if (is.null(total_resultados) || total_resultados == 0) { # Check explicitly for 0 results too
    message(paste0(pattern, "Nenhuma decisao encontrada para os critérios informados.")) # Log no results
    return(NULL)
  }

  n_paginas <- ceiling(total_resultados / 10)
  message(paste0(pattern, "Total de resultados encontrados: ", total_resultados))
  message(paste0(pattern, "Número total de páginas a serem baixadas: ", n_paginas)) # Log total pages

  # Use map to get a list of lists (each inner list is the 'docs' from a page)
  lista_docs <- purrr::map(1:n_paginas, purrr::slowly(~ {
    pagina_atual <- .x
    message(paste0(pattern, "Baixando página ", pagina_atual, " de ", n_paginas, "...")) # Log page download progress

    url <- "https://www.tjrs.jus.br/buscas/jurisprudencia/ajax.php"

    parametros <- list(
      "action" = "consultas_solr_ajax",
      "metodo" = "buscar_resultados",
      "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual={pagina_atual}&q_palavra_chave=&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={dt_julgamento_de}&data_julgamento_ate={dt_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
    )

    res_pagina <- httr::POST( # Renamed variable to avoid conflict
      url = url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0"),
      body = parametros,
      httr::config(ssl_verifypeer = FALSE)
    )

    # Optional: Add basic check for page request status
    if(res_pagina$status_code != 200){
       message(paste0(pattern, "Aviso: Falha ao buscar página ", pagina_atual, " (Status: ", res_pagina$status_code, "). Pulando esta página."))
       return(NULL) # Return NULL for this page if it fails
    }

    conteudo_pagina <- httr::content(res_pagina, as = "text") |> jsonlite::fromJSON() # Renamed variable

    return(conteudo_pagina$response$docs)
  }, purrr::rate_delay(5)))

  # Filter out NULL entries from failed page requests
  lista_docs <- Filter(Negate(is.null), lista_docs)

  if (length(lista_docs) == 0) {
      message(paste0(pattern, "Nenhum documento foi baixado com sucesso após processar as páginas."))
      return(NULL)
  }

  # Flatten the list of lists into a single list
  docs_combinados <- purrr::list_flatten(lista_docs)

  # Convert the combined list to a JSON string
  json_output <- jsonlite::toJSON(docs_combinados, auto_unbox = TRUE)

  message(paste0(pattern, "Busca de jurisprudência no TJRS concluída.")) # Log completion
  return(json_output)
}