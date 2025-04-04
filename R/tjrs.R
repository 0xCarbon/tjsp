#' Buscar jurisprudência no Tribunal de Justiça do RS
#'
#' Esta função permite buscar jurisprudência no Portal de Jurisprudência do Tribunal de Justiça do Estado do Rio Grande do Sul (TJRS) com base no período de julgamento.
#'
#' @param julgamento_inicial Data de julgamento inicial das decisões a serem buscadas (no formato "DD/MM/YYYY"). O padrão é "" (vazio), indicando que não há restrição de data inicial.
#' @param julgamento_final Data de julgamento final das decisões a serem buscadas (no formato "DD/MM/YYYY"). O padrão é "" (vazio), indicando que não há restrição de data final.
#' @param .reportar Se TRUE, exibe uma mensagem informando sobre os parâmetros da busca realizada. O padrão é TRUE.
#' @return Um data frame contendo informações sobre as decisões encontradas, incluindo número do processo, data do julgamento, relator, ementa e tribunal.
#'
#' @importFrom glue glue
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
tjrs_jurisprudencia <- function(julgamento_inicial = "", julgamento_final = "", .reportar = TRUE) {
  url <- "https://www.tjrs.jus.br/buscas/jurisprudencia/ajax.php"

  pagina <- 1
  dt_julgamento_de <- curl::curl_escape(julgamento_inicial)
  dt_julgamento_ate <- curl::curl_escape(julgamento_final)

  parametros <- list(
    "action" = "consultas_solr_ajax",
    "metodo" = "buscar_resultados",
    "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual=1&q_palavra_chave=&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={dt_julgamento_de}&data_julgamento_ate={dt_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
  )

  res <- httr::POST(
    url = url,
    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0"),
    body = parametros,
    httr::config(ssl_verifypeer = FALSE)
  )

  if(res$status_code != 200){
    cat("Erro ao acessar o portal de jurisprudencia do TJRS")
    return(NULL)
  }

  conteudo <- httr::content(res, as = "text") |> jsonlite::fromJSON()

  n_paginas <- ceiling(conteudo$response$numFound / 10)

  if (is.null(conteudo$response$numFound)) {
    glue::glue("Nenhuma decisao encontrada") |> print()
    return(NULL)
  }

  df <- purrr::map_dfr(1:n_paginas, purrr::slowly(~ {
    url <- "https://www.tjrs.jus.br/buscas/jurisprudencia/ajax.php"

    pagina <- .x

    parametros <- list(
      "action" = "consultas_solr_ajax",
      "metodo" = "buscar_resultados",
      "parametros" = glue::glue("aba=jurisprudencia&realizando_pesquisa=1&pagina_atual={pagina}&q_palavra_chave=&conteudo_busca=ementa_completa&filtroComAExpressao=&filtroComQualquerPalavra=&filtroSemAsPalavras=&filtroTribunal=-1&filtroRelator=-1&filtroOrgaoJulgador=-1&filtroTipoProcesso=-1&filtroClasseCnj=-1&assuntoCnj=-1&data_julgamento_de={dt_julgamento_de}&data_julgamento_ate={dt_julgamento_ate}&filtroNumeroProcesso=&data_publicacao_de=&data_publicacao_ate=&facet=on&facet.sort=index&facet.limit=index&wt=json&ordem=desc&start=0")
    )

    res <- httr::POST(
      url = url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0"),
      body = parametros,
      httr::config(ssl_verifypeer = FALSE)
    )

    conteudo <- httr::content(res, as = "text") |> jsonlite::fromJSON()

    return(conteudo$response$docs)
  }, purrr::rate_delay(5)))


  if (.reportar == TRUE){
  if (julgamento_inicial == "") {
    glue::glue(
      "Extrai do Portal de Jurisprudencia do Tribunal de Justica do Estado do Rio Grande do Sul todas as decisoes proferidas ate {julgamento_final}.") |> print()
  } else if (julgamento_final == ""){
    glue::glue(
      "Extrai do Portal de Jurisprudencia do Tribunal de Justica do Estado do Rio Grande do Sul todas as decisoes proferidas desde {julgamento_inicial}.") |> print()
  } else if (julgamento_final == "" & julgamento_inicial == "") {
    glue::glue(
      "Extrai do Portal de Jurisprudencia do Tribunal de Justica do Estado do Rio Grande do Sul todas as decisoes proferidas.") |> print()
  } else {
    glue::glue(
      "Extrai do Portal de Jurisprudencia do Tribunal de Justica do Estado do Rio Grande do Sul todas as decisoes proferidas entre {julgamento_inicial} e {julgamento_final}.") |> print()}}

  return(df)
}