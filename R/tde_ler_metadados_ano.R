#' Lê tabelas de metadados das teses e dissertações
#'
#' @param arquivos Arquivos
#' @param diretorio Informar se não informar arquivos
#'
#' @return tibble
#' @export
#'
tde_ler_metadados_ano <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, pattern='html$',full.names = TRUE)

  }


  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    x <- .x %>%
         xml2::read_html()

    nome <- x %>%
          xml2::xml_find_all("//div[@class='dadosDocNome']/a") %>%
          xml2::xml_text()

    nome_url <- x %>%
      xml2::xml_find_all("//div[@class='dadosDocNome']/a") %>%
      xml2::xml_attr("href")

    titulo <- x %>%
          xml2::xml_find_all("//div[@class='dadosDocTitulo']") %>%
          xml2::xml_text() %>%
          .[-1]

    area <- x %>%
      xml2::xml_find_all("//div[@class='dadosDocArea']/a") %>%
      xml2::xml_text()

    area_url <- x %>%
      xml2::xml_find_all("//div[@class='dadosDocArea']/a") %>%
      xml2::xml_attr("href")


    documento <- x %>%
      xml2::xml_find_all("//div[@class='dadosDocTipo']/a") %>%
      xml2::xml_text()

    documento_url <- x %>%
      xml2::xml_find_all("//div[@class='dadosDocTipo']/a") %>%
      xml2::xml_attr("href") %>%
      paste0("https://teses.usp.br",.)



    unidade <- x %>%
      xml2::xml_find_all("//div[@class='dadosDocUnidade']/a") %>%
      xml2::xml_text()

    unidade_url <- x %>%
      xml2::xml_find_all("//div[@class='dadosDocUnidade']/a") %>%
      xml2::xml_attr("href") %>%
      paste0("https://teses.usp.br",.)

    ano <- x %>%
      xml2::xml_find_all("//div[@class='dadosDocAno']/a") %>%
      xml2::xml_text() %>%
      as.integer()

    ano_url <- x %>%
      xml2::xml_find_all("//div[@class='dadosDocAno']/a") %>%
      xml2::xml_attr("href") %>%
      paste0("https://teses.usp.br",.)

    tibble::tibble(nome,
                   nome_url,
                   titulo,
                   area,
                   area_url,
                   documento,
                   documento_url,
                   ano,
                   ano_url)

  },NULL))

}
