#' Get businesses data.frame
#'
#' @description
#' Creates a business data.frame from an owner page
#'
#' @details
#' It's a scraping function written with rvest and returns a basically all information available in that page, which demands some processing later.
#' To use our functions, it's advised to use Web Archive to save and backup that data. With that method, it's possible to follow a longitudinal study.
#' It works both with the direct links or with a downloaded html page.
#' In case of more than one page, we recommend lapply and bind_rows functions to tie these different pages.
#'
#'
#' @param path A path that leads to a Consultasocio page
#' @param cpf A character vector containing the cpf numbers available in the Consultasocio page
#' @return A business data.frame with many variables, basically following all information available in their page
#' @examples
#' CS_get_bus_df("https://web.archive.org/web/20211221092935/https://www.consultasocio.com/q/sa/arthur-moledo-do-val")
#' do.call(bind_rows, lapply(X = c("https://web.archive.org/web/20220607001536/https://www.consultasocio.com/q/sa/paulo-antonio-skaf",
#'                                 "https://web.archive.org/web/20220607002216/https://www.consultasocio.com/q/sa/paulo-antonio-skaf?page=2"
#'                                 ),
#'                           FUN = CS_get_bus_df,
#'                           cpf = "083628"
#'                           )
#'         )
#' @encoding UTF-8
#' @export
CS_get_bus_df <- function(path, cpf = NA, only_bus = T){
  busin <- rvest::read_html(path) %>%
    rvest::html_nodes(".cnpj p") %>% rvest::html_text()

  pos <- grep("CPF/CNPJ:", busin)

  if (length(pos) < 1){
    print(paste0(path, " is a blank page"))
    return(NA)
  }

  lista_empresas <- list()
  for (i in 1:length(pos)){
    if(i < length(pos)){
      lista_empresas[[i]] <- busin[pos[i]:(pos[i+1]-1)]} else {
        lista_empresas[[i]] <- busin[pos[i]:length(busin)]
      }
  }

  if (!is.na(cpf) && stringr::str_length(cpf) > 2){
    chos_cpf  <- logical(length(lista_empresas))
    for (i in 1:length(lista_empresas)){
      chos_cpf[i]  <- any(grepl(pattern = paste0("\\*\\*\\*", cpf, "\\*\\*"), x = lista_empresas[[i]]))
      }
    if (isFALSE(any(chos_cpf))){
      print(paste0(path, " CPF was not found"))
      return(NULL)
      }
    lista_empresas <- lista_empresas[chos_cpf]
    }

    for (i in 1:length(lista_empresas)){
      lista_empresas[[i]] <- lista_empresas[[i]][2:length(lista_empresas[[i]])]
    }

  lista_empresas <- lapply(X = lista_empresas, FUN = strsplit, split = ":")

  lista_empresas_df <- list()
  # Maybe transform into a function to call it in here
  for (i in 1:length(lista_empresas)){
    values_vector <- character()

    for (j in 1:length(lista_empresas[[i]])){
      value <- stats::setNames(object = lista_empresas[[i]][[j]][2], nm = lista_empresas[[i]][[j]][1])
      values_vector <- c(values_vector, value)
    }

    lista_empresas_df[[i]] <- values_vector
  }

  empresas_df <- do.call(what = dplyr::bind_rows, args = lista_empresas_df)

  names(empresas_df)[grep("Data de entrada", names(empresas_df))] <- "Data de entrada"
  names(empresas_df) <- gsub(" ", ".", x = trimws(names(empresas_df)))
  empresas_df[1:ncol(empresas_df)] <- lapply(X = empresas_df[1:ncol(empresas_df)], FUN = trimws)

  if (only_bus){
  empresas_df <- dplyr::filter(empresas_df, Natureza.jurídica != "Associação Privada (3999).")
  }

  return(empresas_df)
}
#' Save Consultasocio links
#'
#' @description
#' Archive your links directly with SPN2 API
#'
#' @details
#' Still in development, we're trying to improve it. It is basically a code forcing to store your link, so it could take a while.
#' To use that function, you're going to need to create an account in web.archive.org and then request a key for their API.
#' For a better introduction to how to use this API, check their [documentation](https://docs.google.com/document/d/1Nsv52MvSjbLb2PCpHlat0gkzw0EvtSgpKHu4mk0MnrA/view).
#'
#' @param link A link to a consultasocio page that you want to save
#' @param WArch_aut A string containing your Authorization data. To see how to write it, check our example
#' @return A character string containing a web.archive link
#' @examples
#' CS_WArch_store_page("https://www.consultasocio.com/q/sa/arthur-moledo-do-val", "YOUR_KEY <YOUR_SECRET>")
#'
#' @export
WArch_store_page <- function(link, WArch_auth){
  save_link <- paste0("https://web.archive.org/save/", link)
  WArch_url <- paste0("https://web.archive.org/save/", link)

  while(WArch_url == save_link){
    WArch_url <- httr::GET(url = save_link,
                     add_headers(Authorization=WArch_auth,
                                 capture_outlinks = 0,
                                 capture_screenshot = 0,
                                 js_behavior_timeout = 0)
    )
    WArch_url <- WArch_url$url

    Sys.sleep(3)
  }

  print(paste0(link, " was stored as ", WArch_url))

  return(WArch_url)
}
