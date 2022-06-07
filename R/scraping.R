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
#'
#' @export
CS_get_bus_df <- function(path, cpf = NULL){
  busin <- rvest::read_html(path) %>%
    rvest::html_nodes(".cnpj p") %>% rvest::html_text()

  pos <- grep("CPF/CNPJ: \\*\\*\\*", busin)

  lista_empresas <- list()
  for (i in 1:length(pos)){
    if(i < length(pos)){
      lista_empresas[[i]] <- busin[pos[i]:(pos[i+1]-1)]} else {
        lista_empresas[[i]] <- busin[pos[i]:length(busin)]
      }
  }

  if (!is.null(cpf)){
    chos_cpf  <- logical(length(lista_empresas))
    for (i in 1:length(lista_empresas)){
      chos_cpf[i]  <- any(grepl(pattern = paste0("\\*\\*\\*", cpf, "\\*\\*"), x = lista_empresas[[i]]))
      }
    if (isFALSE(any(chos_cpf))){
      print("This CPF was not found")
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
      value <- setNames(object = lista_empresas[[i]][[j]][2], nm = lista_empresas[[i]][[j]][1])
      values_vector <- c(values_vector, value)
    }

    lista_empresas_df[[i]] <- values_vector
  }

  empresas_df <- do.call(what = dplyr::bind_rows, args = lista_empresas_df)

  names(empresas_df)[grep("Data de entrada", names(empresas_df))] <- "Data de entrada"
  names(empresas_df) <- gsub(" ", ".", x = trimws(names(empresas_df)))
  empresas_df[1:ncol(empresas_df)] <- lapply(X = empresas_df[1:ncol(empresas_df)], FUN = trimws)

  return(empresas_df)
}
