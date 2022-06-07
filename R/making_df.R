#' Turn vector into a matrix vector
#'
#' @description
#' Turn a frequency vector into a named vector with boolean values like "y" or "n".
#'
#' @details
#' Recommended for building a MCA dataset, as a way to turn character or numeric values into a matrix-like categorical vector.
#'
#' @param vec A character vector containing values to be transformed into boolean
#' @return A named character vector containing the attributes present in our original vector
#' @examples
#' CS_prep_matrix(c("Transf", "Transf", "Comerc"))
#'
#' @export
CS_prep_matrix <- function(vec){
  vec <- setNames(object = rep("y", length(vec)), nm = vec)
  return(vec)
}
#'
#' Creates a summary data.frame from business data.frame
#'
#' @description
#' Builds a summary vector from a business owner data.frame, to be bound with many other business owners data
#'
#' @param df A business owner page data.frame
#' @return A named character vector with a summary data
#' @examples
#' CS_get_summary_vec_reg(Skaf_bus_df)
#'
#' @export
CS_get_summary_vec_reg <- function(df){
  vec <- c(N_Emp = CS_count_active_bus(df$Situação.Cadastral),
           Maior_n_Func = CS_get_big_n_employees(df$Número.de.funcionários),
           Maior_Fat = CS_get_big_income(df$Faturamento),
           KSoc = CS_get_total_ksoc(unique(df$Capital.social)),
           CS_CNAE_conv(CS_get_CNAE(df$Atividade.econômica)),
           CS_prep_matrix(unique(CS_convert_state_to_region(CS_get_bus_states(df$Endereço))))
  )

  return(vec)
}
#'
#' Tie all summary vec in a data.frame
#'
#' @description
#' Tie your vector into a data.frame and organize them to use them as a factor
#'
#' @param vec_list A list containing multiple summary vectors
#' @return A formatted summary data.frame
#'
#'
CS_get_summary_df <- function(vec_list){
  vec_list[[length(vec_list)+1]] <- base_df_reg

  all_df <- do.call(what = dplyr::bind_rows, args = vec_list)

  df[is.na(df[c("Agr", "Extr", "Transf", "Infra", "Constr", "Comerc", "Serv",
                "Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")])] <- "n"
  return(df)
}
#' Read boards csv
#'
#' @description
#' Scrape, wrangle and organizes data from business owners pages
#'
#' @details
#' Reads a complete csv file, following the patterns shown in our vignette, aiding us in our replicability and organization.
#' There are basically four columns: owner name, organization, cpf and links.
#' It already automatizes some processes, like CPF selection and multiple pages.
#'
#' @param csv_path A path that leads to a .csv file containing businessman links
#' @return A data.frame with the business owners summary data, organized with default values provided by Consultasocio
#' @examples
#' CS_read_board_csv("diretores.csv")
#'
#' @export
CS_read_board_csv <- function(csv_path){
  diretores <- read.csv(csv_path, fileEncoding = "utf8")

  diretores_links <- str_split(string = diretores$Link, pattern = ",")
  names(diretores_links) <- diretores$Diretor

  diretores_df_list <- list()

  for (i in 1:length(diretores_links)){
    if (is.na(diretores$CPF[i])){
      if (length(diretores_links[[i]]) > 1){
        diretores_df_list[[i]]  <- lapply(X = diretores_links[[i]], FUN = CS_get_bus_df) %>% do.call(what = bind_rows, args = .)
      } else {
        diretores_df_list[[i]] <- CS_get_bus_df(path = diretores_links[[i]])
      }
    } else {
      if (length(diretores_links[[i]]) > 1){
        diretores_df_list[[i]]  <- lapply(X = diretores_links[[i]], FUN = CS_get_bus_df, CPF = diretores$CPF[i]) %>% do.call(what = bind_rows, args = .)
      } else {
        diretores_df_list[[i]] <- CS_get_bus_df(path = diretores_links[[i]], cpf = diretores$CPF[i])
      }
    }
  }

  # for (i in 1:length(diretores_df_list)){
  #   names(diretores_df_list[[i]]) <- gsub(pattern = " ", replacement = ".", x = trimws(names(diretores_df_list[[i]])))
  # }

  diretores_df <- lapply(FUN = CS_get_summary_vec_reg, X =  diretores_df_list)
  diretores_df[[length(diretores_df)+1]] <- base_df_reg

  diretores_df <- do.call(what = bind_rows, args = diretores_df)
  diretores_df[is.na(diretores_df)] <- "n"
  diretores_df[1:length(diretores_df)] <- lapply(X = diretores_df[1:length(diretores_df)], FUN = gsub, pattern = "-Inf", replacement = "NA")

  diretores_df[c("N_Emp", "KSoc")] <- lapply(X = diretores_df[c("N_Emp", "KSoc")], as.numeric)
  diretores_df[c("Agr", "Extr", "Transf", "Infra", "Constr", "Comerc", "Serv", "Sudeste", "Centro-Oeste", "Nordeste", "Norte", "Sul")] <- lapply(X = diretores_df[c("Agr", "Extr", "Transf", "Infra", "Constr", "Comerc", "Serv", "Sudeste", "Centro-Oeste", "Nordeste", "Norte", "Sul")], FUN = as.factor)

  diretores_df$Maior_n_Func <- factor(x = diretores_df$Maior_n_Func,
                                      levels = 1:6,
                                      labels = c("Até 19 func", "10-49 func",
                                                 "20-99 func", "50-199 func",
                                                 "100-199 func", "Mais de 200 func")
  )

  diretores_df$Maior_Fat <- factor(x = diretores_df$Maior_Fat,
                                   levels = 1:8,
                                   labels = c("Até 240k Fat", "Até 2,4M Fat", "De 2,4 a 5M Fat", "De 5 a 10M Fat",
                                              "De 10 a 30M Fat", "De 30 a 50M Fat", "CDe 50 a 100M Fat", "Mais de 100M Fat")
  )

  diretores_df$N_Emp <- cut(x = as.numeric(diretores_df$N_Emp), breaks = c(-1, 1, 5, Inf),
                            labels = c("Até 1 Emp", "2 a 5 Emp", "Mais de 5 Emp")
  )

  diretores_df$KSoc <- cut(x = as.numeric(diretores_df$KSoc),
                           breaks = c(0, 100000, 1000000, 10000000, Inf),
                           labels = c("Até 100k KSoc", "100k a 1M KSoc", "1M a 10M KSoc", "Mais de 10M KSoc")
  )

  diretores_df <- as.data.frame(diretores_df)

  row.names(diretores_df) <- diretores$Diretor
  return(diretores_df)
}