#' Calculating difference between planned and predicted sales
#'
#' This function calculates the difference between planned and predicted sales.
#'
#' If someone is interested for a detailed documentation: Go Home!
#'
#' @param .pred List which contains a data frame for each level and horizon regarding the predictions.
#' @param .pred_date Date column of .pred input
#' @param .planned List which contains a data frame for each level and horizon regarding the planned sales.
#' @param .planned_date Date column of .planned input
#' @param .level Level of interest. Input can be a single string or a character vector.
#' @param .key Key variable which is used for calculating the difference.
#' @param .horizon The number of time horizons which are included in .pred.
#' @param .total_horizons The number of total horizons which are included in .pred.
#' @param .total_sales Parameter without any usefull functionality. But there can be something in the future.
#' @param .date_start Parameter without any usefull functionality. But there can be something in the future.
#' @param .date_end Parameter without any usefull functionality. But there can be something in the future.
#'
#' @export


diff_pred_planned <- function(.pred,
                              .pred_date = "date",
                              .planned,
                              .planned_date = "date",
                              .level = "total",
                              .key = "retail_sales_m3",
                              .horizon = 1,
                              .total_horizons = 3,
                              .total_sales = NULL,
                              .date_start = NULL,
                              .date_end = NULL){

  # checkers
  if(missing(.pred)) stop("Predictions not defined!")
  if(missing(.planned)) stop("Actual sales not defined!")
  if(!is(.pred, "list")) stop("Input for .pred needs to be a list object!")
  if(!is(.planned, "list")) stop("Input for .planned needs to be a list object!")



  # function for upper cases tc level
  tcCap <- function(x) {

    checker <- paste(c('gle', 'glc', 'suv'), collapse="|")
    s <- stringr::str_split(x, " ")[[1]]
    for(.i in 1:length(s)){

      if(!stringr::str_detect(s[.i], checker)){
        s[.i] <- paste(toupper(substring(s[.i], 1, 1)), substring(s[.i], 2), sep = "")

      } else{
        s[.i] <- toupper(s[.i])
      }
    }
    paste(s, collapse = " ")
  }


  n_level <- length(.level)
  forecast_level <- .total_horizons + 1
  diff_pred_list <- list()

  for (i in 1:.horizon){

    for (j in 1:n_level){

      if(.level[j] == "total"){

        df_actual <- .planned[[.level[j]]] %>%
          dplyr::mutate(date = !!sym(.planned_date))

        diff_pred_actual <- .pred[[.level[j]]][[forecast_level]][[i]] %>%
          dplyr::mutate(date = !!sym(.pred_date)) %>%
          dplyr::left_join(df_actual, by = c("date" = "date")) %>%
          dplyr::select("date", "model", "mape", "mapesd", "pred", .key) %>%
          dplyr::mutate(pred = round(pred),
                        diff = pred - !!sym(.key),
                        type = "total")

        name_back <- paste0(.level[j], "_diff_h", i)
        assign(paste0(.level[j], "_diff_h", i), diff_pred_actual)

        diff_pred_list[[paste0(.level[j], "_diff_h", i)]] <- diff_pred_actual

      }

      if(.level[j] == "class"){

        names <- names(.pred[[.level[j]]])

        class_names <- names(.pred[[.level[j]]]) %>%
          stringr::str_split("_") %>%
          lapply(function(x) x[[3]]) %>%
          unlist() %>%
          toupper()

        for (k in 1:length(names)){

          df_actual <- .planned[[.level[j]]] %>%
            dplyr::rename(date = !!sym(.planned_date)) %>%
            dplyr::filter(class == class_names[k])

          diff_pred_actual <- .pred[[.level[j]]][[names[k]]][[forecast_level]][[i]] %>%
            dplyr::rename(date = !!sym(.pred_date)) %>%
            dplyr::left_join(df_actual, by = "date") %>%
            dplyr::select(dplyr::contains(.pred_date), "pred", "model", "mape", "mapesd", .key) %>%
            dplyr::mutate(pred = round(pred),
                          diff = pred - !!sym(.key),
                          type = class_names[k])

          name_back <- paste0(.level[j], "_diff_h", i)
          assign(paste0(.level[j], "_diff_h", i), diff_pred_actual)
          name_list <- name_back %>%
            stringr::str_split("_") %>% .[[1]] %>% .[3]

          diff_pred_list[[paste0(.level[j], "_", class_names[k], "_diff_h", i)]] <- diff_pred_actual

        }
      }

      if (.level[j] == "typeclass"){

        names <- names(.pred[[.level[j]]])

        tc_names <- names(.pred[[.level[j]]]) %>%
          stringr::str_split("result_") %>%
          lapply(function(x) x[[2]] %>%
                   stringr::str_split("_") %>%
                   .[[1]] %>%
                   paste(collapse = " ") %>%
                   tcCap()) %>%
          unlist()

        for (k in 1:length(names)){

          df_actual <- .planned[[.level[j]]] %>%
            dplyr::rename(date = !!sym(.planned_date)) %>%
            dplyr::filter(full_name == tc_names[k])

          diff_pred_actual <- .pred[[.level[j]]][[names[k]]][[forecast_level]][[i]] %>%
            dplyr::rename(date = !!sym(.pred_date)) %>%
            dplyr::left_join(df_actual, by = "date") %>%
            dplyr::select(dplyr::contains(.pred_date), "model", "mape", "mapesd", "pred", ".key") %>%
            dplyr::mutate(pred = round(pred),
                          diff = pred - !!sym(.key),
                          type = tc_names[k])

          name_back <- paste0(.level[j], "_diff_h", i)
          assign(paste0(.level[j], "_diff_h", i), diff_pred_actual)
          name_list <- name_back %>%
            stringr::str_split(., "result_") %>%
            .[[1]] %>%
            .[2]

          diff_pred_list[[paste0(.level[j], "_", tc_names[k], "_diff_h", i)]] <- diff_pred_actual

        }
      }
    }
  }

  return(diff_pred_list)
}
