# functions ----

#' This function looks up one or more ids (KTID, PID, RID_) in a REG_ Table
#'
#' @param reg_table a REG_* table from krebsregister database
#' @param id_type the type of id you are looking for (either PID or KTID)
#' @param id_type the PID or KTID
#' @return A tibble
#' @examples
#' KTID:
#' lookup_id_in_reg_table(reg_table = "REG_OPERATION", id_type = "KTID", id = "1049794")
#' 
#' PID:
#' lookup_id_in_reg_table(reg_table = "REG_TODESURSACHE", id_type = "PID", id = "380324")
#' @export
lookup_id_in_reg_table <- function(reg_table, id_type, ids) {
  
  # connect to krebsregister database via DBI
  con <- DBI::dbConnect(odbc::odbc(), "krebsregister", encoding = "latin1")
  
  # specify sql statement
  sql <- "SELECT * FROM ?table WHERE ?id_type in ?ids ;"
  
  # protect against sql injection
  query <- DBI::sqlInterpolate(
    con, sql,  
    table = DBI::dbQuoteIdentifier(con, reg_table), 
    id_type = DBI::dbQuoteIdentifier(con, id_type),
    ids = DBI::SQL(paste0("(", paste(ids, collapse = ","), ")"))
    )

  # execute query
  reg_table <- DBI::dbGetQuery(con, query)
  
  # disconnect from database
  DBI::dbDisconnect(con)
  
  # return table
  return(reg_table)
  
}

#' This function joins all contents of a REG_* Table when the event is triggered
#'
#' @param trigger_data the data underlying the trigger event from shiny::nearPoints()
#' @param reg_table_filtered a filtered REG_* table from krebsregister database according to some id
#' @param reg_table_date name of the date column in the REG_* table
#' @param join_column on which column the REG_* table is joined (either PID or KTID)
#' @return A tibble
#' @examples
#' KTID:
#' join_reg_table_upon_trigger(
#'   trigger_data = test[1, ],
#'   reg_table = lookup_id_in_reg_table("REG_DIAGNOSE", id_type = "KTID", id = "193066"),
#'   reg_table_date = "DIAGNOSEDATUM",
#'   join_column = "KTID"
#' )
#'   
#' PID:
#' join_reg_table_upon_trigger(
#'   trigger_data =test[test$MELDEANLASS_TYP=="Todesursache",][2,],
#'   reg_table = lookup_id_in_reg_table(reg_table = "REG_TODESURSACHE", id_type = "PID", id = "2075781"),
#'   reg_table_date = "DIAGNOSEDATUM",
#'   join_column = "PID"
#' ) 
#' @export
join_reg_table_upon_trigger <- function(
  trigger_data, reg_table_filtered, reg_table_date, join_column
  ) {
  
  dplyr::left_join(
    trigger_data, 
    reg_table_filtered %>% 
      dplyr::rename("Datum" = reg_table_date), by = c(join_column, "Datum"))
  
}


# join_reg_table_upon_trigger(
#   trigger_data = test[test$KTID=="177141" & test$MELDEANLASS_TYP=="Strahlentherapie",] %>% distinct(MELDEANLASS_TYP, .keep_all = T) %>% select(-starts_with("RID")),
#   reg_table_filtered = lookup_id_in_reg_table("REG_STRAHLENTHERAPIE", id_type = "KTID", ids = "177141"),
#   reg_table_date = "BEGINN_DATUM",
#   join_column = c("KTID")
#   ) %>%
#   left_join(.,lookup_id_in_reg_table("REG_STRAHLENTHERAPIE_ZIELGEBIET", "RID_ST", .$RID_ST),
#                    by = c("KTID", "RID_ST")) %>%
#   left_join(.,lookup_id_in_reg_table("REG_NEBENWIRKUNGEN", "RID_ST", .$RID_ST),
#             by = c("KTID", "RID_ST")) -> y
