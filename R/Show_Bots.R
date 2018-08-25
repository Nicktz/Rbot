
#' @title Show_Bots
#' @description Show a list of your bots that are available. By default it will search for bots in path.expand("~").
#' Alternatively,
#' @return Bot names that are available in the specified folder.
#' @param Info_Loc Where the RDS file with your needed bot info is saved. Defaults to path.expand("~")
#' @importFrom glue glue
#' @examples \dontrun{
#' Show_bot()
#' }
#' @export
#'

Show_Bots <- function(Info_Loc = NULL) {

  if(is.null(Info_Loc)) Info_Loc <- path.expand("~")
Files <- list.files(Info_Loc, all.files = TRUE)
Bots <- Files[grepl("Rbots_", Files)]

Bots <- gsub("Rbots_", "", Bots)
Bots <- gsub(".rds", "", Bots)
if(length(Bots) == 0) stop(glue::glue("\nNo bots available in location: {Info_Loc}\n"))

Bots <- paste(Bots, collapse = "\n")

  message( glue::glue("\n * Available bots: \n {Bots}"))

}
