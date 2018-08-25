#' @title Doc_Bot
#' @description This function sends any document from your computer to your phone.
#' This function assumes you have added a bot from Telegram on your phone, noted the Token and added it to the Add_Bot(Token) file.
#' You should have received a confirmation sms after doing this successfully. See ?Add_Bot for information on this.
#' Alternatively, you could simply include your Token and ID as arguments below, but the Add_Bot function will make managing sending messages much simpler.
#' The default location for the bot's saved information is: path.expand("~") (Type this in your console to see the location).
#' If you overrode this, add the Bot's name and the location in the parameters below.
#' Otherwise, add the Token and ID below.
#' @return Bot Document sent
#' @importFrom telegram.bot Bot
#' @importFrom glue glue
#' @param Doc_Location Document to send to your phone from R.
#' @param Bot_Name Name of the bot to do the sending.
#' @param Info_Loc Where the RDS file with your needed bot info is saved. Defaults to path.expand("~")
#' @param Token Token of your bot if not using the saved file call from Rbot::Add_Bot()
#' @param Silent Choose to not print successful execution. Defaults to TRUE.
#' @examples \dontrun{
#' Doc_Location <- YOUR_PDF_LOCATION # e.g. Doc_Location <- "somefile.pdf"
#' Bot_Name <- "Soldier1"
#' Doc_Bot(Doc_Location = Doc_Location, Bot_Name = Bot_Name) # assume default logcation
#' # If you used a specific Add_Bot location:
#' Text_Bot(Msg = Msg, Bot_Name = Bot_Name, Info_Loc = Some_Path)
#' # If you prefer setting the Token and ID directly here:
#' Text_Bot(Msg = Msg, Token = TOKEN, ID = Chat_ID)
#' }
#' @export
#'
#'

Doc_Bot <- function(Doc_Location, Bot_Name = NULL, Info_Loc = NULL, Token = NULL, Silent = TRUE) {

  if(is.null(Info_Loc)) Info_Loc <- path.expand("~")

  if(!file.exists(Doc_Location)) stop(glue::glue("\n=========\n...Please specify a valid document location - could not find:\n{Doc_Location}\n"))

  if( !is.null(Bot_Name) & !is.null(Token) ) {stop("\n=========\n...Either provide a Bot_Name (and optional Info_Loc) OR a Token:, not both.\n=========\n")}

  if( is.null(Token) && is.null(Bot_Name)) {stop("\n=========\n...Please provide a valid Bot_Name.\n=========\n")}

  if( !is.null(Bot_Name) & is.null(Token)) {

    if(!file.exists(glue::glue("{Info_Loc}/Rbots_{Bot_Name}.rds"))) stop(glue::glue("\n==============\n\nPlease check whether you added a valid Bot info using Add_Bot.\nI looked in: {Info_Loc}/{Bot_Name}.rds, but did not find a file.\n\nCheck the location ({Info_Loc}), or Bot_Name ({Bot_Name}) provided.\n==============\n") )

    Bot_Info <-
      readRDS(glue::glue("{Info_Loc}/Rbots_{Bot_Name}.rds"))

    Token <- Bot_Info$Token
    ID <- Bot_Info$ID

  } else
    if( is.null(Bot_Name) & !is.null(Token) ){

      bot <- telegram.bot::Bot(token = Token)
      if(length(bot$getUpdates()) == 0) stop("\nPlease first send a message to your intended bot from your phone (Telegram app), and then run function again...\nConsider using Add_Bot to one time save your Token and ID for a given bot...\n")
      ID <- unique(bot$getUpdates()$message$from$id)

    }

  bot <- telegram.bot::Bot(token = Token)
  Sent <- bot$sendDocument(chat_id = ID, document = Doc_Location)

  # Sent[[2]] == 401: wrong bot. Token issue.
  # Sent[[2]] == 400: Right bot. ID issue.

  if( Sent[[2]] == 400 | Sent[[2]] == 401) {

    warning(glue::glue("\n==============\nDocument sending failed.\nLikely cause:\n\n...You provided a wrong ID.\n...Or if no ID was provided, you may have not yet initiated a chat with your bot (simply send a 'Hi' message)\n\nPlease check and retry.\n\nConversely, check your Bot_Name and perhaps rerun Add_Bot function.\n=======================\n"))

  } else {

    if(!Silent){

      cat("Document successfully sent!")

    }

  }


}
