#' @title Add_Bot
#' @description Adds all the needed information of a new bot.
#' When using this function, required information is saved to a RDS file, with the same name as your bot.
#' The default path is path.expand("~"), but this can be overridden by setting path below.
#' The steps are outlined below before using this function:
#' First, add a bot from the Telegram app on your phone. Then send to Botfather: '/newbot'
#' After this, follow instructions - adding first a name for your bot (remember this one), and also a username for your bot (ending in _bot).
#' Botfather will also give you a Token, the validity of which can be checked by typing into a browser: https://api.telegram.org/bot<YOURTOKEN>/getMe
#' Save this Token (this is the input to the function Add_Bot).
#' If the browser does not give you an error message, it is a valid bot. It should also show you the bot name and bot username.
#' After getting the Token, you must awake your bot by typing /start. You will see the new bot as part of your contacts.
#' Lastly, send your bot a message from your phone (to initiate conversation with your bot. Simply type: 'Hello!')
#' You are now ready to add this bot to your worker bots... Run: Add_Bot(Token)
#' @return RDS file saved with the needed bot information. The printed message after running the function will give the location of the file.
#' @importFrom telegram.bot Bot
#' @importFrom glue glue
#' @importFrom purrr safely
#' @param Token Add the Token provided by the Botfather from the Telegram app on your website.
#' @param Bot_Name Default is to use the bot's name given to the botfather. This will also be the name of the RDS file with your bots' information saved in.
#' @param Info_Loc Where the RDS file with your needed bot info is saved. Defaults to path.expand("~")
#' @param Silent Choose to not print successful execution. Defaults to TRUE.
#' @examples \dontrun{
#' Token <- "12389798751:ASFBVAdaS12408752"
#' Add_Bot(Token)
#' }
#' @export
#'
#'

Add_Bot <- function(Token, Bot_Name = NULL, Info_Loc = NULL, Silent = FALSE) {

if(missing(Token)) {

  stop("\nPlease provide a valid Token (given by the botfather).\n")

}

if(is.null(Info_Loc)){

  Info_Loc <- path.expand("~")

}

bot <- telegram.bot::Bot(token = Token)

Check_Breaking_Changes <-
  tryCatch(
    expr = {
      bot$get_me()
    },
    error = function(e){
      stop('Please use the latest version of telegram.bot and telegram - several breaking changes have been introduced. Use install.packages and try again.')
    },
    finally = ""
  )

WhoIt <- bot$get_me()

if(!WhoIt$is_bot) {

  stop(glue::glue("========================================\n\nYou provided an invalid token:\n\n{Token}\n\nTo check whether your token is correct, enter in a web browser: https://api.telegram.org/bot<YOURTOKEN>/getMe \n For your submitted token, it is: https://api.telegram.org/bot{Token}/getMe \n If you see: error_code:401, you've entered a wrong Token.\n\nThen try again... (or see this link: https://github.com/ebeneditos/telegram.bot/wiki/Tutorial-%E2%80%93-Building-an-R-Bot-in-3-steps for more help.)\n\n========================================"))

} else {

  # if(is.null(Bot_Name)) {

  glue::glue("\n\nNote that the botname that corresponds to the given Token is: officially named: {WhoIt$first_name} on your phone, with username: {WhoIt$username}).\n")

  if(length(bot$getUpdates()) == 0) stop("\nPlease first send a message to your intended bot from your phone on the Telegram app (you might need to do this again), and then run function again...\n")

  updates <- bot$getUpdates()
  ID <- unique(updates[[1L]]$from_chat_id())

  if(!is.null(Bot_Name)) {

    BotName_ToUse <- Bot_Name

  } else {

    BotName_ToUse  <- WhoIt$first_name

  }

  SaveLoc <- glue::glue("{Info_Loc}/.Rbots_{BotName_ToUse}.rds")


  Continue <- "Y"
  if(file.exists(SaveLoc) ) Continue <- readline(prompt = cat(glue::glue("\n\nThere already exists a bot's information in location: {Info_Loc}/.Rbots_{Bot_Name}.rds.\n Are you sure you want to continue overriding the old file?\nType Y to override, N to stop\n")))

  if(Continue %in% c("N", "n") ) {

    message("...Did not overwrite existing Bot info...")

  } else {

  SaveEntry <- list()
  SaveEntry$Description <- glue::glue("This is the bot identifyer for bot call:

                                          {BotName_ToUse}.

                                          You can simply reference this bot's name and location (if location is other than default) when using other functions")
  SaveEntry$BotName <- BotName_ToUse
  SaveEntry$ID <- ID
  SaveEntry$Token <- Token

  saveRDS(object = SaveEntry, SaveLoc)

  Msg <- glue::glue("==========================
                        Hello from R!

                        Your bot is locked and loaded!
                        Info pertaining to your bot can be found on your computer in:

                          {SaveLoc}

                        To call your bot in other functions, use the Bot name:

                        {SaveEntry$BotName}

                        Type in R the following to see if it works:

                        Text_Bot(Msg = 'hello from R!', Bot_Name = '{SaveEntry$BotName}', Info_Loc '{Info_Loc}')

                        Enjoy using Telegram!
                        ==========================")

  # Send cellphone confirmation. SUppress the charToRaw(enc2utf8(val)) message.
  suppressWarnings(bot$sendMessage(chat_id = ID, text = Msg))

  if(!Silent){

    message(cat(glue::glue("===========================\n\n
                     Congratulations! You have successfully created an info file for your bot!

                     Location: {SaveLoc}
                     Bot Name: {SaveEntry$BotName}

                     ...Check that you received a congrats message on your phone...

                     You are now good to use the other functions contained in Rbot.

                     E.g. run the following to verify the ease with which you can operate:

                     Text_Bot(Msg = 'hello from R!', Bot_Name = '{SaveEntry$BotName}', Info_Loc '{Info_Loc}')

                     \n\n===========================")))
  }


}



}

}
