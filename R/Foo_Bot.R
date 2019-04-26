#' @title Foo_Bot
#' @description Execute functions in R from your phone.
#' This will require an open R session connected to your phone.
#' Using this function, you can specify the functions that R will execute
#' on your demand.
#'
#' @return Switches R on to listen for Bot
#' @param Bot_Name Name of the bot to do the sending.
#' @param Info_Loc Where the RDS file with your needed bot info is saved. Defaults to path.expand("~")
#' @param Token Token of your bot if not using the saved file call from Rbot::Add_Bot()
#' @param Function_List Function_List containing for each function: The function to be called, the call handle, as well as the message sent to Telegram. Up to 20 functions can be added.
#' @param LoadMessage What to display on your phone when this call is initialised
#' @param PokePC Ping the PC and check that your connection is still active. No side-effects, simple poke message returned.
#' @param KillR Add option to kill R from your phone. Default to TRUE
#' @param RestartCPU Force restarts the PC - no questions asked. Defaults to FALSE.
#' @param KillCPU Add option to turn off your computer completely, no questions asked. Default to FALSE. Useful if working e.g. on a public CPU that you want to log out from remotely.
#' @importFrom purrr safely
#' @importFrom telegram.bot Bot
#' @importFrom telegram.bot Updater
#' @importFrom telegram.bot CommandHandler
#' @importFrom glue glue
#' @examples \dontrun{
#' # Provide a Function_List with the the following inputs per function:
#'
#' # Function_List$Foo1, Function_List$Foo2, ... : can provide up to 30 functions
#' # Function_List$Call : What to type in Telegram. Used as /Call in Telegram
#' # Function_List$Args : TRUE / FALSE; whether to be able to include an argument. If excluded, it defaults to FALSE.
#' # Function_List$Message : How to describe this function in Telegram.
#' # Example below:
#'
#' Function_List <- list()
#'
#' First_Foo <- function(X){
#'
#'    y <- 20*as.numeric(X)
#'    print(y)
#'    message("....Function 1 executed....")
#'
#' }
#'
#' Function_List$Foo1 <-
#'    list(Function = First_Foo,
#' # How to call your function from Telegram
#'         Call = "F1",
#'         Args = TRUE,
#'         # What to say when executed in Telegram
#' Message = "First function executed. This function sources the data fetch.")
#'
#' Second_Foo <- function(X){
#'
#'    print(paste0("Your provided input: ", X) )
#'    message("....Function 2 executed....")
#'
#' }
#'
#'
#' Function_List$Foo2 <-
#'    list(Function = Second_Foo,
#'         Call = "F2",
#'         Args = TRUE,
#'         Message = "Report is now being built.")
#'
#'
#'
#' Error_Foo <- function(){
#'    x <- 0
#'    if( is.infinite(10/x)) stop("Example of error being thrown, but not breaking connection...")
#'
#' }
#'
#' Function_List$Foo3 <-
#'    list(Function = Error_Foo,
#'         Call = "Error_Example",
#'         Args = TRUE,
#'         Message = "\nError function illustrated: \nThis illustrates that the connection with the phone will  be preserved using purrr::safely")
#'
#'
#'
#'
#' Bot_Name <- "A"
#' Foo_Bot(Bot_Name = Bot_Name, Function_List = Function_List, LoadMessage = "My connection with R",
#'         KillR = TRUE, KillCPU = FALSE)
#'
#' # Alternatively, no Function_List (implying only ability to switch off computer or killR, e.g.):
#'
#' Foo_Bot(Bot_Name = Bot_Name, Function_List = NULL, LoadMessage = "My connection with R",
#'         KillR = TRUE, KillCPU = FALSE)
#'
#' }
#' @export
#'

Foo_Bot <- function( Bot_Name = NULL, Info_Loc = NULL, Token = NULL,
                     Function_List = NULL,
                     LoadMessage = "\nConnection established with R\n",
                     KillR = TRUE, PokePC = TRUE, RestartCPU = FALSE, KillCPU = FALSE) {


  # Check location, Tokens and bot names...
  if(is.null(Info_Loc)) {
    Info_Loc <- path.expand("~")
  }

  if( !is.null(Bot_Name) & !is.null(Token) ) {stop("\n=========\n...Either provide a Bot_Name (and optional Info_Loc) OR a Token:, not both.\n=========\n")}

  if( is.null(Token) && is.null(Bot_Name)) {stop("\n=========\n...Please provide a valid Bot_Name.\n=========\n")}


  if( !is.null(Bot_Name) && is.null(Token)) {

    if(!file.exists(glue::glue("{Info_Loc}/.Rbots_{Bot_Name}.rds"))) stop(glue::glue("\n==============\n\nPlease check whether you added a valid Bot info using Add_Bot.\nI looked in: {Info_Loc}/.Rbots_{Bot_Name}.rds, but did not find a file.\n\nCheck the location ({Info_Loc}), or Bot_Name ({Bot_Name}) provided.\n==============\n") )

    Bot_Info <-
      readRDS(glue::glue("{Info_Loc}/.Rbots_{Bot_Name}.rds"))
    ID <- Bot_Info$ID
    Token <- Bot_Info$Token

  } else
    if( is.null(Bot_Name) & !is.null(Token) ){
      ID <- NULL
      bot <- telegram.bot::Bot(token = Token)
    }

  # Bot calls and setup
  bot <- telegram.bot::Bot(token = Token)
  updater <- telegram.bot::Updater(token = Token)

  if(is.null(ID)) ID <- unique(updates[[1L]]$from_chat_id())

  if(is.null(Function_List)) {


    # Kill Connection
    kill <- function(bot, update){
      bot$sendMessage(chat_id = update$message$chat_id,
                      text = "\nBot Connection Closed Successfully\n")
      # Clean 'kill' update
      bot$getUpdates(offset = update$update_id + 1L)
      # Stop the updater polling
      updater$stop_polling()
    }

    updater <<- updater + telegram.bot::CommandHandler("kill", kill)

    #
    PokePC_Msg <- ""
    RestartPC_Msg <- ""
    KillR_Msg <- ""
    KillCPU_Msg <- ""
    #
    if(KillR) {

      killR <- function(bot, updates){

        bot$sendMessage(chat_id =ID,
                        text = "Your R session will now restart itself...Connection will be lost.\n Goodbye!")
        Sys.sleep(3)
        .rs.restartR()
      }

      updater <<- updater + telegram.bot::CommandHandler("killR", killR)
      KillR_Msg <- "\n...........\nRestart your R session using: \n * /killR\n"

    }

    if(PokePC) {

      Poke <- function () {
        message(paste0("PC Poked at ", Sys.time()))
      }


      PokePC <- function(bot, updates){
        bot$get_updates(offset = updates$update_id + 1)
        bot$sendMessage(chat_id =ID,
                        text = "PC has been poked - you are still connected...")
        Poke()
      }

      updater <<- updater + telegram.bot::CommandHandler("PokePC", PokePC)
      PokePC_Msg <- "\n...........\nThis function will poke your PC to check that it is still connected (no side-effects, just a gentle poke...): \n * /PokePC\n"
    }


    if(RestartCPU) {

      Rstart <- function () {
        Sys.sleep(5)
        shell("shutdown -r -t 0 -f")
      }


      RestartCPU <- function(bot, updates){
        bot$get_updates(offset = updates$update_id + 1)
        bot$sendMessage(chat_id =ID,
                        text = "You've elected to restart your PC in 5 sec.... Bye!")
        Rstart()
      }

      updater <<- updater + telegram.bot::CommandHandler("RestartCPU", RestartCPU)
      RestartPC_Msg <- "\n...........\nThis will force restart your computer (no questions asked...) by typing: \n * /RestartCPU\n"
    }

    if(KillCPU) {

      Shutdown <- function () {
        Sys.sleep(10)
        shell("shutdown -s -t 0")
      }


      killCPU <- function(bot, updates){

        bot$sendMessage(chat_id =ID,
                        text = "Well that escalated quickly! CPU will force shutdown in 10 sec.... Bye!")
        Shutdown()
      }

      updater <<- updater + telegram.bot::CommandHandler("killCPU", killCPU)
      KillCPU_Msg <- "\n...........\nCompletely shut down your computer (no questions, just shutting down...) by typing: \n * /killCPU\n"
    }

    BotMsg <-
      LoadMessage
    BotMsg <-
      glue::glue("\n {LoadMessage} \n\nFUNCTIONS:\n========================\n{BotMsg}\n\nGENERAL INSTRUCTIONS\n========================\nTo Kill this open port:\n * Type: /kill\n{PokePC_Msg}{RestartPC_Msg}{KillR_Msg}{KillCPU_Msg} \n ========================\nConnection opened: {Sys.time()} \n ========================")


    if(!is.null(Bot_Name)){

      Text_Bot(Msg = BotMsg, Bot_Name = Bot_Name, Token = NULL, Info_Loc = Info_Loc, Silent = TRUE)

    } else {

      Text_Bot(Msg = BotMsg, Token = Token, Info_Loc = Info_Loc, Silent = TRUE)

    }

    # Msg Command
    Msg <- function(bot, updates){

      bot$sendMessage(chat_id =ID,
                      text = glue::glue("\nConnection opened: {Sys.time()}"))
      bot$get_updates(offset = updates$update_id)
    }

    updater <- updater + telegram.bot::CommandHandler("Msg", Msg)
    message(glue::glue("\n=================\nOpening Connection with {Bot_Name} at {Sys.time()}.\n=================\nSee your phone's Telegram app for instructions...."))


    unknown <- function(bot, update){
      bot$sendMessage(chat_id = update$message$chat_id,
                      text = "Sorry, I did not understand that command (did you use / before the command?)\nPlease try again.")
    }

    updater <- updater + MessageHandler(unknown, MessageFilters$command)

    updater$start_polling(clean = T)


  } else {


    # Check functions and their calls :
    if(class(Function_List) != "list") stop("\n\nProvide functions, their calls and their messages in a list. Please see ?Foo_Bot for a full example, or set Function_List to NULL to only have ability to switch CPU off.\n\n")

    # Function map creator.
    # Add handles...

    MsgLog <- list()
    FuncList <- list()
    FuncN <- length(Function_List)
    for(i in 1:FuncN){
      # i=1
      Foo <- Function_List[[i]]$Function
      Call <- Function_List[[i]]$Call
      Args <- ifelse(is.null(Function_List[[i]]$Args), FALSE, Function_List[[i]]$Args)
      Msg <- Function_List[[i]]$Message

      if(is.null(Foo) | !is.function(Foo)) stop("Each Function_List entry should have a valid function labelled as Function. See ?Foo_Bot example.")
      if(is.null(Call) | !is.character(Call)) stop("Each Function_List entry should have a valid Call labelled as Call. See ?Foo_Bot example.")
      if(is.null(Msg) | !is.character(Msg)) stop("Each Function_List entry should have a valid Message labelled as Message. See ?Foo_Bot example.")

      assign(paste0("Foo", i), purrr::safely(Foo, otherwise = "FunctionFail")) # Ensure function is safe. Will alert if there is an error, not break connection.
      assign(paste0("Args", i), Args) # Ensure function is safe. Will alert if there is an error, not break connection.
      assign(paste0("Call", i), Call)

      MsgLog[i] <-
        list(
          glue::glue("\n* Call: /{Call}\n  - Description: {Msg}\n..............\n" )
        )
    }


    if(exists("Foo1")){
      Handle_Func1 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args1) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call1}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo1(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call1}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call1}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo1()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call1}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call1}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call1, Handle_Func1, pass_args = TRUE)
    }
    if(exists("Foo2")){
      Handle_Func2 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args2) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call2}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo2(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call2}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call2}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo2()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call2}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call2}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call2, Handle_Func2, pass_args = TRUE)
    }
    if(exists("Foo3")){
      Handle_Func3 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args3) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call3}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo3(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call3}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call3}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo3()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call3}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call3}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call3, Handle_Func3, pass_args = TRUE)
    }
    if(exists("Foo4")){
      Handle_Func4 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args4) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call4}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo4(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call4}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call4}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo4()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call4}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call4}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call4, Handle_Func4, pass_args = TRUE)
    }
    if(exists("Foo5")){
      Handle_Func5 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args5) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call5}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo5(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call5}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call5}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo5()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call5}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call5}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call5, Handle_Func5, pass_args = TRUE)
    }
    if(exists("Foo6")){
      Handle_Func6 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args6) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call6}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo6(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call6}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call6}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo6()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call6}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call6}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call6, Handle_Func6, pass_args = TRUE)
    }
    if(exists("Foo7")){
      Handle_Func7 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args7) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call7}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo7(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call7}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call7}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo7()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call7}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call7}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call7, Handle_Func7, pass_args = TRUE)
    }
    if(exists("Foo8")){
      Handle_Func8 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args8) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call8}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo8(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call8}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call8}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo8()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call8}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call8}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call8, Handle_Func8, pass_args = TRUE)
    }
    if(exists("Foo9")){
      Handle_Func9 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args9) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call9}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo9(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call9}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call9}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo9()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call9}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call9}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call9, Handle_Func9, pass_args = TRUE)
    }
    if(exists("Foo10")){
      Handle_Func10 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args10) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call10}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo10(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call10}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call10}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo10()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call10}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call10}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call10, Handle_Func10, pass_args = TRUE)
    }
    if(exists("Foo11")){
      Handle_Func11 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args11) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call11}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo11(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call11}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call11}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo11()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call11}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call11}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call11, Handle_Func11, pass_args = TRUE)
    }
    if(exists("Foo12")){
      Handle_Func12 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args12) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call12}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo12(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call12}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call12}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo12()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call12}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call12}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call12, Handle_Func12, pass_args = TRUE)
    }
    if(exists("Foo13")){
      Handle_Func13 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args13) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call13}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo13(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call13}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call13}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo13()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call13}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call13}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call13, Handle_Func13, pass_args = TRUE)
    }
    if(exists("Foo14")){
      Handle_Func14 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args14) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call14}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo14(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call14}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call14}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo14()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call14}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call14}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call14, Handle_Func14, pass_args = TRUE)
    }
    if(exists("Foo15")){
      Handle_Func15 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args15) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call15}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo15(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call15}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call15}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo15()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call15}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call15}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call15, Handle_Func15, pass_args = TRUE)
    }
    if(exists("Foo16")){
      Handle_Func16 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args16) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call16}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo16(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call16}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call16}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo16()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call16}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call16}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call16, Handle_Func16, pass_args = TRUE)
    }
    if(exists("Foo17")){
      Handle_Func17 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args17) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call17}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo17(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call17}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call17}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo17()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call17}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call17}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call17, Handle_Func17, pass_args = TRUE)
    }
    if(exists("Foo18")){
      Handle_Func18 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args18) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call18}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo18(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call18}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call18}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo18()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call18}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call18}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call18, Handle_Func18, pass_args = TRUE)
    }
    if(exists("Foo19")){
      Handle_Func19 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args19) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call19}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo19(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call19}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call19}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo19()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call19}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call19}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call19, Handle_Func19, pass_args = TRUE)
    }
    if(exists("Foo20")){
      Handle_Func20 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args20) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call20}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo20(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call20}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call20}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo20()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call20}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call20}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call20, Handle_Func20, pass_args = TRUE)
    }
    if(exists("Foo21")){
      Handle_Func21 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args21) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call21}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo21(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call21}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call21}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo21()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call21}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call21}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call21, Handle_Func21, pass_args = TRUE)
    }
    if(exists("Foo22")){
      Handle_Func22 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args22) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call22}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo22(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call22}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call22}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo22()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call22}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call22}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call22, Handle_Func22, pass_args = TRUE)
    }
    if(exists("Foo23")){
      Handle_Func23 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args23) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call23}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo23(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call23}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call23}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo23()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call23}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call23}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call23, Handle_Func23, pass_args = TRUE)
    }
    if(exists("Foo24")){
      Handle_Func24 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args24) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call24}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo24(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call24}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call24}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo24()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call24}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call24}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call24, Handle_Func24, pass_args = TRUE)
    }
    if(exists("Foo25")){
      Handle_Func25 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args25) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call25}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo25(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call25}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call25}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo25()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call25}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call25}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call25, Handle_Func25, pass_args = TRUE)
    }
    if(exists("Foo26")){
      Handle_Func26 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args26) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call26}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo26(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call26}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call26}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo26()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call26}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call26}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call26, Handle_Func26, pass_args = TRUE)
    }
    if(exists("Foo27")){
      Handle_Func27 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args27) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call27}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo27(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call27}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call27}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo27()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call27}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call27}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call27, Handle_Func27, pass_args = TRUE)
    }
    if(exists("Foo28")){
      Handle_Func28 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args28) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call28}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo28(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call28}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call28}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo28()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call28}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call28}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call28, Handle_Func28, pass_args = TRUE)
    }
    if(exists("Foo29")){
      Handle_Func29 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args29) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call29}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo29(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call29}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call29}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo29()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call29}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call29}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call29, Handle_Func29, pass_args = TRUE)
    }
    if(exists("Foo30")){
      Handle_Func30 <- function( bot, updates, args){
        Add_Msg <- ""
        if(Args30) {
          if (length(args) == 0L) {
            Result <- list()
            Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Call30}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
          } else {
            Result <- Foo30(args)
          }
        }  else {
          if (length(args) > 0L) {
            Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Call30}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
            message(glue::glue(":::NOTE::::\n* Argument for '/{Call30}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
          }
          Result <- Foo30()
        }


        if(!is.null(Result$error)) {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("***ERROR Produced***: \n Function: {Call30}\n\n * Error details:\n {Result$error}"))
          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
        } else {

          bot$get_updates(offset = updates$update_id + 1)
          bot$sendMessage(chat_id =ID,
                          text = glue::glue("Executed: {Call30}{Add_Msg}"))

        }
      }
      updater <- updater + CommandHandler(Call30, Handle_Func30, pass_args = TRUE)
    }


    # if(exists("Fooxxxx")){
    #    Handle_Funcxxxx <- function( bot, updates, args){
    #       Add_Msg <- ""
    #       if(Argsxxxx) {
    #          if (length(args) == 0L) {
    #             Result <- list()
    #             Result$error <- glue::glue("=====> Function not executed as no arguments provided:\nFunction_List indicated that '/{Callxxxx}' requires arguments, but none was provided.\nEither:\n* Add Arguments or\n* Set Args = FALSE in Function_List")
    #          } else {
    #             Result <- Fooxxxx(args)
    #          }
    #       }  else {
    #          if (length(args) > 0L) {
    #             Add_Msg <- glue::glue("|\n:::NOTE::::\n* Argument for '/{Callxxxx}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for.")
    #             message(glue::glue(":::NOTE::::\n* Argument for '/{Callxxxx}' ignored as 'Args' was set to FALSE.\nPlease set Function_List call for Args to TRUE if you want to have your arguments accounted for."))
    #          }
    #          Result <- Fooxxxx()
    #       }
    #
    #
    #       if(!is.null(Result$error)) {
    #
    #          bot$get_updates(offset = updates$update_id + 1)
    #          bot$sendMessage(chat_id =ID,
    #                          text = glue::glue("***ERROR Produced***: \n Function: {Callxxxx}\n\n * Error details:\n {Result$error}"))
    #          message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
    #       } else {
    #
    #          bot$get_updates(offset = updates$update_id + 1)
    #          bot$sendMessage(chat_id =ID,
    #                          text = glue::glue("Executed: {Callxxxx}{Add_Msg}"))
    #
    #       }
    #    }
    #    updater <- updater + CommandHandler(Callxxxx, Handle_Funcxxxx, pass_args = TRUE)
    # }


    if(FuncN > 20){
      stop("Foo_Bot currently supports 10 functions. You can change the source code if you wish to add more...")
    }

    # Now add these functions to handles to be interpreted by Telegram.
    # Handler function requires input of function name.

    # Kill Command

    #
    #
    PokePC_Msg <- ""
    RestartPC_Msg <- ""
    KillR_Msg <- ""
    KillCPU_Msg <- ""
    #

    # Kill Connection
    kill <- function(bot, update){
      bot$sendMessage(chat_id = update$message$chat_id,
                      text = "\nBot Connection Closed Successfully\n")
      # Clean 'kill' update
      bot$getUpdates(offset = update$update_id + 1L)
      # Stop the updater polling
      updater$stop_polling()
    }

    updater <<- updater + telegram.bot::CommandHandler("kill", kill)

    #
    PokePC_Msg <- ""
    RestartPC_Msg <- ""
    KillR_Msg <- ""
    KillCPU_Msg <- ""
    #
    if(KillR) {

      killR <- function(bot, updates){

        bot$sendMessage(chat_id =ID,
                        text = "Your R session will now restart itself...Connection will be lost.\n Goodbye!")
        Sys.sleep(3)
        .rs.restartR()
      }

      updater <<- updater + telegram.bot::CommandHandler("killR", killR)
      KillR_Msg <- "\n...........\nRestart your R session using: \n * /killR\n"

    }

    if(PokePC) {

      Poke <- function () {
        message(paste0("PC Poked at ", Sys.time()))
      }


      PokePC <- function(bot, updates){
        bot$get_updates(offset = updates$update_id + 1)
        bot$sendMessage(chat_id =ID,
                        text = "PC has been poked - you are still connected...")
        Poke()
      }

      updater <<- updater + telegram.bot::CommandHandler("PokePC", PokePC)
      PokePC_Msg <- "\n...........\nThis function will poke your PC to check that it is still connected (no side-effects, just a gentle poke...): \n * /PokePC\n"
    }


    if(RestartCPU) {

      Rstart <- function () {
        Sys.sleep(5)
        shell("shutdown -r -t 0 -f")
      }


      RestartCPU <- function(bot, updates){
        bot$get_updates(offset = updates$update_id + 1)
        bot$sendMessage(chat_id =ID,
                        text = "You've elected to restart your PC in 5 sec.... Bye!")
        Rstart()
      }

      updater <<- updater + telegram.bot::CommandHandler("RestartCPU", RestartCPU)
      RestartPC_Msg <- "\n...........\nThis will force restart your computer (no questions asked...) by typing: \n * /RestartCPU\n"
    }

    if(KillCPU) {

      Shutdown <- function () {
        Sys.sleep(10)
        shell("shutdown -s -t 0")
      }


      killCPU <- function(bot, updates){

        bot$sendMessage(chat_id =ID,
                        text = "Well that escalated quickly! CPU will force shutdown in 10 sec.... Bye!")
        Shutdown()
      }

      updater <<- updater + telegram.bot::CommandHandler("killCPU", killCPU)
      KillCPU_Msg <- "\n...........\nCompletely shut down your computer (no questions, just shutting down...) by typing: \n * /killCPU\n"
    }

    BotMsg <-
      paste(unlist(MsgLog), collapse = "\n")
    BotMsg <-
      glue::glue("\n {LoadMessage} \n\nFUNCTIONS:\n========================\n{BotMsg}\n\nGENERAL INSTRUCTIONS\n========================\nTo Kill this open port:\n * Type: /kill\n{PokePC_Msg}{RestartPC_Msg}{KillR_Msg}{KillCPU_Msg} \n ========================\nConnection opened: {Sys.time()} \n ========================")


    if(!is.null(Bot_Name)){

      Text_Bot(Msg = BotMsg, Bot_Name = Bot_Name, Token = NULL, Info_Loc = Info_Loc, Silent = TRUE)

    } else {

      Text_Bot(Msg = BotMsg, Token = Token, Info_Loc = Info_Loc, Silent = TRUE)

    }

    # Msg Command
    Msg <- function(bot, updates){

      bot$get_updates(offset = updates$update_id + 1)
      bot$sendMessage(chat_id =ID,
                      text = glue::glue("\nConnection opened: {Sys.time()}"))
    }

    updater <- updater + telegram.bot::CommandHandler("Msg", Msg)

    message(glue::glue("\n=================\nOpening Connection with {Bot_Name} at {Sys.time()}.\n=================\nSee your phone's Telegram app for instructions...."))

    unknown <- function(bot, update){
      bot$sendMessage(chat_id = update$message$chat_id,
                      text = "Sorry, I did not understand that command (did you use / before the command?)\nPlease try again.")
    }

    updater <- updater + MessageHandler(unknown, MessageFilters$command)

    updater$start_polling(clean = T)

  }


}
