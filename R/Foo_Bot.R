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
#' @param Function_List Function_List containing for each function: The function to be called, the call handle, as well as the message sent to Telegram.
#' @param LoadMessage What to display on your phone when this call is initialised
#' @param KillR Add option to kill R from your phone. Default to TRUE
#' @param KillCPU Add option to turn off your computer completely, no questions asked. Default to FALSE. Useful if working e.g. on a public CPU that you want to log out from remotely.
#' @importFrom purrr safely
#' @importFrom telegram.bot Bot
#' @importFrom telegram.bot Updater
#' @importFrom telegram.bot CommandHandler
#' @importFrom glue glue
#' @examples \dontrun{
#' Create function list to fill with your commands. Each function needs a Function, Call and Message. You will be alerted if one is missing.
#' Function_List <- list()
#'First_Foo <- function(){
#'x <- 10
#'y <- 20
#'message("....Message 1 executed....")
#'x+y }
#'Function_List$Foo1 <-
#'list(Function = First_Foo,
#'Call = "F1",  # How to call your function from Telegram
#'Message = "First function executed. This function sources the data fetch.")
#'Second_Foo <- function(){
#'x <- 100
#'y <- 20
#'message("....Message 2 executed....")
#' x*y
#'}
#'Function_List$Foo2 <-
#'list(Function = Second_Foo,
#'       Call = "F2",  # How to call your function from Telegram
#'       Message = "Report is now being built.")
#' Bot_Name <- "MyBot"
#' Foo_Bot(Bot_Name = Bot_Name, Function_List = Function_List, LoadMessage = "My connection with R" )
#' }
#' @export
#'

Foo_Bot <- function( Bot_Name = NULL, Info_Loc = NULL, Token = NULL,
                     Function_List,
                     LoadMessage = "\nConnection established with R\n",
                     KillR = TRUE, KillCPU = FALSE) {


  # Check functions and their calls :
  if(class(Function_List) != "list") stop("\n\nProvide functions, their calls and their messages in a list. Please see ?Foo_Bot\n\n")

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
    ID <<- Bot_Info$ID
    Token <- Bot_Info$Token

  } else
    if( is.null(Bot_Name) & !is.null(Token) ){
    ID <- NULL
      bot <<- telegram.bot::Bot(token = Token)
    }

  # Bot calls and setup
  bot <<- telegram.bot::Bot(token = Token)
  updater <<- telegram.bot::Updater(token = Token)
  dispatcher <- updater$dispatcher
  updates <- bot$get_updates()
  if(is.null(ID)) ID <<- unique(updates[[1L]]$from_chat_id())

  # Function map creator.
  # Add handles...

  MsgLog <- list()
  FuncList <- list()
  FuncN <- length(Function_List)
  for(i in 1:FuncN){
    # i=1
    Foo <- Function_List[[i]]$Function
    Call <- Function_List[[i]]$Call
    Msg <- Function_List[[i]]$Message

    if(is.null(Foo) | !is.function(Foo)) stop("Each Function_List entry should have a valid function labelled as Function. See ?Foo_Bot example.")
    if(is.null(Call) | !is.character(Call)) stop("Each Function_List entry should have a valid Call labelled as Call. See ?Foo_Bot example.")
    if(is.null(Msg) | !is.character(Msg)) stop("Each Function_List entry should have a valid Message labelled as Message. See ?Foo_Bot example.")

    assign(paste0("Foo", i), purrr::safely(Foo, otherwise = "FunctionFail")) # Ensure function is safe. Will alert if there is an error, not break connection.
    assign(paste0("Call", i), Call)

    MsgLog[i] <-
      list(
        glue::glue("\n* Call: /{Call}\n  - Description: {Msg}\n..............\n" )
      )
  }

  if(exists("Foo1")){

    Handle_Func1 <- function( bot, update){

      Result <- Foo1()

      if(!is.null(Result$error)) {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("***ERROR Produced***: \n Function: {Call1}\n\n * Error details:\n {Result$error}"))
        message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
      } else {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("Executed: {Call1}"))

      }


    }
    # Handle_Pass1 <- telegram.bot::CommandHandler(Call, Handle_Func1, pass_args = TRUE)
    Handle_Pass1 <- telegram.bot::CommandHandler(command = Call1, Handle_Func1)
    dispatcher$add_handler(Handle_Pass1)

  }

  if(exists("Foo2")){
    Handle_Func2 <- function( bot, update){
      Result <- Foo2()

      if(!is.null(Result$error)) {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("***ERROR Produced***: \n Function: {Call2}\n\n * Error details:\n {Result$error}"))
        message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
      } else {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("Executed: {Call2}"))

      }
    }
    # Handle_Pass2 <- telegram.bot::CommandHandler(Call, Handle_Func2, pass_args = TRUE)
    Handle_Pass2 <- telegram.bot::CommandHandler(command = Call2, Handle_Func2)
    dispatcher$add_handler(Handle_Pass2)
  }

  if(exists("Foo3")){
    Handle_Func3 <- function( bot, update){
      Result <- Foo3()

      if(!is.null(Result$error)) {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("***ERROR Produced***: \n Function: {Call3}\n\n * Error details:\n {Result$error}"))
        message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
      } else {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("Executed: {Call3}"))

      }
    }
    # Handle_Pass3 <- telegram.bot::CommandHandler(Call, Handle_Func3, pass_args = TRUE)
    Handle_Pass3 <- telegram.bot::CommandHandler(command = Call3, Handle_Func3)
    dispatcher$add_handler(Handle_Pass3)
  }
  if(exists("Foo4")){
    Handle_Func4 <- function( bot, update){
      Result <- Foo4()

      if(!is.null(Result$error)) {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("***ERROR Produced***: \n Function: {Call4}\n\n * Error details:\n {Result$error}"))
        message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
      } else {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("Executed: {Call4}"))

      }
    }
    # Handle_Pass4 <- telegram.bot::CommandHandler(Call, Handle_Func4, pass_args = TRUE)
    Handle_Pass4 <- telegram.bot::CommandHandler(command = Call4, Handle_Func4)
    dispatcher$add_handler(Handle_Pass4)
  }

  if(exists("Foo5")){
    Handle_Func5 <- function( bot, update){
      Result <- Foo5()

      if(!is.null(Result$error)) {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("***ERROR Produced***: \n Function: {Call5}\n\n * Error details:\n {Result$error}"))
        message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
      } else {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("Executed: {Call5}"))

      }
    }
    # Handle_Pass5 <- telegram.bot::CommandHandler(Call, Handle_Func5, pass_args = TRUE)
    Handle_Pass5 <- telegram.bot::CommandHandler(command = Call5, Handle_Func5)
    dispatcher$add_handler(Handle_Pass5)
  }

  if(exists("Foo6")){
    Handle_Func6 <- function( bot, update){
      Result <- Foo6()

      if(!is.null(Result$error)) {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("***ERROR Produced***: \n Function: {Call6}\n\n * Error details:\n {Result$error}"))
        message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
      } else {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("Executed: {Call6}"))

      }
    }
    # Handle_Pass6 <- telegram.bot::CommandHandler(Call, Handle_Func6, pass_args = TRUE)
    Handle_Pass6 <- telegram.bot::CommandHandler(command = Call6, Handle_Func6)
    dispatcher$add_handler(Handle_Pass6)

  }
  if(exists("Foo7")){
    Handle_Func7 <- function( bot, update){
      Result <- Foo7()

      if(!is.null(Result$error)) {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("***ERROR Produced***: \n Function: {Call7}\n\n * Error details:\n {Result$error}"))
        message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
      } else {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("Executed: {Call7}"))

      }
    }
    # Handle_Pass7 <- telegram.bot::CommandHandler(Call, Handle_Func7, pass_args = TRUE)
    Handle_Pass7 <- telegram.bot::CommandHandler(command = Call7, Handle_Func7)
    dispatcher$add_handler(Handle_Pass7)

  }

  if(exists("Foo8")){
    Handle_Func8 <- function( bot, update){
      Result <- Foo8()

      if(!is.null(Result$error)) {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("***ERROR Produced***: \n Function: {Call8}\n\n * Error details:\n {Result$error}"))
        message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
      } else {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("Executed: {Call8}"))

      }
    }
    # Handle_Pass8 <- telegram.bot::CommandHandler(Call, Handle_Func8, pass_args = TRUE)
    Handle_Pass8 <- telegram.bot::CommandHandler(command = Call8, Handle_Func8)
    dispatcher$add_handler(Handle_Pass8)

  }

  if(exists("Foo9")){
    Handle_Func9 <- function( bot, update){
      Result <- Foo9()

      if(!is.null(Result$error)) {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("***ERROR Produced***: \n Function: {Call9}\n\n * Error details:\n {Result$error}"))
        message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
      } else {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("Executed: {Call9}"))

      }
    }
    # Handle_Pass9 <- telegram.bot::CommandHandler(Call, Handle_Func9, pass_args = TRUE)
    Handle_Pass9 <- telegram.bot::CommandHandler(command = Call9, Handle_Func9)
    dispatcher$add_handler(Handle_Pass9)

  }

  if(exists("Foo10")){
    Handle_Func10 <- function( bot, update){
      Result <- Foo10()

      if(!is.null(Result$error)) {

        # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("***ERROR Produced***: \n Function: {Call10}\n\n * Error details:\n {Result$error}"))
        message(glue::glue("\n***Function ERROR*** \n{Result$error}\n"))
      } else {

        # # bot$get_updates(offset = update$update_id + 1)
        bot$sendMessage(chat_id = ID,
                        text = glue::glue("Executed: {Call10}"))

      }
    }
    # Handle_Pass10 <- telegram.bot::CommandHandler(Call, Handle_Func10, pass_args = TRUE)
    Handle_Pass10 <- telegram.bot::CommandHandler(command = Call10, Handle_Func10)
    dispatcher$add_handler(Handle_Pass10)

  }

  if(FuncN > 10){
    stop("Foo_Bot currently supports 10 functions. You can change the source code if you wish to add more...")
  }

  # Now add these functions to handles to be interpreted by Telegram.
  # Handler function requires input of function name.

  # Kill Command
  kill <- function(bot, update){

    bot$sendMessage(chat_id = ID,
                    text = "\nBot Connection Closed Successfully\n")
    bot$get_updates(offset = update$update_id + 1)
    # Stop the updater polling
    updater$stop_polling()

  }

  kill_handler <- telegram.bot::CommandHandler('kill', kill)
  dispatcher$add_handler(kill_handler)



  #
  #
  KillR_Msg <- ""
  KillCPU_Msg <- ""
  #
  if(KillR) {

    killR <- function(bot, update){

      bot$sendMessage(chat_id = ID,
                      text = "Your R session will now restart itself...Connection will be lost.\n Goodbye!")
      Sys.sleep(3)
      .rs.restartR()
    }

    killR_handler <- telegram.bot::CommandHandler('killR', killR)
    dispatcher$add_handler(killR_handler)
    KillR_Msg <- "\n...........\nRestart your R session using: \n * /killR\n"

  }



  if(KillCPU) {

    Shutdown <- function () {
      Sys.sleep(10)
      shell("shutdown -s -t 0")
    }


    killCPU <- function(bot, update){

      bot$sendMessage(chat_id = ID,
                      text = "Well that escalated quickly! CPU will shutdown in 10 sec.... Bye!")
      Shutdown()
    }

    killCPU_handler <- telegram.bot::CommandHandler('killCPU', killCPU)
    dispatcher$add_handler(killCPU_handler)
    KillCPU_Msg <- "\n...........\nCompletely shut down your computer (no questions, just shutting down...) by typing: \n * /killCPU\n"
  }

  BotMsg <-
    paste(unlist(MsgLog), collapse = "\n")
  BotMsg <-
    glue::glue("\n {LoadMessage} \n\nFUNCTIONS:\n========================\n{BotMsg}\n\nGENERAL INSTRUCTIONS\n========================\nTo Kill this open port:\n * Type: /kill\n{KillR_Msg}{KillCPU_Msg} \n ========================\nConnection opened: {Sys.time()} \n ========================")


  if(!is.null(Bot_Name)){

    Text_Bot(Msg = BotMsg, Bot_Name = Bot_Name, Token = NULL, Info_Loc = Info_Loc, Silent = TRUE)

  } else {

    Text_Bot(Msg = BotMsg, Token = Token, Info_Loc = Info_Loc, Silent = TRUE)

  }

  # Msg Command
  Msg <- function(bot, update){

    bot$sendMessage(chat_id = ID,
                    text = glue::glue("\nConnection opened: {Sys.time()}"))
    bot$get_updates(offset = update$update_id)
  }

  Msg_handler <- telegram.bot::CommandHandler('Msg', Msg)
  dispatcher$add_handler(Msg_handler)

  message(glue::glue("\n=================\nOpening Connection with {Bot_Name} at {Sys.time()}.\n=================\nSee your phone's Telegram app for instructions...."))
  updater$start_polling()

}
