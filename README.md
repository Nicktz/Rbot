Rbot: Making R and your Phone Best Friends
------------------------------------------

#### Brief Overview

This package is intended to be a simplifying tool to make R and your phone communicate, and motivate why this is a powerful combination. Rbot is a wrapper to the APIs developed from the Telegram community, and facilitates the use of the telegram and telegram.bot apps in R.

> Using this package, your workflow for setting up with your phone to talk to R is as simple as 1 2 3 4:

1.  Install the [Telegram app](https://telegram.org/) on your phone.

2.  Open the app, and then send [botfather](https://core.telegram.org/bots#6-botfather) the message: **/newbot**.

    -   Note - you will first have to start a conversation with botfather - search for \_\_@botfather\_\_ from the Chats search bar, then start conversation and type /newbot.

-   Click on the t.me/<BotName>\_bot link in the "Done! Congratulations..." message from botfather. Press start below and send your bot a message ("Hi Bot!")

-   Take note of the Token given by @botfather. It is a looong Token that you can easily misspell. **TIP**: mail this to yourself from your phone and copy Token into R (you will likely type something wrong...).

1.  In R, run the following code to save your bot's credentials on your computer (assuming default locations apply):

``` r
Token = YOURTOKEN  # given by the Botfather... Looks like: 3148997513::AFSIBIGEk2352235mpfFA
Add_Bot(Token = Token) # By default, this is saved in: path.expand("~")
```

-   Note: you might get a message that says you "Please first send a message to your intended bot". Go back to Telegram and send your bot a "hi" message. If you get an error: *object 'R\_TELEGRAM\_BOT\_RBot' not found*, you have misspelled your Token (very easy to do).

1.  Now you can send messages, pdfs and images from R to your phone simply as follows:

``` r
# Text:
BOTNAME <- "MyRbot"
Msg <- "Hello!"
Text_Bot(Msg, Bot_Name = BOTNAME)
```

``` r
# PDF:
BOTNAME <- "MyRbot"
Doc_Loc <- "SomePDF.PDF"
Doc_Bot(Doc_Location = Doc_Loc, Bot_Name = BOTNAME)
```

``` r
# Image:
png('Example_Plot.png')
plot(rnorm(100))
dev.off()

BOTNAME <- "MyRbot"
Img_Loc <- "Example_Plot.png"
Pic_Bot(Img_Loc = Img_Loc, Caption = "Cool Figure Right?", Bot_Name = BOTNAME)
```

-   The package also has a workflow for opening a connection that allow you to execute functions in R from your phone - see the next section.

#### Detailed Overview

**Rbot** is a simplified wrapper package that shows a workflow to make it easy to communicate with R from your phone using the [Telegram App](https://telegram.org/). You should be ready to go in less than a minute provided you:

-   Have the telegram app installed on your phone.

-   Message botfather (he will appear amongst your contacts automatically) the following:

> /newbot

-   Now simply follow instructions from the Botfather (on your phone in the Telegram app) and create a new bot.

    -   Then, take note of the **Token** botfather provided, and click on the link: t.me/<yourbotname> in the confirmation message from botfather.

    -   This takes you to the bot. Hit start, and send the bot a simple message to initialise your chat (simply type "hi").

    -   You can check the validity of your bot by typing into a browser on your computer (replacing YOURTOKEN with your token...): <https://api.telegram.org/botYOURTOKEN/getMe>.

    -   If the browser does not give you an error message, it is a valid bot. It should also show you the bot name and bot username.

    -   Telegram bots can receive messages or commands. Commands are always prefixed with a / character (e.g. when you told botfather /newbot).

> Now... simply remember your Token, and try the functions below.

Add\_Bot
--------

The Add\_Bot function makes it easy to store all the needed information of your Telegram bots required for communication from and to R.

This makes it much easier to make use of your bots in R code, as you will otherwise be required to always input Tokens and IDs (which is not easy to memorize).

This little helper function will, by default, use the **path.expand("~")** location on your computer, but this can be overridden by using the **Info\_Loc** argument and specifying your ideal path.

Let's save your bot's information as follows (you will be prompted to enter a name to identify this bot from your R console when running this function...):

``` r

Token = YOURTOKEN  # given by the Botfather...
Add_Bot(Token = Token)
```

You can override the parameters to save this to a specific directory. See ?Add\_Bot

If you chose the default path, your bot's information is saved under the name you entered into the console prompt, with the prefix .Rbots\_. you can now use the following to send messages from R to your newly created bot:

``` r

BOTNAME <- "Nicks_Phone" # You can of course assign a bot to a phone
Msg <- "Hello!"
Text_Bot(Msg, Bot_Name = BOTNAME, Info_Loc = NULL, Token = NULL, Silent = TRUE)
```

You can now use messages in your R code very simply to alert you to progress, messages, errors or whatever text you desire - straight to your phone. E.g.:

``` r

BOTNAME <- "Sams_Phone"
    
Text_Bot(Msg = "Analysis Started", Bot_Name = BOTNAME)
foo(x, y)
foo2(x, y)
Text_Bot(Msg = "Analysis Ended", Bot_Name = BOTNAME)
```

Or you can send yourself pdf versions of your rmarkdown files after it is built, or figures produced using ggplot.

A workflow example could look like this:

``` r

# PDF:
BOTNAME <- "Charlottes_Phone"
Doc_Loc <- "SomePDF.PDF"
Doc_Bot(Doc_Location = Doc_Loc, Bot_Name = BOTNAME)
```

``` r

BOTNAME <- "Gids_Phone"
Rmd_File <- "input.Rmd"

Workflow <- function(BOTNAME, Rmd_File){

# Monthly Report generator function
  
Rbot::Text_Bot(Msg = "Analysis Started", Bot_Name = BOTNAME)

# Quickly create a sanity check figure (which will be deleted after)  
png('Example_Plot.png')
plot(rnorm(100))
dev.off()
Rbot::Pic_Bot(Img_Loc = "Example_Plot.png", Caption = "Check Figure that appears in pdf file.", Bot_Name = BOTNAME)
rm("Example_Plot.png")

rmarkdown::render(Rmd_File, "pdf_document")  
Rbot::Doc_Bot(Doc_Location = gsub("Rmd", "pdf", Rmd_File), Bot_Name = BOTNAME)
Rbot::Text_Bot(Msg = "\nAnalysis complete. Please check pdf built prior to sending out to clients.\n\n * Email me at: UseR@gmail.com if something does not look accurate.", Bot_Name = BOTNAME)

}


Workflow(BOTNAME, Rmd_File)
```

Show\_Bot
---------

This allows you to quickly view what Bots you have saved:

``` r

Show_Bots() # Assumes default path.expand("~") is used.
```

Text\_Bot, Img\_Bot, Doc\_Bot
-----------------------------

All of these functions work on the same premise:

-   Provide your bot's name in the function

-   Add the message, location of a document (e.g. pdf) or the location of a figure on your computer

-   Send your meesage, document or figure to your phone.

By default, the path.expand("~") location will be used (run this in R to see where on your computer the location is) to retrieve the saved Bot's information.

If you prefer not to save your bots' Tokens on your computer, you could instead directly add your Bot's Token to each of the functions.

Foo\_Bot
--------

This function offers a simple workflow for developers to open a connection between Telegram on your phone and their R consoles.

> Note running this function implies your R session will be occupied (talking to Telegram). This can be stopped from your phone or your console.

The main idea behind this function is to allow you to execute a set of functions on your computer from your phone.

Currently, the function allows you to have 10 functions, with your own calls, open for execution from your phone.

The functions will be safely executed, implying if there is an error - it will not interrupt your connection to the phone. Rather, it will show the error message on your phone, and allow you to keep the connection open.

Another useful addition - is to switch your computer completely off from your phone. This is useful when running computers unattended, specifically in public spaces (e.g. from public terminals).

Your ability to scale this function is limited only by your imagination - e.g. using it in a workflow run from a server where your connection to a bot is constantly open. From it, you could then initialise bash scripts from R, that in turn applies production functions.

Below I show a simple example, as well as an illustration of a workflow using **Foo\_Bot**.

``` r

Function_List <- list()

First_Foo <- function(){
x <- 10
y <- 20
print(x*y)
message("....Function 1 executed....")

}

Function_List$Foo1 <-
list(Function = First_Foo,
     # How to call your function from Telegram
     Call = "F1",  
     # What to say when executed in Telegram
     Message = "First function executed. This function sources the data fetch.")    

Second_Foo <- function(){

x <- 100
y <- 20

message("....Function 2 executed....")
}


Function_List$Foo2 <-
list(Function = Second_Foo,
       Call = "F2",
       Message = "Report is now being built.")



Error_Foo <- function(){
log(a)
message("Example of error.")

}

Function_List$FooError <-
list(Function = Error_Foo,
     Call = "Error_Example", 
     Message = "\nError function illustrated: \nThis illustrates that the connection with the phone will  be preserved using purrr::safely") 



 Bot_Name <- "Gobot"
 Foo_Bot(Bot_Name = Bot_Name, Function_List = Function_List, LoadMessage = "My connection with R", 
         KillR = TRUE, KillCPU = FALSE)
    
```

#### Safe functions

-   The functions above are safe - implying an error will not break the connection.

-   You can see e.g. when running the third function call: **/Error\_Example**, it gives you an error, but keeps open the connection.

> Also note, you can restart R with KillR, or even switch off your computer completely by setting KillCPU to TRUE, and typing /killCPU. Take caution - this is a no questions asked shutdown of your pc - great for use on public computers where you are, e.g., downloading data from remotely.

#### More

For more details on the functions, use: ?FunctionName

I hope you enjoy the teamwork from R and your phone from now on. You are welcome to submit any additions to this package on github.
