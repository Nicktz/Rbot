Rbot: Making R and your Phone Best Friends
------------------------------------------

#### Brief Overview

This package is intended to be a simplifying tool to make R and your phone communicate, and motivate why this is a powerful combination. Rbot is a wrapper to the APIs developed from the Telegram community, and facilitates the use of the telegram and telegram.bot apps in R.

> Using this package, your workflow for setting up with your phone to talk to R is as simple as 1 2 3 4:

1.  Install the Telegram app on your phone.

2.  Open the app, and then send [botfather](https://core.telegram.org/bots#6-botfather) the message: /newbot.

-   Click on the t.me/<BotName>\_bot link in the "Done! Congratulations..." message from botfather. Press start below and send your bot a message ("Hi Bot!")

1.  In R, run the following code to save your bot's credentials on your computer (assuming default locations apply):

``` r
Token = YOURTOKEN  # given by the Botfather... Looks like: 3148997513::AFSIBIGEk2352235mpfFA
Add_Bot(Token = Token) # By default, this is saved in: path.expand("~")
```

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

This package makes it easy to manage R from your phone using the Telegram App on your phone. You should be ready to go in less than a minute provided you:

-   Have the telegram app installed on your phone.

-   Messag botfather (he will appear amongst your contacts automatically) the following:

> /newbot

-   Now simply follow instructions from the Botfather and create a new bot.

    -   Then, take note of the **Token** botfather provided, and click on the link: t.me/<yourbotname> in the confirmation message from botfather.

    -   This takes you to the bot. Hit start, and send the bot a simple message to initialise your chat (simply type "hi").

    -   You can check the validity of your bot by typing into a browser (replacing YOURTOKEN with your token...): <https://api.telegram.org/botYOURTOKEN/getMe>.

    -   If the browser does not give you an error message, it is a valid bot. It should also show you the bot name and bot username.

> Now simply remember your Token...and try the functions below!

Add\_Bot
--------

This function makes it easy to store all the needed information from your bot for communication from R, to your computer.

This makes it easier to use your bots, as you will be otherwise required to always input Tokens and IDs (which could be a kill-joy).

This little function will, by default, use the **path.expand("~")** location on your computer, but this can be overridden by setting path below.

``` r
# Check where this is located on your computer:
path.expand("~")
```

Now, you can save your bot's information as follows (you will be prompted to enter your bot's name that appears in your console...):

``` r

Token = YOURTOKEN  # given by the Botfather...
Add_Bot(Token = Token)
```

You can override the parameters to save this to a specific directory. See ?Add\_Bot

If you chose the default, and your bot's name is: **BOTNAME**, you can now use the following to send messages from R to your bot:

``` r

Msg <- "Hello!"
Text_Bot(Msg, Bot_Name = BOTNAME, Info_Loc = NULL, Token = NULL, Silent = TRUE)
```

You can now use messages in your workflow simply, e.g.:

``` r
Bot_Name <- "xxxx"
    
Text_Bot(Msg = "Analysis Started", Bot_Name)
foo(x, y)
foo2(x, y)
Text_Bot(Msg = "Analysis Ended", Bot_Name)
```

Or of course even send yourself pdf versions of your markdown files after it is built, or figures of your ggplots.

The possibility of using these wrappers is endless.

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
Call = "F1",  # How to call your function from Telegram
Message = "First function executed. This function sources the data fetch.")    # What to say when executed in Telegram

Second_Foo <- function(){

x <- 100
y <- 20

message("....Function 2 executed....")
}


Function_List$Foo2 <-
list(Function = Second_Foo,
       Call = "F2",  # How to call your function from Telegram
       Message = "Report is now being built.")    # What to say when executed in Telegram



Error_Foo <- function(){
log(a)
message("Example of error.")

}

Function_List$FooError <-
list(Function = Error_Foo,
Call = "Error_Exmple",  # How to call your function from Telegram
Message = "\nError function illustrated: \nThis illustrates that the connection with the phone will  be preserved using purrr::safely")    # What to say when executed in Telegram



 Bot_Name <- "Gobot"
 Foo_Bot(Bot_Name = Bot_Name, Function_List = Function_List, LoadMessage = "My connection with R" )
    
```

#### Safe functions

-   The functions above are safe - implying an error will not break the connection.

-   Using the purrr package's safely call, it will produce an error message, while keeping the connection open.

-   See the silly example below.

> Also note, in this example I add the command to restart R (as opposed to simply stopping connection), as well as a command to shut down the computer:

``` r

Function_List <- list()

Error_Foo <- function(){

log(a)
print("....Message 1 executed....")

}

Function_List$Foo1 <-
list(Function = Error_Foo,
Call = "Error_func",  # How to call your function from Telegram
Message = "First function executed.")    # What to say when executed in Telegram

Second_Foo <- function(){

x <- 100
y <- 20

print("....Message 2 executed....")
}

Function_List$Foo2 <-
list(Function = Second_Foo,
       Call = "F2",  # How to call your function from Telegram
       Message = "Report is now being built.")    # What to say when executed in Telegram

 Bot_Name <- "NewBot"
 Foo_Bot(Bot_Name = Bot_Name, Function_List = Function_List, LoadMessage = "My connection with R", KillR = TRUE, KillCPU = TRUE )
```

#### Source functions

Ideally, you should use foo\_bot to source R scripts that achieve your intended goals.

#### More

For more details on the functions, use: ?<FunctionName>

I hope you enjoy the teamwork from R and your phone from now on. You are welcome to submit any additions to this package on github.
