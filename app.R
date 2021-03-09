#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!(require(shiny))){
    install.packages("shiny", quiet = T)
    require(shiny)
}
if (!(require(shiny.i18n))){
    install.packages("shiny.i18n", quiet = T)
    require(shiny.i18n)
}
if (!(require(shinyWidgets))){
    install.packages("shinyWidgets", quiet = T)
    require(shinyWidgets)
}
if (!(require(rdrop2))){
    install.packages("rdrop2", quiet = T)
    require(rdrop2)
}
if (!(require(RSQLite))){
    install.packages("RSQLite", quiet = T)
    require(RSQLite)
}
if (!(require(showtext))){
    install.packages("showtext", quiet = T)
    require(showtext)
}
showtext_auto()

##################################
####    IMPORTANT ################
# Have to be configured to use SQLite - Uncomment after done
# sqlitePath <- "/path/to/sqlite/database"
# check https://shiny.rstudio.com/articles/persistent-data-storage.html#SQLite for instructions
##################################
####    IMPORTANT  ###############
# To use Dropbox, you'll need the .httr-oauth file
# To create this file, use the following command
# library(rdrop2)
# drop_auth()
# This will launch your browser and request access to your Dropbox account. You will be prompted to log in if you aren't already logged in.
# Once completed, close your browser window and return to R to complete authentication.
# The credentials are automatically cached (you can prevent this) for future use.
##################################


translator <- Translator$new(translation_csvs_path = "data")

# setup the order of language with hard coding
languages <- c ("中文","en")

# Define UI
ui <- uiOutput('page_content')

# Define server logic
server <- function(input, output, session) {
    # state variable initialization
    state = reactiveVal(0)
    prevstate = reactiveVal(0)
    # state update - have to add cn translation
    observeEvent(input$go, {
        # Check if name email is there
        if (input$name == ''){
            sendSweetAlert(
                session = session,
                title = i18n()$t("ERROR!"),
                text = i18n()$t("Please enter your name and email*"),
                type = "error"
            )
        } else { tryCatch({
        if (input$choice == i18n()$t("Part of management")){
            state(1)
        } else if (input$choice == i18n()$t("Owner/Maintainer of one of the pilot projects/ potential pilot projects")){
            state(2)
        } else if (input$choice == i18n()$t("Developer, not in any of the above roles")){
            state(3)
        } }, error=function(cond) {
            sendSweetAlert(
                session = session,
                title = i18n()$t("ERROR!"),
                text = i18n()$t("Select an option!"),
                type = "error"
            )
            state(0)
        })}
    })

    i18n <- reactive({
        
        selected <- input$selected_language
        if (length(selected) > 0 && selected %in% translator$get_languages()) {
        translator$set_translation_language(selected)
        }
        translator
    })
    
    # #save to dropbox
    # observeEvent(input$done, {
    #     tryCatch({
    #         if (state() == 1){
    #             df = data.frame(type='Manager',ID=input$name, v1=input$M1, v2=input$M2, v3=input$M3, v4=input$M4, v5=input$M5, v6=input$M6, v7=input$M7, other=input$otherConcern, positive = input$positive)
    #             fname = sprintf('Manager_%s.csv',as.integer(Sys.time()))
    #         } else if (state() == 2){
    #             df = data.frame(type='Owner',ID=input$name,v1=input$O1, v2=input$O2, v3=input$O3, v4=input$O4, v5=input$O5, v6=input$O6, v7=input$O7, v8=input$O8, v9=input$O9, v10=input$O10, v11=input$O11, other=input$otherConcern, positive = input$positive)
    #             fname = sprintf('Owner_%s.csv',as.integer(Sys.time()))
    #         } else if (state() == 3){
    #             df = data.frame(type='Developer',ID=input$name,v1=input$D1, v2=input$D2, v3=input$D3, v4=input$D4, v5=input$D5, v6=input$D6, v7=input$D7, v8=input$D8, other=input$otherConcern, positive = input$positive)
    #             fname = sprintf('Developer_%s.csv',as.integer(Sys.time()))
    #         }
    #         write.table(df, file = fname, row.names = F, sep = ',')
    #         drop_upload(fname, path = 'Public/HS_data')
    #         sendSweetAlert(
    #             session = session,
    #             title = i18n()$t("Thank you!"),
    #             text = i18n()$t("The File has been saved in Dropbox"),
    #             type = "success"
    #         )
    #     state(4)
    #     }, error=function(cond) {
    #         sendSweetAlert(
    #             session = session,
    #             title = i18n()$t("ERROR!"),
    #             text = i18n()$t("Dropbox Authentication is not properly set up"),
    #             type = "error"
    #         )
    #     })
    # 
    # })

    #save to local storage
    observeEvent(input$local, {
        tryCatch({
            if (state() == 1){
                df = data.frame(type='Manager',ID=input$name, v2=input$M2, v3=input$M3, v4=input$M4, v5=input$M5, v6=input$M6, v7=input$M7, other=input$otherConcern, positive = input$positive)
                fname = sprintf('Manager_%s.csv',as.integer(Sys.time()))
            } else if (state() == 2){
                df = data.frame(type='Owner',ID=input$name,v1=input$O1, v2=input$O2, v3=input$O3, v4=input$O4, v5=input$O5, v6=input$O6, v7=input$O7, v8=input$O8, v9=input$O9, v10=input$O10, v11=input$O11, other=input$otherConcern, positive = input$positive)
                fname = sprintf('Owner_%s.csv',as.integer(Sys.time()))
            } else if (state() == 3){
                df = data.frame(type='Developer',ID=input$name,v1=input$D1, v2=input$D2, v3=input$D3, v4=input$D4, v5=input$D5, v6=input$D6, v7=input$D7, v8=input$D8, other=input$otherConcern, positive = input$positive)
                fname = sprintf('Developer_%s.csv',as.integer(Sys.time()))
            }
            write.table(df, file = fname, row.names = F, sep = ',')

            sendSweetAlert(
                session = session,
                title = i18n()$t("Thank you!"),
                text = i18n()$t("The File has been saved"),
                type = "success"
            )
            prevstate(state())
            state(4)
        }, error=function(cond) {
            sendSweetAlert(
                session = session,
                title = i18n()$t("ERROR!"),
                text = i18n()$t("Something went wrong"),
                type = "error"
            )
        })

    })


    # # Download data
    # output$saveData = downloadHandler(
    #     filename = function() {
    #         sprintf('data_%s.csv',as.integer(Sys.time()))
    #     },
    #     content = function(file) {
    #         if (state() == 1){
    #             df = data.frame(type='Manager',ID=input$name,v1=input$M1, v2=input$M2, v3=input$M3, v4=input$M4, v5=input$M5, v6=input$M6, v7=input$M7, other=input$otherConcern, positive = input$positive)
    #             write.table(df, file = file,  row.names = F, sep = ',')
    #         } else if (state() == 2){
    #             df = data.frame(type='Owner',ID=input$name,v1=input$O1, v2=input$O2, v3=input$O3, v4=input$O4, v5=input$O5, v6=input$O6, v7=input$O7, v8=input$O8, v9=input$O9, v10=input$O10, v11=input$O11, other=input$otherConcern, positive = input$positive)
    #             write.table(df, file = file,  row.names = F, sep = ',')
    # 
    #         } else if (state() == 3){
    #             df = data.frame(type='Developer',ID=input$name,v1=input$D1, v2=input$D2, v3=input$D3, v4=input$D4, v5=input$D5, v6=input$D6, v7=input$D7, v8=input$D8, other=input$otherConcern, positive = input$positive)
    #             write.table(df, file = file,  row.names = F, sep = ',')
    #         }
    #     }
    # )

    ## save to SQLite database
    # observeEvent(input$sql, {
    #     tryCatch({
    #         if (state() == 1){
    #              table <- "responses_Manager" #--- NEED TO BE CREATED FIRST
    #             df = data.frame(type='Manager',ID=input$name,v1=input$M1, v2=input$M2, v3=input$M3, v4=input$M4, v5=input$M5, v6=input$M6, v7=input$M7, other=input$otherConcern, positive = input$positive)
    #         } else if (state() == 2){
    #             table <- "responses_Owner" #--- NEED TO BE CREATED FIRST
    #             df = data.frame(type='Owner',ID=input$name,v1=input$O1, v2=input$O2, v3=input$O3, v4=input$O4, v5=input$O5, v6=input$O6, v7=input$O7, v8=input$O8, v9=input$O9, v10=input$O10, v11=input$O11, other=input$otherConcern, positive = input$positive)
    #         } else if (state() == 3){
    #             table <- "responses_Developer" #--- NEED TO BE CREATED FIRST
    #             df = data.frame(type='Developer',ID=input$name,v1=input$D1, v2=input$D2, v3=input$D3, v4=input$D4, v5=input$D5, v6=input$D6, v7=input$D7, v8=input$D8, other=input$otherConcern, positive = input$positive)
    #             }
    #         #state(4)
    #         # Connect to the database
    #         db <- dbConnect(SQLite(), sqlitePath)
    #         # Construct the update query by looping over the data fields
    #         query <- sprintf(
    #             "INSERT INTO %s (%s) VALUES ('%s')",
    #             table,
    #             paste(names(df), collapse = ", "),
    #             paste(df, collapse = "', '")
    #         )
    #         # Submit the update query and disconnect
    #         dbGetQuery(db, query)
    #         dbDisconnect(db)
    #         sendSweetAlert(
    #             session = session,
    #             title = i18n()$t("Thank you!"),
    #             text = i18n()$t("The File has been saved in SQLIite DB"),
    #             type = "success"
    #         )
    #     },
    #     error=function(cond) {
    #         sendSweetAlert(
    #             session = session,
    #             title = i18n()$t("ERROR!"),
    #             text = i18n()$t("SQLite is not properly set up"),
    #             type = "error"
    #         )
    #     })
    # 
    # })
    # UI
    output$page_content <- renderUI({
        fluidPage(
                div(style = "float: right;",
                    selectInput('selected_language',
                                i18n()$t("Change language"),
                                choices = languages,
                                selected = input$selected_language),
                ),
            
        
        # Application title
        title = i18n()$t("InnerSource Obstacles Survey"),
        useSweetAlert(),
        h1(i18n()$t("InnerSource Obstacles Survey")),
        br(),br(),
        # changing content
        fluidRow(
            column(12,htmlOutput('content')),
            br(),
        # questions - Initial text
        if (state() == 0){
            column(12, textInput("name", width = '100%', label = h4(i18n()$t("Please enter your name and email*")), value = ""),)
        },
        if (state() == 0){
            column(12, radioButtons("choice", width = '100%',
                         label = h4(i18n()$t("* Initially, we are starting a number of pilot projects to fine-tune the InnerSource process. To inform you better about how you can be a part of this exciting and important initiative, we first ask you to identify your role:")),
                         choices = list(i18n()$t("Part of management"),
                                        i18n()$t("Owner/Maintainer of one of the pilot projects/ potential pilot projects"),
                                        i18n()$t("Developer, not in any of the above roles")),
                         selected = 1))
       } else if (state() == 1 | state() == 2 | state() == 3){
           column(12, h4(i18n()$t("We would like to know more about any concerns you might have so that we can address and account for them. A few common concerns are listed below. For the following questions (starting with CONCERN: ), please rate how big of a concern that particular option is for InnerSource adoption from your personal perspective on a scale of 0 to 10, where 0: 'Not at all a concern', 10: 'Major concern' ")))
       },
       
       br(),
       
       
       if (state() == 0){
           column(2, offset=1, actionBttn("go",label = i18n()$t("Submit"),
                                           style = "pill", color = "danger"))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M1", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: Timeline pressures and feature content commitments might not allow Developers to contribute to InnerSource projects"))
           ))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M2", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: I do not know what InnerSource is/ I am not sure about the “benefits” of InnerSource"))
           ))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M3", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: We don’t have enough resources to let the developers contribute to projects from other teams"))
           ))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M4", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: Contributions from other teams won’t be of sufficient quality"))
           ))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M5", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: Not knowing the person(s) responsible for a bug might slow responses to customers"))
           ))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M6", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: Potential change in the management structure would disrupt the established workflow"))
           ))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M7", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: There might not be a budget allocated for InnerSource"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O1", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: Contributions by others would not be of sufficient quality"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O2", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: People would create issues, but won’t submit any patches, and I’ll have to maintain it"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O3", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: I’m unsure about the responsibilities"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O4", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: I won’t have enough time for mentoring other contributors"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O5", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: If I publish code as InnerSource, others will take the code and the credit"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O6", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: I do not know how to attract other developers to my project"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O7", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: I am not sure what is suitable for InnerSource"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O8", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: My immediate line manager would not be supportive of my contributions to InnerSource"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O9", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: There is too much to learn – new tools, new coding standards, etc."))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O10", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: Not knowing who consumes a shared system or code can make it difficult to add features without risking breaking a consumer"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O11", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: The incentives for all this hard work are not appropriate"))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D1", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: I do not know what InnerSource is"))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D2", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: I do not know how to contribute to InnerSource"))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D3", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: My immediate line manager would not be supportive of my contributions to InnerSource"))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D4", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: No appropriate incentive for me to contribute to InnerSource"))
           ))
       },

       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D5", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: I do not have time to contribute to InnerSource"))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D6", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: There is too much to learn – new tools, new coding standards, etc."))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D7", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: The projects are not interesting to me"))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D8", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: I feel that I’ll be judged by others if I make any mistakes"))
           ))
       },
       if (state() == 1 | state() == 2 | state() == 3){
           column(8, offset = 1, textAreaInput("otherConcern", label = h4(i18n()$t("If you feel that you have concern(s) that is (are) not listed here, please feel free to write that down with the score you think it should have")), value = "")  %>%
                      shiny::tagAppendAttributes(style = 'width: 100%;')
                  )
       },
       if (state() == 1 | state() == 2 | state() == 3){
           column(8, offset = 1, textAreaInput("positive", label = h4(i18n()$t("In addition, if you could think of ONE reason that would make you give InnerSource a try, what would that be?")), value = "")  %>%
                      shiny::tagAppendAttributes(style = 'width: 100%;')
           )
       },
       br(),
       # if (state() == 1 | state() == 2 | state() == 3){
       #     column(4, offset=2, actionBttn("done",label = i18n()$t("Save to Dropbox"),
       #                                    style = "pill", color = "success",
       #                                    icon = icon('save')))
       # },
       if (state() == 1 | state() == 2 | state() == 3){
           column(4, offset=2, actionBttn("local",label = i18n()$t("Save to Local Storage"),
                                          style = "pill", color = "danger",
                                          icon = icon('save')))
       },
       # if (state() == 1 | state() == 2 | state() == 3){
       #     column(2, offset = 0, downloadBttn("saveData",
       #                                        label = i18n()$t("Download Data"),
       #                                        style = "jelly", color = "royal"))
       # },
       # if (state() == 1 | state() == 2 | state() == 3){
       #     column(3, offset=0, actionBttn("sql",label = i18n()$t("Save to SQLite DB"),
       #                                    style = "pill", color = "primary",
       #                                    icon = icon('save')))
       # },
       br(),


        )
    )})
    
    # output content
    output$content <- renderUI({
        state0_line1 = i18n()$t("The InnerSource initiative is of significant importance to the top management who seek to foster a collaborative environment to ensure the long-term sustainability of software projects.")
        state0_line2 = i18n()$t("InnerSource seeks to apply the recipes of success from the Open-Source paradigm within a company and is heavily practiced by top companies around the world, e.g., Microsoft, Baidu, Tencent, SAP, Broadcom, and many more.")
        state0_line3 = i18n()$t("In a <a href='https://tapjdey.github.io/InnerSource_Survey_2020/adoption.html#effect-of-innersource-adoption-on-self'>recent survey</a>, 81% of the participants reported an increase in job satisfaction and 57% reported an increase in productivity in their own daily work after adopting the InnerSource style of working.")
        state0_html = paste("<h4>", state0_line1, "<br>", state0_line2, "<br>", state0_line3, "<hr></h4>")
        
        state1_line1 = i18n()$t("As a member of the management, your support for the InnerSource initiative would be hugely appreciated. 85% of the participants in a <a href='https://tapjdey.github.io/InnerSource_Survey_2020/success.html#attitude-of-the-management-towards-innersource-projects'>recent survey</a>,  confirmed that support from the management is critical for the success of the initiative.")
        state1_line2 = i18n()$t("We humbly request your understanding that although the initiative might face a few hiccups in the beginning, the long-term success will outweigh any short-term disruptions. Research suggests that management support in the initial tentative InnerSource phase is critical to long-term success.")
        state1_line3 = i18n()$t("It might seem that if your employees are spending time fixing other people’s code the productivity of the team will suffer, but we assure you that won’t be the case.")
        state1_line4 = i18n()$t("As mentioned earlier, the productivity of most employees seems to increase when they start working in InnerSource style.")
        state1_line5 = i18n()$t("You would be their guide in this journey, helping them along the way and also keeping them on-track to maintain a balance among the various responsibilities of the employees, and we are sure you’ll be pleasantly surprised when the process stabilizes and you can actually see an increase in the team productivity and innovation.")
        state1_html = paste("<h4>", state1_line1, "<br>", state1_line2, "<br>", state1_line3, " ", state1_line4, " ", state1_line5, "<hr></h4>")
        
        state2_line1 = i18n()$t("We’d like to start by congratulating you for taking on/considering to take on the responsibility of maintaining one of the first InnerSource projects. The journey will be challenging, but in the longer-term, the benefits would likely be worth the effort.")
        state2_line2 = i18n()$t("You have a number of responsibilities in terms of getting the ancillary materials set up, advertising the project to potential contributors, reviewing contributions, and mentoring.")
        state2_line3 = i18n()$t("We strongly suggest you check the “training materials” to better prepare yourself.")
        state2_line4 = i18n()$t("We can promise you that it would be a story worth telling.")
        state2_html = paste("<h4>", state2_line1, "<br>", state2_line2, "<br>", state2_line3, "<br>", state2_line4, "<hr></h4>")
        
        state3_line1 = i18n()$t("As a developer, you are the most valuable player in the InnerSource initiative. It is with your contributions across different projects that the initiative has any hope for success.")
        state3_line2 = i18n()$t("But we request you not to view it as simply another management mandated activity – this one is for your own benefit.")
        state3_line3 = i18n()$t("This initiative lets you work on projects and features YOU like and YOU care about, and, if you’re up for it, work with other people who like the same things you do.")
        state3_line4 = i18n()$t("So, you might be wondering, how do I get started and what do I have to do?")
        state3_line5 = i18n()$t("Well, for starters, we’d encourage you to look at one of the pilot projects and see if you find any of them interesting.")
        state3_line6 = i18n()$t("If you do, please try to see what are the limitations and if you can find any bugs, and do report them.")
        state3_line7 = i18n()$t("If you are so inclined, and it’d be great if you are, please consider adding a feature that you think might be useful and/or fix one of the annoying bugs.")
        state3_html = paste("<h4>", state3_line1, "<br>", state3_line2, "<br>", state3_line3, "<br>", state3_line4, "<br><br>", state3_line5, "<br>", state3_line6, "<br>", state3_line7, "<hr></h4>")
        
        state4_line1 = i18n()$t("Thank You! Your response has been recorded.")
        state4_line2 = i18n()$t("Based on your answers we have a few suggestions that you might find useful, mostly in terms of existing patterns which were proposed by field experts:<br>")
        
        state4_html = paste("<h2>", state4_line1, '</h2><br><br><h3>')
        # Managers' Feedback
        state4_line_IN = i18n()$t("InnerSource refers to the application of successful Open Source development practices within the bounds of a company. For concerns regarding InnerSource principles and benefits, please check out  <a href='https://innersourcecommons.org/getting-started/'> this link for getting started</a>.")
        state4_line_R = i18n()$t("For concerns regarding resource allocation, we suggest starting the initiative as an experiment to figure out what works for your company. You might find the <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/start-as-experiment.md'>start-as-experiment</a> pattern useful.")
        state4_line_Q = i18n()$t("For concerns regarding quality of contributions from outsiders, you might consider implementing the <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/30-day-warranty.md'>30-day Warranty</a> pattern or the <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/service-vs-library.md'>service-vs-library</a> pattern.")
        state4_line_TM = i18n()$t("For concerns regarding InnerSource activity interfering with time and feature commitments, we don't have a proven solution, but setting up a <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/gig-marketplace.md'> gig-marketplace </a> and the ideas proposed in the <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/1-initial/overcoming-project-management-time-pressures.md'>overcoming-project-management-time-pressures</a> pattern might be useful.")
        state4_line_MS = i18n()$t("For concerns regarding potential change in management structure disrupting the workflow, one possible solution could be setting  up a <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/review-committee.md'>review committee</a>.")
        
        # Code Owners' Feedback
        state4_line_TC = i18n()$t("For concerns about having to handle all the work yourself and/or having to mentor every new participant, we strongly suggest assigning a <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/trusted-committer.md'>Trusted Committer</a> for your project.")
        state4_line_LI = i18n()$t("For concerns regarding other people using your code without proper attribution, we suggest using an <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/innersource-license.md'>InnerSource license</a>. This prepares the ground for handling any improper code reuse in future.")
        state4_line_SU = i18n()$t("For concerns about what might be suitable for InnerSource and how to attract other developers, we have a few suggestions: For attracting others, setting up a <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/gig-marketplace.md'>gig-marketplace</a> or an <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/innersource-portal.md'>InnerSource portal</a> might be useful. <br> You can check the suitability of your project by checking the <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/1-initial/good-first-project.md'>good-first-project</a> pattern, and the <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/crossteam-project-valuation.md'>cross-team project valuation</a> pattern gives insights on measuring a project's value across different teams.")
        state4_line_LP = i18n()$t("InnerSource is more about changing the development mindset and less about individual tools. You can check out the InnerSource <a href='https://innersourcecommons.org/resources/learningpath/'>Learning Path</a> to get started.")
        state4_line_IDK = i18n()$t("We are working on designing the appropriate incentives and dealing with problems of dependent projects. In the meantime, some the ideas listed in <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/1-initial/developer-incentive-alignment-for-innersource-contribution.md'>this pattern</a> might be useful.")
        state4_line_LM = i18n()$t("With regards to concerns about getting support from your line manager, working on InnerSource projects on a contract basis, as suggested by the <a href='https://github.com/InnerSourceCommons/InnerSourcePatterns/blob/master/patterns/2-structured/contracted-contributor.md'>contracted-contributor</a> pattern, might be useful.")
        state4_line_PI = i18n()$t("If you are not sure which project to contribute to or fear being judged by others, we suggest you look at a project you think would be useful to you and try to integrate the project/parts of the project in your workflow and follow the project discussions. It is a common practice in Open Source that a user gradually becomes more involved with a project (see <a href='https://speaking.sasharosenbaum.com/zIYMi4/survival-of-the-most-open-microsofts-open-source-journey#sN7WpHK'>this slide</a> for reference).")
        
        
        if (prevstate() == 1){
            state4_html = paste(state4_html,  state4_line2,'<br><ul>')
            if (input$M2 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_IN, '</li><br>')
            }
            if (input$M3 > 4 || input$M7 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_R, '</li><br>')
            }
            if (input$M4 > 4 || input$M5 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_Q, '</li><br>')
            }
            if (input$M6 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_MS, '</li><br>')
            }
            if (input$M1 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_TM, '</li><br>')
            }
        } else if (prevstate() == 2){
            state4_html = paste(state4_html,  state4_line2,'<br><ul>')
            if (input$O1 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_Q, '</li><br>')
            }
            if (input$O2 > 4 || input$O4 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_TC, '</li><br>')
            }
            if (input$O3 > 4 || input$O6 > 4 || input$O7 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_SU, '</li><br>')
            }
            if (input$O5 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_LI, '</li><br>')
            }
            if (input$O8 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_LM, '</li><br>')
            }
            if (input$O9 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_LP, '</li><br>')
            }
            if (input$O10 > 4 || input$O11 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_IDK, '</li><br>')
            }
        } else if (prevstate() == 3){
            state4_html = paste(state4_html,  state4_line2,'<br><ul>')
            if (input$D1 > 4 || input$D2 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_IN, '</li><br>')
            }
            if (input$D3 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_LM, '</li><br>')
            }
            if (input$D4 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_IDK, '</li><br>')
            }
            if (input$D5 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_TM, '</li><br>')
            }
            if (input$D6 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_LP, '</li><br>')
            }
            if (input$D7 > 4 || input$D8 > 4){
                state4_html = paste(state4_html, '<li>', state4_line_PI, '</li><br>')
            }
        }
            state4_html = paste(state4_html, '</ul><hr></h3>')
        if (state() == 0){
            HTML(state0_html)
        } else if (state() == 1){
            HTML(state1_html)
        } else if (state() == 2){
            HTML(state2_html)
        } else if (state() == 3){
            HTML(state3_html)
        } else if (state() == 4){
            HTML(state4_html)
        }
    })



}

# Run the application
shinyApp(ui = ui, server = server)
