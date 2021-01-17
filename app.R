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
if (!(require(showtext))){
    install.packages("showtext", quiet = T)
    require(showtext)
}
showtext_auto()

translator <- Translator$new(translation_csvs_path = "data")


# Define UI 
ui <- uiOutput('page_content')

# Define server logic 
server <- function(input, output, session) {
    # state variable initialization
    state = reactiveVal(0)  
    # state update
    observeEvent(input$go, {
        state(input$choice)
    })
    
    i18n <- reactive({
        selected <- input$selected_language
        if (length(selected) > 0 && selected %in% translator$get_languages()) {
            translator$set_translation_language(selected)
        }
        translator
    })
    
    #save to dropbox
    observeEvent(input$done, {
        if (state() == 1){
            df = data.frame(q='Manager',v1=input$M1, v2=input$M2, v3=input$M3, v4=input$M4, v5=input$M5, v6=input$M6, v7=input$M7, other=input$otherConcern, positive = input$positive)
            fname = sprintf('Manager_%s.csv',as.integer(Sys.time()))
            write.table(df, file = fname,  row.names = F, sep = ',')
            drop_upload(fname, path = 'Public/HS_data')
            sendSweetAlert(
                session = session,
                title = i18n()$t("Thank you!"),
                text = i18n()$t("The File has been saved in Dropbox"),
                type = "success"
            )
        } else if (state() == 2){
            df = data.frame(q='Owner',v1=input$O1, v2=input$O2, v3=input$O3, v4=input$O4, v5=input$O5, v6=input$O6, v7=input$O7, v8=input$O8, v9=input$O9, v10=input$O10, v11=input$O11, other=input$otherConcern, positive = input$positive)
            fname = sprintf('Owner_%s.csv',as.integer(Sys.time()))
            write.table(df, file = fname,  row.names = F, sep = ',')
            drop_upload(fname, path = 'Public/HS_data')
            sendSweetAlert(
                session = session,
                title = i18n()$t("Thank you!"),
                text = i18n()$t("The File has been saved in Dropbox"),
                type = "success"
            )
        } else if (state() == 3){
            df = data.frame(q='Developer',v1=input$D1, v2=input$D2, v3=input$D3, v4=input$D4, v5=input$D5, v6=input$D6, v7=input$D7, v8=input$D8, other=input$otherConcern, positive = input$positive)
            fname = sprintf('Developer_%s.csv',as.integer(Sys.time()))
            write.table(df, file = fname, row.names = F, sep = ',')
            drop_upload(fname, path = 'Public/HS_data')
            sendSweetAlert(
                session = session,
                title = i18n()$t("Thank you!"),
                text = i18n()$t("The File has been saved in Dropbox"),
                type = "success"
            )}
        state(4)
        
    })
    
    # UI 
    output$page_content <- renderUI({
        fluidPage(
        div(style = "float: right;",
            selectInput('selected_language',
                        i18n()$t("Change language"),
                        choices = translator$get_languages(),
                        selected = input$selected_language),
        ),
        # Application title
        title = i18n()$t("Huawei InnerSource 2.0 Survey"),
        useSweetAlert(),
        h1(i18n()$t("Huawei InnerSource 2.0 Survey")),
        br(),br(),
        # changing content
        fluidRow(
            column(12,htmlOutput('content')),
            br(),
        # questions - Initial text
        if (state() == 0){
            column(12, radioButtons("choice", width = '100%',
                         label = h4("* Initially, we are starting a number of pilot projects to fine-tune the InnerSource process to meet the needs of Huawei. To inform you better about how you can be a part of this exciting and important initiative, we first ask you to identify your role:"),
                         choices = list("Part of management" = 1, 
                                        "Owner/Maintainer of one of the pilot projects/ potential pilot projects" = 2, 
                                        "Developer, not in any of the above roles" = 3), 
                         selected = 1))
       } else if (state() == 1 | state() == 2 | state() == 3){
           column(12, h4("We would like to know more about any concerns you might have so that we can address and account for them. A few common concerns are listed below. For the following questions (starting with CONCERN: ), please rate how big of a concern that particular option is for InnerSource adoption from your personal perspective on a scale of 0 to 10, where 0: 'Not at all a concern', 10: 'Major concern' "))
       } else {
           column(12, h3("Thank You! Your response has been recorded"))
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
               label = h4(i18n()$t("CONCERN: I do not know what InnerSource is"))
           ))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M2", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN: I am not sure about the “benefits” of InnerSource"))
           ))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M3", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  We don’t have enough resources to let the developers contribute to projects from other teams"))
           ))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M4", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  Contributions from other teams won’t be of sufficient quality"))
           ))
       },
       if (state() == 1){
           column(8, offset = 1, sliderTextInput(
               inputId = "M5", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  Not knowing the person(s) responsible for a bug might slow responses to customers"))
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
               label = h4(i18n()$t("CONCERN:  I’m unsure about the responsibilities"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O4", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  I won’t have enough time for mentoring other contributors"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O5", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  If I publish code as InnerSource, others will take the code and the credit"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O6", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  I would not be happy to publish my code as InnerSource for others to review"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O7", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  I am not sure what is suitable for InnerSource"))
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
               label = h4(i18n()$t("CONCERN:  Not knowing who consumes a shared system or code can make it difficult to add features without risking breaking a consumer"))
           ))
       },
       if (state() == 2){
           column(8, offset = 1, sliderTextInput(
               inputId = "O11", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  The incentives for all this hard work are not appropriate"))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D1", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  I do not know what InnerSource is"))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D2", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  I do not know how to contribute to InnerSource"))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D3", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  My immediate line manager would not be supportive of my contributions to InnerSource"))
           ))
       },
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D4", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  No appropriate incentive for me to contribute to InnerSource"))
           ))
       },
       
       if (state() == 3){
           column(8, offset = 1, sliderTextInput(
               inputId = "D5", width = '100%', choices = seq(from = 0, to = 10, by = 1),
               grid = TRUE, selected = 5, force_edges = TRUE,
               label = h4(i18n()$t("CONCERN:  I do not have time to contribute to InnerSource"))
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
               label = h4(i18n()$t("CONCERN:  I feel that I’ll be judged by others if I make any mistakes"))
           ))
       },
       if (state() == 1 | state() == 2 | state() == 3){
           column(8, offset = 1, textAreaInput("otherConcern", label = h4(i18n()$t("If you feel that you have concern(s) that is (are) not listed here, please feel free to write that down with the score you think it should have")), value = "")  %>%
                      shiny::tagAppendAttributes(style = 'width: 100%;')
                  )
       },
       if (state() == 1 | state() == 2 | state() == 3){
           column(8, offset = 1, textAreaInput("positive", label = h4(i18n()$t("In addition, if you could think of ONE reason that would make you give InnerSource a try, what would that be? ")), value = "")  %>%
                      shiny::tagAppendAttributes(style = 'width: 100%;')
           )
       },
       br(),
       if (state() == 1 | state() == 2 | state() == 3){
           column(2, offset=1, actionBttn("done",label = i18n()$t("Submit"), 
                                          style = "pill", color = "success"))
       },
       
       
        )
    )})
    
    # output content
    output$content <- renderUI({
        if (state() == 0){
            HTML("<h4> 
                 You might be aware of the InnerSource 2.0 initiative launched by Huawei.
                 <br></br>
                 It is of significant importance to top management in Huawei who seek 
                 to foster a collaborative environment to ensure the long-term
                 sustainability of software projects. 
                 <br></br>
                 InnerSource seeks to apply the recipes of success from the Open-Source
                 paradigm within a company and is heavily practiced by top companies 
                 around the world, e.g., Microsoft, Baidu, Tencent, SAP, Broadcom, and 
                 many more. 
                 <br></br>
                 In a <a href='https://tapjdey.github.io/InnerSource_Survey_2020/adoption.html#effect-of-innersource-adoption-on-self'>recent survey</a>, 
                 81% of the participants reported an increase in job satisfaction and 57%
                 reported an increase in productivity in their own daily work after
                 adopting the InnerSource style of working. <hr>
                 </h4>"
            )} else if (state() == 1){
            HTML("<h4>As a member of the management, your support for the InnerSource initiative would be hugely appreciated. 85% of the participants in a <a href='https://tapjdey.github.io/InnerSource_Survey_2020/success.html#attitude-of-the-management-towards-innersource-projects'>recent survey</a>,  confirmed that support from the management is critical for the success of the initiative.  
<br></br>
We humbly request your understanding that although the initiative might face a few hiccups in the beginning, the long-term success will outweigh any short-term disruptions. Research suggests that management support in the initial tentative InnerSource phase is critical to long-term success.  
<br></br>
It might seem that if your employees are spending time fixing other people’s code the productivity of the team will suffer, but we assure you that won’t be the case.
As mentioned earlier, the productivity of most employees seems to increase when they start working in InnerSource style. 
You would be their guide in this journey, helping them along the way and also keeping them on-track to maintain a balance among the various responsibilities of the employees, and we are sure you’ll be pleasantly surprised when the process stabilizes and you can actually see an increase in the team productivity and innovation. <hr>  </h4>")
            } else if (state() == 2){
            HTML("<h4>We’d like to start by congratulating you for taking on/considering to take on the responsibility of maintaining one of the first InnerSource projects. 
The journey will be challenging, but in the longer-term, the benefits would likely be worth the effort. <br>
You have a number of responsibilities in terms of getting the ancillary materials set up, advertising the project to potential contributors, reviewing contributions, and mentoring.
We strongly suggest you check the “training materials” to better prepare yourself. <br>
We can promise you that it would be a story worth telling. <hr></h4>")
            } else if (state() == 3){
            HTML("<h4>As a developer, you are the most valuable player in the InnerSource initiative.
It is with your contributions across different projects that the initiative has any hope for success.<br>
But we request you not to view it as simply another management mandated activity – this one is for your own benefit. <br>
This initiative lets you work on projects and features YOU like and YOU care about, and, if you’re up for it, work with other people who like the same things you do.  
<br></br>
So, you might be wondering, how do I get started and what do I have to do?
<br></br>
Well, for starters, we’d encourage you to look at one of the pilot projects and see if you find any of them interesting.<br>
If you do, please try to see what are the limitations and if you can find any bugs, and do report them.<br>
If you are so inclined, and it’d be great if you are, please consider adding a feature that you think might be useful and/or fix one of the annoying bugs. <hr> </h4>")
        }
    })
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
