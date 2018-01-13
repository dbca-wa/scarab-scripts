library(shiny)
library(gh)
library(tidyverse)
library(magrittr)
library(stringr)
library(markdown)
library(DT)
library(networkD3)

#------------------------------------------------------------------------------#
# Settings - change as appropriate
#
Sys.setenv(GH_OWNER = "parksandwildlife")
Sys.setenv(GH_REPO = "scarab")
# Sys.setenv(GITHUB_PAT="my-GH-personal-access-token")

#------------------------------------------------------------------------------#
# Helper functions and general witchcraft
#
#' Map given function, handle null as NA and flatten_chr()
map_chr_hack <- function(.x, .f, ...) {
    purrr::map(.x, .f, ...) %>%
        purrr::map_if(is.null, ~ NA_character_) %>%
        purrr::flatten_chr(.x)
}

#' Format url and label as HTML hyperlink
as_url <- function(url, label) {
    paste0("<a href=\"", url, "\" target=\"_\">", label, "</a>")
}

#' Convert a string of markdown to HTML
md_to_html <- function(md) {
    if (is.null(md) || is.na(md)) {md <- "no content"}
    markdown::markdownToHTML(file = NULL, text = md, fragment.only = TRUE)
}

#' Extract a element with name or number n from a list x
extract_name <- function(x, n)sapply(x, `[[`, n)

#' Extract a list of mentioned GitHub issue numbers from text
#' require(testthat)
#' testthat::expect_equal(extract_related("#5 #65 #123 #7# #test # test ## #8\n#9"), c(5, 65, 123, 8, 9))
#' testthat::expect_equal(extract_related("#7# #test # test ## "), NA_integer_)
#' testthat::expect_equal(extract_related(""), NA_integer_)
extract_related <- function (x) {
    out <- x %>%
        str_extract_all("(?<=#)\\d+(?=\\s|\\n|\\<|$)") %>%
        purrr::compact %>%
        unlist %>%
        extract_name(1) %>%
        as.integer %>%
        purrr::compact
    if (length(out) == 0) return(NA_integer_) else return(out)
}

#' Wrap gh::gh, use with "issues", "milestones", "labels"
gethub <- function(what,
                   owner = Sys.getenv("GH_OWNER"),
                   repo = Sys.getenv("GH_REPO"),
                   # .token = Sys.getenv("GITHUB_PAT"),
                   .limit = Inf) {
    gh(paste0("/repos/:owner/:repo/", what),
       owner = owner,
       repo = repo,
       # .token = .token,
       .limit = .limit)
}

#' Substitute boolean values with "yes" or "no". Per popular request.
bool2yn <- . %>% ifelse("yes", "no")

#' Build a tbl_df of GH issues from a GH API response
make_issues <- function(ii){
    i1 <- ii %>% {
        tibble::tibble(
            id = map_int(., "number"),
            title = map_chr(., "title"),
            body = map_chr_hack(., "body") %>% map(md_to_html),
            body_md = map_chr_hack(., "body"),
            related = body %>% map(extract_related),
            state = map_chr(., "state"),
            url = map_chr(., "html_url"),
            html_url = map_chr(., "html_url") %>% map(as_url, "View issue"),
            comments_url = map_chr(., "comments_url") %>% map(as_url, "View comments"),
            labels_url = map_chr(., "labels_url") %>% map(as_url, "View labels"),
            labels = map(., "labels") %>% map(extract_name, "name"),
            labels_chr = map(labels, paste, collapse = ", "),
            created_by = map_chr(., c("user", "login")),
            assignee = map_chr(., c("assignee", "login")),
            milestone = map_chr(., c("milestone", "title")),
            created_at = map_chr(., "created_at") %>% as.Date(),
            updated_at = map_chr_hack(., "updated_at") %>% as.Date(),
            due_at = map_chr_hack(., "due_on") %>% as.Date(),
            closed_at = map_chr_hack(., "closed_at") %>% as.Date()
            # ,
            # tagging = milestone %in% c(
            #     "Overarching requirements",
            #     "Turtle Tagging",
            #     "Turtle Tag Asset Management") %>% bool2yn,
            # tracks = milestone %in% c(
            #     "Overarching requirements",
            #     "Turtle Tracks and Nests") %>% bool2yn,
            # strandings = milestone %in% c(
            #     "Overarching requirements",
            #     "Marine Wildlife Strandings") %>% bool2yn
        )
    } %>% arrange(id)


    # untangle labels, loses a few columns, hard-codes existing tags
    # This will blow up on multiple "... Requirements"
    i2 <- i1 %>%
        separate_rows(labels) %>%
        mutate(tagslog = TRUE) %>%
        spread(labels, tagslog, fill = FALSE)  %>%
        select(id, Business, Stakeholder, Functional, Internal, must, should) %>%
        mutate(
            Business = Business %>% bool2yn,
            Stakeholder = Stakeholder %>% bool2yn,
            Functional = Functional %>% bool2yn,
            Internal = Internal %>% bool2yn,
            must  = must %>% bool2yn,
            should = should %>% bool2yn)

    iss <- left_join(i1, i2, by = "id")
    iss
}

# Issues in long form as MS Word document ------------------------------------#
as_md <- function(id, title, milestone, body, url, labels){
    paste0(
        "# Requirement ", id, " ", title, "\n\n",
        "[View #", id, " online](", url, ")\n\n",
        "## Component\n", milestone, "\n\n",
        "## Requirement type and priority\n", labels, "\n\n",
        body, "\n\n"
    )
}

issues_md <- function(issue_tbl){
    issue_tbl %>%
        mutate(md = as_md(id, title, milestone, body_md, url, labels_chr)) %>%
        select(md)
}

prepare_issues_md <- function(issues_tbl){
    issues_oa <- issues_tbl %>%
        dplyr::filter(milestone == "Overarching requirements") %>%
        issues_md

    issues_tag <- issues_tbl %>%
        dplyr::filter(milestone %in% c("Turtle Tagging",
                                       "Turtle Tag Asset Management")) %>%
        issues_md

    issues_track <- issues_tbl %>%
        dplyr::filter(milestone == "Turtle Tracks and Nests") %>%
        issues_md

    issues_strand <- issues_tbl %>%
        dplyr::filter(milestone == "Marine Wildlife Strandings") %>%
        issues_md

    issues_out <- rbind(issues_oa, issues_tag, issues_track, issues_strand)

    issues_out
}

write_md <- function(md, fname="issues.md"){
    out <- paste0(
        "---
output:
  word_document: default
  html_document: default
---
", md)
    write.table(out, file = fname, sep = "\n", fileEncoding = "utf-8",
                quote = F, row.names = F, col.names = F)
}

write_docx <- function(issues_md){
    write_md(issues_md$md, fname = "requirements.md")
    rmarkdown::render("requirements.md")
}


#' Build a tbl_df of GH milestones from a GH API response
make_milestones <- function(mm){
    mm %>% {
        tibble::tibble(
            id = map_int(., "id"),
            number = map_int(., "number"),
            title = map_chr(., "title"),
            description = map_chr_hack(., "description"),
            state = map_chr(., "state"),
            html_url = map_chr(., "html_url"),
            labels_url = map_chr(., "labels_url"),
            created_by = map_chr(., c("creator", "login")),
            created_at = map_chr(., "created_at") %>% as.Date(),
            updated_at = map_chr_hack(., "updated_at") %>% as.Date(),
            due_at = map_chr_hack(., "due_on") %>% as.Date(),
            closed_at = map_chr_hack(., "closed_at") %>% as.Date()
        )
    }
}

#' Build a tbl_df of GH labels from a GH API response
make_labels <- function(ll){
    ll %>% {
        tibble::tibble(
            id = map_int(., "id"),
            url = map_chr(., "url") %>% map(as_url, "View requirements"),
            name = map_chr(., "name"),
            colour = map_chr(., "color")
        )
    }
}

#' Return selected columns for a tbl_df of GH issues (labels pivoted)
make_requirements <- function(ii){
    ii %>%
        transmute(
            ID = id,
            Link = html_url,
            # Date = due_at,
            Related_IDs = related,
            Requirement = title,
            Tagging_MVP = tagging,
            Tracks_MVP = tracks,
            Strandings_MVP = strandings,
            Business_Req = Business,
            Stakeholder_Req = Stakeholder,
            Functional_Req = Functional,
            # # Nonfunctional = Nonfunctional,
            Internal_Req = Internal,
            Must_have = must,
            Should_have = should,
            # could_have = could,
            # wont_have = wont
            Categories = labels_chr
            #, Requirement = body # too large to include
            # Source = url
        )
}

#' Return selected columns for a tbl_df of GH milestones
make_business_needs <- function(mm){
    mm %>%
        transmute(
            ID = number,
            # Date = due_at,
            # Source = created_by,
            Requirements = title
        )
}

lookup_issue_index <- function(issue_lookup, issue_id){
    issue_lookup %>% filter(id == issue_id) %>% extract("zerorn") %>% as.integer
}

#' Extract related issues (0-indexed) if mentioned in issue body
make_relations <- function(df){
    dd <- df %>%
        mutate(zerorn = as.integer(rownames(df)) - 1) %>%
        select(id, zerorn)

    out <- df %>%
        select(id, related) %>%
        filter(!is.na(related)) %>%
        rowwise() %>%
        do(expand.grid(.$id, .$related)) %>%
        transmute(# source_id = Var1,
            # target_id = Var2,
            source = lookup_issue_index(dd, Var1),
            target = lookup_issue_index(dd, Var2)) %>%
        filter(!is.na(target)) %>%
        data.frame
}

#------------------------------------------------------------------------------#
# UI
#
ui <- navbarPage(
    "Biosys Turtles Requirements",

    tabPanel(
        "Explore",

        column(
            7,
            h1("Explore"),
            p("Click to view details, drag to re-order"),
            forceNetworkOutput("force", width = "100%", height = "800px")
        ),

        column(
            5,
            h1("Download"),
            uiOutput("download_requirements_csv"),
            uiOutput("download_requirements_docx"),
            h1("Details"),
            uiOutput("issue_selector"),
            uiOutput("issue_detail"))),

    tabPanel(
        "Business Needs",
        shiny::dataTableOutput("business_needs_table")),

    tabPanel(
        "Requirements",
        shiny::dataTableOutput("requirements_table"))
)

#------------------------------------------------------------------------------#
# Server
#
server <- function(input, output) {

    #----------------------------------------------------------------------------#
    # Summon data from GitHub API
    #
    issue_list <- reactive(
        withProgress(message = 'Loading requirements...', {gethub("issues")}))

    milestone_list <- reactive(
        withProgress(message = 'Loading business needs...', {gethub("milestones")}))

    labels_list <- reactive(
        withProgress(message = 'Loading categories...', {gethub("labels")}))

    #----------------------------------------------------------------------------#
    # Transform primary data
    #

    issues <- reactive({
        ii <- issue_list()
        if (is.null(ii)) return(NULL)
        make_issues(ii)
    })

    milestones <- reactive({
        mm <- milestone_list()
        if (is.null(mm)) return(NULL)
        make_milestones(mm)
    })

    labels <- reactive({
        ll <- labels_list()
        if (is.null(ll)) return(NULL)
        make_labels(ll)
    })

    #----------------------------------------------------------------------------#
    # Prepare secondary data
    #
    requirements <- reactive({
        ii <- issues()
        if (is.null(ii)) return(NULL)
        make_requirements(ii)
    })

    business_needs <- reactive({
        mm <- milestones()
        if (is.null(mm)) return(NULL)
        make_business_needs(mm)
    })

    relations <- reactive({
        ii <- issues()
        if (is.null(ii)) return(NULL)
        make_relations(ii)
    })


    #----------------------------------------------------------------------------#
    # Build dropdowns
    #
    output$issue_selector <- renderUI({
        ii <- issues()
        if (is.null(ii)) return(NULL)
        selectizeInput(
            "selected_issue",
            "Backspace and type to search; select to view details",
            setNames(rownames(ii), ii$title),
            width = '100%')
    })

    output$category_selector <- renderUI({
        ll <- labels()
        if (is.null(ll)) return(NULL)
        vals <- ll %>%
            dplyr::filter(grepl("Req", name)) %>%
            select(name) %>%
            extract2(1)
        selectizeInput(
            "selected_categories",
            "Categories",
            vals,
            width = '50%',
            selected = vals,
            multiple = TRUE)
    })

    output$priority_selector <- renderUI({
        ll <- labels()
        if (is.null(ll)) return(NULL)
        vals <- ll %>%
            dplyr::filter(grepl("have", name)) %>%
            select(name) %>%
            extract2(1)
        # vals <- c("Must have", "Should have", "Could have", "Won't have")
        selectizeInput(
            "selected_priorities",
            "Priorities",
            vals,
            width = '50%',
            selected = vals,
            multiple = TRUE)
    })

    #----------------------------------------------------------------------------#
    # Build datatables
    #
    opts = list(
        autoWidth = TRUE,
        lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
        pageLength = 10,
        fixedHeader = list(header = TRUE, footer = TRUE)
    )

    output$business_needs_table <- shiny::renderDataTable(
        business_needs(), options = opts, escape = FALSE
    )

    output$requirements_table <- shiny::renderDataTable(
        requirements(), options = opts, escape = FALSE
    )

    output$issue_detail <- renderUI({
        ii <- issues()
        sel <- input$selected_issue
        if (is.null(ii) || is.null(sel)) return(NULL)
        HTML(paste(
            "<h2>", ii$title[[as.integer(sel)]], "</h2>",
            ii$html_url[[as.integer(sel)]],
            ii$body[[as.integer(sel)]]
        ))
    })

    output$force <- renderForceNetwork({
        forceNetwork(
            Links = relations(),
            Nodes =  data.frame(issues()),
            Source = "source",
            Target = "target",
            Value = 2,
            NodeID = "title",
            # Nodesize = calcNodesize(""),
            Group = "milestone",
            opacity = 0.8,
            fontSize = 14,
            fontFamily = "sans",
            charge = -50,
            legend = TRUE,
            arrows = TRUE,
            bounded = TRUE,
            zoom = TRUE,
            height = 900,
            width = 900,
            clickAction = 'Shiny.onInputChange("selected_issue", d.index + 1)'
        )
    })

    output$downloadRequirementsCSV <- downloadHandler(
        filename = "requirements.csv",
        content = function(file) {
            d <- requirements() %>% select(-Link)
            if (is.null(d)) {return(NULL)}
            dd <- data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
            write.csv(dd, file, row.names = FALSE, fileEncoding = "utf-8")
        }
    )

    output$download_requirements_csv <- renderUI({
        d <- requirements()
        if (is.null(d)) {return(NULL)}
        downloadButton('downloadRequirementsCSV', label = "csv")
    })

    output$downloadRequirementsDOCX <- downloadHandler(
        filename = "requirements.docx",
        content = function(file) {

            # issues as tbl_df
            d <- issues()
            if (is.null(d)) {return(NULL)}

            # issues as md
            issues_md <- prepare_issues_md(d)

            # write md to file
            issues_md_file <- tempfile()
            write_md(issues_md$md, fname = issues_md_file)

            # render md file to docx file
            rmarkdown::render(issues_md_file, output_file = file)
        }
    )

    output$download_requirements_docx <- renderUI({
        d <- issues()
        if (is.null(d)) {return(NULL)}
        downloadButton('downloadRequirementsDOCX', label = "docx")
    })

}

shinyApp(ui = ui, server = server)

# Export issues as DOCX:
# gethub("issues") %>% make_issues %>% prepare_issues_md %>% write_docx

