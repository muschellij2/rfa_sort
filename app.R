library(shiny)

library(rvest)
library(lubridate)
library(tibble)
library(dplyr)
library(DT)
url = "https://grants.nih.gov/grants/guide/search_results.htm?scope=rfa&year=active"

doc = read_html(url)
bad_string = "ZZZZZ"
doc <- read_html(gsub("<br>", bad_string, doc))
# nodes = html_nodes(doc, xpath = ".//table")
tabs = html_table(doc, fill = TRUE)
keep_tab = sapply(tabs, function(x) {
    cn = colnames(x)
    any(grepl("^Announcement", cn))
})

# tabs = html_table(nodes, fill = TRUE)
# keep_tab = sapply(tabs, function(x) {
#     cn = colnames(x)
#     any(grepl("^Announcement", cn))
# })
stopifnot(sum(keep_tab) == 1)

tab = tabs[[which(keep_tab)]]
cn = colnames(tab)
cn = gsub(bad_string, "\n", cn)
cn = gsub("\\s+", " ", cn)
cn = trimws(cn)
colnames(tab) = cn
tab = tibble::as_tibble(tab)

tab = tab %>% 
    mutate_at(vars(contains("Date")), mdy)

gg = sapply(lapply(tab, grepl, pattern = bad_string), any)
tab[gg] = lapply(tab[gg], gsub, pattern = bad_string, " ")

nih_link = function(id, value) {
    x = paste0("https://grants.nih.gov/grants/guide/search_results.htm?text_curr=", 
               id, "&related=yes&sort=")
    x = paste0('<a href="', x, '" target="_blank">', value, "</a>")
    x
}

rfa_link = function(id, value) {
    x = paste0("https://grants.nih.gov/grants/guide/rfa-files/",
               id, ".html")
    x = paste0('<a href="', x, '" target="_blank">', value, "</a>")
    x
}

tab = tab %>% 
    mutate(`Announcement Number` = trimws(`Announcement Number`),
           `Related Announc.` = nih_link(id = `Announcement Number`, 
                                         `Related Announc.`),
           `Announcement Number` = rfa_link(id = `Announcement Number`,
                                            value = `Announcement Number`))


# Define UI for application that draws a histogram
ui <- fluidRow(
    
    # Application title
    titlePanel("NIH RFA data"),
    p(
        HTML(
            paste0(
                "This data is from the page of the ", 
                a(href = "https://grants.nih.gov/grants/guide/search_results.htm?scope=rfa&year=active",
                  "active NIH RFAs"),
                " but has the ability to use filter using DataTable."
            )
        )
    ),
    helpText(HTML(
        'The source code of this app is <a href="https://github.com/muschellij2/rfa_sort">on GitHub</a>.'
    )),
    
    # # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    #     
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         plotOutput("distPlot")
    #     )
    # )
    
    column(12,
           dataTableOutput("table")
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$table <- renderDataTable({
        tab
        DT::datatable(tab, filter = "top",
                      options = list(
                          autoWidth = TRUE,
                          paging = FALSE,
                          pageLength = 30), escape = FALSE,
                      rownames = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


