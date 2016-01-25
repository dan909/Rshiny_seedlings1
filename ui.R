pkgTest <- function(x)
{
    if (!require(x,character.only = TRUE))
    {
        install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
}


pkgTest("shiny")
pkgTest("ggplot2")

SowLst <- list('1','2','3','4','5','6','7','8','9', "All")
FilmLst <- list('Film', 'none', "All")
PrimeLst <- list('P', 'NP', "All")
LociLst <- list('Aber', 'Blank', "All")
YearLst <- list('2013 Dat', '2014 Dat', 'All')
XLst <- c("Location", "Sowing", "Treatment", "P.NP", "Year")
YLst <- c("No.Stems", "M.Height", "M.Leaves", "M.Thickness", 'BioM.Index', 'BioM.Index_NA', "Senescence", "No.Plants.est", "Harvist", "Harvist_NA","DW","DW_NA","MC", "Germ.First.20", "Germ.First.32",	"Germ.First.50", "Germ.15.43","Temp1","Wat1","Temp5","Wat5","Temp15","Wat15","Temp40","Wat40")#, "Model.germ20", "Germ.First.20p")
extensions <- c('wmf','png','tiff','pdf','jpeg','tex','bmp','svg','emf','eps')

shinyUI(pageWithSidebar(
    
    headerPanel("End of Year Aber Blankney Data"),
    
    sidebarPanel(
        
        
        h3("Data Specification"),
        selectInput('Sowz', 'Sowing Number', SowLst, SowLst[10]),
        selectInput('Filmz', 'Film Application', FilmLst, FilmLst[3]),
        selectInput('Primez', 'Priming Treatmrent', PrimeLst, PrimeLst[3]),
        selectInput('Lociz', 'Location of trial', LociLst, LociLst[1]),
        selectInput('Yearz', 'Year of Data', YearLst, LociLst[1]),
        br(),
        h3("Graph Setup"),
        
        selectInput('x', 'X', XLst, XLst[[3]]),
        selectInput('y', 'Y', YLst, YLst[[5]]),
        selectInput('color', 'Color by', c('X Value', XLst)),
        
        checkboxInput('points', 'Points'),
        checkboxInput('boxplot', 'Boxplot', value = TRUE),
        checkboxInput('viol', 'Violin?', value = F),
        checkboxInput('ratio', 'Ratio of y1 to y2', value = F),
        #p(actionButton('test1','Test1'), actionButton('test2','Test2')),
        h4("Will Save As..."),
        uiOutput("Name"),
        textInput("ExtraName", 'Identifyer for graph', value = Sys.Date()),
        selectInput('ext', 'File extension', extensions, extensions[1]),
        actionButton("save", label = "Save"),
        actionButton("ChangeDir", label = "Change Dir"),
        helpText("Curent Dir is..."),
        uiOutput("DIR")
    ),
    
    mainPanel(
        fluidRow(
            column(12, wellPanel(
            plotOutput('plot', height = 740)
            )
            )
        ),
        fluidRow(
            column(8, offset = 2, wellPanel(
            htmlOutput("text")
            )
        )
    )
    )
))

