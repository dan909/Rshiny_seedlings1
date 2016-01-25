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
pkgTest("xtable")
AbBkSCol <- c("springgreen4", "cornflowerblue", "mediumvioletred", "darkorange", "red4", "plum3", "wheat3", "orangered2", "blue4")

shinyServer(function(input, output) {
        
    
    data.set <- reactive({
        
        
        Seed.Dat <- read.csv("all-najust3 + MET.csv", stringsAsFactors=FALSE)
        
        Seed.Dat$Sowing <- as.character(Seed.Dat$Sowing)        
        
        if( input$Sowz != "All") {
            Seed.Dat <-  Seed.Dat[Seed.Dat$Sowing == input$Sowz, ]
        } 
        if(input$Filmz != "All") {
            Seed.Dat <-  Seed.Dat[Seed.Dat$Treatment == input$Filmz, ]
        } 
        if(input$Primez != "All") {
            Seed.Dat <-  Seed.Dat[Seed.Dat$P.NP == input$Primez, ]
        } 
        if(input$Lociz != "All") {
            Seed.Dat <-  Seed.Dat[Seed.Dat$Location == input$Lociz, ]
        }
        if(input$Yearz != "All") {
            Seed.Dat <-  Seed.Dat[Seed.Dat$Year == input$Yearz, ]
        } 
        
        sub <- substring(text = input$name, first = 1, last = (nchar(input$name)-4))
        Seed.Dat <- Seed.Dat
        
        
    })
    
    output$text <- renderUI({
        if(input$ratio) {
            if(input$Yearz != "All") {stop('In order to compere the years you must enter All in the years selection field')}
            X <- data.set()
            X[is.na(X)] <- 0
            
            fst <- X[X$Year == '2013 Dat', ]
            fst <- fst[order(fst$P.NP, fst$Treatment, fst$Sowing, fst$Location, fst$PLOT),] 
            last <- fst
            snd <- X[X$Year == '2014 Dat', ]
            snd <- snd[order(snd$P.NP, snd$Treatment, snd$Sowing, snd$Location, snd$PLOT),] 
            
            if(nrow(fst) != nrow(snd)) {stop('The to sets of data are not the same length, And must be!')}
            
            for(i in seq(1, nrow(fst), 1)) {
                last$'No.Plants.est'[i] <- snd$'No.Plants.est'[i] - fst$'No.Plants.est'[i]
            }
            xopt <- unique(last[,input$x])
            text.norm <- paste("Not/Normal distribution")
            
            if(input$x != "Sowing") {
                for(Nor in xopt) {
                    isNorm <- shapiro.test(x = last[,input$y][last[,input$x] == Nor])
                    text.norm <- paste(text.norm, "<br/><b>", Nor, "</b> is ", if(isNorm$p.value >= 0.05) {"<font color=#385737> Normally Distributed </font color=#385737>"}else {"<font color=#902277><b> Not Normally Distributed </b></font color=#902277>"})
                } 
                if(isNorm$p.value >= 0.05) {ttest <- t.test(x = last[,input$y][last[,input$x] == xopt[1]], y = last[,input$y][last[,input$x] == xopt[2]])
                                            text.test <- paste("T Test P value = ", round(ttest$p.value, digits = 3), "   - Result is", if(ttest$p.value >= 0.05) {"<font color=#111111> Not Significantly Different </font color=#111111>"}else {"<font color=#ff2f82><b> Significantly Different </b></font color=#ff2f82>"} )
                }else {npwill <- wilcox.test(last[,input$y][last[,input$x] == xopt[1]], last[,input$y][last[,input$x] == xopt[2]])
                       text.test <- paste("Wilcox Test P value = ", round(npwill$p.value, digits = 3), "   - Result is", if(npwill$p.value >= 0.05) {"<font color=#111111> Not Significantly Different </font color=#111111>"}else {"<font color=#ff2f82><b> Significantly Different </b></font color=#ff2f82>"} )
                }
            }
            
            #Anno <- aov(last[,input$y] ~ as.factor(last[,input$x]))
            Npanno <- kruskal.test(last[,input$y]~as.factor(last[,input$x]))
            #print(summary(Anno))
            #print(TukeyHSD(Anno, conf.level = 0.95))
            #print(pairwise.t.test(last[,input$y], last[,input$x], p.adjust="bonferroni"))
            text.k <- paste("Kruskal-Wallis Test P value = ", round(Npanno$p.value, digits = 3), "   - Result is", if(Npanno$p.value >= 0.05) {"<font color=#111111> Not Significantly Different </font color=#111111>"}else {"<font color=#ff2f82><b> Significantly Different </b></font color=#ff2f82>"} )
            if(Npanno$p.value <= 0.05) {
                posthoc <- pairwise.wilcox.test(last[,input$y], last[,input$x], p.adj="bonferroni", exact=F)
                posthoc <- as.matrix(posthoc$p.value)
                posthock <- ifelse(posthoc <= 0.05, paste("<font color=#ff2f82>",round(posthoc, 3),  "</font>"), paste("<font color=#051153>",round(posthoc, 3),  "</font>"))
                print("* denotes some significance see aboth for values")
                #posthock <- xtable(posthock)
                posthock <- print.xtable(xtable(posthock,digits=4), type="html",sanitize.text.function=function(x){x}, html.table.attributes='border=5 align=center width=500')
                #print(posthock, type = "html", html.table.attributes='border=5 align=center width=500' )
                texttest <- paste("<p style=font-size:12px><font color=#952c1e><i> See below output for post hoc pairwise wilcox test p values</i></font color=#952c1e></p style=font-size:12px><br/>", posthock)
                NormNorm <- 100;
            }
            
        }else {
            All <- data.set()
            xopt <- unique(All[,input$x])
            text.norm <- paste("Not/Normal distribution")
            
            NormNorm <- 0;
            
            if(input$x != "Sowing") {
                for(Nor in xopt) {
                    isNorm <- shapiro.test(x = All[,input$y][All[,input$x] == Nor])
                    text.norm <- paste(text.norm, "<br/><b>", Nor, "</b> is ", if(isNorm$p.value >= 0.05) {"<font color=#385737> Normally Distributed </font color=#385737>"}else {"<font color=#902277><b> Not Normally Distributed </b></font color=#902277>"})
                    NormNorm <- isNorm$p.value + NormNorm
                } 
                if(NormNorm/length(xopt) >= 0.05) {ttest <- t.test(x = All[,input$y][All[,input$x] == xopt[1]], y = All[,input$y][All[,input$x] == xopt[2]])
                                                   text.test <- paste("T Test P value = ", round(ttest$p.value, digits = 4), "   - Result is", if(ttest$p.value >= 0.05) {"<font color=#111111> Not Significantly Different </font color=#111111>"}else {"<font color=#ff2f82><b> Significantly Different </b></font color=#ff2f82>"} )
                }else {npwill <- wilcox.test(All[,input$y][All[,input$x] == xopt[1]], All[,input$y][All[,input$x] == xopt[2]])
                       text.test <- paste("Wilcox Test P value = ", round(npwill$p.value, digits = 4), "   - Result is", if(npwill$p.value >= 0.05) {"<font color=#111111> Not Significantly Different </font color=#111111>"}else {"<font color=#ff2f82><b> Significantly Different </b></font color=#ff2f82>"} )
                }
            } else {
            
            #Anno <- aov(All[,input$y] ~ as.factor(All[,input$x]))
            Npanno <- kruskal.test(All[,input$y]~as.factor(All[,input$x]))
            #print(summary(Anno))
            #print(TukeyHSD(Anno, conf.level = 0.95))
            #print(pairwise.t.test(All[,input$y], All[,input$x], p.adjust="bonferroni"))
            text.k <- paste("Kruskal-Wallis Test P value = ", round(Npanno$p.value, digits = 4), "   - Result is", if(Npanno$p.value >= 0.05) {"<font color=#111111> Not Significantly Different </font color=#111111>"}else {"<font color=#ff2f82><b> Significantly Different </b></font color=#ff2f82>"} )
            if(Npanno$p.value <= 0.05) {
                posthoc <- pairwise.wilcox.test(All[,input$y], All[,input$x], p.adj="bonferroni", exact=F)
                posthoc <- as.matrix(posthoc$p.value)
                posthock <- ifelse(posthoc <= 0.05, paste("<font color=#ff2f82>",round(posthoc, 3),  "</font>"), paste("<font color=#051153>",round(posthoc, 3),  "</font>"))
                print("* denotes some significance see aboth for values")
                #posthock <- xtable(posthock)
                posthock <- print.xtable(xtable(posthock,digits=4), type="html",sanitize.text.function=function(x){x}, html.table.attributes='border=5 align=center width=500')
                    #print(posthock, type = "html", html.table.attributes='border=5 align=center width=500' )
                texttest <- paste("<p style=font-size:12px><font color=#952c1e><i> See below output for post hoc pairwise wilcox test p values</i></font color=#952c1e></p style=font-size:12px><br/>", posthock)
                NormNorm <- 100;
            }
            }
        }
        
        
        
        disclamer <- "<p style=font-size:9px><font color=#ff5737>Theas stats are only a indication and should probably be checked outside of an automated script</p style=font-size:9px></font color=#ff5737>"
        
        HTML(paste(text.norm, "-", if(input$x != "Sowing") {text.test} else {text.k}, if(NormNorm == 100) {texttest}, disclamer, sep = '<br/>'))
    })
    
    
    output$plot <- renderPlot({
        
        if(input$ratio) {
            if(input$Yearz != "All") {stop('In order to compere the years you must enter All in the years selection field')}
            X <- data.set()
            X[is.na(X)] <- 0
            
            fst <- X[X$Year == '2013 Dat', ]
            fst <- fst[order(fst$P.NP, fst$Treatment, fst$Sowing, fst$Location, fst$PLOT),] 
            last <- fst
            snd <- X[X$Year == '2014 Dat', ]
            snd <- snd[order(snd$P.NP, snd$Treatment, snd$Sowing, snd$Location, snd$PLOT),] 
            
            if(nrow(fst) != nrow(snd)) {stop('The to sets of data are not the same length, And must be!')}
            
            for(i in seq(1, nrow(fst), 1)) {
                last$'No.Plants.est'[i] <- snd$'No.Plants.est'[i] - fst$'No.Plants.est'[i]
            }
            p <- ggplot(last, aes_string(x=input$x, y=input$y)) + scale_colour_manual(values=AbBkSCol)
        }else {
            p <- ggplot(data.set(), aes_string(x=input$x, y=input$y)) + scale_colour_manual(values=AbBkSCol)
        }
        
        
        if (input$boxplot)
            p <- p + geom_boxplot()
        
        if (input$color != 'X Value') {
            p <- p + aes_string(color=input$color)
        } else {
            p <- p + aes_string(color=input$x)
        }
        
        if(input$viol)
            p <- p + geom_violin(position = "dodge")
        
        if (input$points)
            p <- p + geom_point() + geom_jitter()
        
################# use this to set the y axis        
        #p <- p + ylim(0,60)
#################
        print(p)
        
    }, height=700)
    

     observe({
         if (input$ChangeDir == 0) {
             write(getwd(), "Dir")
             return()
         }
             
         
         isolate({
             Dir <- choose.dir("C://", "Input Destination Folder")
             print(Dir)
             if(is.na(Dir)) {
                 write(getwd(), "Dir")
             }else {
                 write(Dir, "Dir")
             }
             
         })
         
     })
 
 TheDir <- reactive({
     input$ChangeDir
     Dir <- read.table("Dir", sep = "&")
     Dir <- as.character(Dir[1,1])
     Dir
 })
 
 output$DIR <- renderText({
     Dir <- paste(TheDir(), "/", sep = "")
     Dir
 })


observe({    
    Name <- paste(input$x, " by ", input$y,
                  if(input$color != "X Value") {paste(" color-", input$color, sep = "")},
                  "_", if(input$Sowz != "All") {paste(" sowing-", input$Sowz, "_", sep = "")},
                  if(input$Filmz != "All") {paste(" film-", input$Filmz, "_", sep = "")},
                  if(input$Primez != "All") {paste(" priming-", input$Primez, "_", sep = "")},
                  if(input$Lociz != "All") {paste(" in-", input$Lociz, "_", sep = "")},
                  if(input$Yearz != "All") {paste(" in-", input$Yearz, "_", sep = "")},
                  input$ExtraName, ".", input$ext,sep = "")
    print(Name)
    if(file.exists(Name)) {
        print("OverWrite!?")
    }
    write(Name, "FileName")
})

IdealName <- reactive({
    input$Sowz;input$Filmz;input$Primez;input$Lociz;input$ExtraName;input$Yearz;input$x;input$y;input$color;input$ext;
    IdealName <- read.table("FileName", sep = "&")
    IdealName <- as.character(IdealName[1,1])
    IdealName
})

output$Name <- renderUI({
    Name <- HTML(paste("<font color=#b71e66> <b> ", IdealName(), " </b> </font color=#b71e66><br/>",sep = ""))
})


    observe({
        if (input$save == 0)
            return()
        
        isolate({
            ggsave(paste(TheDir(), "/", IdealName(), sep = ""), dpi = 500)
            #         if(input$csv){
            #             write.csv(paste(sub, ".csv", sep = ""), data.set())
            #         }
        })
    })
    
    
    
    
})



