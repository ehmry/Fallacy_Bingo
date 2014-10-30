require(gridExtra)
require(gridBase)
require(ggplot2)

title <- "Logical Fallacy BINGO"

fallacy.vector <- sort(c(
    "Ad hominem",
    "Appeal to\ninappropriate\nauthority",
    "Strawman",
    "Guilt by\nassociation",
    "Factual\nerror",
    "Equivocation",
    "False\ndilemma",
    "Correlation\nimplies causation",
    "Loaded\nquestion",
    "Bandwagon",
    "Begging the\nquestion",
    "Proof' by\nanecdote",
    "Cherry-picking\ndata",
    "The fallacy\nfallacy",
    "Argument from\nignorance",
    "Ambiguous\nlanguage",
    "Middle\nground",
    "Appeal to\nconsequences",
    "Arguement from\nincredulity",
    "Confirmation\nbias",
    "Gambler's\nfallacy",
    "Inflation of\nconflict",
    "Irrelevant\nconclusion",
    "Moral\nhigh ground",
    "Moving the\ngoalposts",
    "Perfect solution\nfallacy",
    "Onus\nprobandi",
    "Proof by\nassertion",
    "Red herring",
    "Shotgun\nargumentation",
    "Hasty\ngeneralization",
    "Appeal to\nemotion",
    "Argument\nfrom silence",
    "Genetic\nfallacy",
    "Reductio ad\nHitlerum",
    "Slippery\nslope"
))

nSquares <- 24;

mkCard <- function(x, y) {
       pushViewport(viewport(layout.pos.col=x,
                             layout.pos.row=y))
       pushViewport(viewport(width=0.9, height=0.9))


        indexes <- sample(1:length(fallacy.vector), nSquares, replace=FALSE)

        ##
        # cardId is a unique identifier for this bingo card.
        #
        #cardId <- paste(format.hexmode(indexes, width=2, upper.case = TRUE), collapse="")

        names <- list(1:5, c("B","I","N","G","O"))
        Bingo.vector <- fallacy.vector[indexes]
        Bingo.vector2 <- c(Bingo.vector[1:12], "FREE SPACE", Bingo.vector[13:24])
        Bingo.matrix <- matrix(data=Bingo.vector2, nrow=5, ncol=5, dimnames=names)
        Bingo.table <- as.table(Bingo.matrix)

        grid.table(Bingo.table, 
                   gpar.corefill = gpar(fill = "white", col = NA),
                   gpar.colfill = gpar(fill = "white"),
                   gpar.rowfill = gpar(fill = "white"),
                   gpar.coretext =  gpar(col = "black", cex = 0.3),
                   separator = "black",
                   show.box = "TRUE",                   
                   show.vlines = TRUE, show.hlines = TRUE, 
                   show.namesep = TRUE, show.csep = TRUE, 
                   show.rsep=TRUE,
                   padding.v = unit(.6, "cm"),
                   padding.h = unit(.4, "cm"))

        popViewport()
        popViewport()
}

pdf(file="bingo.pdf", onefile=TRUE, title=title)


     # Demonstrate viewport clipping
     clip.demo <- function(i, j, clip1, clip2) {
       pushViewport(viewport(layout.pos.col=i,
                              layout.pos.row=j))
       pushViewport(viewport(width=0.6, height=0.6))
       grid.rect(gp=gpar(fill="white"))
       popViewport()
       popViewport()
     }


for(i in 1:3)
{
        grid.newpage()
        #grid.rect(gp=gpar(fill="grey"))
        pushViewport(viewport(layout=grid.layout(3, 2)))

        mkCard(1, 1)
        mkCard(1, 2)
        mkCard(1, 3)
        mkCard(2, 1)
        mkCard(2, 2)
        mkCard(2, 3)

        popViewport()
}

dev.off()
