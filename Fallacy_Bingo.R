require(gridExtra)

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

pdf(file="bingo.pdf", onefile=TRUE, title=title)

for(i in 1:20)
{
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

        plot.new()
        grid.table(Bingo.table, 
                   gpar.corefill = gpar(fill = "white", alpha = .5, col = NA),
                   gpar.colfill = gpar(fill = "white"),
                   gpar.rowfill = gpar(fill = "white"),
                   separator = "black",
                   show.box = "TRUE",                   
                   show.vlines = TRUE, show.hlines = TRUE, 
                   show.namesep = TRUE, show.csep = TRUE, 
                   show.rsep=TRUE,
                   equal.width = TRUE, equal.height = TRUE, 
                   padding.v = unit(1.8, "cm") )
}

dev.off()
