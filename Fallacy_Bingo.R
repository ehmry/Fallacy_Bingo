require(gridExtra)

title <- "Logical Fallacy BINGO"

fallacyNames <- c(
    "Ad hominem",
    "Appeal to inappropriate authority",
    "Strawman",
    "Guilt by association",
    "Factual error",
    "Equivocation",
    "False dilemma",
    "Correlation implies causation",
    "Loaded question",
    "Bandwagon",
    "Begging the question",
    "Proof by anecdote",
    "Cherry-picking data",
    "The fallacy fallacy",
    "Argument from ignorance",
    "Ambiguous language",
    "Middle ground",
    "Appeal to consequences",
    "Arguement from incredulity",
    "Confirmation bias",
    "Gambler's fallacy",
    "Inflation of conflict",
    "Irrelevant conclusion",
    "Moral high ground",
    "Moving the goalposts",
    "Perfect solution fallacy",
    "Onus probandi",
    "Proof by assertion",
    "Red herring",
    "Shotgun argumentation",
    "Hasty generalization",
    "Appeal to emotion",
    "Argument from silence",
    "Genetic fallacy",
    "Reductio ad Hitlerum",
    "Slippery slope"
)

# because I don't know how to make a 2x36 -EH

fallacyDescriptions <- c(
    "criticism directed at something about the person one is criticizing, rather than something independent of that person", #"Ad hominem"
    "using an expert of dubious credentials and/or using only one opinion", #"appeal to inappropriate authority"
    "an argument based on misrepresentation of an opponent's position", #"Strawman"
    "arguing that because two things share a property they are the same", #"Guilt by association"
    "", #"Factual error"
    "misleading use of a term with more than one meaning (by glossing over which meaning is intended at a particular time)", #"Equivocation"
    "two alternative statements are held to be the only possible options, when in reality there are more", #"False dilemma"
    "faulty assumption that correlation between two variables implies that one causes the other", #"Correlation implies causation"
    "question which contains a controversial or unjustified assumption (e.g., a presumption of guilt)", #"Loaded question"
    "a proposition is claimed to be true or good solely because many people believe it to be so", #"Bandwagon"
    "providing what is essentially the conclusion of the argument as a premise", #"Begging the question"
    "", #"Proof' by anecdote"
    "pointing at individual cases or data, while ignoring a significant portion of related cases or data that may contradict that position", #"Cherry-picking data"
    "", #"The fallacy fallacy"
    "assuming that a claim is true because it has not been or cannot be proven false, or vice versa", #"Argument from ignorance"
    "using words with ambiguous meanings, then changing the meaning of them later", #"Ambiguous language"
    "assuming that the compromise between two positions is always correct", #"Middle ground"
    "conclusion is supported by a premise that asserts positive or negative consequences from some course of action", #"Appeal to consequences"
    "\"I cannot imagine how this could be true, therefore it must be false\"", #"Arguement from incredulity"
    "tendency to favor information that confirms one's beliefs or hypotheses and to ignore information that disagrees with one's point of view", #"Confirmation bias"
    "incorrect belief that separate, independent events can affect the likelihood of another random event", #"Gambler's fallacy"
    "experts of a field of knowledge disagree on a certain point, therefore the legitimacy of their entire field is put to question", # "Inflation of conflict"
    "argument that may in itself be valid, but does not address the issue in question", # "Irrelevant conclusion"
    "one assumes a \"holier-than-thou\" attitude in an attempt to make oneself look good to win an argument", #"Moral high ground"
    "evidence presented in response to a specific claim is dismissed and some other evidence is demanded", #"Moving the goalposts"
    "solutions to problems are rejected because they are not perfect", #"Perfect solution fallacy"
    "the burden of proof is on the person who makes the claim, not on the person who denies (or questions the claim", #"Onus probandi"
    "a proposition is repeatedly restated regardless of contradiction", #"Proof by assertion"
    "deviating from the topic at hand by introducing a separate argument the speaker believes is easier to speak to", #"Red herring"
    "arguer offers such a large number of arguments for their position that the opponent can't possibly respond to all of them", #"Shotgun argumentation"
    "basing a broad conclusion on a small sample", #"Hasty generalization"
    "an argument is made due to the manipulation of emotions, rather than the use of valid reasoning", #"Appeal to emotion"
    "conclusion based on silence or lack of contrary evidence", #"Argument from silence"
    "conclusion is suggested based solely on something or someone's origin rather than its current meaning or context", #"Genetic fallacy"
    "comparing an opponent or their argument to Hitler or Nazism in an attempt to associate a position with one that is universally reviled", #"Reductio ad Hitlerum"
    "some event must inevitably follow from another without rational argument for the inevitability of the event in question" #"Slippery slope"
)

newFallacies <- function() { return(sample(1:length(fallacyNames), nSquares, replace=FALSE)) }

nSquares <- 24;

drawFront <- function(fallacies, x, y) {
       pushViewport(viewport(layout.pos.col=x, layout.pos.row=y))

        ##
        # cardId is a unique identifier for this bingo card.
        #
        #cardId <- paste(format.hexmode(indexes, width=2, upper.case = TRUE), collapse="")

        names <- list(1:5, c("B","I","N","G","O"))

        ## word wrap
        squares <- sapply(lapply(fallacyNames[fallacies], strwrap, width=17), paste, collapse="\n")

        Bingo.vector <- c(squares[1:12], "FREE\nSPACE", squares[13:24])
        Bingo.matrix <- matrix(data=Bingo.vector, nrow=5, ncol=5, dimnames=names)
        Bingo.table <- as.table(Bingo.matrix)

        grid.table(Bingo.table,
                   gpar.corefill = gpar(fill = "white", col = NA),
                   gpar.colfill = gpar(fill = "white"),
                   gpar.rowfill = gpar(fill = "white"),
                   gpar.coretext =  gpar(col = "black", cex = 0.38),
                   separator = "black",
                   show.box = "TRUE",
                   show.vlines = TRUE, show.hlines = TRUE,
                   show.namesep = TRUE, show.csep = TRUE,
                   show.rsep=TRUE,
                   padding.v = unit(.52, "cm"),
                   padding.h = unit(.4, "cm"))

        popViewport()
}

drawBack <- function(fallacies, x, y) {
        pushViewport(viewport(layout.pos.col=x, layout.pos.row=y, layout=grid.layout(nSquares,8)))

        names <- list(fallacyNames[fallacies], ("description"))
        #descs <- sapply(lapply(fallacyDescriptions[fallacies], strwrap, width=80), paste, collapse="\n")
        descs <- fallacyDescriptions[fallacies]

        descMatrix <- matrix(descs, nrow=nSquares, ncol=1, dimnames=names)

        grid.table(as.table(descMatrix),
                   cols=NULL,
                   gpar.corefill = gpar(fill = "white", col = NA),
                   gpar.colfill = gpar(fill = "white"),
                   gpar.rowfill = gpar(fill = "white"),
                   gpar.coretext =  gpar(col = "black", cex = 0.26),
                   gpar.rowtext =  gpar(col = "black", cex = 0.25),
                   separator = "black",
                   show.box = "TRUE",
                   show.rsep=TRUE,
                   padding.v = unit(.1, "cm"),
                   padding.h = unit(.1, "cm")
                   )

        popViewport()
}

for(i in 1:6)
{
        f1 <- newFallacies()
        f2 <- newFallacies()
        f3 <- newFallacies()
        f4 <- newFallacies()
        f5 <- newFallacies()
        f6 <- newFallacies()

        ## Draw the cards
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(3, 2)))
        drawFront(f1, 1, 1)
        drawFront(f2, 1, 2)
        drawFront(f3, 1, 3)
        drawFront(f4, 2, 1)
        drawFront(f5, 2, 2)
        drawFront(f6, 2, 3)
        popViewport()

        ## Draw the descriptions
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(3, 2)))
        drawBack(f1, 2, 1)
        drawBack(f2, 2, 2)
        drawBack(f3, 2, 3)
        drawBack(f4, 1, 1)
        drawBack(f5, 1, 2)
        drawBack(f6, 1, 3)
        popViewport()
}
