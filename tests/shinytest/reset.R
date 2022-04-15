app <- ShinyDriver$new("../../")
app$snapshotInit("reset")

app$setInputs(`PL-kingdomGroup` = c("Plantae", "Fungi"))
app$setInputs(`PL-kingdomGroup` = "Plantae")
app$setInputs(`PL-kingdomGroup` = character(0))
app$setInputs(`PL-reset` = "click")
app$snapshot()
