app <- ShinyDriver$new("../../")
app$snapshotInit("fungi-pl")

app$setInputs(`PL-kingdomGroup` = c("Plantae", "Fungi"))
app$setInputs(`PL-kingdomGroup` = "Fungi")
app$snapshot()
