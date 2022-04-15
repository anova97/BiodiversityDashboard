app <- ShinyDriver$new("../../")
app$snapshotInit("pl_Animalia")

app$setInputs(`PL-kingdomGroup` = c("Animalia", "Fungi"))
app$setInputs(`PL-kingdomGroup` = "Animalia")
app$setInputs(`PL-scientific` = "Dendrocopos leucotos")
app$snapshot()
