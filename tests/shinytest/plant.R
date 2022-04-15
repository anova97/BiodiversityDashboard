app <- ShinyDriver$new("../../")
app$snapshotInit("plant")

app$setInputs(`PL-kingdomGroup` = c("Plantae", "Fungi"))
app$setInputs(`PL-kingdomGroup` = "Plantae")
app$setInputs(`PL-kingdomGroup` = c("Plantae", "Fungi"))
app$setInputs(`PL-kingdomGroup` = "Plantae")
app$snapshot()
