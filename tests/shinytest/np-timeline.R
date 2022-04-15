app <- ShinyDriver$new("../../")
app$snapshotInit("np-timeline")

app$setInputs(`NP-scientific` = "Gracupica contra")
app$snapshot()
