# Testing the functions in the R package rSCB:
# file: SCBdata.R
# require(testthat)
# test_file("inst/tests/tests_finddata.R")
# load("inst/tests/testFiles.Rdata")
# test_package("sweSCB")

cat("findData.inputBaseCat : ")

test_that(desc=".findData.inputBaseCat",{
  load("testFiles.Rdata")
  expect_output(.findData.inputBaseCat(1:2,test_codedAlt),
                "('q' = Quit, 'b' = Back)")
  expect_output(.findData.inputBaseCat(c(3,6),test_codedAlt),
                "('*' = Select all, 'a' = Show all)")

})

cat("\nfindData.printNode : ")

test_that(desc=".findData.printNode",{
  xscb <-data.frame(id=c("01","02","03"),
                    text=c("Värde 1","Värde 2", "Värde 3"))
  
  expect_output(.findData.printNode(xscb, print=TRUE),"Värde 3")
  expect_output(.findData.printNode(xscb, print=TRUE),"2. ")
  expect_that(.findData.printNode(xscb, print=FALSE),is_a("character"))
  expect_match(.findData.printNode(xscb, print=FALSE),"Värde 1")
})

cat("\nfindData.printCode : ")

test_that(desc=".findData.printCode",{
  varListText <- c("first","second","last") 

  expect_output(.findData.printCode(url="urladress", varListText, clean=TRUE),
                "urladress")
  expect_output(.findData.printCode(url="urladress", varListText, clean=TRUE),
                "clean = TRUE")
  expect_output(.findData.printCode(url="urladress", varListText, clean=TRUE),
                "list\\(first")
})

cat("\nfindData.inputConvert : ")

test_that(desc=".findData.inputConvert",{
  expect_that(.findData.inputConvert(c("2","2:3","3:7","6")), 
              is_equivalent_to(c("2","3","4","5","6","7")))
  expect_that(.findData.inputConvert(c("4:5")), 
              is_equivalent_to(c("4","5")))
  expect_that(.findData.inputConvert(c("2","10:11","5")), 
              is_equivalent_to(c("2","5","10","11")))
  expect_that(.findData.inputConvert("*"), 
              is_equivalent_to("*"))  
})

cat("\nfindData.Download : ")

test_that(desc=".findData.Download",{
  load("testFiles.Rdata")
  expect_output(.findData.Download(dataNode=testNullNode, test_input=c("n", "n", "y")),
                "To download the same data from SCB again, use the following code:")
})

cat("\nfindData.input : ")

test_that(desc=".findData.input",{
  load("testFiles.Rdata")
  
  expect_that(.findData.input(type="yesno","Testing 'y'", test_input="y", silent=TRUE),is_equivalent_to("y"))
  expect_that(.findData.input(type="yesno","Testing 'n'", test_input="n", silent=TRUE),is_equivalent_to("n"))
  expect_that(.findData.input(type="text","Testing 'MyData1'", test_input="MyData1", silent=TRUE), is_equivalent_to("MyData1"))
  expect_that(.findData.input(type="node", testBaseNode, test_input="3", silent=TRUE),
              is_equivalent_to("3"))
  expect_that(.findData.input(type="node", testBaseNode, test_input="b", silent=TRUE),
              is_equivalent_to("b"))

  test_varDF <- list(data.frame(id = as.character(seq(0.5,10,0.5)),
                      text = paste("Värde", as.character(seq(0.5,10,0.5))),
                      stringsAsFactors = FALSE),
                     "testingVärde")
  expect_that(.findData.input(type="alt", input=test_varDF, test_input="10:12, 1 ,3:1, 2", silent=TRUE),
              is_equivalent_to(c("1","2","3","10","11","12")))

})

cat("\n")


