# Test suite interactive_pxweb()

context("interactive_pxweb.R")

test_that(desc="findData.inputBaseCat",{
  load(system.file("extdata/test_files/testFiles.Rdata", package = "pxweb"))
  expect_equal(pxweb:::findData.inputBaseCat(1:2,test_codedAlt),
                "\n('q' = Quit, 'b' = Back)")
  expect_equal(pxweb:::findData.inputBaseCat(c(3,6),test_codedAlt),
                "\n('*' = Select all, 'a' = Show all)")

})

test_that(desc="findData.printNode",{
  xscb <-data.frame(id=c("01","02","03"),
                    text=c("Värde 1","Värde 2", "Värde 3"))
  
  expect_output(pxweb:::findData.printNode(xscb, print=TRUE),"Värde 3")
  expect_output(pxweb:::findData.printNode(xscb, print=TRUE),"2. ")
  expect_that(pxweb:::findData.printNode(xscb, print=FALSE),is_a("character"))
  expect_match(pxweb:::findData.printNode(xscb, print=FALSE),"Värde 1")
})

test_that(desc="findData.printCode",{
  varListText <- c("first","second","last") 

  expect_output(pxweb:::findData.printCode(url="urladress", varListText, clean=TRUE),
                "urladress")
  expect_output(pxweb:::findData.printCode(url="urladress", varListText, clean=TRUE),
                "clean = TRUE")
  expect_output(pxweb:::findData.printCode(url="urladress", varListText, clean=TRUE),
                "list\\(first")
})

test_that(desc="findData.inputConvert",{
  expect_that(pxweb:::findData.inputConvert(input=c("2","2:3","3:7","6")), 
              is_equivalent_to(c("2","3","4","5","6","7")))
  expect_that(pxweb:::findData.inputConvert(c("4:5")), 
              is_equivalent_to(c("4","5")))
  expect_that(pxweb:::findData.inputConvert(c("2","10:11","5")), 
              is_equivalent_to(c("2","5","10","11")))
  expect_that(pxweb:::findData.inputConvert("*"), 
              is_equivalent_to("*"))  
  expect_that(pxweb:::findData.inputConvert(input=":3"), 
              is_equivalent_to(as.character(1:3)))  
  expect_that(pxweb:::findData.inputConvert(c(":3","5")), 
              is_equivalent_to(as.character(c(1:3,5))))
  expect_that(pxweb:::findData.inputConvert(c("1", "3", "5:"), max_value=7), 
              is_equivalent_to(as.character(c(1,3,5:7))))
  expect_that(pxweb:::findData.inputConvert(c("1", "3", "5:")), 
              is_equivalent_to(as.character(c(1,3,5))))
})

test_that(desc="download_pxweb",{
  load(system.file("extdata/test_files/testFiles.Rdata", package = "pxweb"))
  expect_output(pxweb:::download_pxweb(dataNode = testNullNode, test_input = c("n", "n", "y")),
                "To download the same data again, use the following code:")
})

test_that(desc="findData.input",{
  load(system.file("extdata/test_files/testFiles.Rdata", package = "pxweb"))
  
  expect_that(pxweb:::findData.input(type="yesno","Testing 'y'", test_input="y", silent=TRUE),is_equivalent_to("y"))
  expect_that(pxweb:::findData.input(type="yesno","Testing 'n'", test_input="n", silent=TRUE),is_equivalent_to("n"))
  expect_that(pxweb:::findData.input(type="text","Testing 'MyData1'", test_input="MyData1", silent=TRUE), is_equivalent_to("MyData1"))
  expect_that(pxweb:::findData.input(type="node", testBaseNode, test_input="3", silent=TRUE),
              is_equivalent_to("3"))
  expect_that(pxweb:::findData.input(type="node", testBaseNode, test_input="b", silent=TRUE),
              is_equivalent_to("b"))

  test_varDF <- list(data.frame(id = as.character(seq(0.5,10,0.5)),
                      text = paste("Värde", as.character(seq(0.5,10,0.5))),
                      stringsAsFactors = FALSE),
                     "testingVärde")
  expect_that(pxweb:::findData.input(type="alt", input=test_varDF, test_input="10:12, 1 ,3:1, 2", silent=TRUE),
              is_equivalent_to(c("1","2","3","10","11","12")))
  expect_that(pxweb:::findData.input(type="alt", input=test_varDF, test_input="7:,3", silent=TRUE),
              is_equivalent_to(c("3",as.character(7:20))))

})

