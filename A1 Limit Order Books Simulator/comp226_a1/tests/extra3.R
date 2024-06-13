source("common.R")
source("template.R")

# 加载订单簿
book1 <- book.load('input/book_3.csv')

result1 <- book.extra3(book1)
print(result1)