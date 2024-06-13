source("common.R")
source("template.R")

# 加载订单簿
book1 <- book.load('input/book_3.csv')

# 定义测试的买单大小
sizes <- c(100, 167, 180, 189, 208)

# 执行测试并打印结果
cat("测试结果：\n")
for (size in sizes) {
  result <- book.extra2(book1, size)
  cat(sprintf("对于size = %d，预期中价 = %f\n", size, result))
}
