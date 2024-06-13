source("common.R")
source("template.R")

# 加载订单簿
book2 <- book.load('input/book_3.csv')

# 测试不同的k值
k_values <- c(1.1, 3.62, 3.63)

# 执行测试并打印结果
cat("测试结果：\n")
for (k in k_values) {
  result <- book.extra4(book2, k)
  cat(sprintf("对于k = %.2f%%，最大买单量v = %d\n", k, result))
}
