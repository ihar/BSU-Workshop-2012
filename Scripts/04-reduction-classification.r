#
# Классификация редуцированных признаков. Эффективность классификаторов.
#

train <- read.csv("../Data/train.csv")

# Используем метод главных компонент для уменьшения размерности данных 42000 × 784
# Номера классов, которые в первом столбце, нужно предварительно убрать
pca <- prcomp(train[,-1])
# Решаем сколько главных компонент оставлять, анализируя график
plot(pca) # 10 компонент должно хватить
# Или смотрим на сводную информацию и принимаем решение, основываясь на этой информации
summary(pca) # 154 компоненты (95% порог)
# Или решаем исходя и картинки с восстановленными признаками при разном количестве компонент
# digit-reconstruction.png → 200 компонент - хорошо

# Проведём эксперимент по тренировке классификатора и распознавания 
# для этих трёх разных значений: 10, 154 и 200 главных компонент.
# То есть, осуществляем классификацию для трёх наборов данных с размерностями
#   42000 × 10
#   42000 × 154
#   42000 × 200

# Для обеспечения повторяемости
set.seed(13)
# Оставляем нужное количество главных компонент
pc.num <- 200
train.pca <- pca$x[,1:pc.num]
# Разбиваем на тренировочное и тестовое множество примерно в нужном отношении 
split <- runif(dim(train.pca)[1]) > 0.2  
train.data <- train.pca[split,]  
test.data <- train.pca[!split,] 
# Отдельно сохраняем метки классов 
# и убираем столбец с метками классов в тренировочных и тестовых данных
train.labels <- train[split,1]
test.labels <- train[!split,1]

# knn, 
# 10 pc - 92.23266
# 154 pc - 96.52688
# 200 pc - 96.52688
library("FNN")
knn.res <- (0:9)[FNN::knn(train.data, test.data, train.labels, k = 10, algorithm="cover_tree")]
knn.accuracy <- sum(knn.res == test.labels)/dim(test.data)[1]
100 * knn.accuracy

# randomForest,
# 10 pc - 90.66745
# 154 pc - 94.2469
# 200 pc - 94.03426
library("randomForest")
rf <- randomForest(train.data, as.factor(train.labels), xtest = test.data, ntree=100)
rf.predictions <- (0:9)[rf$test$predicted]
rf.accuracy <- sum(rf.predictions == test.labels)/dim(test.data)[1]
100 * rf.accuracy

# svm
# 10 pc - 93.29002
# 154 pc - 96.94034
# 200 pc - 96.71589
library("e1071")
svm.model <- svm(x = train.data, y = as.factor(train.labels))
svm.predictions <- predict(svm.model, test.data)
svm.accuracy <- sum(svm.predictions == test.labels)/dim(test.data)[1]
100*svm.accuracy