#
# Классификация. Эффективность классификаторов.
#

train <- read.csv("../Data/train.csv")

# Весь набор train нужно разбить на две части. Данными первой части будем
# тренировать классификаторы, на данных второй части будем измерять эффективность классификатора
# Обычно 80% данных используется для тренировки, 20% для тестирования

# Для обеспечения повторяемости
set.seed(13)

# Разбиваем на тренировочное и тестовое множество примерно в нужном отношении 
split <- runif(dim(train)[1]) > 0.2  
train.data <- train[split,]  
test.data <- train[!split,] 

# Отдельно сохраняем метки классов 
# и убираем столбец с метками классов в тренировочных и тестовых данных
train.labels <- train.data[,1]
test.labels <- test.data[,1]
train.data <- train.data[,-1]
test.data <- test.data[,-1]

# Алгоритмы классификации на больших наборах данных могут работать медленно,
# используя большое количество ресурсов компьютера

# Классификация методом k ближайших соседей
# http://ru.wikipedia.org/wiki/Метод_k_ближайших_соседей
# http://en.wikipedia.org/wiki/Nearest_neighbor_search
library("FNN")
knn.res <- (0:9)[knn(train.data, test.data, train.labels, k = 10, algorithm = "cover_tree")]
# Сколько объектов правильно классифицировано
knn.accuracy <- sum(knn.res == test.labels)/dim(test.data)[1]
# в процентах 
100 * knn.accuracy # 96.33786%

# Случайный лес
# http://ru.wikipedia.org/wiki/Random_forest
library("randomForest")
# ntree обычно не меньше 500
rf <- randomForest(train.data, as.factor(train.labels), xtest = test.data, ntree=100, do.trace = T)

rf.predictions <- (0:9)[rf$test$predicted]
rf.accuracy <- sum(rf.predictions == test.labels)/dim(test.data)[1]
# в процентах
100 * rf.accuracy #  96.64501%

# Метод опорных векторов
# http://ru.wikipedia.org/wiki/Метод_опорных_векторов
library("e1071")
svm.model <- svm(x = train.data, y = as.factor(train.labels))
svm.predictions <- predict(svm.model, test.data)
svm.accuracy <- sum(svm.predictions == test.labels)/dim(test.data)[1]
100*svm.accuracy


