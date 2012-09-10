#
# Понижение размерности признаков. Взаимное расположение признаков
#

##  Визуализация объекта из тестового или обучающего множества 
#   \param  digit.data  строчка файла без метки класса
#   \result изображение объекта
display.digit <- function(digit.data) {
    an.object <- matrix(as.numeric(digit.data), nrow=28)
    image(an.object, ylim=c(1, 0), col = gray(255:0/255), axes = F)  
}

train <- read.csv("../Data/train.csv")
labels <- as.numeric(train[,1])
train <- train[,-1]

# Метод главных компонент
pca <- prcomp(train)
# Вычисление главных компонент для этого набора данных занимает много времени.
# Результат лучше сохранить, чтобы позже его не пересчитывать.
#save.image(file="dim-reduction.RData")
# Главные компоненты находятся в переменной x
str(pca$x)
pca$x[1:10, 1:20]
summary(pca)

# Если для каждой цифры определить свой цвет, 
# то в редуцированном пространстве признаков всё множество из файла выглядит так
plot(pca$x[,1], pca$x[,2], col=labels)

# Можно сравнить попарное расположение классов
class1 <- 5
class2 <- 6
plot.set1 <- pca$x[labels==class1,]
plot.set2 <- pca$x[labels==class2,]
plot(plot.set1[,1], plot.set1[,2], col = "blue", pch = 16)
points(plot.set2[,1], plot.set2[,2], col = "red", pch = 16)
# Добавим третий класс
class3 <- 2
plot.set3 <- pca$x[labels==class3,]
points(plot.set3[,1], plot.set3[,2], col = "green", pch = 16)

# Другие методы понижения размерности могут не заработать.
# Многомерное шкалирование cmdscale сделать не получается из-за ограничений оперативной памяти.

# Как выглядят цифры если сначала размерность признаков уменьшить, а потом восстановить.
# Размерность редуцированного признака pca.num (при исходных 784)
pca.num <- 5
# Восстанавливаем изображение
reconstruction.data <- pca$x[,1:pca.num] %*% t(pca$rotation[, 1:pca.num])
display.digit(reconstruction.data[20,])

# Обзорная картинка по всем классам (цифрам) и разным уровнем реконструкции
# Первые 10 разных цифр из обучающего множества
first.10.places <- c()
for (i in 0:9) {
    first.10.places <- c(first.10.places, which(labels == i)[1])
}
# Уровни, из которых изображение восстанавливается
pca.nums <- c(2, 10, 20, 50, 75, 100, 150, 200)
op <- par(no.readonly = TRUE)
par(mfrow=c(8, 10))
par(mar=c(0, 0, 0, 0))
for (pca.num in pca.nums) {
    reconstruction.data <- pca$x[,1:pca.num] %*% t(pca$rotation[, 1:pca.num])
    for (digit.pos in first.10.places) {
        display.digit(reconstruction.data[digit.pos,])
    }
}
par(op)



