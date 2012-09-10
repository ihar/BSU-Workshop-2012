#
# Предварительный анализ данных. Визуализация.
#

##  Визуализация объекта из тестового или обучающего множества 
#   \param  digit.data  строчка файла без метки класса
#   \result изображение объекта
display.digit <- function(digit.data) {
  an.object <- matrix(as.numeric(digit.data), nrow=28)
  image(an.object, ylim=c(1, 0), col = gray(255:0/255), axes = F)  
}

# Загружаем данные из файла с данными для обучения
train <- read.csv("../Data/train.csv")
# Структура обучающего множества
str(train)
# Количество объектов в обучающем множестве (=количеству строчек)
dim(train)[1]

# Метки классов
labels <- as.numeric(train[,1])
# Классы без меток
train <- train[,-1]
# Размер одного вектора признаков
length(train[1,])

# Количество объектов в каждом из 10 классов
# Видно, что представителей в каждом классе не одинаковое число, 
# но примерно совпадает
table(labels)

# Что представляет собой отдельный вектор-объект.
# Номер (класс) объекта в базе для обучения
object.num <- 1090
# Какой именно объект под этим номером в базе
labels[object.num]
# Как он выглядит в виде изображения
display.digit(train[object.num,])

# Для визуализации отбираем по 10 первых представителей каждого класса.
vis.data <- c()
for (curr.class in 0:9) {
  # Десятка первых элементов класса i
  curr.class.set <- train[labels == curr.class,][1:10,]
  vis.data <- rbind(vis.data, curr.class.set)
}
# Настройки для более красивого отображения нескольких объектов на одном листе
op <- par(no.readonly = TRUE)
par(mfrow=c(10,10))
par(mar=c(0, 0, 0, 0))  
for (i in 1:100) {
  display.digit(vis.data[i,])
}
par(op)

# Можно посмотреть на усреднённого представителя каждого класса
par(mfrow=c(2, 5))
par(mar=c(0, 0, 0, 0))
for (curr.class in 0:9){
  average.class <- colMeans(train[labels == curr.class,])
  display.digit(average.class)
}
par(op)