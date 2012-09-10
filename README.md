# Материалы к семинару в БГУ, сентябрь 2012 г.

**Тема семинара:** «Классификация изображений на примере распознавания рукописных цифр».   
**Цель семинара:** познакомить учащихся с этапами классификации после извлечения признаков изображений.

## Содержимое репозитория
Материалы презентации разложены по папкам в соответствии со следующей структурой.

*  Data
   *  train.csv.7z — архив с данными для классификации. Текстовый файл содержит 40000 тестовых примеров с отмеченными классами в первом столбце.
*  Images
   *  0blue-1red-2green.png — взаимное расположение элементов из трёх классов на плоскости; синий цвет — цифра 0, красный — 1, зелёный — 2;
   *  0blue-vs-1red.png — взаимное расположение элементов двух классов на плоскости; синий цвет — 0, красный — 1;
   *  0blue-vs-8red.png — взаимное расположение элементов двух классов на плоскости; синий цвет — 0, красный — 8;
   *  5blue-vs-6red.png — взаимное расположение элементов двух классов на плоскости; синий цвет — 5, красный — 6;
   *  all-digits-plot-reduction.png — все элементы из набора данных; разных цвет соответствует представителям разных классов;
   *  an-object.jpg — как выглядит один объект из набора данных, в данном случае цифра 5, нормализованное и центрированное изображение в 256 оттенках серого;
   *  average-classes.png — все 10 усреднённых классов;
   *  digit-reconstruction.png — восстановление исходных изображений после редуцирования пространства признаков; первая строчка - восстановление по 2 главным компонентам, вторая строчка - восстановление по 10 главным компонентам, третья — 20, четвёртая — 50, пятая — 75, шестая — 100, седьмая — 150, восьмая — 200;
   *  med-text-example.jpg — рукописное заключение врача;
   *  med-text-example-02.jpg — рукописное заключение врача с попыткой распознавания человеком;
   *  med-text-example-03.jpg — рукописное заклюение врача;
   *  test-set-items.png — так выглядят первые 10 элементов каждого класса в тестовом наборе данных.
*  Scripts
   * 01-data-overview.r — скрипт R визуализации и предварительного анализа данных;
   * 02-dim-reduction.r — понижение размерности признаков, взаимное расположение представителей классов на графике;
   * 03-classification.r — обучение и классификация методами knn, random forests, svm;
   * 04-reduction-classification.r — обучение и классификация на редуцированных данных, методы те же;
*  Video
   * MVVA-prototype-demo.avi — видео с демонстрацией работы тестового стенда; для распознавания объектов (выбор классификатора) использовалась изложенная методика.
*  BSU-Workshop-2012-slides.pdf — файл с презентацией.

## Ссылки
*  Язык программирования для статистической обработки и программная среда вычислений R, [http://www.r-project.org/](http://www.r-project.org/)
*  База рукописных цифр и соревнование по их распознаванию, [http://www.kaggle.com/c/digit-recognizer
](http://www.kaggle.com/c/digit-recognizer)


