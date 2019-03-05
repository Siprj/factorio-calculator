#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

#include "recipe.h"
#include "recipeviewmodel.h"


namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

public slots:
    void clearModel();

private:
    Ui::MainWindow *ui;
    Recipes recipes;
    RecipeViewModel model;
};

#endif // MAINWINDOW_H
