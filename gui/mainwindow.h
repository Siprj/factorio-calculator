#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QVBoxLayout>
#include <QTreeWidgetItem>
#include <QSharedPointer>
#include <QGraphicsScene>

#include "recipe.h"
#include "recipelist.h"


namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private slots:
    void showRecipeDetails(Recipe recipe);

private:
    Ui::MainWindow *ui;
    Recipes recipes;
    RecipeList recipList;

    QGraphicsScene scene;

    QList<QSharedPointer<QTreeWidgetItem>> recipDetailsItems;
};

#endif // MAINWINDOW_H
