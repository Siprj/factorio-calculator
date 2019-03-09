#ifndef RECIPELIST_H
#define RECIPELIST_H

#include <QWidget>

#include "recipeviewmodel.h"
#include "recipe.h"


namespace Ui {
class RecipeList;
}

class RecipeList : public QWidget
{
    Q_OBJECT

public:
    explicit RecipeList(QWidget *parent = nullptr);
    ~RecipeList();

    void setRecipeList(Recipes &recipes);

signals:
    void clicked(Recipe recipe);

public slots:
    void clearFilter();
    void filter(QString filterText);

private slots:
    void recipeClicked(QModelIndex index);

private:
    void addRecipesToModel();

    Ui::RecipeList *ui;
    RecipeViewModel model;
    Recipes allRecipes;
    Recipes filteredRecipes;
};

#endif // RECIPELIST_H
