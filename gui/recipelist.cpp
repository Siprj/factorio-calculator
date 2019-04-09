#include "recipelist.h"
#include "ui_recipelist.h"

#include <QListView>
#include <QDebug>
#include <QPainter>
#include <QPixmap>
#include <math.h>
#include <QRegularExpression>

#include "icon.h"


RecipeList::RecipeList(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::RecipeList)
{
    ui->setupUi(this);

    ui->recipeView->setViewMode(QListView::IconMode);
    ui->recipeView->setIconSize(QSize(32, 32));
    ui->recipeView->setGridSize(QSize(32, 32));
    ui->recipeView->setSpacing(0);
    ui->recipeView->setResizeMode(QListView::Adjust);

    ui->recipeView->setModel(&model);

    connect(ui->clearPushButton, &QPushButton::clicked, this, &RecipeList::clearFilter);
    connect(ui->filterLineEdit, &QLineEdit::textEdited, this, &RecipeList::filter);
    connect(ui->recipeView, &QListView::clicked, this, &RecipeList::recipeClicked);
}

RecipeList::~RecipeList()
{
    delete ui;
}

void RecipeList::setRecipeList(Recipes &recipes)
{
    this->allRecipes = recipes;
    this->filteredRecipes = recipes.values();

    addRecipesToModel();
}

void RecipeList::clearFilter()
{
    filteredRecipes = allRecipes.values();
    ui->filterLineEdit->clear();

    addRecipesToModel();
}

void RecipeList::filter(QString filterText)
{
    QRegularExpression regExp(filterText);
    filteredRecipes.clear();
    filteredRecipes.reserve(allRecipes.size());
    for (auto &item : allRecipes)
    {
        if (regExp.match(item.name).hasMatch())
        {
            filteredRecipes += item;
        }
    }
    addRecipesToModel();
}

void RecipeList::recipeClicked(QModelIndex index)
{
    emit clicked(filteredRecipes.at(index.row()));
}

void RecipeList::addRecipesToModel()
{
    model.clearModel();

    for (auto &v : filteredRecipes)
    {
        model.addPiece(icon::composeIcon(v.icons), &v);
    }
}
