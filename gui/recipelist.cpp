#include "recipelist.h"
#include "ui_recipelist.h"

#include <QListView>
#include <QDebug>
#include <QPainter>
#include <QPixmap>
#include <math.h>
#include <QRegularExpression>


RecipeList::RecipeList(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::RecipeList)
{
    ui->setupUi(this);

    ui->recipeView->setViewMode(QListView::IconMode);
    ui->recipeView->setIconSize(QSize(32,32));
    ui->recipeView->setGridSize(QSize(32,32));
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
    this->filteredRecipes = recipes;

    addRecipesToModel();
}

void RecipeList::clearFilter()
{
    filteredRecipes = allRecipes;
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

    for (auto v : filteredRecipes)
    {
        QPixmap iconPixmap(32, 32);
        iconPixmap.fill(QColor(0,0,0, 0));
        QPainter paint(&iconPixmap);
        for (int i = 0; i < v.icons.size(); i++)
        {
            Icon icon = v.icons.at(i);

            QPixmap iconPart(icon.filePath);
            QPixmap scaledIconPart(iconPart.scaled
                ( static_cast<int>(std::round(iconPart.width() * icon.scale))
                , static_cast<int>(std::round(iconPart.height() * icon.scale))
                ));

            int x = 0;
            int y = 0;

            if (icon.shift.has_value())
            {
                // Move shift coordinate system to center and scale it.
                x = static_cast<int>(((32/2) * icon.scale) + (static_cast<double>(icon.shift.value().x)));
                y = static_cast<int>(((32/2) * icon.scale) + (static_cast<double>(icon.shift.value().y)));
            }

            paint.drawPixmap(x, y, scaledIconPart.width(), scaledIconPart.height(), scaledIconPart);
        }
        model.addPiece(iconPixmap, v.name);
    }
}
