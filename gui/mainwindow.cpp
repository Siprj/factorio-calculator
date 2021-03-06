#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <QListView>
#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QDebug>
#include <QDir>
#include <QTreeWidgetItem>
#include <QTreeWidget>
#include <QList>

#include "path.h"
#include "factorio-data.h"
#include "node.h"
#include "icon.h"
#include "nodeitem.h"


MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);

    QString fileName(dirPath);
    fileName.append("data.json");
    QFile file(fileName);
    file.open(QIODevice::ReadOnly);

    QString val = file.readAll();
    file.close();
    QJsonDocument d = QJsonDocument::fromJson(val.toUtf8());
    factorioData = fromJsonObject(d.object());

    QDir::setCurrent(dirPath);

    QVBoxLayout *layout = new QVBoxLayout(ui->widget);
    layout->addWidget(&recipList);
    layout->setMargin(0);

    recipList.setRecipeList(factorioData.recipes);

    connect(&recipList, &RecipeList::clicked, this, &MainWindow::showRecipeDetails);

    scene.reset(new NodeScene(&factorioData));
    ui->graphicsView->setScene(scene.get());

    for (auto &item : factorioData.items)
    {
        itemMap.insert(item.name, icon::composeIcon(item.icons));
    }

    auto iter = itemMap.begin();

    QList<NodeItem> inputs;
    inputs.append(NodeItem{iter.key(), iter.value(), 0});
    QList<NodeItem> outputs;
    ++iter;
    outputs.append(NodeItem{iter.key(), iter.value(), 0});
    Node *node = new Node("pokus", inputs, outputs);

    scene->addItem(node);
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::showRecipeDetails(Recipe recipe)
{
    recipDetailsItems.clear();
    ui->recipeDetailsTreeWidget->clear();

    QSharedPointer<QTreeWidgetItem> name = QSharedPointer<QTreeWidgetItem>(new QTreeWidgetItem);
    name->setData(0, Qt::DisplayRole, "name");
    name->setData(1, Qt::DisplayRole, recipe.name);
    recipDetailsItems.append(name);
    ui->recipeDetailsTreeWidget->addTopLevelItem(name.get());

    QSharedPointer<QTreeWidgetItem> ingredients = QSharedPointer<QTreeWidgetItem>(new QTreeWidgetItem);
    ingredients->setData(0, Qt::DisplayRole, "ingredients");
    recipDetailsItems.append(ingredients);
    for (auto &ingredient : recipe.ingredients)
    {
        QSharedPointer<QTreeWidgetItem> ingredientItem =
            QSharedPointer<QTreeWidgetItem>(new QTreeWidgetItem(ingredients.get()));
        ingredientItem->setData(0, Qt::DisplayRole, ingredient.name);
        ingredientItem->setData(1, Qt::DisplayRole, ingredient.amount);
        recipDetailsItems.append(ingredientItem);
    }
    ui->recipeDetailsTreeWidget->addTopLevelItem(ingredients.get());
    QSharedPointer<QTreeWidgetItem> results = QSharedPointer<QTreeWidgetItem>(new QTreeWidgetItem);
    results->setData(0, Qt::DisplayRole, "results");
    recipDetailsItems.append(results);
    for (auto &product : recipe.products)
    {
        QSharedPointer<QTreeWidgetItem> resultItem =
            QSharedPointer<QTreeWidgetItem>(new QTreeWidgetItem(results.get()));
        resultItem->setData(0, Qt::DisplayRole, product.name);
        resultItem->setData(1, Qt::DisplayRole, product.amount);
        recipDetailsItems.append(resultItem);
    }
    ui->recipeDetailsTreeWidget->addTopLevelItem(results.get());

    ui->recipeDetailsTreeWidget->expandAll();
}
