#include "recipeviewmodel.h"

#include <QIcon>
#include <QDebug>
#include <QMimeData>

#include "mime-data.h"


RecipeViewModel::RecipeViewModel(QObject *parent)
    : QAbstractListModel(parent)
{

}

QVariant RecipeViewModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();

    if (role == Qt::DisplayRole)
        return recipes.value(index.row())->name;
    if (role == Qt::ToolTipRole)
        return recipes.value(index.row())->name;
    if (role == Qt::DecorationRole)
        return QIcon(pixmaps.value(index.row()));

    return QVariant();
}

void RecipeViewModel::addPiece(const QPixmap &pixmap, Recipe *recipe)
{
    int row = pixmaps.size();

    beginInsertRows(QModelIndex(), row, row);
    pixmaps.insert(row, pixmap);
    recipes.insert(row, recipe);
    endInsertRows();
}

int RecipeViewModel::rowCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;
    else
        return pixmaps.size();
}

Qt::ItemFlags RecipeViewModel::flags(const QModelIndex&) const
{
    return Qt::ItemIsDragEnabled | Qt::ItemIsEnabled | Qt::ItemIsSelectable;
}

QMimeData* RecipeViewModel::mimeData(const QModelIndexList &indexes) const
{

    QMimeData *mimeData = new QMimeData;

    QByteArray data;
    QDataStream stream(&data, QIODevice::WriteOnly);

    auto &index = indexes.first();
    if (index.isValid())
    {
        stream << recipes.at(index.row())->name;
    }

    mimeData->setData(recipeMimeType, data);

    return mimeData;
}

void RecipeViewModel::clearModel()
{
    beginRemoveRows(QModelIndex(), 0, recipes.size() - 1);
    pixmaps.clear();
    recipes.clear();
    endRemoveRows();
}
