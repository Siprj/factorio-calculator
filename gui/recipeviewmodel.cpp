#include "recipeviewmodel.h"

#include <QIcon>
#include <QDebug>


RecipeViewModel::RecipeViewModel(QObject *parent)
    : QAbstractListModel(parent)
{

}

QVariant RecipeViewModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();

    if (role == Qt::DisplayRole)
        return names.value(index.row());
    if (role == Qt::ToolTipRole)
        return names.value(index.row());
    if (role == Qt::DecorationRole)
        return QIcon(pixmaps.value(index.row()));

    return QVariant();
}

void RecipeViewModel::addPiece(const QPixmap &pixmap, QString name)
{
    int row = pixmaps.size();

    beginInsertRows(QModelIndex(), row, row);
    pixmaps.insert(row, pixmap);
    names.insert(row, name);
    endInsertRows();
}

int RecipeViewModel::rowCount(const QModelIndex &parent) const
{
    if (parent.isValid())
        return 0;
    else
        return pixmaps.size();
}

void RecipeViewModel::clearModel()
{
    beginRemoveRows(QModelIndex(), 0, names.size() - 1);
    pixmaps.clear();
    names.clear();
    endRemoveRows();
}
