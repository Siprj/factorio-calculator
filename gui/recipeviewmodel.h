#ifndef RECIPEVIEWMODEL_H
#define RECIPEVIEWMODEL_H

#include <QAbstractListModel>
#include <QList>
#include <QPixmap>
#include <QPoint>
#include <QStringList>

#include "factorio-data.h"


class RecipeViewModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit RecipeViewModel(QObject *parent = nullptr);

    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
    void addPiece(const QPixmap &pixmap, Recipe *recipe);
    int rowCount(const QModelIndex &parent) const override;

    Qt::ItemFlags flags(const QModelIndex &) const override;
    QMimeData* mimeData(const QModelIndexList &indexes) const override;

    void clearModel();

private:
    QList<QPixmap> pixmaps;
    QList<Recipe*> recipes;

    int m_PieceSize;
};

#endif // RECIPEVIEWMODEL_H
