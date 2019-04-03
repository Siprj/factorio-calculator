#ifndef RECIPEVIEWMODEL_H
#define RECIPEVIEWMODEL_H

#include <QAbstractListModel>
#include <QList>
#include <QPixmap>
#include <QPoint>
#include <QStringList>


class RecipeViewModel : public QAbstractListModel
{
    Q_OBJECT
public:
    explicit RecipeViewModel(QObject *parent = nullptr);

    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
    void addPiece(const QPixmap &pixmap, QString name);
    int rowCount(const QModelIndex &parent) const override;

    Qt::ItemFlags flags(const QModelIndex &) const override;
    QMimeData* mimeData(const QModelIndexList &indexes) const override;

    void clearModel();

private:
    QList<QPixmap> pixmaps;
    QList<QString> names;

    int m_PieceSize;
};

#endif // RECIPEVIEWMODEL_H
