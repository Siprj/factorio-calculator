#ifndef NODEITEM_H
#define NODEITEM_H

#include <QPixmap>
#include <QString>


struct NodeItem
{
    QString name;
    QPixmap icon;
    qreal amount;
};

#endif // NODEITEM_H
