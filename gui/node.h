#ifndef NODE_H
#define NODE_H

#include <QObject>
#include <QWidget>
#include <QGraphicsItem>
#include <QRectF>
#include <QPair>


class Node : public QGraphicsItem
{
public:
    // TODO: Use some normal (custom) data structure for inputs and outputs...
    Node
        ( QString name
        , QList<QPair<QString, QPixmap>> inputs
        , QList<QPair<QString, QPixmap>> outputs
        );

    void paint
        ( QPainter *painter
        , const QStyleOptionGraphicsItem *option
        , QWidget *widget
        );

    QRectF boundingRect() const;

    QString getName() const;

private:
    QString name;
    QList<QPair<QString, QPixmap>> inputs;
    QList<QPair<QString, QPixmap>> outputs;
    qreal roundDiameter;
    int titleHeight;
    int titleWidth;
    int rowHeight;
};

#endif // NODE_H
