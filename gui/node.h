#ifndef NODE_H
#define NODE_H

#include <QWidget>
#include <QGraphicsItem>
#include <QRectF>
#include <QPair>

#include "nodeitem.h"
#include "nodeport.h"


class Node : public QGraphicsItem
{
public:
    Node
        ( QString name
        , QList<NodeItem> inputs
        , QList<NodeItem> outputs
        );

    void paint
        ( QPainter *painter
        , const QStyleOptionGraphicsItem *option
        , QWidget *widget
        );

    QRectF boundingRect() const;

    QString getName() const;

private:
    void drawLine(QPainter *painter, NodeItem item, qreal y);

    QString name;
    QString fontName{"Nimbus Romana"};
    QList<NodeItem> inputs;
    QList<NodeItem> outputs;
    const qreal itemCountWidth{20};
    const qreal roundDiameter{15};
    const qreal textHeight{12};
    const qreal titleHeight{20};
    const qreal rowHeight{40};
    const qreal iconSize{32};
    const qreal columnPadding{5};
    qreal nodeWidth;
    QRectF _boundingRect;
};

#endif // NODE_H
