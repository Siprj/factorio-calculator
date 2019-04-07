#include "nodeport.h"

#include <QPainter>
#include <QRadialGradient>


NodePort::NodePort(qreal x, qreal y, PortType portType, QGraphicsItem *parent)
{
    setPos(x, y);
    setParentItem(parent);
    this->portType = portType;
}

void NodePort::paint
    ( QPainter *painter
    , const QStyleOptionGraphicsItem*
    , QWidget*
    )
{
    QPainterPath titlePath;
    titlePath.moveTo(size, size/2);
    titlePath.arcTo(0, 0, size, size, 0.0, 360.0);

    QRadialGradient gradient(size/2, size/2, 10, size/2, size/2);

    switch (portType) {
    case PortType::INPUT:
        gradient.setColorAt(0.0, QColor(50, 232, 9));
        gradient.setColorAt(0.6, QColor(1, 173, 10));
        painter->setPen(QPen(QColor(1, 173, 10)));
        break;
    case PortType::OUTPUT:
        gradient.setColorAt(0.0, QColor(255, 153, 0));
        gradient.setColorAt(0.6, QColor(255, 153, 51));
        painter->setPen(QPen(QColor(255, 153, 0)));
        break;
    }

    QBrush brush(gradient);
    painter->setBrush(brush);

    painter->drawPath(titlePath);
}

QRectF NodePort::boundingRect() const
{
    return {0, 0, size, size};
}
