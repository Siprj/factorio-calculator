#include "node.h"

#include <QPainter>
#include <QPainterPath>


Node::Node
    ( QString name
    , QList<QPair<QPixmap, QString>> inputs
    , QList<QPair<QPixmap, QString>> outputs
    )
    : name(name)
    , inputs(inputs)
    , outputs(outputs)
    , roundDiameter(15)
    , height(20)
    , width(200)
{

}

void Node::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
    Q_UNUSED(option)
    Q_UNUSED(widget)

    QPainterPath titlePath;
    titlePath.moveTo(width, roundDiameter / 2);
    titlePath.arcTo(width - roundDiameter, 0, roundDiameter, roundDiameter, 0.0, 90.0);
    titlePath.lineTo(roundDiameter / 2, 0);
    titlePath.arcTo(0, 0, roundDiameter, roundDiameter, 90.0, 90.0);
    titlePath.lineTo(0, height);
    titlePath.lineTo(width, height);
    titlePath.closeSubpath();

    painter->setRenderHint(QPainter::Antialiasing);
    painter->setRenderHint(QPainter::HighQualityAntialiasing);
    const QBrush brush = painter->brush();
    QBrush harderBrush(QColor("#b3ffff"));
    painter->setBrush(harderBrush);
    QPen pen;
    pen.setWidth(2);
    painter->setPen(pen);

    painter->drawPath(titlePath);

    QFont font("Nimbus Romana", height - 5);
    painter->setFont(font);
    painter->drawText(0, 0, width, height, Qt::AlignCenter | Qt::AlignVCenter, name);
}

QRectF Node::boundingRect() const
{
    return QRect(0,0, 100, 50);
}

int Node::getHeight() const
{
    return height;
}

void Node::setHeight(int value)
{
    height = value;
}

int Node::getWidth() const
{
    return width;
}

void Node::setWidth(int value)
{
    width = value;
}

QString Node::getName() const
{
    return name;
}
