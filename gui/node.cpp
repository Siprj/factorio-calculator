#include "node.h"

#include <QPainter>
#include <QPainterPath>


Node::Node
    (QString name
    , QList<QPair<QString, QPixmap>> inputs
    , QList<QPair<QString, QPixmap>> outputs
    )
    : name(name)
    , inputs(inputs)
    , outputs(outputs)
    , roundDiameter(15)
    , titleHeight(20)
    , titleWidth(200)
    , rowHeight(36)
{

}

void Node::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
    Q_UNUSED(option)
    Q_UNUSED(widget)

    painter->setRenderHint(QPainter::Antialiasing);
    painter->setRenderHint(QPainter::HighQualityAntialiasing);


    qreal roundRadius = (roundDiameter / 2);

    QPainterPath titlePath;
    titlePath.moveTo(titleWidth, roundRadius);
    titlePath.arcTo(titleWidth - roundDiameter, 0, roundDiameter, roundDiameter, 0.0, 90.0);
    titlePath.lineTo(roundDiameter / 2, 0);
    titlePath.arcTo(0, 0, roundDiameter, roundDiameter, 90.0, 90.0);
    titlePath.lineTo(0, titleHeight);
    titlePath.lineTo(titleWidth, titleHeight);
    titlePath.closeSubpath();

    QBrush titleBrush(QColor("#b3ffff"));
    painter->setBrush(titleBrush);
    QPen pen;
    pen.setWidth(2);
    painter->setPen(pen);
    painter->drawPath(titlePath);

    QFont font("Nimbus Romana", titleHeight - 5);
    painter->setFont(font);
    painter->drawText(0, 0, titleWidth, titleHeight, Qt::AlignCenter | Qt::AlignVCenter, name);

    qreal bodySize = (inputs.size() + outputs.size()) * rowHeight;
    qreal bottomY = titleHeight + bodySize + roundRadius;
    // Body
    QPainterPath bodyPath;
    bodyPath.moveTo(titleWidth, titleHeight);
    bodyPath.lineTo(titleWidth, bottomY - roundRadius);
    bodyPath.arcTo(titleWidth - roundDiameter, bottomY - roundDiameter, roundDiameter, roundDiameter, 0.0, -90.0);
    bodyPath.lineTo(roundRadius, bottomY);
    bodyPath.arcTo(0, bottomY - roundDiameter, roundDiameter, roundDiameter, -90.0, -90.0);
    bodyPath.lineTo(0, titleHeight);
//    bodyPath.lineTo(roundDiameter / 2, 0);
//    bodyPath.arcTo(0, 0, roundDiameter, roundDiameter, 90.0, 90.0);
//    bodyPath.lineTo(0, titleHeight);
//    bodyPath.lineTo(titleWidth, titleHeight);
    bodyPath.closeSubpath();

    QBrush bodyBrush(QColor("#666666"));
    painter->setBrush(bodyBrush);
    painter->drawPath(bodyPath);
}

QRectF Node::boundingRect() const
{
    return QRect(0,0, 100, 50);
}

QString Node::getName() const
{
    return name;
}
