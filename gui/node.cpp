#include "node.h"

#include <QPainter>
#include <QPainterPath>
#include <QDebug>
#include <QFontMetricsF>


Node::Node
    ( QString name
    , QList<NodeItem> inputs
    , QList<NodeItem> outputs
    )
    : name(name)
    , inputs(inputs)
    , outputs(outputs)
{
    QFont titleFont(fontName, static_cast<int>(titleHeight - 5));
    QFont font(fontName, static_cast<int>(textHeight));

    QFontMetricsF titleFontMatrics{titleFont};
    nodeWidth = titleFontMatrics.width(name);

    QFontMetricsF fontMatrics{font};
    for (auto &item : inputs)
    {
        qreal width = fontMatrics.width(item.name);
        if (width > nodeWidth)
            nodeWidth = width;
    }
    for (auto &item : outputs)
    {
        qreal width = fontMatrics.width(item.name);
        if (width > nodeWidth)
            nodeWidth = width;
    }

    // Add icon size and some padding.
    nodeWidth += iconSize + itemCountWidth + columnPadding * 4;

    qreal bodySize = (inputs.size() + outputs.size()) * rowHeight;
    qreal bottomY = titleHeight + bodySize + roundDiameter / 2;
    _boundingRect = {0, 0, nodeWidth, bottomY};

    for (int i = 0; i < inputs.size(); ++i)
    {
        new NodePort{-5, titleHeight + rowHeight/2 - 5 + rowHeight * i, PortType::INPUT, this};
    }
    for (int i = 0; i < outputs.size(); ++i)
    {
        new NodePort{nodeWidth - 5, titleHeight + rowHeight/2 - 5 + rowHeight * (i + inputs.size()), PortType::OUTPUT, this};
    }

    setFlag(QGraphicsItem::ItemIsMovable, true);

}

void Node::paint
    ( QPainter *painter
    , const QStyleOptionGraphicsItem*
    , QWidget*
    )
{
    painter->setRenderHint(QPainter::Antialiasing);
    painter->setRenderHint(QPainter::HighQualityAntialiasing);

    qreal roundRadius = (roundDiameter / 2);

    QPainterPath titlePath;
    titlePath.moveTo(nodeWidth, roundRadius);
    titlePath.arcTo(nodeWidth - roundDiameter, 0, roundDiameter, roundDiameter, 0.0, 90.0);
    titlePath.lineTo(roundDiameter / 2, 0);
    titlePath.arcTo(0, 0, roundDiameter, roundDiameter, 90.0, 90.0);
    titlePath.lineTo(0, titleHeight);
    titlePath.lineTo(nodeWidth, titleHeight);
    titlePath.closeSubpath();

    QBrush titleBrush{QColor{"#b3ffff"}};
    painter->setBrush(titleBrush);
    QPen pen;
    pen.setWidth(2);
    painter->setPen(pen);
    painter->drawPath(titlePath);

    QFont font(fontName, static_cast<int>(titleHeight - 5));
    painter->setFont(font);
    painter->drawText
        ( 0
        , 0
        , static_cast<int>(nodeWidth)
        , static_cast<int>(titleHeight)
        , Qt::AlignCenter | Qt::AlignVCenter
        , name
        );

    // Body
    QPainterPath bodyPath;
    bodyPath.moveTo(_boundingRect.width(), titleHeight);
    bodyPath.lineTo(_boundingRect.width(), _boundingRect.bottom() - roundRadius);
    bodyPath.arcTo(_boundingRect.width() - roundDiameter, _boundingRect.bottom() - roundDiameter, roundDiameter, roundDiameter, 0.0, -90.0);
    bodyPath.lineTo(roundRadius, _boundingRect.bottom());
    bodyPath.arcTo(0, _boundingRect.bottom() - roundDiameter, roundDiameter, roundDiameter, -90.0, -90.0);
    bodyPath.lineTo(0, titleHeight);
    bodyPath.closeSubpath();

    QBrush bodyBrush(QColor("#666666"));
    painter->setBrush(bodyBrush);
    painter->drawPath(bodyPath);

    // Items
    QPen bodyPen(QColor("#fcfcfc"));
    painter->setPen(bodyPen);

    font.setPixelSize(static_cast<int>(textHeight));
    painter->setFont(font);

    qreal rowY = titleHeight + (rowHeight - iconSize) / 2;
    for (auto &input : inputs)
    {
        drawLine(painter, input, rowY);
        rowY += rowHeight;
    }

    for (auto &output : outputs)
    {
       drawLine(painter, output, rowY);
        rowY += rowHeight;
    }
}

QRectF Node::boundingRect() const
{
    return _boundingRect;
}

void Node::drawLine(QPainter *painter, NodeItem item, qreal y)
{
    painter->drawPixmap
        ( static_cast<int>(columnPadding)
        , static_cast<int>(y)
        , static_cast<int>(iconSize)
        , static_cast<int>(iconSize)
        , item.icon
        );
    painter->drawText
        ( static_cast<int>(columnPadding * 2 + iconSize)
        , static_cast<int>(y)
        , static_cast<int>(itemCountWidth)
        , static_cast<int>(iconSize)
        , Qt::AlignLeft | Qt::AlignVCenter
        , QString("%1").arg(item.amount)
        );
    painter->drawText
        ( static_cast<int>(columnPadding * 3 + iconSize + itemCountWidth)
        , static_cast<int>(y)
        , static_cast<int>(nodeWidth - columnPadding * 2 + iconSize + itemCountWidth)
        , static_cast<int>(iconSize)
        , Qt::AlignLeft | Qt::AlignVCenter
        , item.name
        );
}

QString Node::getName() const
{
    return name;
}
