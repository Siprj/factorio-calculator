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
        , QList<QPair<QPixmap, QString>> inputs
        , QList<QPair<QPixmap, QString>> outputs
        );

    void paint
        ( QPainter *painter
        , const QStyleOptionGraphicsItem *option
        , QWidget *widget
        );

    QRectF boundingRect() const;

    int getHeight() const;
    void setHeight(int value);

    int getWeight() const;
    void setWeight(int value);

    int getWidth() const;
    void setWidth(int value);

    QString getName() const;

private:
    QString name;
    QList<QPair<QPixmap, QString>> inputs;
    QList<QPair<QPixmap, QString>> outputs;
    qreal roundDiameter;
    int height;
    int width;
};

#endif // NODE_H
