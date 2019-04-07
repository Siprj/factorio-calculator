#ifndef NODEPORT_H
#define NODEPORT_H

#include <iostream>
#include <QGraphicsItem>


enum class PortType
{
    INPUT,
    OUTPUT
};

class NodePort : public QGraphicsItem
{
public:

  NodePort(qreal x, qreal y, PortType portType, QGraphicsItem *parent = nullptr);

  void paint
      ( QPainter *painter
      , const QStyleOptionGraphicsItem *option
      , QWidget *widget
      );

  QRectF boundingRect() const;

private:
  PortType portType;
  const static int size = 10;

};

#endif // NODEPORT_H
