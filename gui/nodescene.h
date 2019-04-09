#ifndef NODESCENE_H
#define NODESCENE_H

#include <QObject>
#include <QGraphicsScene>

#include "factorio-data.h"


class NodeScene : public QGraphicsScene
{
public:
    NodeScene(QObject *parent = nullptr);
    NodeScene(FactorioData *factorioData, QObject *parent = nullptr);

protected:
    void dropEvent(QGraphicsSceneDragDropEvent *event);
    void mousePressEvent(QGraphicsSceneMouseEvent *event);
    void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
    void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);
    void dragEnterEvent(QGraphicsSceneDragDropEvent *event);
    void dragMoveEvent(QGraphicsSceneDragDropEvent *event);
    void dragLeaveEvent(QGraphicsSceneDragDropEvent *event);

    FactorioData *factorioData;
};

#endif // NODESCENE_H
