#include "nodescene.h"

#include <QGraphicsSceneDragDropEvent>
#include <QMimeData>
#include <QDebug>

#include "mime-data.h"
#include "node.h"
#include "nodeitem.h"
#include "icon.h"


NodeScene::NodeScene(QObject *parent) : QGraphicsScene(parent)
{
}

NodeScene::NodeScene(FactorioData *factorioData, QObject *parent) : QGraphicsScene(parent)
{
    this->factorioData = factorioData;
}

void NodeScene::dropEvent(QGraphicsSceneDragDropEvent *event)
{
    // TODO: User `formats` for retrieving types of data attached to this drop.
    QByteArray data = event->mimeData()->data(recipeMimeType);
    if (data.size() != 0)
    {
        event->accept();

        QDataStream stream(&data, QIODevice::ReadOnly);
        QString name;
        stream >> name;

        Recipe recipe;
        recipe = factorioData->recipes.value(name);
        QList<NodeItem> inputs;
        for (auto &v : recipe.ingredients)
        {
            Item item = factorioData->items[v.name];
            inputs.append({v.name, icon::composeIcon(item.icons), static_cast<qreal>(v.amount)});
        }
        QList<NodeItem> outputs;
        for (auto &v : recipe.products)
        {
            Item item = factorioData->items[v.name];
            outputs.append({v.name, icon::composeIcon(item.icons), static_cast<qreal>(v.amount)});
        }
        Node *node = new Node(name, inputs, outputs);
        node->setPos(event->scenePos());
        addItem(node);
    }

    QGraphicsScene::dropEvent(event);
}

void NodeScene::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    QGraphicsScene::mousePressEvent(event);
}

void NodeScene::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
    QGraphicsScene::mouseMoveEvent(event);
}

void NodeScene::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
    event->accept();
    QGraphicsScene::mouseReleaseEvent(event);
}

void NodeScene::dragEnterEvent(QGraphicsSceneDragDropEvent *event)
{
    if (event->mimeData()->formats().contains(recipeMimeType))
    {
        event->accept();
    }
    else
    {
        QGraphicsScene::dragEnterEvent(event);
    }
}

void NodeScene::dragMoveEvent(QGraphicsSceneDragDropEvent *event)
{
    if (event->mimeData()->formats().contains(recipeMimeType))
    {
        event->accept();
    }
    else
    {
        QGraphicsScene::dragMoveEvent(event);
    }
}

void NodeScene::dragLeaveEvent(QGraphicsSceneDragDropEvent *event)
{
    if (event->mimeData()->formats().contains(recipeMimeType))
    {
        event->accept();
    }
    else
    {
        QGraphicsScene::dragLeaveEvent(event);
    }
}
