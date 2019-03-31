#include "icon.h"

#include <math.h>
#include <QPainter>


QPixmap icon::composeIcon(QList<Icon> iconParts)
{
    QPixmap iconPixmap(32, 32);
    iconPixmap.fill(QColor(0,0,0, 0));
    QPainter paint(&iconPixmap);
    for (auto &iconPart : iconParts)
    {
        QPixmap pixmapPart(iconPart.filePath);
        QPixmap scaledIconPart(pixmapPart.scaled
            ( static_cast<int>(std::round(pixmapPart.width() * iconPart.scale))
            , static_cast<int>(std::round(pixmapPart.height() * iconPart.scale))
            ));

        int x = 0;
        int y = 0;

        if (iconPart.shift.has_value())
        {
            // Move shift coordinate system to center and scale it.
            x = static_cast<int>(((32/2) * iconPart.scale) + (static_cast<double>(iconPart.shift.value().x)));
            y = static_cast<int>(((32/2) * iconPart.scale) + (static_cast<double>(iconPart.shift.value().y)));
        }

        paint.drawPixmap(x, y, scaledIconPart.width(), scaledIconPart.height(), scaledIconPart);
    }

    return iconPixmap;
}
