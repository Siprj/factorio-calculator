#ifndef ICON_H
#define ICON_H

#include <QPixmap>
#include <QList>

#include "factorio-data.h"


namespace icon {
    QPixmap composeIcon(QList<Icon> iconParts);
};

#endif // ICON_H
