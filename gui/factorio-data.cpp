#include "factorio-data.h"

#include <QJsonObject>
#include <QJsonArray>
#include <QDebug>


Recipe Recipe::fromJsonObject(QJsonObject obj)
{
    QJsonArray jsonIngredients = obj.value("ingredients").toArray();
    QList<Ingredient> ingredients;
    for (auto val: jsonIngredients) {
        ingredients.append(Ingredient::fromJsonObject(val.toObject()));
    }

    QJsonArray jsonResults = obj.value("products").toArray();
    QList<Product> products;
    for (auto val: jsonResults) {
        products.append(Product::fromJsonObject(val.toObject()));
    }

    QJsonArray jsonIcon = obj.value("icon").toArray();
    QList<Icon> icons;
    for (auto val: jsonIcon) {
        icons.append(Icon::fromJsonObject(val.toObject()));
    }


    return {obj.value("name").toString(), ingredients, products, icons};
}

Ingredient Ingredient::fromJsonObject(QJsonObject obj)
{
    return
        { obj.value("name").toString()
        , obj.value("amount").toInt()
        };
}

Icon Icon::fromJsonObject(QJsonObject obj)
{
    std::optional<Shift> shift;
    if (obj.contains("shift") && obj.value("shift").isObject())
    {
        shift = std::make_optional(Shift::fromJsonObject(obj.value("shift").toObject()));
    }
    double scale = 1.0;
    if (obj.contains("scale") && obj.value("scale").isDouble())
        scale = obj.value("scale").toDouble();
    return { obj.value("icon").toString(), scale, shift};
}

Product Product::fromJsonObject(QJsonObject obj)
{
    return
        { obj.value("name").toString()
        , obj.value("amount").toInt()
        , static_cast<float>(obj.value("probablity").toDouble())
        };
}


Shift Shift::fromJsonObject(QJsonObject obj)
{

    return
        { obj.value("x").toInt()
        , obj.value("y").toInt()
        };
}

Item Item::fromJsonObject(QJsonObject obj)
{
    QJsonArray jsonIcon = obj.value("icon").toArray();
    QList<Icon> icons;
    for (auto val: jsonIcon) {
        icons.append(Icon::fromJsonObject(val.toObject()));
    }

    return
        { obj.value("name").toString()
        , icons
        };
}

FactorioData fromJsonObject(QJsonObject obj)
{

    QJsonValue recipesArray = obj.value("recipes");
    QList<Recipe> recipes;
    for (auto val: recipesArray.toArray()) {
        recipes.append(Recipe::fromJsonObject(val.toObject()));
    }
    QJsonValue itemsArray = obj.value("items");
    QList<Item> items;
    for (auto val: recipesArray.toArray()) {
        items.append(Item::fromJsonObject(val.toObject()));
    }
    return { recipes, items };
}
