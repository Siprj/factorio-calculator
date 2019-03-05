#include "recipe.h"

#include <QJsonObject>
#include <QJsonArray>

Recipe Recipe::fromJsonObject(QJsonObject obj)
{
    QJsonArray jsonIngredients = obj.value("ingredients").toArray();
    QList<Ingredient> ingredients;
    for (auto val: jsonIngredients) {
        ingredients.append(Ingredient::fromJsonObject(val.toObject()));
    }

    QJsonArray jsonResults = obj.value("results").toArray();
    QList<Result> results;
    for (auto val: jsonResults) {
        results.append(Result::fromJsonObject(val.toObject()));
    }

    QJsonArray jsonIcon = obj.value("icon").toArray();
    QList<Icon> icons;
    for (auto val: jsonIcon) {
        icons.append(Icon::fromJsonObject(val.toObject()));
    }


    return {obj.value("name").toString(), ingredients, results, icons};
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
    return { obj.value("icon").toString() };
}

Result Result::fromJsonObject(QJsonObject obj)
{
    return
        { obj.value("name").toString()
        , obj.value("amount").toInt()
        , static_cast<float>(obj.value("probablity").toDouble())
        };
}

Recipes fromJsonObject(QJsonValue obj)
{
    QList<Recipe> recipes;
    for (auto val: obj.toArray()) {
        recipes.append(Recipe::fromJsonObject(val.toObject()));
    }
    return { recipes };
}
