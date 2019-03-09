#ifndef RECIPE_H
#define RECIPE_H

#include <QString>
#include <QList>
#include <QJsonObject>
#include <QJsonArray>

#include <optional>


struct Ingredient
{
    QString name;
    int amount;

    static Ingredient fromJsonObject(QJsonObject);
};

struct Shift
{
    int x;
    int y;
    static Shift fromJsonObject(QJsonObject obj);
};

struct Icon
{
    QString filePath;
    double scale;
    std::optional<Shift> shift;

    static Icon fromJsonObject(QJsonObject obj);
};

struct Result
{
    QString name;
    int amount;
    float probablity;

    static Result fromJsonObject(QJsonObject obj);
};

struct Recipe
{
    QString name;
    QList<Ingredient> ingredients;
    QList<Result> results;
    QList<Icon> icons;

    static Recipe fromJsonObject(QJsonObject);
};

using Recipes = QList<Recipe>;

Recipes fromJsonObject(QJsonValue obj);

#endif // RECIPE_H
