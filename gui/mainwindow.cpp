#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <QListView>
#include <QFile>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QDebug>

#include "path.h"
#include "recipe.h"


MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow),
    model(this)
{
    ui->setupUi(this);

    QString fileName(dirPath);
    fileName.append("data.json");
    QFile file(fileName);
    file.open(QIODevice::ReadOnly);

    QString val = file.readAll();
    file.close();
    QJsonDocument d = QJsonDocument::fromJson(val.toUtf8());
    QJsonArray array = d.array();

    ui->listView->setViewMode(QListView::IconMode);
    ui->listView->setIconSize(QSize(32,32));
    ui->listView->setGridSize(QSize(32,32));
    ui->listView->setSpacing(0);
    ui->listView->setResizeMode(QListView::Adjust);

    recipes = fromJsonObject(array);
    ui->listView->setModel(&model);
    for (auto v : recipes)
    {
        for (int i = 0; i < v.icons.size(); i++)
        {
            QString iconFileName(dirPath);
            iconFileName.append(v.icons.at(i).filePath);
            qDebug() << iconFileName;
            QPixmap image = QPixmap(iconFileName).scaled(32, 32, Qt::IgnoreAspectRatio, Qt::SmoothTransformation);
            model.addPiece(image, v.name.append("-icon-%1").arg(i));
        }
    }

    connect(ui->pushButton, &QPushButton::clicked, this, &MainWindow::clearModel);
}



MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::clearModel()
{
    model.clearModel();
}
