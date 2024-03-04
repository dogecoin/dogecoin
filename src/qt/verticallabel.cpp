// Copyright (c) 2014 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "verticallabel.h"

#include <QPainter>

VerticalLabel::VerticalLabel(QWidget* parent)
    : QLabel(parent)
{
}

VerticalLabel::VerticalLabel(const QString& text, QWidget* parent)
    : QLabel(text, parent)
{
}

VerticalLabel::~VerticalLabel()
{
}

void VerticalLabel::paintEvent(QPaintEvent*)
{
    QPainter painter(this);
    painter.setPen(Qt::black);
    painter.setBrush(Qt::Dense1Pattern);
    painter.translate(width() / 2, height());
    painter.rotate(270);

    painter.drawText(0, 0, text());
}

QSize VerticalLabel::minimumSizeHint() const
{
    QSize s = QLabel::minimumSizeHint();
    return QSize(s.height(), s.width());
}

QSize VerticalLabel::sizeHint() const
{
    QSize s = QLabel::sizeHint();
    return QSize(s.height(), s.width());
}
