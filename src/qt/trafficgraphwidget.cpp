// Copyright (c) 2011-2015 The Bitcoin Core developers
// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "trafficgraphwidget.h"
#include "clientmodel.h"
#include "utiltime.h"

#include <QPainter>
#include <QPainterPath>
#include <QColor>
#include <QTimer>

#include <cmath>

#define NUM_DISPLAYED_SAMPLES         800
#define BASE_SAMPLES_BUF_SIZE         24 * 60 * 60
#define BASE_SAMPLE_PERIOD_MS         1000

#define XMARGIN                 10
#define YMARGIN                 10

TrafficGraphWidget::TrafficGraphWidget(QWidget *parent) :
    QWidget(parent),
    fMax(0.0f),
    nMins(0),

    vTimespanSamplesIn(NUM_DISPLAYED_SAMPLES),
    vTimespanSamplesOut(NUM_DISPLAYED_SAMPLES),
    nTimespanLastBytesIn(0),
    nTimespanLastBytesOut(0),
    nTimespanLastSampleTimeMs(0),
    nTimespanHeadIndex(0),
    timespanTimer(0),
    vBaseSamplesIn(BASE_SAMPLES_BUF_SIZE),
    vBaseSamplesOut(BASE_SAMPLES_BUF_SIZE),
    nBaseLastBytesIn(0),
    nBaseLastBytesOut(0),
    nBaseLastSampleTimeMs(0),
    nBaseSamplesHeadIndex(0),
    baseTimer(0),

    clientModel(0)
{
    timespanTimer = new QTimer(this);
    connect(timespanTimer, SIGNAL(timeout()), SLOT(updateTimespanRates()));

    baseTimer = new QTimer(this);
    connect(baseTimer, SIGNAL(timeout()), SLOT(updateBaseRates()));
}

void TrafficGraphWidget::setClientModel(ClientModel *model)
{
    clientModel = model;
    if(model) {
        nTimespanLastBytesIn = model->getTotalBytesRecv();
        nTimespanLastBytesOut = model->getTotalBytesSent();
        nBaseLastBytesIn = nTimespanLastBytesIn;
        nBaseLastBytesOut = nTimespanLastBytesOut;

        baseTimer->start(BASE_SAMPLE_PERIOD_MS);
    }
}

int TrafficGraphWidget::getGraphRangeMins() const
{
    return nMins;
}

float TrafficGraphWidget::resampleSamples(const QVector<float> &source, QVector<float> &dest) const
{
    int numSourceSamples = nMins * 60;
    float stride = (float)numSourceSamples / dest.size();
    float currentStride = stride;
    float maxSample = 0.0f;
    float lastAverageRate = 0.0f;

    int sourceTailIndex = nBaseSamplesHeadIndex - numSourceSamples;
    if (sourceTailIndex < 0) {
        sourceTailIndex += BASE_SAMPLES_BUF_SIZE;
    }

    int currentSampleIndex = sourceTailIndex;
    for (int destIndex = 0; destIndex < dest.size(); destIndex++) {
        int numSamplesToRead = (int)currentStride;

        float aggregate = 0.0f;
        for (int i = 0; i < numSamplesToRead; ++i) {
            float currentSample = source[currentSampleIndex];

            aggregate += currentSample;

            currentSampleIndex++;
            if (currentSampleIndex >= source.size()) {
                currentSampleIndex = 0;
            }
        }

        float averageRate = 0.0f;
        if (numSamplesToRead == 0) {
            // We seem to be trying to upsample. just use the previous sample.
            averageRate = lastAverageRate;
        }
        else {
            averageRate = aggregate / numSamplesToRead;
        }

        maxSample = std::max(maxSample, averageRate);

        dest[destIndex] = averageRate;
        lastAverageRate = averageRate;

        currentStride -= numSamplesToRead;
        currentStride += stride;
    }

    return maxSample;
}

void TrafficGraphWidget::paintPath(QPainterPath &path, const QVector<float> &samples, int samplesHead)
{
    int h = height() - YMARGIN * 2;
    int w = width() - XMARGIN * 2;
    int lastY = 0;

    path.moveTo(XMARGIN, YMARGIN + h);

    int samplesIndex = samplesHead;
    for (int i = 0; i < samples.size(); ++i) {
        int x = w * i / NUM_DISPLAYED_SAMPLES;
        int y = h - (int)(h * samples[samplesIndex] / fMax);

        path.lineTo(XMARGIN + x, YMARGIN + y);

        samplesIndex++;
        if (samplesIndex >= samples.size()) {
            samplesIndex = 0;
        }

        lastY = y;
    }
    path.lineTo(XMARGIN + w, YMARGIN + lastY);
    path.lineTo(XMARGIN + w, YMARGIN + h);
}

void TrafficGraphWidget::paintEvent(QPaintEvent *)
{
    QPainter painter(this);
    painter.fillRect(rect(), Qt::black);

    if (fMax <= 0.0f) {
        return;
    }

    QColor axisCol(Qt::gray);
    int h = height() - YMARGIN * 2;
    painter.setPen(axisCol);
    painter.drawLine(XMARGIN, YMARGIN + h, width() - XMARGIN, YMARGIN + h);

    // decide what order of magnitude we are
    int base = floor(log10(fMax));
    float val = pow(10.0f, base);

    const QString units     = tr("KB/s");
    const float yMarginText = 2.0;

    // draw lines
    painter.setPen(axisCol);
    painter.drawText(XMARGIN, YMARGIN + h - h * val / fMax-yMarginText, QString("%1 %2").arg(val).arg(units));
    for(float y = val; y < fMax; y += val) {
        int yy = YMARGIN + h - h * y / fMax;
        painter.drawLine(XMARGIN, yy, width() - XMARGIN, yy);
    }

    // if we drew 3 or fewer lines, break them up at the next lower order of magnitude
    if(fMax / val <= 3.0f) {
        axisCol = axisCol.darker();
        val = pow(10.0f, base - 1);
        painter.setPen(axisCol);
        painter.drawText(XMARGIN, YMARGIN + h - h * val / fMax-yMarginText, QString("%1 %2").arg(val).arg(units));
        int count = 1;
        for(float y = val; y < fMax; y += val, count++) {
            // don't overwrite lines drawn above
            if(count % 10 == 0)
                continue;
            int yy = YMARGIN + h - h * y / fMax;
            painter.drawLine(XMARGIN, yy, width() - XMARGIN, yy);
        }
    }

    QPainterPath samplesPathIn;
    paintPath(samplesPathIn, vTimespanSamplesIn, nTimespanHeadIndex);
    painter.fillPath(samplesPathIn, QColor(0, 255, 0, 128));
    painter.setPen(Qt::green);
    painter.drawPath(samplesPathIn);

    QPainterPath samplesPathOut;
    paintPath(samplesPathOut, vTimespanSamplesOut, nTimespanHeadIndex);
    painter.fillPath(samplesPathOut, QColor(255, 0, 0, 128));
    painter.setPen(Qt::red);
    painter.drawPath(samplesPathOut);
}

void TrafficGraphWidget::updateTimespanRates()
{
    if (!clientModel) {
        return;
    }

    int64_t currentTimeMs = GetTimeMillis();
    int64_t elapsedMs = currentTimeMs - nTimespanLastSampleTimeMs;
    if (elapsedMs <= 0) {
        return;
    }

    quint64 bytesIn = clientModel->getTotalBytesRecv();
    quint64 bytesOut = clientModel->getTotalBytesSent();

    float inRate = (bytesIn - nTimespanLastBytesIn) / 1000.0f * (1000.0f / elapsedMs);
    float outRate = (bytesOut - nTimespanLastBytesOut) / 1000.0f * (1000.0f / elapsedMs);

    nTimespanLastBytesIn = bytesIn;
    nTimespanLastBytesOut = bytesOut;
    nTimespanLastSampleTimeMs = currentTimeMs;

    vTimespanSamplesIn[nTimespanHeadIndex] = inRate;
    vTimespanSamplesOut[nTimespanHeadIndex] = outRate;

    nTimespanHeadIndex++;
    if (nTimespanHeadIndex >= NUM_DISPLAYED_SAMPLES) {
        nTimespanHeadIndex = 0;
    }

    fMax = 0.0f;
    const int nInSampleSize = std::min(vTimespanSamplesIn.size(), vTimespanSamplesOut.size());
    for (int i = 0; i < nInSampleSize; i++)
    {
        fMax = std::max(std::max(fMax, vTimespanSamplesIn[i]), vTimespanSamplesOut[i]);
    }

    update();
}

void TrafficGraphWidget::updateBaseRates()
{
    if (!clientModel) {
        return;
    }

    int64_t currentTimeMs = GetTimeMillis();
    int64_t elapsedMs = currentTimeMs - nBaseLastSampleTimeMs;
    if (elapsedMs <= 0) {
        return;
    }

    quint64 bytesIn = clientModel->getTotalBytesRecv();
    quint64 bytesOut = clientModel->getTotalBytesSent();

    float inRate = (bytesIn - nBaseLastBytesIn) / 1000.0f * 1000.0f / elapsedMs;
    float outRate = (bytesOut - nBaseLastBytesOut) / 1000.0f * 1000.0f / elapsedMs;

    nBaseLastBytesIn = bytesIn;
    nBaseLastBytesOut = bytesOut;
    nBaseLastSampleTimeMs = currentTimeMs;

    vBaseSamplesIn[nBaseSamplesHeadIndex] = inRate;
    vBaseSamplesOut[nBaseSamplesHeadIndex] = outRate;

    nBaseSamplesHeadIndex++;
    if (nBaseSamplesHeadIndex >= BASE_SAMPLES_BUF_SIZE) {
        nBaseSamplesHeadIndex = 0;
    }
}

void TrafficGraphWidget::setGraphRangeMins(int mins)
{
    nMins = mins;
    nTimespanHeadIndex = 0;

    float maxIn = resampleSamples(vBaseSamplesIn, vTimespanSamplesIn);
    float maxOut = resampleSamples(vBaseSamplesOut, vTimespanSamplesOut);
    fMax = std::max(maxIn, maxOut);

    timespanTimer->start((mins * 60 * 1000) / NUM_DISPLAYED_SAMPLES);
}

void TrafficGraphWidget::clear()
{
    vTimespanSamplesIn.fill(0.0f);
    vTimespanSamplesOut.fill(0.0f);
    vBaseSamplesIn.fill(0.0f);
    vBaseSamplesOut.fill(0.0f);
    fMax = 0.0f;

    if (clientModel) {
        nTimespanLastBytesIn = clientModel->getTotalBytesRecv();
        nTimespanLastBytesOut = clientModel->getTotalBytesSent();
        nBaseLastBytesIn = nTimespanLastBytesIn;
        nBaseLastBytesOut = nTimespanLastBytesOut;
    }
}
