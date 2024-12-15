// Copyright (c) 2011-2015 The Bitcoin Core developers
// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_QT_TRAFFICGRAPHWIDGET_H
#define BITCOIN_QT_TRAFFICGRAPHWIDGET_H

#include <QWidget>
#include <QVector>

class ClientModel;

QT_BEGIN_NAMESPACE
class QPaintEvent;
class QTimer;
QT_END_NAMESPACE

class TrafficGraphWidget : public QWidget
{
    Q_OBJECT

public:
    explicit TrafficGraphWidget(QWidget *parent = 0);
    void setClientModel(ClientModel *model);
    int getGraphRangeMins() const;

protected:
    void paintEvent(QPaintEvent *);

public Q_SLOTS:
    void updateTimespanRates();
    void updateBaseRates();
    void setGraphRangeMins(int mins);
    void clear();

private:
    void paintPath(QPainterPath &path, const QVector<float> &samples, int samplesHead);
    float resampleSamples(const QVector<float> &source, QVector<float> &dest) const;

    float fMax;
    int nMins;

    /*
    *These QVector<float>s are implemented as circular buffers so that we always have a maximum of 
    *BASE_SAMPLES_BUF_SIZE (defined in trafficgraphwidget.cpp, default of 24 hours * 60 minutes * 60 seconds, 
    *or 86400 samples per day at a samplerate of 1sps) to work with. A second circular "display buffer" is 
    *implemented with a maximum of NUM_DISPLAYED_SAMPLES (defined in trafficgraphwidget.cpp, default of 800) 
    *which are the 'visual samples' which can represent different amounts of time according to the slider. 
    *
    *The "base samples" for a given timespan are resampled into the "displayed samples" (in this case 800) to 
    *redraw and show a history of the selected timespan. When these variables are incremented, we check limits 
    *to see if we need to wrap around and begin overwriting the oldest values in the circular buffers.
    */
    
    QVector<float> vTimespanSamplesIn;
    QVector<float> vTimespanSamplesOut;
    quint64 nTimespanLastBytesIn;
    quint64 nTimespanLastBytesOut;
    int64_t nTimespanLastSampleTimeMs;
    int nTimespanHeadIndex;
    QTimer *timespanTimer;

    QVector<float> vBaseSamplesIn;
    QVector<float> vBaseSamplesOut;
    quint64 nBaseLastBytesIn;
    quint64 nBaseLastBytesOut;
    int64_t nBaseLastSampleTimeMs;
    int nBaseSamplesHeadIndex;
    QTimer *baseTimer;

    ClientModel *clientModel;
};

#endif // BITCOIN_QT_TRAFFICGRAPHWIDGET_H
