#include <QList>
#include <QDateTime>
#include <QString>

class RecurrentOptions
{

public:
	RecurrentOptions(QString id, QDateTime dateTime, int status);

	QString getId();
	QDateTime getDateTime();
	int getStatus();

private:
	QString id;
	QDateTime payTime;
	int status;
};