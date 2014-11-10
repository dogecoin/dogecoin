#include "recurrentoptions.h"

RecurrentOptions::RecurrentOptions(QString id, QDateTime dateTime, int status)
{
	this->id = id;
	this->payTime = dateTime;
	this->status = status;
}

QString RecurrentOptions::getId()
{
	return this->id;
}

QDateTime RecurrentOptions::getDateTime()
{
	return this->payTime;
}
int RecurrentOptions::getStatus()
{
	return this->status;
}