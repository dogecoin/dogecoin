// Copyright (c) 2021 The Dogecoin Core developers
// Portiosn Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.
// Inspired from Original althorithm posted to voidware.com/moon_phase.htm. 
// Details Original Althrithim below can be found on https://www.subsystems.us/free-resources.html
// PDF: https://www.subsystems.us/uploads/9/8/9/4/98948044/moonphase.pdf (Thanks @delbonis)
// PDF Mirror: https://geekwisdom.org/moonphase.pdf
#include <dogequirks.h>
#include<stdio.h>
int CDogeQuirks::moon_phase(int64_t nTime)
{
    struct tm ts;
    time_t time_val = nTime;
    if (gmtime_r(&time_val, &ts) == nullptr) {
        return -1; //timesync problem
    }
return moon_phase(ts.tm_year+1900,ts.tm_mon+1,ts.tm_mday);
}

int CDogeQuirks::moon_phase(int year, int month, int day)
{
    /*
      calculates the moon phase. Returns a number between 0 and 7.
      where:
      0 =  new moon.
      4 = full moon.

      Approximite cycle of each segment about 3 days. 0 and 8 are the same.
      */

    const double DAYS_IN_YEAR = 365.25;
    const double MOON_PERIOD=  29.53;
    const double AVG_DAYS_IN_MONTH=30.6;

    int num_days_year_part;
    int num_days_month_part;
    double total_days_elapsed;
    double total_cycles_elapsed;
    int total_cycles_elapsed_floor;
    double total_cycles_remainder;
    int final_cycle;

    if (month < 3) {
        year--;
        month += 12;
    }
    month++;
    num_days_year_part = year * DAYS_IN_YEAR;
    num_days_month_part = month * AVG_DAYS_IN_MONTH;

    total_days_elapsed = num_days_year_part +num_days_month_part +day-694039.09;  /* subtract off at Fri Jan 09 1970 00:47:19 GMT+0000 */
    total_cycles_elapsed = total_days_elapsed / MOON_PERIOD; //ie:   (29.53 days) */
    total_cycles_elapsed_floor = total_cycles_elapsed; /* cast to int to make floor */
    total_cycles_remainder = total_cycles_elapsed - total_cycles_elapsed_floor;
    final_cycle = total_cycles_remainder *8 + 0.5; /* count up to nearest int */
    final_cycle = final_cycle & 7;		   /* 0 and 8 are the same so turn 8 into 0 */
    return final_cycle;
}


