unit uInfoUtils;
{  A Set of utilitoes to gather information to display.

   The daylisght saving stuff is based on this -
   http://www.delphiforfun.org/Programs/delphi_techniques/TimeZoneInfo.htm
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, windows, dateutils, dialogs, math;

function getDaylightSaving(year: integer): TStringList;
function GetNthDSTDOW(Y,M,DST_DOW,N:word):integer;
function getEasterDates(Year: integer): TStringList;
function getLentDates(year: integer): TStringList;
function getPower: TStringList;
function getMoonPhase: TStringList;
function getEasterSunday(year: integer): TdateTime;
function julianTime: double;

implementation

CONST
  LAST_NEW_MOON = 2451549.516678;
  SIDEREAL_PERIOD = 29.53059;

function getDaylightSaving(Year: integer): TStringList;
{  Returns daylight saving stuff.    }
var
  timezoneinfo: TTimezoneinformation;
  strResults: TStringList;
  t:TDatetime;
  d2:word;
  r:word;
begin
  strResults := TStringList.Create;
  r := GetTimezoneInformation(timezoneinfo);

  if r > 0 then   //  if r = 0, then error
  begin
    with timezoneinfo do
    begin
      DaylightDate.Year := year;

      case r of
        time_zone_Id_unknown:
        begin
          strResults.add('Current daylight status is unknown');
          strResults.Add('');
        end;
        time_zone_id_standard:
        begin
          strResults.add('We are not currently in the daylight savings time period');
          strResults.Add(StandardName);
          strResults.Add('');
        end;
        time_zone_id_daylight:
        begin
          strResults.add('We are currently in the daylight savings time period');
          strResults.Add(Daylightname);
          strResults.Add('');
        end;
      end;

      strResults.add(format('Daylight Saving for %d', [year]));
      strResults.Add('');

      with daylightdate do
      begin
        if (Daylightname = '') then
        begin
          strResults.Add('No Daylight Saving Time information available');
          strResults.Add('');
        end
        else
        begin
          d2 := getNthDSTDOW(year, wmonth, wDayOfWeek, wDay);
          t := encodedate(year, wmonth, d2) + encodetime(whour, wminute, wsecond, wmilliseconds);
          strResults.add(formatdatetime('"Daylight saving starts: " mmmm dd  hh:nn am/pm', t));
          strResults.Add('');
        end;   //  if (Daylightname = '')
      end;     //  with daylightdate

      standarddate.Year := year;
      with standarddate do
      begin
        d2 := getNthDSTDOW(year, wmonth, wDayOfWeek, wday);
        t := encodedate(year, wmonth, d2) + encodetime(whour, wminute, wsecond, wmilliseconds);
        strResults.add(formatdatetime('"Daylight saving ends: " mmmm dd  hh:nn am/pm', t));
        strResults.Add('');
      end;     //  with standarddate
    end;       //  with timezoneinfo
  end          //  if r >
  else
    strResults.add('Time zone information not available');

  result := strResults;
end;

function GetNthDSTDOW(Y,M,DST_DOW,N:word):integer;
{For Year "Y" and Month "M"  and DayOfWeek "DST_DOW", return the day of month for "DST_DOW" number "N"}
{If  N  is larger than the number of DST_DOW's in the month, return the day of the last one}
{If Y, M, or N are otherwise invalid, return 0}
var
  dt:TDateTime;
  NdayDom, maxdays:integer;
begin
  if TryEncodeDate(y,m,1,dt) and (n>0) then {get date of first of month}
  begin
    if n>5 then n:=5;
    NdayDOM:=8+DST_DOW-DayOfTheWeek(dt);  {1st DST_DOW Day of Month}
    result:=NdayDOM+7*(n-1);
    maxdays:=daysinMonth(dt);
    If result>maxdays  then
    repeat dec(Result,7) until Result<=Maxdays;
  end
  else result:=0;
end;

function getEasterDates(year: integer): TStringList;
{  Returns the Easter dates for a given year.    }
var
  strResults: TStringList;
  easter: TdateTime;
begin
  strResults := TStringList.Create;

  easter := getEasterSunday(year);

  strResults.add('');
  strResults.add('');
  strResults.add(format('Easter Dates for %d', [year]));
  strResults.add('');
  strResults.add(FormatDateTime('"Good Friday   :: "DD MMM YYYY', incDay(easter, -2)));
  strResults.add(FormatDateTime('"Easter Sunday :: "DD MMM YYYY', easter));
  strResults.add(FormatDateTime('"Easter Monday :: "DD MMM YYYY', incDay(easter)));

  result := strResults;
end;

function getLentDates(year: integer): TStringList;
{  Returns the Lent dates for a given year.
   lent start on Ash Wednesday, which is 46 days before Easter Sunday.
}
var
  strResults: TStringList;
  easter: TdateTime;
begin
  strResults := TStringList.Create;

  easter := getEasterSunday(year);

  strResults.add('');
  strResults.add('');
  strResults.add(format('Lent Dates for %d', [year]));
  strResults.add('');
  strResults.add(FormatDateTime('"Lent Starts [Ash Wednesday] :: "DD MMM YYYY', incDay(easter, -46)));
  strResults.add(FormatDateTime('"Lent Ends   [Easter Sunday] :: "DD MMM YYYY', easter));

  result := strResults;
end;

function getPower: TStringList;
{  see https://msdn.microsoft.com/en-us/library/windows/desktop/aa373232(v=vs.85).aspx  }
var
  PowerStatus: TSystemPowerStatus;
begin
  result := TStringList.Create;   //  This solves the promle of not being able to
                                  //  free the StringList and stop the memory leak.
                                  //  Also stope the double creation of the stringList
                                  //  which casues a sig fault.

  if GetSystemPowerStatus(PowerStatus) then
  begin
    if powerStatus.ACLineStatus = 1 then
      result.add('Mains power online')
    else
      result.add('No mains Power');

    result.add('');

    case powerStatus.BatteryFlag of
      1: result.add('High — the battery capacity is at more than 66 percent');
      2: result.add('Low — the battery capacity is at less than 33 percent');
      4: result.add('Critical — the battery capacity is at less than five percent');
      8: result.add('Charging');
      128: result.add('No system battery');
      255: result.add('Unknown status — unable to read the battery flag information');
    end;

    if powerStatus.ACLineStatus = 0 then
    begin
      if powerStatus.BatteryLifePercent <> 0 then
        result.add(format('Battery Life %d %%', [powerStatus.BatteryLifePercent]));
      if powerStatus.BatteryFullLifeTime <> 0 then
        result.add(format('Battery Life %d secs', [powerStatus.BatteryFullLifeTime]));
      if powerStatus.BatteryLifeTime <> 0 then
        result.add(format('Battery Full Life %d secs', [powerStatus.BatteryFullLifeTime]));
    end;

  end
  else
  begin
    result.add('');
    result.add('');
    result.add('');
    result.add('');
    result.add('Unable to get system power details');
  end;
end;

function getEasterSunday(year: integer): TdateTime;
{  Easter function after Carl Friedrich Gauss (1800). Return value
   is the date of Easter Sunday in the given year
   Scope: 1583 - 8702, this is set min and max on the spin control.
}
var
  a :  integer ;
  b :  integer ;
  c :  integer ;
  d :  integer ;
  e :  integer ;
  f :  integer ;
  days :  word ;
  month :  word ;
begin
  // The Gauss formula
  a := year mod  19 ;
  b := year div  100 ;
  c := (8 * b + 13) div 25 - 2 ;
  d := b - (year div 400) - 2 ;
  e := (19 * a + ((15 - c + d) mod 30)) mod 30;

  if e = 28 then
    if a> 10 then
      e := 27
    else if e = 29 then
      e := 28 ;

  f := (d + 6 * e + 2 * (year mod 4) + 4 * (year mod 7) + 6) mod 7;

  if (e + f + 22) > 31  then
  begin
    month := 4;
    days := ((e + f + 22) - 31);
  end
  else
  begin
    month := 3;
    days := (e + f + 22);
  end ;

  result :=  EncodeDate (year, month, days) ;
end;

function getMoonPhase: TStringList;
{  Determins the phase of the moon and the illuminayion.
   This is acheived bu findinf the days differance between now and a known
   full moon, both dates are express as jullian dates.
   This differance is then divided by the Sidereal Period of the moon [this is
   mean values between succseve full moons].  This gives the number of days
   into the current moon cycle.
   This is also used to calculate the amounbt of illumination from the moon.

   Known full moon date used - 6 january 200 @ 12:24:01 [2451549.516678 Julian].

   NB : accurate to plus or minus one day of moon phase and only after year 2000
}
VAR
  julianToday: double;
  julianSinceNew: double;
  noOfNewMoons: double;
  moonPhase: integer;
  moonPhaseDesc: string;
  moonIllumination: double;
begin
  result := TStringList.Create;   //  This solves the promle of not being able to
                                  //  free the StringList and stop the memory leak.
                                  //  Also stope the double creation of the stringList
                                  //  which casues a sig fault.

  julianToday := julianTime;
  julianSinceNew := julianToday - LAST_NEW_MOON;
  noOfNewMoons := julianSinceNew / SIDEREAL_PERIOD;
  moonPhase := floor(frac(noOfNewMoons) * SIDEREAL_PERIOD);

  case moonPhase of
    0, 29: moonPhaseDesc := 'New Moon';
    1, 2, 3, 4, 5, 6: moonPhaseDesc := 'Waxing Crescent';
    7: moonPhaseDesc := 'First Quarter';
    8, 9, 10, 11, 12, 13, 14: moonPhaseDesc := 'Waxing Gibbous';
    15: moonPhaseDesc := 'Full Moon';
    16, 17, 18, 19, 20: moonPhaseDesc := 'Waning Gibbous';
    21: moonPhaseDesc := 'Last Quarter';
    22, 23, 24, 25, 26, 27, 28: moonPhaseDesc := 'Waning Crescent';
  end;

  moonIllumination := 0.5 * (1 + COS(moonPhase / SIDEREAL_PERIOD * 360));

  result.add(format('Phase of moon a %s', [moonPhaseDesc]));
  result.add(format('Illumination of the moon = %2.3F%%', [moonIllumination * 100]));
end;

function julianTime: double;
{  returns Julian Date Time - will only work in windows.
   Formulae pinched from http://en.wikipedia.org/wiki/Julian_day               }
var
  day,month,year: word;
  a,y,m: longint;
begin
  DecodeDate ( Now, year, month, day );
  a := (14-month) div 12;
  y := year + 4800 - a;
  m := month + (12*a) - 3;
  result := day + ((153*m+2) div 5) + (365*y) + (y div 4) - (y div 100) + (y div 400) - 32045.5 + frac(Now);
end;

end.


