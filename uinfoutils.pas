unit uInfoUtils;
{  A Set of utilitoes to gather information to display.

   The daylisght saving stuff is based on this -
   http://www.delphiforfun.org/Programs/delphi_techniques/TimeZoneInfo.htm
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, windows, dateutils, dialogs;

function getDaylightSaving(year: integer): TStringList;
function GetNthDSTDOW(Y,M,DST_DOW,N:word):integer;
function getEasterDates(Year: integer): TStringList;
function getLentDates(year: integer): TStringList;
function getPower: TStringList;
function getEasterSunday(year: integer): TdateTime;

implementation

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
        end;
        time_zone_id_daylight:
        begin
          strResults.add('We are currently in the daylight savings time period');
          strResults.Add(Daylightname);
        end;
      end;

      strResults.add(format('Daylight Saving for %d', [year]));

      with daylightdate do
      begin
        if (Daylightname = '') then
        begin
          strResults.Add('No Daylight Saving Time information available');
        end
        else
        begin
          d2 := getNthDSTDOW(year, wmonth, wDayOfWeek, wDay);
          t := encodedate(year, wmonth, d2) + encodetime(whour, wminute, wsecond, wmilliseconds);
          strResults.add(formatdatetime('"Daylight saving starts: " mmmm dd  hh:nn am/pm', t));
        end;   //  if (Daylightname = '')
      end;     //  with daylightdate

      standarddate.Year := year;
      with standarddate do
      begin
        d2 := getNthDSTDOW(year, wmonth, wDayOfWeek, wday);
        t := encodedate(year, wmonth, d2) + encodetime(whour, wminute, wsecond, wmilliseconds);
        strResults.add(formatdatetime('"Daylight saving ends: " mmmm dd  hh:nn am/pm', t));
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
  strResults.add(format('Easter Dates for %d', [year]));
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
  strResults.add(format('Easter Dates for %d', [year]));
  strResults.add(FormatDateTime('"Lent Starts [Ash Wednesday] :: "DD MMM YYYY', incDay(easter, -46)));
  strResults.add(FormatDateTime('"Lent Ends   [Easter Sunday] :: "DD MMM YYYY', easter));

  result := strResults;
end;

function getPower: TStringList;
{  see https://msdn.microsoft.com/en-us/library/windows/desktop/aa373232(v=vs.85).aspx  }
var
  strResults: TStringList;
  PowerStatus:TSystemPowerStatus;
begin
  strResults := TStringList.Create;

  if GetSystemPowerStatus(PowerStatus) then
  begin
    if powerStatus.ACLineStatus = 1 then
      strResults.add('Mains power online')
    else
      strResults.add('No mains Power');

    case powerStatus.BatteryFlag of
      1: strResults.add('High — the battery capacity is at more than 66 percent');
      2: strResults.add('Low — the battery capacity is at less than 33 percent');
      4: strResults.add('Critical — the battery capacity is at less than five percent');
      8: strResults.add('Charging');
      128: strResults.add('No system battery');
      255: strResults.add('Unknown status — unable to read the battery flag information');
    end;

    if powerStatus.ACLineStatus = 0 then
    begin
      if powerStatus.BatteryLifePercent <> 0 then
        strResults.add(format('Battery Life %d %%', [powerStatus.BatteryLifePercent]));
      if powerStatus.BatteryFullLifeTime <> 0 then
        strResults.add(format('Battery Life %d secs', [powerStatus.BatteryFullLifeTime]));
      if powerStatus.BatteryLifeTime <> 0 then
        strResults.add(format('Battery Full Life %d secs', [powerStatus.BatteryFullLifeTime]));
    end;

  end
  else
  begin
    strResults.add('');
    strResults.add('');
    strResults.add('');
    strResults.add('');
    strResults.add('Unable to get system power details');
  end;

  result := strResults;
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

end.


