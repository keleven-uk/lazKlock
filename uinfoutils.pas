unit uInfoUtils;
{  A Set of utilitoes to gather information to display.

   The daylisght saving stuff is based on this -
   http://www.delphiforfun.org/Programs/delphi_techniques/TimeZoneInfo.htm

   The TStringList are create in this mudule because -

       This solves the promle of not being able to free the StringList and
       stop the memory leak.  Also stops the double creation of the stringList
       which casues a sig fault.
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, windows, dateutils, dialogs, Moon, MoonComp;

function getDaylightSaving(year: integer): TStringList;
function GetNthDSTDOW(Y,M,DST_DOW,N:word):integer;
function getEasterDates(Year: integer): TStringList;
function getLentDates(year: integer): TStringList;
function getPower: TStringList;
function getMoonStuff: TStringList;
function getSunStuff: TStringList;

implementation

uses
  formklock;

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
  easter: TdateTime;
begin
  result := TStringList.Create;

  easter := EasterDate(year);  //  return date of Easter Sunday.

  result.add('');
  result.add('');
  result.add(format('Easter Dates for %d', [year]));
  result.add('');
  result.add(FormatDateTime('"Good Friday   :: "DD MMM YYYY', incDay(easter, -2)));
  result.add(FormatDateTime('"Easter Sunday :: "DD MMM YYYY', easter));
  result.add(FormatDateTime('"Easter Monday :: "DD MMM YYYY', incDay(easter, +1)));
  result.add('');
  result.add('');
  result.add(FormatDateTime('"Orthodox Easter Sunday   :: "DD MMM YYYY', EasterDateJulian(year)));
end;

function getLentDates(year: integer): TStringList;
{  Returns the Lent dates for a given year.
   lent start on Ash Wednesday, which is 46 days before Easter Sunday.
}
var
  lent: TdateTime;
begin
  result := TStringList.Create;

  lent := EasterDate(year);    //  return date of Easter Sunday.

  result.add('');
  result.add('');
  result.add(format('Lent Dates for %d', [year]));
  result.add('');
  result.add(FormatDateTime('"Lent Starts [Ash Wednesday] :: "DD MMM YYYY', incDay(lent, -46)));
  result.add(FormatDateTime('"Lent Ends   [Easter Sunday] :: "DD MMM YYYY', lent));
end;

function getPower: TStringList;
{  see https://msdn.microsoft.com/en-us/library/windows/desktop/aa373232(v=vs.85).aspx  }
var
  PowerStatus: TSystemPowerStatus;
begin
  result := TStringList.Create;

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

function getMoonStuff: TStringList;
{  Retuns Moon Info using DelphiMoon.
   Works with UTC.

   nb : timezone declared in formKlock.
}
var
  moonDate: TDateTime;
  moonPhase: TMoonPhase;
  age: double;
  hour, min, sec, ms: word;
begin
  moonDate := now;

  result := TStringList.Create;

  age := AgeOfMoon(moonDate);
  DecodeTime(age, hour, min, sec, ms);
  result.add(format('Age of the moon %d days, %d hours, %d minutes', [trunc(age), hour, min]));
  result.add(format('Illumination of the moon = %2.1f', [Current_Phase(moonDate)]));
  result.add(format('Distance of the moon = %.0n Km', [Moon_Distance(now)]));

  moonPhase := Nearest_Phase(moonDate);

  case moonPhase of
    Newmoon: result.add('Phase of the moon NewMoon');
    WaxingCrescent: result.add('Phase of the moon Waxing Crescent');
    FirstQuarter: result.add('Phase of the moon First Quarter');
    WaxingGibbous: result.add('Phase of the moon Waxing Gibbous');
    Fullmoon: result.add('Phase of the moon Full Moon');
    WaningGibbous: result.add('Phase of the moon Waning Gibbous');
    LastQuarter: result.add('Phase of the moon Last Quarter');
    WaningCrescent: result.add('Phase of the moon Waning Crescent');
  end;
  result.add('');
  result.add(format('Lunation of new moon = %d', [Lunation(moonDate)]));
  result.add(format('Next Full Moon = %s', [FormatDateTime('dd/mm/yyy',Next_Phase(moonDate, FullMoon))]));
  result.add(format('Next Blue Moon = %s', [FormatDateTime('dd/mm/yyy',Next_Blue_Moon(moonDate))]));
  result.add('');
  result.add(format('Moon Rise = %s', [FormatDateTime('dd/mm/yyyy  hh:mm:ss',
                     Moon_Rise(moonDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add(format('Moon Set = %s', [FormatDateTime('dd/mm/yyyy  hh:mm:ss',
                     Moon_Set(moonDate, userOptions.Latitude, userOptions.Longitude))]));
  try
    age := Moon_Transit(moonDate, userOptions.Latitude, userOptions.Longitude);
    result.add(format('Moon Transit = %s', [FormatDateTime('dd/mm/yyyy  hh:mm:ss', age)]));
  except
    result.add('The Moon stays below horizon for the whole day ');
  end;

end;

function getSunStuff: TStringList;
{  Retuns Moon Info using DelphiMoon.
   Works with UTC.

   nb : timezone declared in formKlock.
}
var
  sunDate: TDateTime;
begin
  sunDate := TimeZone.UniversalTime;

  result := TStringList.Create;

  result.add(format('Sun Rise = %s', [FormatDateTime('hh:mm:ss  dd/mm/yyyy ',
                     Sun_Rise(sunDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add(format('Sun Set = %s', [FormatDateTime('hh:mm:ss  dd/mm/yyyy',
                     Sun_Set(sunDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add(format('Sun Transit = %s', [FormatDateTime('hh:mm:ss  dd/mm/yyyy',
                     Sun_Transit(sunDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add('');
  result.add(format('Morning Twilight = %s', [FormatDateTime('hh:mm:ss  dd/mm/yyyy',
                     Morning_Twilight_Civil(sunDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add(format('Evening Twilight = %s', [FormatDateTime('hh:mm:ss  dd/mm/yyyy',
                     Evening_Twilight_Civil(sunDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add('');
  result.add(format('Distance of the sun = %.0n Km', [Sun_Distance(now) * 149597869]));


end;

end.


