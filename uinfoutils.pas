unit uInfoUtils;
{  A Set of utilities to gather information to display.

   The TStringList are created in this module because -

       This solves the problem of not being able to free the StringList and
       stop the memory leak.  Also stops the double creation of the stringList
       which causes a sig fault.
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, windows, dateutils, dialogs, Moon, MoonComp, ActiveX,
  ComObj, Variants, Forms;

function getDaylightSaving(year: integer): TStringList;
function GetNthDSTDOW(Y,M,DST_DOW,N:word):integer;
function getEasterDates(Year: integer): TStringList;
function getLentDates(year: integer): TStringList;
function getChineseDates(year: integer): TStringList;
function getPower: TStringList;
function isBitSet(AValue, ABitNumber : integer): boolean;
function getMoonStuff: TStringList;
function getSunStuff: TStringList;
function getMonitorStuff: TStringList;
function getName(thing: OLEVariant; count: integer): string;


implementation

uses
  formklock;

function getDaylightSaving(Year: integer): TStringList;
{  Returns daylight saving stuff.

   The daylight saving stuff is based on this -
   http://www.delphiforfun.org/Programs/delphi_techniques/TimeZoneInfo.htm
}
var
  timezoneinfo       : TTimezoneinformation;
  dayLightSaving     : TDatetime;
  dayLightOffSet     : word;
  TimezoneInformation: word;
begin
  result := TStringList.Create;

  TimezoneInformation := GetTimezoneInformation(timezoneinfo);

  if TimezoneInformation > 0 then   //  if TimezoneInformation = 0, then error
  begin
    with timezoneinfo do
    begin
      DaylightDate.Year := year;

      case TimezoneInformation of
        time_zone_Id_unknown:
        begin
          result.add('Current daylight status is unknown');
          result.add('');
        end;
        time_zone_id_standard:
        begin
          result.add('We are not currently in the daylight savings time period');
          result.add(StandardName);
          result.add('');
        end;
        time_zone_id_daylight:
        begin
          result.add('We are currently in the daylight savings time period');
          result.add(Daylightname);
          result.add('');
        end;
      end;

      result.add(format('Daylight Saving for %d', [year]));
      result.add('');

      with daylightdate do
      begin
        if (Daylightname = '') then
        begin
          result.add('No Daylight Saving Time information available');
          result.add('');
        end
        else
        begin
          dayLightOffSet := getNthDSTDOW(year, wmonth, wDayOfWeek, wDay);
          dayLightSaving := encodedate(year, wmonth, dayLightOffSet) + encodetime(whour, wminute, wsecond, wmilliseconds);
          result.add(formatdatetime('"Daylight saving starts: " mmmm dd  hh:nn am/pm', dayLightSaving));
          result.add('');
        end;   //  if (Daylightname = '')
      end;     //  with daylightdate

      standarddate.Year := year;
      with standarddate do
      begin
        dayLightOffSet := getNthDSTDOW(year, wmonth, wDayOfWeek, wday);
        dayLightSaving := encodedate(year, wmonth, dayLightOffSet) + encodetime(whour, wminute, wsecond, wmilliseconds);
        result.add(formatdatetime('"Daylight saving ends: " mmmm dd  hh:nn am/pm', dayLightSaving));
        result.add('');
      end;     //  with standarddate
    end;       //  with timezoneinfo
  end          //  if r >
  else
    result.add('Time zone information not available');

end;

function GetNthDSTDOW(Y,M,DST_DOW,N:word):integer;
{  For Year "Y" and Month "M"  and DayOfWeek "DST_DOW", return the day of month for "DST_DOW" number "N"
   If  N  is larger than the number of DST_DOW's in the month, return the day of the last one
   If Y, M, or N are otherwise invalid, return 0
}
var
  dt:TDateTime;
  NdayDom, maxdays:integer;
begin
  if TryEncodeDate(y,m,1,dt) and (n>0) then //  get date of first of month
  begin
    if n>5 then n:=5;
    NdayDOM := 8+DST_DOW-DayOfTheWeek(dt);  //  1st DST_DOW Day of Month
    result  := NdayDOM+7*(n-1);
    maxdays := daysinMonth(dt);
    If result>maxdays  then
    repeat dec(Result,7) until Result<=Maxdays;
  end
  else result := 0;
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
   Lent start on Ash Wednesday, which is 46 days before Easter Sunday.
}
var
  lent: TdateTime;
begin
  result := TStringList.Create;
  lent   := EasterDate(year);    //  return date of Easter Sunday.

  result.add('');
  result.add('');
  result.add(format('Lent Dates for %d', [year]));
  result.add('');
  result.add(FormatDateTime('"Lent Starts [Ash Wednesday] :: "DD MMM YYYY', incDay(lent, -46)));
  result.add(FormatDateTime('"Lent Ends   [Easter Sunday] :: "DD MMM YYYY', lent));
end;

function getChineseDates(year: integer): TStringList;
{  Returns the Lent dates for a given year.
   Lent start on Ash Wednesday, which is 46 days before Easter Sunday.
}
var
  chinese      : TChineseDate;
  ChineseZodiac: array[TChineseZodiac] of string;
begin
  ChineseZodiac[ch_rat]     := 'Rat';
  ChineseZodiac[ch_ox]      := 'Ox';
  ChineseZodiac[ch_tiger]   := 'Tiger';
  ChineseZodiac[ch_rabbit]  := 'Rabbit';
  ChineseZodiac[ch_dragon]  := 'Dragon';
  ChineseZodiac[ch_snake]   := 'Snake';
  ChineseZodiac[ch_horse]   := 'Horse';
  ChineseZodiac[ch_Goat]    := 'Goat';
  ChineseZodiac[ch_monkey]  := 'Monkey';
  ChineseZodiac[ch_chicken] := 'Chicken';
  ChineseZodiac[ch_dog]     := 'Dog';
  ChineseZodiac[ch_pig]     := 'Pig';

  chinese := ChineseDate(ChineseNewYear(year));
  result  := TStringList.Create;

  result.add('');
  result.add(FormatDateTime('"Chinese New year :: "DD MMM YYYY', ChineseNewYear(year)));
  result.add('Currently the year of the ' + ChineseZodiac[chinese.yearcycle.zodiac]);
end;

function getPower: TStringList;
{  see https://msdn.microsoft.com/en-us/library/windows/desktop/aa373232(v=vs.85).aspx  }
var
  PowerStatus: TSystemPowerStatus;
  batteryTime : TDateTime;
  lifeTime : double;
begin
  result := TStringList.Create;

  if GetSystemPowerStatus(PowerStatus) then

  begin
    if powerStatus.ACLineStatus = 1 then
      result.add('Mains power online')
    else
      result.add('No mains Power');

    result.add('');

    {  The battery flag can be made of several flags anded together.  }
    if isBitSet(powerStatus.BatteryFlag, 0) then
      result.add('High - the battery capacity is at more than 66 percent');
    if isBitSet(powerStatus.BatteryFlag, 1) then
      result.add('Low - the battery capacity is at less than 33 percent');
    if isBitSet(powerStatus.BatteryFlag, 2) then
      result.add('Critical - the battery capacity is at less than five percent');
    if isBitSet(powerStatus.BatteryFlag, 3) then
      result.add('Charging');
    if isBitSet(powerStatus.BatteryFlag, 7) then
      result.add('No system battery');
    if powerStatus.BatteryFlag = 255 then
      result.add('Unknown status - unable to read the battery flag information');

    result.add('');

    if powerStatus.ACLineStatus = 0 then
    begin
      if powerStatus.BatteryLifePercent <> 0 then
        result.add(format('Battery Life %d %%', [powerStatus.BatteryLifePercent]));

      if powerStatus.BatteryLifeTime <> 0 then
      begin
        batteryTime := TimeStampToDateTime(MSecsToTimeStamp(powerStatus.BatteryLifeTime * 1000));
        result.add(format('Battery Life %s', [FormatDateTime('hh:nn:ss', batteryTime)]));

        //  will calculate expected battery life - don't beleive returned values.
        lifeTime    := (100 / powerStatus.BatteryLifePercent) * powerStatus.BatteryLifeTime;
        batteryTime := TimeStampToDateTime(MSecsToTimeStamp(trunc(lifeTime * 1000)));
        result.add(format('Battery Full Life %s', [timeToStr(batteryTime)]));
      end;
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

function isBitSet(AValue, ABitNumber : integer): boolean;
{  Returns true if bit ABitNumber is set to in AValue.    }
begin
  result := odd(Avalue shr ABitNumber);
end;

function getMoonStuff: TStringList;
{  Returns Moon Info using DelphiMoon.
   Works with UTC.

   NB : timezone declared in formKlock.
}
var
  moonDate          : TDateTime;
  moonPhase         : TMoonPhase;
  age               : double;
  hour, min, sec, ms: word;
begin
  moonDate := trunc(now);

  result := TStringList.Create;

  age := AgeOfMoon(moonDate);
  DecodeTime(age, hour, min, sec, ms);
  result.add(format('Age of the moon %d days, %d hours, %d minutes', [trunc(age), hour, min]));
  result.add(format('Illumination of the moon   %2.1f', [Current_Phase(moonDate)]));
  result.add(format('Distance of the moon       %.0n Km', [Moon_Distance(now)]));

  moonPhase := Nearest_Phase(moonDate);

  case moonPhase of
    Newmoon       : result.add('Phase of the moon NewMoon');
    WaxingCrescent: result.add('Phase of the moon Waxing Crescent');
    FirstQuarter  : result.add('Phase of the moon First Quarter');
    WaxingGibbous : result.add('Phase of the moon Waxing Gibbous');
    Fullmoon      : result.add('Phase of the moon Full Moon');
    WaningGibbous : result.add('Phase of the moon Waning Gibbous');
    LastQuarter   : result.add('Phase of the moon Last Quarter');
    WaningCrescent: result.add('Phase of the moon Waning Crescent');
  end;
  result.add('');
  result.add(format('Lunation of new moon   %d', [Lunation(moonDate)]));
  result.add(format('Next Full Moon         %s', [FormatDateTime('dd/mm/yyy',Next_Phase(moonDate, FullMoon))]));
  result.add(format('Next Blue Moon         %s', [FormatDateTime('dd/mm/yyy',Next_Blue_Moon(moonDate))]));
  result.add('');
  result.add(format('Moon Rise              %s', [FormatDateTime('dd/mm/yyyy  hh:mm:ss',
                                                  Moon_Rise(moonDate, userOptions.Latitude, userOptions.Longitude))]));

  try
    age := Moon_Transit(moonDate, userOptions.Latitude, userOptions.Longitude);
    result.add(format('Moon Transit         %s', [FormatDateTime('dd/mm/yyyy  hh:mm:ss', age)]));
  except
    result.add('The Moon stays below horizon for the whole day ');
  end;
  result.add(format('Moon Set               %s', [FormatDateTime('dd/mm/yyyy  hh:mm:ss',
                                                  Moon_Set(moonDate, userOptions.Latitude, userOptions.Longitude))]));
end;

function getSunStuff: TStringList;
{  Returns Moon Info using DelphiMoon.
   Works with UTC.

   NB : timezone declared in formKlock.
}
var
  sunDate: TDateTime;
begin
  sunDate := trunc(now);
  result  := TStringList.Create;

  result.add(format('Sun Rise              %s', [FormatDateTime('hh:mm:ss  dd/mm/yyyy ',
                     Sun_Rise(sunDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add(format('Sun Transit           %s', [FormatDateTime('hh:mm:ss  dd/mm/yyyy',
                     Sun_Transit(sunDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add(format('Sun Set               %s', [FormatDateTime('hh:mm:ss  dd/mm/yyyy',
                     Sun_Set(sunDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add('');
  result.add(format('Morning Twilight      %s', [FormatDateTime('hh:mm:ss  dd/mm/yyyy',
                     Morning_Twilight_Civil(sunDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add(format('Evening Twilight      %s', [FormatDateTime('hh:mm:ss  dd/mm/yyyy',
                     Evening_Twilight_Civil(sunDate, userOptions.Latitude, userOptions.Longitude))]));
  result.add('');
  result.add(format('Distance of the sun   %.0n Km', [Sun_Distance(now) * 149597869]));

end;

function getMonitorStuff: TStringList;
{  Added Monitor Stuff to the Info menu.
   This shows information about all the display monitors connects to the system.
   This us gathered from Lazarus Tscreen and the WMI system [probably only windows then].

   It is a combination of several routines sourced from the internet.
   As always, thanks guys.
}
const
  WbemUser            = '';
  WbemPassword        = '';
  WbemComputer        = 'localhost';
  wbemFlagForwardOnly = $00000020;
var
  FSWbemLocator : OLEVariant;
  FWMIService   : OLEVariant;
  FWbemObjectSet: OLEVariant;
  FWbemObject   : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;
  count         : integer = 0;
begin
  result := TStringList.Create;

  result.add(format('Number of displays %s', [intToStr(Screen.MonitorCount)]));
  result.add(format('Overall Desktop size %d x %d', [screen.DesktopHeight, screen.DesktopWidth]));
  result.add('');

  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer(WbemComputer, 'root\WMI', WbemUser, WbemPassword);
  FWbemObjectSet:= FWMIService.ExecQuery('SELECT * FROM WmiMonitorID','WQL',wbemFlagForwardOnly);
  oEnum         := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;

  while oEnum.Next(1, FWbemObject, iValue) = 0 do
  begin
    result.add(Format('Active                    %s', [String(FWbemObject.Active)]));

    if Screen.Monitors[count].Primary then
      result.add('Primary                   True');

    result.add(Format('Instance Name             %s', [String(FWbemObject.InstanceName)]));

    result.add(Format('Manufacturer Name         %s', [getName(FWbemObject.ManufacturerName, 16)]));
    result.add(Format('Serial Number             %s', [getName(FWbemObject.SerialNumberID, 16)]));
    result.add(Format('User Friendly Name        %s', [getName(FWbemObject.UserFriendlyName, FWbemObject.UserFriendlyNameLength)]));

    result.add(format('Display resolution        %d x %d', [Screen.Monitors[count].Width, Screen.Monitors[count].Height]));

    result.add(Format('Week Of Manufacture       %d', [Integer(FWbemObject.WeekOfManufacture)]));
    result.add(Format('Year Of Manufacture       %d', [Integer(FWbemObject.YearOfManufacture)]));

    result.add('');
    inc(count);
    FWbemObject := Unassigned;
  end;
end;

function getName(thing: OLEVariant; count: integer): string;
{  The thing is an OLEVariant that contains a string, but held as a number
   of characters held in ASCII format.  This routines converts each number back
   to its ASCII characters and return the joined string.
}
var
  f: integer;
  s: string;
begin
  s := '';
  for f := 0 to count - 1 do
  begin
    if chr(thing[f]) in ['A' .. 'Z', 'a' .. 'z', '0' .. '9'] then
    begin
      s += chr(thing[f]);
    end;
  end;
  result := s;
end;

end.


