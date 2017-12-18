unit UFuzzyTime;

{$mode objfpc}{$H+}

{
  Kevin Scott         January 2012

  returns the time as a string, depending on the value of displayFuzzy.

  displayFuzzy set to True  :: getTime returns time as five past ten.
  displayFuzzy set to False :: getTime returns time as 10:05:00.

  NB :: Bar Code Time, Semaphore Time, Nancy Blackett Time & Braille Time
  are done by font substitution.
}

interface

uses
  Classes, SysUtils, DateUtils, Windows, strutils, Dialogs;

type
  FuzzyTime = class

  private
    _displayFuzzy: integer;
    _fuzzyBase: integer;
    _fuzzyTypes: TStringList;
    _display24Hour: boolean;        //  Disply time has 24 hour if true, else 12 hour.

    // globally declare arrays, so can be used by more then one sub and also not re-created every call of the sub.
    const hourTxt: array [0..12] of string = ('twelve', 'one', 'two', 'three', 'four',
                     'five', 'six', 'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve');

    const tensTxt: array [0..11] of string = ('zero', 'ten', 'eleven', 'twelve', 'thirteen',
          'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen', 'twenty');

    const unitsTxt: array [0..12] of string = ('zero', 'one', 'two', 'three', 'four',
                    'five', 'six', 'seven', 'eight', 'nine', 'ten', 'eleven', 'twelve');

    function fTime: string;
    function wordTime: string;
    function netTime: string;
    function unixTime: string;
    function utcTime: string;
    function swatchTime: string;
    function julianTime: string;
    function decimalTime: string;
    function hexTime: string;
    function radixTime: string;
    function percentTime: string;
    function getDblTime: string;
    function codeTime(mode: string): string;
    function toMorse(time: integer): string;
    function toRoman(time: integer): string;
  public
    property displayFuzzy: integer read _displayFuzzy write _displayFuzzy;
    property fuzzyBase: integer read _fuzzyBase write _fuzzyBase;
    property fuzzyTypes: TStringList read _fuzzyTypes;                    //  read only.
    property display24Hour: boolean read _display24Hour write _display24Hour;

    constructor Create; overload;
    function getTime: string;
  end;


implementation

uses
  formklock;

constructor FuzzyTime.Create; overload;
begin
  fuzzyBase := 2;

  _fuzzyTypes := TStringList.Create;
  _fuzzyTypes.CommaText := ('"Fuzzy Time", "Word Time", "Local Time", "NET Time", "Unix Time", "UTC Time",' +
   '"Swatch Time", "Julian Time", "Decimal Time", "Hex Time", "Radix Time", "Percent Time", "Double Time",'  +
   '"Roman Time", "Morse Time", "Bar Code Time", "Semaphore Time", "Nancy Blackett Time", "Braille Time",');
 end;

function FuzzyTime.fTime: string;
{  Returns the current [local] time as fuzzy time i.e. ten past three in the afternoon.
   But rounds to the nearest five minutes.
}
var
  hour: word;
  mins: word;
  secs: word;
  mscs: word;
  nrms: word;      //  nearest five minutes
  ampm: string;    //  am pm indicator, also used for afternoon or evening
  sRtn: string;    //  return string
  dummy: string;    //  hour text
begin
  DecodeTime(Time, hour, mins, secs, mscs);

  if hour < 12 then
    ampm := ' in the morning'
  else
    ampm := 'pm';

  nrms := mins - (mins mod 5);       //  gets nearest five mins

  if (mins mod 5) > 2 then           // closer to next five minutes, go next
    nrms := nrms + 5;

  case nrms of
    0: sRtn := '';
    5: sRtn := 'five past ';
    10: sRtn := 'ten past ';
    15: sRtn := 'quarter past ';
    20: sRtn := 'twenty past ';
    25: sRtn := 'twenty-five past ';
    30: sRtn := 'half past ';
    35: sRtn := 'twenty-five to ';
    40: sRtn := 'twenty to ';
    45: sRtn := 'quarter to ';
    50: sRtn := 'ten to ';
    55: sRtn := 'five to ';
    60: sRtn := '';
  end;

  if nrms > 30 then
    hour := hour + 1;

  if (hour = 12) and (nrms = 0) then   //  fix for noon.
    ampm := ' about noon'
  else if (hour = 0) and (nrms = 0) then
    ampm := ' about Midnight'
  else if (hour = 24) and (nrms = 0) then
    ampm := ' about Midnight'
  else
  begin
    hour := hour - 12;
    if hour >= 5 then
      ampm := ' in the evening'
    else
      ampm := ' in the afternoon';
  end;

  dummy := hourTxt[hour];

  Result := sRtn + dummy + ampm;
end;

function FuzzyTime.wordTime: string;
{  Returns the current [local] time as fuzzy time i.e. ten past three in the afternoon.
   But uses the exact minute.
}
var
  hour: word;
  mins: word;
  secs: word;
  mscs: word;
  ampm: string;    //  am pm indicator, also used for afternoon or evening
  pastTo: string;    //  return string
  sRtn: string;    //  hour text
 begin
    DecodeTime(Time, hour, mins, secs, mscs);
    pastTo := 'past';

    if mins > 30 then
    begin
      hour := hour + 1;
      pastTo := 'to';
      mins := 60 - mins;
    end;

    if hour < 12 then
      ampm := 'in the morning'
    else
      begin
        hour := hour - 12;
        if hour >= 5 then
          ampm := 'in the evening'
        else
          ampm := 'in the afternoon';
      end;

    case mins of
      0: sRtn := format('%s Oclock %s', [hourTxt[hour], ampm]);
      1..9: sRtn := format('%s minutes %s %s %s', [unitsTxt[mins], pastTo, hourTxt[hour], ampm ]);
      10..20: sRtn := format('%s minutes %s %s %s', [tensTxt[mins - 9], pastTo, hourTxt[hour], ampm ]);
      21..29: sRtn := format('twenty%s minutes %s %s %s', [unitsTxt[mins mod 10], pastTo, hourTxt[hour], ampm ]);
      30: sRtn := format('thirty minutes %s %s %s', [pastTo, hourTxt[hour], ampm ]);
    end;

    Result := sRtn;
end;

function FuzzyTime.netTime: string;
{  New Earth Time [or NET] splits the day into 260 degrees. Each degree is
   further split into 60 minutes and further into 60 seconds.

   Only returns NET time in NET 15 second intervals [equals 1 normal second]  }
var
  deg: int64;
  min: int64;
  sec: int64;
begin

  if userOptions.netTimeSeconds then
  begin
    deg := (MilliSecondOfTheDay(Time) div 240000);
    min := (MilliSecondOfTheDay(Time) - (deg * 240000)) div 4000;
    sec := (MilliSecondOfTheDay(Time) - (deg * 240000) - (min * 4000)) div 100;
  end
  else
  begin
    deg := (SecondOfTheDay(Time) div 240);
    min := (SecondOfTheDay(Time) - (deg * 240)) div 4;
    sec := (SecondOfTheDay(Time) - (deg * 240) - (min * 4)) * 15;
  end;

  Result := format('%d deg %d min %d sec', [deg, min, sec]);
end;

function FuzzyTime.unixTime: string;
{  return UNIX epoch time                                                      }
var
  unix: integer;

begin
  unix := DateTimeToUnix(Now);

  Result := format('%d', [unix]);
end;

function FuzzyTime.utcTime: string;
{  returns UTC, Coordinated Universal Time - will only work in windows.
   This is then encoded into a string.                                         }
var
  utc: TSystemTime;
begin
  GetSystemTime(utc);              //  Get current time in UTC
  Result := TimeToStr(EncodeTime(utc.Hour, utc.Minute, utc.Second, utc.Millisecond));
end;

function FuzzyTime.swatchTime: string;
{  returns Swatch Time - will only work in windows.
   Swatch time is made up of 1000 beats per day i.e. 1 beat = 0.1157 seconds.
   This is then encoded into a string.                                         }
var
  utc: TSystemTime;
  noOfSeconds: double;
  noOfBeats: double;
begin
  GetSystemTime(utc);                    //  Get current time in UTC
  noOfSeconds := SecondOfTheDay(EncodeTime(utc.Hour, utc.Minute, utc.Second, utc.Millisecond));

  noOfBeats := (noOfSeconds * 0.01157);    // 1000 beats per day

  if userOptions.swatchCentibeats then
    Result := format('@ %3.2f BMT', [noOfBeats])
  else
    Result := format('@ %3.f BMT', [noOfBeats]);

end;

function FuzzyTime.julianTime: string;
{  returns Julian Date Time - will only work in windows.
   Formulae pinched from http://en.wikipedia.org/wiki/Julian_day               }
var
  a, y, m: double;
  jt: double;
  utc: TSystemTime;
begin

  GetSystemTime(utc);                    //  Get current time in UTC

  a := (14 - utc.month) / 12;
  y := utc.year + 4800 - a;
  m := utc.month + (12 * a) - 3;

  jt := utc.day + ((153 * m + 2) / 5) + (365 * y) + (y / 4) - (y / 100) + (y / 400) - 32045;
  jt := jt + ((utc.Hour - 12) / 24) + (utc.Minute / 1440) + (utc.Second / 86400);

  Result := format('%7.7f', [jt]);
end;

function FuzzyTime.decimalTime: string;
{  returns the current time in decimal notation.
   The day is divided into 10 hours, each hour is then split into 100 minutes
   of 100 seconds.                                                             }
var
  noOfSeconds: longword;
  noOfDecSecs: integer;

  secs: integer;
  mins: integer;
  hrs: integer;
begin
  noOfSeconds := SecondOfTheDay(Time);
  noOfDecSecs := round(noOfSeconds * (100000 / 84600));  // a decimal second is
                                                         // smaller then a normal second
  hrs := noOfDecSecs div 10000;
  mins := (noOfDecSecs - hrs * 10000) div 100;
  secs := noOfDecSecs mod 100;


  Result := format('%2.d:%2.d:%2.d', [hrs, mins, secs]);
end;

function FuzzyTime.hexTime: string;
var
  noOfSeconds: longword;
  noOfHexSecs: integer;

  sec: integer;
  min: integer;
  hrs: integer;

  ssec: string;
  smin: string;
  shrs: string;
begin
  noOfSeconds := SecondOfTheDay(Time);
  noOfHexSecs := round(noOfSeconds * (65536 / 84600));  // a Hexadecimal second is
  // larger then a normal second
  hrs := noOfHexSecs div 4096;
  min := (noOfHexSecs - hrs * 4096) div 16;
  sec := noOfHexSecs mod 16;

  shrs := Dec2Numb(hrs, 1, 16);
  smin := Dec2Numb(min, 2, 16);
  ssec := Dec2Numb(sec, 1, 16);

  Result := format('%s_%s_%s', [shrs, smin, ssec]);
end;

function FuzzyTime.radixTime: string;
var
  hrs: word;
  min: word;
  sec: word;
  msc: word;

  ssec: string;
  smin: string;
  shrs: string;
begin
  DecodeTime(Time, hrs, min, sec, msc);

  shrs := Dec2Numb(hrs, 5, fuzzyBase);
  smin := Dec2Numb(min, 6, fuzzyBase);
  ssec := Dec2Numb(sec, 6, fuzzyBase);

  Result := format('%s %s %s', [shrs, smin, ssec]);
end;

function FuzzyTime.codeTime(mode: string): string;
var
  hour: word;
  mins: word;
  secs: word;
  msec: word;
  codeHour: String;
  codeMins: String;
  codeSecs: String;
begin
  DecodeTime(time, hour, mins, secs, msec);

  case mode of
    'Roman':
      begin
        codeHour := toRoman(hour);
        codeMins := toRoman(mins);
        codeSecs := toRoman(secs);
      end;
    'Morse':
      begin
        if hour < 9 then
          codeHour := toMorse(hour)
        else
          codeHour := toMorse(hour div 10) + toMorse(hour mod 10);

        if mins < 9 then
          codeMins := toMorse(mins)
        else
          codeMins := toMorse(mins div 10) + toMorse(mins mod 10);

        if hour < 9 then
          codeSecs := toMorse(secs)
        else
          codeSecs := toMorse(secs div 10) + toMorse(secs mod 10);
      end;
  end;  //  case mode of

  result := format('%s %s %s', [codeHour, codeMins, codeSecs]);
end;

function FuzzyTime.percentTime: string;
var
  noOfSeconds: longword;
  percentSeconds: double;
begin
  noOfSeconds := SecondOfTheDay(Time);
  percentSeconds := noOfSeconds / 86400;

  Result := format('%0.4f PMH', [percentSeconds * 100]);
end;

function FuzzyTime.getDblTime: string;
{  returns current time as a float, in the format hh.mm.
   a bit klunky - needs a rewrite, when i know how                             }
var
  hour: word;
  min: word;
  sec: word;
  msec: word;
  fhour: double;
  fmin: double;
  fsec: double;
begin
  DecodeTime(time, hour, min, sec, msec);

  fhour := hour;
  fmin := min / 100;
  fsec := sec / 10000;

  Result := FloatToStr(fhour + fmin + fsec);
end;

function FuzzyTime.getTime: string;
begin

  case displayFuzzy of
    0: Result := fTime;
    1: Result := wordTime;
    2: if display24Hour then Result := FormatDateTime('hh : nn : ss', Time) else Result := FormatDateTime('hh : nn : ss am/pm', Time);
    3: Result := netTime;
    4: Result := unixTime;
    5: Result := utcTime;
    6: Result := swatchTime;
    7: Result := julianTime;
    8: Result := decimalTime;
    9: Result := hexTime;
    10: Result := radixTime;
    11: Result := percentTime;
    12: Result := getDblTime;
    13: Result := codeTime('Roman');
    14: Result := codeTime('Morse');
  end;

end;

function FuzzyTime.toRoman(time: integer): string;
{  Returns string in Morse code.    }
var
  roman: string;
begin
  roman := '';
  repeat
  begin
    Case time of
        50 .. 60:
          begin
            roman := roman + 'L';
            time := time - 50;
          end;
        40 .. 49:
          begin
            roman := roman + 'XL';
            time := time - 40;
          end;
        10 .. 39:
          begin
            roman := roman + 'X';
            time := time - 10;
          end;
        9:
          begin
            roman := roman + 'IX';
            time := time - 9;
          end;
        5 .. 8:
          begin
            roman := roman + 'V';
            time := time - 5;
          end;
        4:
          begin
            roman := roman + 'IV';
            time := time - 4;
          end;
        1 .. 3:
          begin
            roman := roman + 'I';
            time := time - 1;
          end;
        0: roman := '';
      End;
  end;
  until time < 1;

  result := roman;
End;

function FuzzyTime.toMorse(time: integer): string;
var
  morse: string;
begin
  Case time of
    1: morse := '·----';
    2: morse := '··---';
    3: morse := '···--';
    4: morse := '····-';
    5: morse := '·····';
    6: morse := '-····';
    7: morse := '--···';
    8: morse := '---··';
    9: morse := '----·';
    0: morse := '-----';
  End;
  result := morse;
end;

end.
