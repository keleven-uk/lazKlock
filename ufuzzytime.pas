unit UFuzzyTime;

{$mode objfpc}{$H+}

{
Kevin Scott         January 2012

returns the time as a string, depending on the value of displayFuzzy.

displayFuzzy set to True  :: getTime returns time as five past ten.
displayFuzzy set to False :: getTime returns time as 10:05:00.
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

    function fTime: string;
    function netTime: string;
    function unixTime: string;
    function utcTime: string;
    function swatchTime: string;
    function julianTime: string;
    function decimalTime: string;
    function hexTime: string;
    function radixTime: string;
    function percentTime: string;
  public
    property displayFuzzy: integer read _displayFuzzy write _displayFuzzy;
    property fuzzyBase: integer read _fuzzyBase write _fuzzyBase;
    property fuzzyTypes: TStringList read _fuzzyTypes;                    //  read only.

    constructor Create; overload;
    function getTime: string;
    function getDblTime: double;
  end;


implementation

uses
  formklock;

constructor FuzzyTime.Create; overload;
begin
  fuzzyBase := 2;

  _fuzzyTypes := TStringList.Create;
  _fuzzyTypes.CommaText := ('"Fuzzy Time", "Local Time", "NET Time", "Unix Time", "UTC Time", "Swatch Time",' +
    '"Julian Time", "Decimal Time", "Hex Time", "Radix Time", "Percent Time"');
end;

function FuzzyTime.fTime: string;
type
  Hours = array [0..12] of string;
var
  t: TDateTime;
  hour: word;
  mins: word;
  secs: word;
  mscs: word;
  nrms: word;      //  nearest five minutes
  ampm: string;    //  am pm indicator, also used for afternoon or evening
  sRtn: string;    //  return string
  dummy: string;    //  hour text

  hourTxt: Hours;
begin
  hourTxt[0] := 'twelve';     //  same as 12 o'clock
  hourTxt[1] := 'one';
  hourTxt[2] := 'two';
  hourTxt[3] := 'three';
  hourTxt[4] := 'four';
  hourTxt[5] := 'five';
  hourTxt[6] := 'six';
  hourTxt[7] := 'seven';
  hourTxt[8] := 'eight';
  hourTxt[9] := 'nine';
  hourTxt[10] := 'ten';
  hourTxt[11] := 'eleven';
  hourTxt[12] := 'twelve';

  t := Time;
  DecodeTime(t, hour, mins, secs, mscs);

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
    ampm := ' noon';

  if ampm = 'pm' then
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

  {****}
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
  t: TDateTime;
  hrs: word;
  min: word;
  sec: word;
  msc: word;

  ssec: string;
  smin: string;
  shrs: string;
begin
  t := Time;
  DecodeTime(t, hrs, min, sec, msc);

  shrs := Dec2Numb(hrs, 5, fuzzyBase);
  smin := Dec2Numb(min, 6, fuzzyBase);
  ssec := Dec2Numb(sec, 6, fuzzyBase);

  Result := format('%s %s %s', [shrs, smin, ssec]);
end;

function FuzzyTime.percentTime: string;
var
  noOfSeconds: longword;
  percentSeconds: double;
begin
  noOfSeconds := SecondOfTheDay(Time);
  percentSeconds := noOfSeconds / 86400;

  Result := format('%0.4f % PMH', [percentSeconds * 100]);
end;

function FuzzyTime.getDblTime: double;
{  returns current time as a float, in the format hh.mm.
   a bit klunky - needs a rewrite, when i know how                             }
var
  hour: word;
  min: word;
  sec: word;
  msec: word;
  fhour: double;
  fmin: double;
begin
  DecodeTime(time, hour, min, sec, msec);

  fhour := hour;
  fmin := min / 100;

  Result := fhour + fmin;
end;

function FuzzyTime.getTime: string;
begin

  case displayFuzzy of
    0: Result := fTime;
    1: Result := TimeToStr(Time);
    2: Result := netTime;
    3: Result := unixTime;
    4: Result := utcTime;
    5: Result := swatchTime;
    6: Result := julianTime;
    7: Result := decimalTime;
    8: Result := hexTime;
    9: Result := radixTime;
    10: Result := percentTime;
  end;

end;


end.
