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
  Classes, SysUtils, DateUtils, Windows, UOptions, strutils;

type
  FuzzyTime = class

Private
  Function fTime       : String;
  Function netTime     : string;
  Function unixTime    : string;
  Function utcTime     : string;
  Function swatchTime  : string;
  Function julianTime  : string;
  Function decimalTime : string;
  Function hexTime     : string;
  Function binaryTime  : string;
Public
  displayFuzzy : Integer ;
  Constructor init ;
  Function getTime : string ;
  Function getDblTime : Double;
end;


implementation

Constructor FuzzyTime.init;
begin
  self.displayFuzzy := OptionsRec.DefaultTime;
end;

Function FuzzyTime.fTime : String ;
Type
  Hours = Array [0..12] of String;
VAR
  t     : TDateTime;
  hour  : word;
  mins  : word;
  secs  : word;
  mscs  : word;
  nrms  : word;      //  nearest five minutes
  ampm  : string;    //  am pm indicator, also used for afternoon or evening
  sRtn  : string;    //  return string
  dummy : string;    //  hour text

  hourTxt : Hours;
begin
  hourTxt[0] := 'twelve';     //  same as 12 o'clock
  hourTxt[1]  := 'one';
  hourTxt[2]  := 'two';
  hourTxt[3]  := 'three';
  hourTxt[4]  := 'four';
  hourTxt[5]  := 'five';
  hourTxt[6]  := 'six';
  hourTxt[7]  := 'seven';
  hourTxt[8]  := 'eight';
  hourTxt[9]  := 'nine';
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
     0 : sRtn := '';
     5 : sRtn := 'five past ';
    10 : sRtn := 'ten past ';
    15 : sRtn := 'quarter past ';
    20 : sRtn := 'twenty past ';
    25 : sRtn := 'twenty-five past ';
    30 : sRtn := 'half past ';
    35 : sRtn := 'twenty-five to ';
    40 : sRtn := 'twenty to ';
    45 : sRtn := 'quarter to ';
    50 : sRtn := 'ten to ';
    55 : sRtn := 'five to ';
    60 : sRtn := '';
  end;

  if nrms >30 then
    hour := hour + 1;

  if (hour = 12) and (nrms = 0) then   //  fix for noon.
    ampm := ' noon';

  if ampm = 'pm' then begin
    hour := hour - 12;
  if hour >= 5 then
      ampm := ' in the evening'
    else
      ampm := ' in the afternoon';
  end;

  dummy := hourTxt[hour];

  ftime := sRtn + dummy + ampm;
end ;

Function FuzzyTime.netTime : string;
{  New Earth Time [or NET] splits the day into 260 degrees. each degree is
   further split into 60 minutes and further into 60 seconds.

   Only returns NET time in NET 15 second intervals [equals 1 normal second]  }
VAR
  deg : Int64;
  min : Int64;
  sec : Int64;
begin

  if OptionsRec.NetTimeSeconds then begin
    deg := (MilliSecondOfTheDay(Time) div 240000);
    min := (MilliSecondOfTheDay(Time) - (deg * 240000)) div 4000;
    sec := (MilliSecondOfTheDay(Time) - (deg * 240000) - (min * 4000)) div 100;
  end
  else begin
    deg := (SecondOfTheDay(Time) div 240);
    min := (SecondOfTheDay(Time) - (deg * 240)) div 4;
    sec := (SecondOfTheDay(Time) - (deg * 240) - (min * 4)) * 15;
  end;

  netTime := format('%d deg %d min %d sec', [deg, min, sec]);
end;

Function FuzzyTime.unixTime : string;
{  return UNUX epoch time                                                      }
VAR
  unix : integer;

begin
  unix := DateTimeToUnix(Now);

  unixTime := format('%d', [unix]);
end;
Function FuzzyTime.utcTime : string;
{  returns UTC, Coordinated Universal Time - will only work in windows.
   This is then encoded into a string.                                         }
VAR
  utc : TSystemTime;
begin
  GetSystemTime(utc);              //  Get current time in UTC
  utcTime := TimeToStr(EncodeTime(utc.Hour, utc.Minute, utc.Second, utc.Millisecond));
end;
Function FuzzyTime.swatchTime : string;
{  returns Swatch Time - will only work in windows.
   Swatch time is made up of 1000 beats per day i.e. 1 beat = 0.1157 seconds.
   This is then encoded into a string.                                         }
VAR
  utc         : TSystemTime;
  noOfSeconds : double;
  noOfBeats   : double;
begin
  GetSystemTime(utc);                    //  Get current time in UTC
  noOfSeconds := SecondOfTheDay(EncodeTime(utc.Hour, utc.Minute, utc.Second, utc.Millisecond));

  noOfBeats := (noOfSeconds * 0.01157);    // 1000 beats per day

  if OptionsRec.SwatchCentibeats then
    swatchTime := format('@ %3.2f BMT', [noOfBeats])
  else
    swatchTime := format('@ %3.f BMT', [noOfBeats]);

end;

Function FuzzyTime.julianTime : string ;
{  returns Julian Date Time - will only work in windows.
   Formulae pinched from http://en.wikipedia.org/wiki/Julian_day               }
VAR
  a, y, m : double;
  jt : double;
  utc         : TSystemTime;
begin

  GetSystemTime(utc);                    //  Get current time in UTC

  a := (14 - utc.month) / 12;
  y := utc.year + 4800 - a;
  m := utc.month + (12 * a) - 3;

  jt := utc.day + ((153 * m +2) / 5) + (365 * y) + (y/4) - (y/100) + (y/400) - 32045;
  jt := jt + ((utc.Hour - 12) / 24) + (utc.Minute / 1440) + (utc.Second / 86400);

  julianTime := format('%7.7f', [jt]);
end;

Function FuzzyTime.decimalTime : string ;
{  returns the current time in decimal notation.
   The day is divided into 10 hours, each hour is then split into 100 minutes
   of 100 seconds.                                                             }
VAR
  noOfSeconds : LongWord;
  noOfDecSecs : integer;

  secs : integer;
  mins : integer;
  hrs  : integer;
begin
  noOfSeconds := SecondOfTheDay(Time);
  noOfDecSecs := round(noOfSeconds * ( 100000 / 84600));  // a decimal second is
                                                          // smaller then a normal second
  hrs  := noOfDecSecs div 10000;
  mins := (noOfDecSecs - hrs * 10000) div 100;
  secs := noOfDecSecs mod 100;


  decimalTime := format('%2.d:%2.d:%2.d',[hrs, mins, secs]);
end;

Function FuzzyTime.hexTime : string ;
VAR
  noOfSeconds : LongWord;
  noOfHexSecs : integer;

  sec : integer;
  min : integer;
  hrs : integer;

  ssec : String;
  smin : String;
  shrs : String;
begin
  noOfSeconds := SecondOfTheDay(Time);
  noOfHexSecs := round(noOfSeconds * ( 65536 / 84600));  // a Hexadecimal second is
                                                         // larger then a normal second
  hrs := noOfHexSecs div 4096;
  min := (noOfHexSecs - hrs * 4096) div 16;
  sec := noOfHexSecs mod 16;

  shrs := Dec2Numb(hrs, 1, 16);
  smin := Dec2Numb(min, 2, 16);
  ssec := Dec2Numb(sec, 1, 16);

  hexTime := format('%s_%s_%s', [shrs, smin, ssec])
end;

Function FuzzyTime.binaryTime : string ;
VAR
  t     : TDateTime;
  hrs  : word;
  min  : word;
  sec  : word;
  msc  : word;

  ssec : String;
  smin : String;
  shrs : String;
begin
  t := Time;
  DecodeTime(t, hrs, min, sec, msc);

  shrs := Dec2Numb(hrs, 5, 2);
  smin := Dec2Numb(min, 6, 2);
  ssec := Dec2Numb(sec, 6, 2);

  binaryTime := format('%s %s %s', [shrs, smin, ssec])
end;

Function FuzzyTime.getDblTime : Double ;
{  returns current time as a float, in the format hh.mm.
   a bit klunky - needs a rewrite, when i know how                             }
VAR
  hour  : word;
  min   : word;
  sec   : word;
  msec  : word;
  fhour : double;
  fmin  : double;
begin
  DecodeTime(time, hour, min, sec, msec);

  fhour := hour;
  fmin  := min / 100;

  getDBlTime := fhour + fmin ;
end;

Function FuzzyTime.getTime : String;
begin

  case self.displayFuzzy of
    0 : getTime := self.fTime;
    1 : getTime := TimeToStr(Time);
    2 : getTime := self.netTime;
    3 : getTime := self.unixTime;
    4 : getTime := utcTime;
    5 : getTime := swatchTime;
    6 : getTime := julianTime;
    7 : getTime := decimalTime;
    8 : getTime := hexTime;
    9 : getTime := binaryTime;
  end;

end ;


end.

