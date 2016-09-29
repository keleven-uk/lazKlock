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
  Classes, SysUtils, DateUtils;

type
  FuzzyTime = class

Private
  Function fTime    : String ;
  Function netTime  : string;
  Function unixTime : string;
Public
  displayFuzzy : Integer ;
  Constructor init ;
  Function getTime : string ;

end;


implementation

Constructor FuzzyTime.init;
begin
  self.displayFuzzy := 1;
end;

Function FuzzyTime.fTime : String ;
Type
  Hours = Array [0..11] of String;
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


  t     := Time;
  DecodeTime(t, hour, mins, secs, mscs);

  if hour < 12 then
    ampm := ' in the morning'
  else
    ampm := 'pm';

  nrms := mins - (mins mod 5);       //  gets nearest five mins

  if (mins mod 5) > 2 then           // closer to next five minutes, go next
    nrms := nrms + 5;

  if nrms = 0 then begin
    sRtn := '';
  end
  else if nrms = 5 then begin
    sRtn := 'five past ';
  end
  else if nrms = 10 then begin
    sRtn := 'ten past ';
  end
  else if nrms = 15 then begin
    sRtn := 'quarter past ';
  end
  else if nrms = 20 then begin
    sRtn := 'twenty past ';
  end
  else if nrms = 25 then begin
    sRtn := 'twenty-five past ';
  end
  else if nrms = 30 then begin
    sRtn := 'half past ';
  end
  else if nrms = 35 then begin
    sRtn := 'twenty-five to ';
    hour := hour + 1;
  end
  else if nrms = 40 then begin
    sRtn := 'twenty to ';
    hour := hour + 1;
  end
  else if nrms = 45 then begin
    sRtn := 'quarter to ';
    hour := hour + 1;
  end
  else if nrms = 50 then begin
    sRtn := 'ten to ';
    hour := hour + 1;
  end
  else if nrms = 55 then begin
    sRtn := 'five to ';
    hour := hour + 1;
  end
  else if nrms = 60 then begin
    sRtn := '';
    hour := hour + 1;
  end;

  if ampm = 'pm' then begin
    hour := hour - 12;
    if (hour = 0) and (nrms = 0) then         // 12 o'clock : noon
      ampm := 'noon'
    else if hour >= 5 then
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
  deg : Integer;
  min : Integer;
  sec : Integer;

begin
  deg := SecondOfTheDay(Time) div 240;
  min := (SecondOfTheDay(Time) - (deg * 240)) div 4;
  sec := (SecondOfTheDay(Time) - (deg * 240) - (min * 4)) * 15;

  netTime := format('%d deg %d min %d sec', [deg, min, sec]);
end;

Function FuzzyTime.unixTime : string;
VAR
  unix : integer;

begin
  unix := DateTimeToUnix(Time);

  unixTime := format('%d', [unix]);
end;

Function FuzzyTime.getTime : String;
begin
  if self.displayFuzzy = 0 then                   //  normal time
    getTime := TimeToStr(Time)
  else if self.displayFuzzy = 1 then              //  fuzzy time
    getTime := self.fTime
  else if self.displayFuzzy = 2 then              //  net time
    getTime := self.netTime
  else if self.displayFuzzy = 3 then              // unix time
    getTime := self.unixTime
end ;


end.

