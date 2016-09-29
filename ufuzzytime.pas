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
  Classes, SysUtils;

type
  FuzzyTime = class

Private
  Function fTime : String ;
Public
  displayFuzzy : Boolean ;
  Constructor init ;
  Function getTime : string ;

end;


implementation

Constructor FuzzyTime.init;
begin
  self.displayFuzzy := True;
end;

Function FuzzyTime.fTime : String ;
Type
  Hours = Array [1..12] of String;
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

  t     := Time;
  DecodeTime(t, hour, mins, secs, mscs);

  if hour >= 12 then
    ampm := 'pm'
  else
    ampm := ' in the morning';

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

  if hour < 1 then              //  fix for noon/midnight
    hour := hour + 1;

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

Function FuzzyTime.getTime : String;
begin
  if self.displayFuzzy then
    getTime := self.fTime
  else
    getTime := TimeToStr(Time);
end ;


end.

