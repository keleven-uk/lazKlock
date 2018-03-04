unit UFuzzyTime;

{  Kevin Scott         January 2012

   returns the time as a string, depending on the value of displayFuzzy.

   displayFuzzy set to True  :: getTime returns time as five past ten.
   displayFuzzy set to False :: getTime returns time as 10:05:00.

   NB :: Bar Code Time, Semaphore Time, Nancy Blackett Time & Braille Time
   are done by font substitution.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, strutils, Dialogs, uPascalTZ, Moon;

type
  FuzzyTime = class

  private
    _displayFuzzy: integer;
    _fuzzyBase: integer;
    _fuzzyTypes: TStringList;
    _display24Hour: boolean;        //  Disply time has 24 hour if true, else 12 hour.
    _filename: string;              //  filename of the log file
    _dirname : string;              //  directory where the log file lives.

    timeZone: TPascalTZ;            //  used for local and utc time, will take into
                                    //  account time zones.

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
    function localTime: string;
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
    function getFlowTime: string;
    function getMetricTime: string;
    function getBCDTime: string;
    function getBinaryTime: string;
    procedure writeLog(message: string);
  public
    property filename: string read _filename write _filename;
    property dirname: string read _dirname write _dirname;
    property displayFuzzy: integer read _displayFuzzy write _displayFuzzy;
    property fuzzyBase: integer read _fuzzyBase write _fuzzyBase;
    property fuzzyTypes: TStringList read _fuzzyTypes;                    //  read only.
    property display24Hour: boolean read _display24Hour write _display24Hour;

    constructor Create; overload;
    destructor Destroy; override;
    function getTime: string;
  end;


implementation

uses
  formklock;

constructor FuzzyTime.Create; overload;
{  run on create.    }
begin
  dirname := GetAppConfigDir(False);
  filename := dirname + 'fuzzytime_' + FormatDateTime('DDMMMYYYY', now) + '.log';

  //writeLog('............................................................');
  //writeLog('Log Created');

  fuzzyBase := 2;

  _fuzzyTypes := TStringList.Create;
  _fuzzyTypes.CommaText := ('"Fuzzy Time", "Word Time", "Local Time", "NET Time", "Unix Time", "UTC Time",' +
   '"Swatch Time", "Julian Time", "Decimal Time", "Hex Time", "Radix Time", "Percent Time", "Double Time",'  +
   '"Roman Time", "Morse Time", "Flow Time", "Metric Time", "Binary Time", "BCD Time"');

  //  Load and parse the time zone data base.
  timeZone := TPascalTZ.Create;
  timeZone.DatabasePath :='tzdata';
  timeZone.ParseDatabaseFromDirectory('tzdata');

  //writeLog('End of Create');
 end;

destructor FuzzyTime.Destroy;
{  run on destroy.    }
begin
  _fuzzyTypes.free;
  timeZone.Free;

  inherited;
end;

procedure FuzzyTime.writeLog(message: string);
{  write a text message to the log file.
   The logfile is created when the class is created, it should exist.
}
VAR
  logFile: TextFile;
begin
  AssignFile(logFile, filename);

  try
    if FileExists(filename) then
      append(logFile)
    else
      rewrite(logFile);

    writeLn(LogFile, FormatDateTime('DDMMMYYYY hhnnss : ', now) + message);
    CloseFile(LogFile);
  except
    on E: EInOutError do
      ShowMessage('ERROR : Appending to Log File');
  end;
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
  //writeLog('Start of fTime');

  DecodeTime(Time, hour, mins, secs, mscs);

  if hour < 12 then
    ampm := ' in the morning'
  else
    ampm := ' in the afternoon';

  nrms := mins - (mins mod 5);       //  gets nearest five mins

  if (mins mod 5) > 2 then           // closer to next five minutes, go next
    nrms := nrms + 5;

  //writeLog('case of fTime');
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

  //writeLog(format('hour = %D nrms = %D',[hour, nrms]));

  if (hour = 12) and (nrms = 0) then   //  fix for noon.
    ampm := ' about noon'
  else if (hour = 0) and (nrms = 0) then
    ampm := ' about Midnight'
  else if (hour = 24) and (nrms = 0) then
    ampm := ' about Midnight'
  else if (hour > 12) then
  begin
    hour := hour - 12;
    if hour >= 5 then
      ampm := ' in the evening'
    else
      ampm := ' in the afternoon';
  end;

  dummy := hourTxt[hour];

  //writeLog('End of fTime');

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
  ampm: string;       //  am pm indicator, also used for afternoon or evening
  pastTo: string;     //  return string
  sRtn: string;       //  hour text
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
      1: sRtn := format('%s minute %s %s %s', [unitsTxt[mins], pastTo, hourTxt[hour], ampm ]);
      2..9: sRtn := format('%s minutes %s %s %s', [unitsTxt[mins], pastTo, hourTxt[hour], ampm ]);
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
{  returns UTC, Coordinated Universal Time, taking into acount local time zone.
   This is then encoded into a string.                                         }
begin
  Result := TimeToStr(TimeZone.UniversalTime);
end;

function FuzzyTime.localTime: string;
{  return local time, taking into acount local time zone.    }
begin
  if display24Hour then
    Result := FormatDateTime('hh : nn : ss', TimeZone.LocalTime)
  else
    Result := FormatDateTime('hh : nn : ss am/pm', TimeZone.LocalTime);
end;

function FuzzyTime.swatchTime: string;
{  returns Swatch Time - will only work in windows.
   Swatch time is made up of 1000 beats per day i.e. 1 beat = 0.1157 seconds.
   This is then encoded into a string.                                         }
var
  noOfSeconds: double;
  noOfBeats: double;
begin
  noOfSeconds := SecondOfTheDay(TimeZone.UniversalTime);

  noOfBeats := (noOfSeconds * 0.01157);    // 1000 beats per day

  if userOptions.swatchCentibeats then
    Result := format('@ %3.2f BMT', [noOfBeats])
  else
    Result := format('@ %3.f BMT', [noOfBeats]);

end;

function FuzzyTime.julianTime: string;
{  returns Julian Date Time - using DelphiMoon.     }
begin
  Result := format('%7.7f', [Julian_Date(now)]);
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

function FuzzyTime.getFlowTime: string;
{  Returns the current [local] time as Flow Time.
   Flow Time still divides the day into 24 hours, but each hour is divided
   into 100 minutes of 100 seconds.
   A Quick conversion is takes 2/3 of the minute [or second] and add it to it's self.
}
var
  hour: word;
  mins: word;
  secs: word;
  msec: word;
  m: double;
  s: double;
begin
  DecodeTime(time, hour, mins, secs, msec);

  m := mins * (5/3);
  s := secs * (5/3);

  result := format('%d %d %d', [hour, trunc(m), trunc(s)]);
end;

function FuzzyTime.getMetricTime: string;
{  Returns the current [local] time in Metric time.
   Metric time is the measure of time interval using the metric system,
   which defines the second as the base unit of time, and multiple and
   submultiple units formed with metric prefixes, such as kilo-seconds and milliseconds.
   Only Kilo-seconds are used here.
}
var
  noOfSeconds: longword;
  noOfKSeconds: double;
begin
  noOfSeconds := SecondOfTheDay(Time);
  noOfKSeconds := noOfSeconds / 1000;

  Result := format('%0.3f Kilo-Seconds', [noOfKSeconds]);
end;

function FuzzyTime.getBinaryTime: string;
{  Returns current [local] time in binary [base 2] format.
   This is only a binary representation of the current time.
}
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

  if not userOptions.display24Hour and (hrs > 12) then        //  use 12 hour klock.
    hrs -= 12;

  shrs := Dec2Numb(hrs, 5, 2);
  smin := Dec2Numb(min, 6, 2);
  ssec := Dec2Numb(sec, 6, 2);

  Result := format('%s:%s:%s', [shrs, smin, ssec]);
end;

function FuzzyTime.getBCDTime: string;
{  Returns current [local] time in Binary-Coded Decimal [base 2] format.
   This is only a binary representation of the current time.
   If below 9, still return a two bcd string i.e first digit is 0.
}
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

  if not userOptions.display24Hour and (hrs > 12) then        //  use 12 hour klock.
    hrs -= 12;

  if hrs < 9 then
    shrs := format('0000%s', [Dec2Numb(hrs, 4, 2)])
  else
    shrs := format('%s%s', [Dec2Numb(hrs div 10, 4, 2), Dec2Numb(hrs mod 10, 4, 2)]);

  if min < 9 then
    smin := format('0000%s', [Dec2Numb(min, 4, 2)])
  else
    smin := format('%s%s', [Dec2Numb(min div 10, 4, 2), Dec2Numb(min mod 10, 4, 2)]);

  if sec < 9 then
    ssec := format('0000%s', [Dec2Numb(sec, 4, 2)])
  else
    ssec := format('%s%s', [Dec2Numb(sec div 10, 4, 2), Dec2Numb(sec mod 10, 4, 2)]);

  Result := format('%s:%s:%s', [shrs, smin, ssec]);
end;

function FuzzyTime.getTime: string;
begin

  case displayFuzzy of
    0: Result := fTime;
    1: Result := wordTime;
    2: Result := localTime;
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
    15: Result := getFlowTime;
    16: Result := getMetricTime;
    17: Result := getBinaryTime;
    18: Result := getBCDTime;
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
