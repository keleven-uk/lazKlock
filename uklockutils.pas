unit UKlockUtils;

{ A collection of useful stuff used else where in Klock.    }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, Process, formAnalogueKlock,
  MMSystem, dateutils, registry, typinfo, LCLVersion, strutils, Windows, Graphics,
  formLEDKlock, formBinaryKlock, formSmallTextKlock, DCPrijndael, DCPsha256, Moon,
  MouseAndKeyInput, LCLType, formFloatingKlock;

type
  //  Used for hour chimes file name.  Zero not used, inserted as a dummy to keep count correct.
  chimes = (zero, one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve);
var
  isPlaying: Boolean = False;

function FontToString(f: TFont): string;
function StringToFont(s: string): TFont;
function getTextFont(f1: TFont; f2: TFont): TFont;
procedure doSystemEvent(event: integer);
procedure abortSystemEvent;
procedure doCommandEvent(command: string; args: string);
procedure displayHelp(chm: string; topic: string);
procedure doPlaySound(sound: string; volume: string);
procedure SendMCICommand(command: string);
procedure applyRunAtStartUp(flag: boolean);
procedure playChime(mode: String);
procedure logHeader;
procedure logMessage(message: string);
procedure logFooter;
procedure logSplashFooter;
function getUpTime(system: string): string;
function getWindowsVersion: string;
function everyMinute(myNow: TdateTime; mins: integer): Boolean;
function isMinute(myNow: TdateTime; mins: integer): Boolean;
function isChristmas: Boolean;
function isEaster: Boolean;
function isValentines: Boolean;
function isHalloween: Boolean;
procedure KillOtherKlocks;
function encrypt(s: string; pwd: string): string;
function decrypt(s: string; pwd: string): string;
procedure pressF15;
procedure jiggleMouse;
procedure keepMonitorAwake;


implementation

uses
  formklock, formSplashScreen;


function FontToString(f: TFont): string;
{  Produces a string representation of a given font.
   All the font attributes are converted to strings and packed together.

   Does not yet handle errors - will there be any ;o)                                              }
var
  rtnStr: string;
begin
  rtnStr := '';
  rtnStr := rtnStr + IntToStr(f.Charset) + ':';
  rtnStr := rtnStr + ColorToString(f.Color) + ':';
  rtnStr := rtnStr + IntToStr(f.Height) + ':';
  rtnStr := rtnStr + (f.Name) + ':';
  rtnStr := rtnStr + IntToStr(f.Orientation) + ':';
  rtnStr := rtnStr + IntToStr(f.size) + ':';
  rtnStr := rtnStr + IntToStr(Ord(f.Pitch)) + ':';
  rtnStr := rtnStr + IntToStr(Ord(f.Quality)) + ':';

  //                             TFontStyles is a set and scanned differently.
  if fsBold in f.Style then
    rtnStr := rtnStr + 'B'
  else
    rtnStr := rtnStr + '.';

  if fsItalic in f.Style then
    rtnStr := rtnStr + 'I'
  else
    rtnStr := rtnStr + '.';

  if fsStrikeOut in f.Style then
    rtnStr := rtnStr + 'S'
  else
    rtnStr := rtnStr + '.';

  if fsUnderline in f.Style then
    rtnStr := rtnStr + 'U'
  else
    rtnStr := rtnStr + '.';


  Result := rtnStr;
end;

function StringToFont(s: string): TFont;
{  Produces a font from a given string representation [produced by FonttoString.
   The string is read a bit at at time [up to the next :] and this is converted
   to the given font attribute.

   NB :: pos starts from position 1, where copy start at position 1.
         TFontStyles is a set and scanned differently.

   Does not yet handle errors - will there be any ;o)                                              }
var
  p   : integer;
  err : integer;
  chrs: integer;
  clr : TColor;
  Hght: integer;
  nme : string;
  Ortn: integer;
  sze : integer;
  ptch: integer;
  qlty: integer;

  fnt : TFont;
  fstyles: TFontStyles;
begin
  fnt := TFont.Create;
  fstyles := [];             //  empty set ??

  p := Pos(':', s);                                   //  Character set of font
  val(copy(s, 0, p - 1), chrs, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  p   := Pos(':', s);                                //  colour of font
  clr := StringToColor(copy(s, 0, p - 1));
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  height of font
  val(copy(s, 0, p - 1), Hght, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  p   := Pos(':', s);                                //  name of font
  nme := copy(s, 0, p - 1);
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  orientation of font
  val(copy(s, 0, p - 1), Ortn, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  size of font
  val(copy(s, 0, p - 1), sze, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  p := Pos(':', s);                                  //  pitch of font
  val(copy(s, 0, p - 1), ptch, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  p := Pos(':', s);                                 //  quality of font
  val(copy(s, 0, p - 1), qlty, err);
  //if err = 0 then klog.writeLog(format('StringToFont error : %d', [err]));
  Delete(s, 1, p);

  if Pos('B', s) <> 0 then include(fstyles, fsBold);
  if Pos('I', s) <> 0 then include(fstyles, fsItalic);
  if Pos('S', s) <> 0 then include(fstyles, fsStrikeOut);
  if Pos('U', s) <> 0 then include(fstyles, fsUnderline);

  fnt.Charset     := chrs;                 //  Character set of font
  fnt.Color       := clr;                  //  colour of font
  fnt.Height      := Hght;                 //  height of font
  fnt.Name        := nme;                  //  name of font
  fnt.Orientation := Ortn;                 //  orientation of font
  fnt.Size        := sze;                  //  size of font
  fnt.Pitch       := TFontPitch(ptch);     //  pitch of font
  fnt.Quality     := TFontQuality(qlty);   //  quality of font
  fnt.Style       := fstyles;

  Result := fnt;
end;


function getTextFont(f1: TFont; f2: TFont): TFont;
{  takes two fonts, global[f2] and local[f1] and returns one according the rule.
   If the local is default return the local, else return the global.           }
begin
  if f1.IsDefault then
    Result := f2  //  use local colour
  else
    Result := f1;  //  use global colour
end;

procedure doSystemEvent(event: integer);
{  called to do a system event.
         0 = shut down PC
         1 = reboot PC
         2 = Hibernate PC
         3 = log off current user

    NB :: if cross platform, shutdown command would need changing.

    It seems that things changed a little in windows 10, a little.
    The parameters now start with a / and not a -.
    Also, passing parameters with contain spaces seem to be problematic for Lazarus,
    Needs more work to make compatible with older versions of windows, to include a message and
    make cross platform.
}
var
  AProcess: TProcess;
begin
  AProcess := TProcess.Create(nil);

  case event of
    0: AProcess.Parameters.Add('/s');
    1: AProcess.Parameters.Add('/r');
    2: AProcess.Parameters.Add('/h');
    3: AProcess.Parameters.Add('/l');
  end;

  try
    AProcess.Options    := [poWaitOnExit];
    AProcess.Executable := 'shutdown';
    AProcess.Execute;
  finally
    AProcess.Free;
  end;
end;

procedure abortSystemEvent;
{  tries to abort a system event - i.e. shutdown                               }
begin
  with TProcess.Create(nil) do
    try
      Executable := 'shutdown';
      Parameters.Add('/a');
      Execute;
    finally
      Free;
    end;    //  with TProcess.Create(nil)
end;

procedure doCommandEvent(command: string; args: string);
{  called to execute a command, which hopefully is in command.
   NB  :: does not check if the command is really executable.
}
begin
  if command <> '' then
  begin
    with TProcess.Create(nil) do
      try
        Executable := command;
        Parameters.Add(args);
        //Options := Options + [poWaitOnExit];
        Execute;
      finally
        Free;
      end;    //  with TProcess.Create(nil)
  end       //  if command <> ''
  else
    ShowMessage('Even Klock cant run with nowt!!');
end;

procedure displayHelp(chm: string; topic: string);
{  called to display a chm help file, this is passed to hh.exe.
   I would use lhelp.exe which comes with Lazarus, but seems buggy.
}
begin
  If FileExists(chm) Then
  begin
    with TProcess.Create(nil) do
      try
        Executable := 'hh.exe';
        Parameters.Add(format('%s::%s', [chm, topic]));
        Execute;
      finally
        Free;
     end;
  end
  else
    showmessage('ERROR :: Loading ' + chm);
end;

procedure doPlaySound(sound: string; volume: string);
{  Plays a sound file at a specified volume, both passed in has strings.
   The file is assumed to be a valid sound file and volume is between 1 - 1000.
   The correct path is attached by this routine.

   TODO :: Work out how status works, so that it can be determined when play has finished.
           This would then eliminate the global variable isPlying.

   Fails silently if sound file does not exist.
}
VAR
  soundFile: String;
begin
  soundFile := ExtractFilePath(Application.ExeName) + '\sounds\' + sound;

  If FileExists(soundFile) Then
  begin
    if isPlaying then
    begin
      SendMCICommand('close KlockAudio');                       //  close player if alrady been used.
      isPlaying := false;
    end;

    isPlaying := True;
    SendMCICommand('open ' + soundFile + ' type mpegvideo alias KlockAudio');      //  open audio player.

    SendMCICommand('setaudio KlockAudio volume to ' + volume);                     //  Sets volume [1 - 1000].

    //  SendMCICommand('seek KlockAudio to nil');              //  finds starts of  file, not used.

    SendMCICommand('play KlockAudio');                         //  Play audio file.

    //  SendMCICommand('status KlockAudio mode')               //  supposed to return status of play.
  end;
end;

procedure SendMCICommand(command: string);
{  pinched from http://www.programmersforum.ru/showthread.php?t=133180}
var
  returnValue: integer;
  errorMessage: array [0 .. 254] of char;
begin
  returnValue := mciSendString(PChar(command), nil, 0, 0);
  if returnValue <> 0 then
  begin
    { get message for returned value }
    mciGetErrorString(returnValue, errorMessage, 255);
    ShowMessage(errorMessage);
  end;
end;

procedure applyRunAtStartUp(flag: boolean);

const
  rootPath = HKEY_CURRENT_USER;
  regpath  = '\Software\Microsoft\Windows\CurrentVersion\run';
var
  registry  : TRegistry;
  AppPath   : string;
  AppName   : string;
  openResult: boolean;
begin
  registry         := TRegistry.Create;
  registry.RootKey := rootpath;
  registry.Access  := KEY_WRITE;

  AppPath := Application.ExeName;
  AppName := ExtractFileName(AppPath);

  try
    openResult := registry.OpenKey(regpath, True);

    if not openResult then
      ShowMessage('Error in opening key')
    else
    begin
      if flag then
        registry.WriteString(AppNAme, AppPath)
      else
        registry.DeleteValue(AppNAme);
    end;

    registry.CloseKey;
  finally
    registry.Free;
  end;  //  try
end;

function isMinute(myNow: TdateTime; mins: integer): Boolean;
{  Returns true if the current minutes match the supplied minutes.
   i.e. true n minutes past the hour.
}
Var
  hour, minute, second, milliSeconds: word;
begin
  DecodeTime(myNow, hour, minute, second, milliSeconds);
  result := (minute = mins) and (second = 0);
end;

function everyMinute(myNow: TdateTime; mins: integer): Boolean;
{  Returns true if the current minutes is a multiple of the supplied minute.
   i.e. true every n minutes.
}
Var
  hour, minute, second, milliSeconds: word;
begin
  DecodeTime(myNow, hour, minute, second, milliSeconds);
  result := (minute mod mins = 0) and (second = 0);
end;

procedure playChime(mode: String);
{  When called,play a requested chime.     }
var
  hour, minute, second, milliSeconds: word;
  arg: string;
begin
  DecodeTime(Time, hour, minute, second, milliSeconds);

  if hour > 12 then
    hour := hour - 12;

  case mode of
    'pips'        : arg := 'thepips.mp3';
    'hour'        : arg := GetEnumName(TypeInfo(chimes), hour) + '.mp3';
    'half'        : arg := 'halfchime.mp3';
    'quarter'     : arg := 'quarterchime.mp3';
    'threequarter': arg := 'threequarterchime.mp3';
  else ;
    arg := '';
  end;

  doPlaySound(arg, userOptions.volume);
end;

procedure logHeader;
{  Write header information to log file.    }
begin
  kLog.writeLog('............................................................');
  kLog.writeLog(userOptions.InternalName);
  kLog.writeLog('User : ' + SysUtils.GetEnvironmentVariable('USERNAME'));  // not the one in windows.
  kLog.writeLog('PC   : ' + SysUtils.GetEnvironmentVariable('COMPUTERNAME'));
  kLog.writeLog('OS   : ' + getWindowsVersion);
  kLog.writeLog(format('lazKlock Build   :: %s', [userOptions.productVersion]));
  kLog.writeLog(format('lazKlock Version :: %s', [userOptions.fileVersion]));
  {$ifdef WIN32}
    kLog.writeLog(format('Built with 32 bit Lazarus Version :: %s', [lcl_version]));
  {$else}
    kLog.writeLog(format('Built with 64 bit Lazarus Version :: %s', [lcl_version]));
  {$endif}
  kLog.writeLog('App Dir : ' + ExtractFilePath(Application.ExeName));
  kLog.writeLog('............................................................');

  if (frmSplashScreen <> nil) then
  begin
    frmSplashScreen.MemoSplashScreenData.Lines.Append(userOptions.InternalName);
    frmSplashScreen.MemoSplashScreenData.Lines.Append('User : ' + SysUtils.GetEnvironmentVariable('USERNAME'));
    frmSplashScreen.MemoSplashScreenData.Lines.Append('PC   : ' + SysUtils.GetEnvironmentVariable('COMPUTERNAME'));
    frmSplashScreen.MemoSplashScreenData.Lines.Append('OS   : ' + getWindowsVersion);
    frmSplashScreen.MemoSplashScreenData.Lines.Append(format('lazKlock Build   :: %s', [userOptions.productVersion]));
    frmSplashScreen.MemoSplashScreenData.Lines.Append(format('lazKlock Version :: %s', [userOptions.fileVersion]));
    {$ifdef WIN32}
      frmSplashScreen.MemoSplashScreenData.Lines.Append(format('Built with 32 bit Lazarus Version :: %s', [lcl_version]));
    {$else}
      frmSplashScreen.MemoSplashScreenData.Lines.Append(format('Built with 64 bit Lazarus Version :: %s', [lcl_version]));
    {$endif}
    frmSplashScreen.MemoSplashScreenData.Lines.Append('App Dir : ' + ExtractFilePath(Application.ExeName));
  end;
end;

procedure logMessage(message: string);
{  Write a message to the log file and the splash file.
   Should only really be used when the splash screen could be open i.e. when
   the application is starting or finishing.  Was created when there was a fault
   logging such messages when the fonts where being loaded and removed.
}
begin
  klog.writeLog(message);
  if (frmSplashScreen <> nil) then
    frmSplashScreen.MemoSplashScreenInfo.Lines.Add(message);
end;

procedure logFooter;
{  Write footer to log file.    }
begin
  kLog.writeLog('..............................................................');
  kLog.writeLog('System has been running for ' + getUpTime('System'));
  kLog.writeLog('Klock has been running for  ' + getUpTime('Application'));
  kLog.writeLog('Klock Ending [normaly]');
  klog.writeLog('Bye');
  kLog.writeLog('..............................................................');
end;

procedure logSplashFooter;
{  Write footer to Splash Screen.    }
begin
  frmSplashScreen.MemoSplashScreenData.Lines.Add(userOptions.InternalName);
  frmSplashScreen.MemoSplashScreenData.Lines.Add('..............................................................');
  frmSplashScreen.MemoSplashScreenData.Lines.Add('System has been running for ' + getUpTime('System'));
  frmSplashScreen.MemoSplashScreenData.Lines.Add('Klock has been running for  ' + getUpTime('Application'));
  frmSplashScreen.MemoSplashScreenData.Lines.Add('Klock Closing Down [normaly]');
  frmSplashScreen.MemoSplashScreenData.Lines.Add('..............................................................');
end;

function getUpTime(system: string): string;
{  Determines the up time of either System or Application - depending on argument S or A.
   Used in formAbout & uKlockUtils.

   appStartTime := GetTickCount64; needs to be run when the app starts.

   NOTE :: Windows Only - use LclIntf.GetTickCount for cross platform.

   TODO : Need to check for roll over and account for it.
}
var
  noTicks    : int64;
  noSeconds  : integer;
  noOfDays   : integer;
  noOfHours  : integer;
  noOfMinutes: integer;
  noOfSeconds: integer;
begin
  system := AnsiLowerCase(system);

  if AnsiStartsStr('s', system) then
    noTicks := GetTickCount64                     //  How long has the system been running.
  else
    noTicks := GetTickCount64 - appStartTime;     //  How long has the application been running.

  noSeconds := noTicks div 1000;                  //  1000 ticks per second.

  noOfDays    := noSeconds div 86400;
  noSeconds   := noSeconds - (noOfDays * 86400);
  noOfHours   := noSeconds div 3600;
  noSeconds   := noSeconds - (noOfHours * 3600);
  noOfMinutes := noSeconds div 60;
  noSeconds   := noSeconds - (noOfMinutes * 60);
  NoOfSeconds := noSeconds;

  Result := format('%d days : %d hours : %d mins : %d secs', [noOfDays, noOfHours, noOfMinutes, noOfSeconds]);
end;

function getWindowsVersion: string;
{  Gets the version of the windows OS.
   This is achieved by capturing the output from the DOS command ver.
   The ver command is run  into a file and then the file is read back.
}
VAR
  AProcess   : TProcess;
  winVer     : TStringList;
  tmpWinVer  : string;
  tmpFileName: string;
begin
  tmpFileName := GetTempDir(true) + 'ver.txt';

  AProcess            := TProcess.Create(nil);
  AProcess.Options    := [poWaitOnExit, poUsePipes];
  AProcess.Executable := 'CMD ';
  AProcess.Parameters.Add('/C ver >' + tmpFileName);
  AProcess.Execute;

  winVer := TStringList.Create;
  winVer.LoadFromFile(tmpFileName);
  tmpWinVer := winVer[1];

  AProcess.Free;
  winVer.Free;

  Result := tmpWinVer;
end;

function isChristmas: Boolean;
{  Returns true if current date is within 12 days of Christmas day.    }
VAR
  year, month, day: word;
  chritmasDay: TDateTime;
  currentYear: integer;
begin
  DecodeDate(Date, year, month, day);

  if month < 7 then                       //  if in first half of year, compare
    currentYear := year - 1               //  agiinst christmas of last year.
  else
    currentYear := year;

  chritmasDay := EncodeDate(currentYear, 12, 25);

  if DaysBetween(chritmasDay, Date) < 13 then
    Result := true
  else
    Result := False;
end;

function isEaster: Boolean;
{  Returns true if current date is within one week of Easter Sunday.
   Easter date is form the DelphiMoon component.
}
VAR
  easter: TdateTime;
begin
  easter := EasterDate(currentYear);  //  return date of Easter Sunday.

  if DaysBetween(easter, Date) < 7 then
    Result := true
  else
    Result := False;
end;

function isValentines: Boolean;
{  Returns true if current date is Valentines Day.    }
VAR
  valantimnesDay: TdateTime;
begin
  valantimnesDay := Encodedate(currentYear, 02, 14);

  if valantimnesDay = Date then
      Result := true
  else
    Result := False;
end;

function isHalloween: Boolean;
{  Returns true if current date is Halloween.    }
VAR
  halloweenDay: TdateTime;
begin
  halloweenDay := Encodedate(currentYear, 10, 31);

  if halloweenDay = Date then
      Result := true
  else
    Result := False;
end;

procedure KillOtherKlocks;
{  Kill any other klocks that are visible.    }
begin
  if frmAnalogueKlock.Visible  then frmAnalogueKlock.Visible  := False;
  if frmLEDKlock.Visible       then frmLEDKlock.Visible       := False;
  if frmBinaryKlock.Visible    then frmBinaryKlock.Visible    := False;
  if frmSmallTextKlock.Visible then frmSmallTextKlock.Visible := False;
  if frmFloatingKlock.Visible  then frmFloatingKlock.Visible  := false;
end;
//
//...................................... encrypt and decrypt ...................
//
function encrypt(s: string; pwd: string): string;
{  Encrypts a stings using a pasword pwd.

   see https://forum.lazarus.freepascal.org/index.php?topic=33013.0
}
var
  Crypt  : TDCP_rijndael;
  EncText: string;
begin
  Crypt := TDCP_rijndael.Create(nil);
  try
    // encrypt string using password
    Crypt.InitStr(Pwd, TDCP_sha256);
    EncText := Crypt.EncryptString(s);
  finally
    FreeAndNil(Crypt);
  end;

  result := EncText;
end;

function decrypt(s: string; pwd: string): string;
{  Decrypts a stings using a password pwd.
   The string must of been encrypted with the same password using the above
   encrypt function.
}
var
  Crypt  : TDCP_rijndael;
  DecText: string;
begin
  Crypt := TDCP_rijndael.Create(nil);

  try
    // now decrypt it to show it works
    Crypt.InitStr(Pwd, TDCP_sha256);
    DecText := Crypt.DecryptString(s);
  finally
    FreeAndNil(Crypt);
  end;

  result := DecText;
end;

procedure pressF15;
{  This simulates the pressing of the <CTRL F15> key.
   This is used to keep the monitor awake i.e. not going into sleep mode.

   <CTRL F15> should be reconised by most systems, but is rarely used in applications.
                                                   and does not appear on most keyboards.
}
begin
  KeyInput.Apply([ssCtrl]);
  KeyInput.Press(VK_F15);                // This will simulate press of <CTRL F15> key.
  KeyInput.Unapply([ssCtrl]);
end;

procedure jiggleMouse;
{  This jiggles the mouse, it moves the mouse one poixel and then back again.
    This is used to keep the monitor awake where <CTRL F15> can't be used.
}
begin
  MouseInput.MoveBy([], 1, 1, 0);
  MouseInput.MoveBy([], -1, -1, 0);
end;

procedure keepMonitorAwake;
begin
  if userOptions.keepMonitorAwakeF15 then
    pressF15
  else
    jiggleMouse;
end;

end.
