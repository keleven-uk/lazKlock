unit ULogging;
{  A simple logging class.

   kLog := Logger.Create; - to start.
   kLog.writeLog(message: string); - to write a log message.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, LazFileUtils, DateUtils, ShellApi;

type
  Logger = class

    private
      _filename: string;       //  filename of the log file
      _dirname : string;       //  directory where the log file lives.
      _handle  : QWord;        //  handle of main form.

      property handle: QWord read _handle write _handle;

      procedure deleteToRycycle(aFile: string);
    public
      property filename: string read _filename write _filename;
      property dirname : string read _dirname  write _dirname;

      constructor Create;
      procedure writeLog(message: string);
      procedure readLogFile(logFiles:TStringlist);
      procedure cullLogFile(CullLogsDays: Integer);
  end;

implementation

constructor Logger.Create;
{  Creates the logger.
   A new logfile is created each day.
   If the logfile does not exist then create else append.

   GetAppConfigDir(False) -> c:\Users\<user>\AppData\Local\<app Name>\
   GetAppConfigDir(True)  -> c:\ProgramData\<app Name>\
}
VAR
  logFile: TextFile;
begin
  dirname  := GetAppConfigDir(False);
  filename := dirname + 'klog_' + FormatDateTime('DDMMMYYYY', now) + '.log';

  try
    AssignFile(logFile, filename);
    try
      if FileExists(filename) then
            append(logFile)
          else
            rewrite(logFile);

          writeLn(LogFile, FormatDateTime('DDMMMYYYY hhnnss : ', now) + 'Log Created');
    finally
      CloseFile(LogFile);
    end;

  except
    on E: EInOutError do
      ShowMessage('ERROR : Creating Log File');
  end;
end;

procedure Logger.writeLog(message: string);
{  write a text message to the log file.
   The logfile is created when the class is created, it should exist.
}
VAR
  logFile: TextFile;
begin
  try
    AssignFile(logFile, filename);
    try
      if FileExists(filename) then
      append(logFile)
    else
      rewrite(logFile);

    writeLn(LogFile, FormatDateTime('DDMMMYYYY hhnnss : ', now) + message);

    finally
      CloseFile(LogFile);
    end;

  except
    on E: EInOutError do
      ShowMessage('ERROR : Appending to Log File');
  end;
end;

procedure Logger.readLogFile(logFiles:TStringlist);
{  Scans for all log files in user data area.    }
begin
  FindAllFiles(logFiles, dirname, '*.log', True);
end;

procedure Logger.cullLogFile(CullLogsDays: Integer);
{  Will delete [to recycle bin] all log files older then a specified time, if switched on.  }
var
  logFiles:TStringlist;
  logFile : string;
  logDate :TDateTime;
  logAge  :longInt;
begin
  logFiles := TstringList.Create;

  try
    readLogFile(logFiles);       //  read in all log files.
    for logFile in logFiles do
        begin
          logDate := FileDateTodateTime(FileAgeUTF8(logFile));
          logAge  := DaysBetween(Now, logDate);

          if logAge > CullLogsDays then
          begin
            deleteToRycycle(logFile);
            writeLog(format('Deleting %S with age %D  %S', [logfile, logAge, FormatDateTime('DD MMM YYYY', logDate)]));
          end;

        end;  //  for logFile in logFiles do
  finally
    freeandnil(logFiles);
  end;

end;

procedure Logger.deleteToRycycle(aFile: string);
{  Deletes files to the Recycle bin.
    Thanks to Lush - http://forum.lazarus-ide.org/index.php?topic=12288.0

    FOF_ALLOWUNDO -> moves file to the bin.
    FOF_SILENT -> deletes the file permanently.
    Add FOF_NOCONFIRMATION to any of the previous constants to disable the "are you sure" dialog box.

    NB : Seems to ignore directories at the moment.
}

var
  fileOpStruct: TSHFileOpStruct;
begin
  with fileOpStruct do
  begin
    Wnd           := handle;
    wFunc         := FO_DELETE;
    pFrom         := PChar(aFile + #0#0);
    pTo           := nil;
    hNameMappings := nil;

    fFlags := FOF_ALLOWUNDO;                    //  Use recycle bin.
    fFlags := fFlags or FOF_NOCONFIRMATION;
  end;

  try
    SHFileOperation(fileOpStruct);
  except
    on E: Exception do
      writeLog(E.Message);
  end;

end;

end.

