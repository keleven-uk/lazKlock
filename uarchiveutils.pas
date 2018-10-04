unit uArchiveUtils;

{  some helper routines to archive [load and save] data files.

   NB: All path are absolute.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Zipper, dialogs, LCLType, Controls, Forms, LazFileUtils,
  strUtils;

function getArchiveFiles(relative: boolean): TStringList;
procedure saveArchive(fname: String; archiveFiles: TStringList; relative: boolean);
procedure LoadArchive(fname: String);

implementation

uses
  formklock;


function getArchiveFiles(relative: boolean): TStringList;
{  Adds to the archive list, known files [if exist] that need to be backed up.
}
var
  optionsFile: String;
  eventFile  : String;
  memoFile   : String;
  stickyFile : String;
  unitsFile  : String;
  friendFile : String;

begin
  optionsFile := userOptions.optionsName;
  eventFile   := userOptions.eventName;
  memoFile    := userOptions.memoName;
  stickyFile  := userOptions.stickyName;
  unitsFile   := userOptions.unitsName;
  friendFile  := userOptions.friendName;

  result := TStringList.Create;

  if relative then
  begin
    if FileExists(optionsFile) then result.add(ExtractFileName(optionsFile));
    if FileExists(eventFile)   then result.add(ExtractFileName(eventFile));
    if FileExists(memoFile)    then result.add(ExtractFileName(memoFile));
    if FileExists(stickyFile)  then result.add(ExtractFileName(stickyFile));
    if FileExists(unitsFile)   then result.add(ExtractFileName(unitsFile));
    if FileExists(friendFile)  then result.add(ExtractFileName(friendFile));
  end
  else
  begin
    if FileExists(optionsFile) then result.add(optionsFile);
    if FileExists(eventFile)   then result.add(eventFile);
    if FileExists(memoFile)    then result.add(memoFile);
    if FileExists(stickyFile)  then result.add(stickyFile);
    if FileExists(unitsFile)   then result.add(unitsFile);
    if FileExists(friendFile)  then result.add(friendFile);
  end;

end;

procedure saveArchive(fname: String; archiveFiles: TStringList; relative: boolean);
{  Create a zip file [fname] and add all the files in archiveFiles.

   if relative is chosen the passed in the filenames will be relatibe pathnames.
   if absolute is chosen the passed in the filenames will be absolute pathnames.

   The fileComment of the archive is set to either relative or absolute dependinf upon choice.
   The is user on loading the archive to determine the mode.
}
var
  archiveZip      : TZipper;
  absoluteFileName: string;
  f               : integer;
  baseDir         : string;
  noError         : boolean;
begin
  klog.writeLog('Saving archive file : ' + fname);

  if fileExists(fname) then
    if QuestionDlg ('Archive File', 'Do You Really Want To overwrite the archive file',
                     mtConfirmation, [mrYes,'yes', mrNo, 'No', 'IsDefault'],'')
                    = mrNo then exit;

  noError    := true;
  archiveZip := TZipper.Create;

  try
    try
      if relative then         //  zip filename with realtive pathnames
      begin                    //  note : the fnames passed in will be relative.
        baseDir := GetAppConfigDir(False);
        for f := 0 to  archiveFiles.Count - 1 do
        begin
          absoluteFileName := baseDir + archiveFiles[f];
          archiveZip.Entries.AddFileEntry(absoluteFileName, archiveFiles[f]);
          archiveZip.FileComment := 'Klock relative';                        //  make archive smart.
        end;
      end
      else  //  if relative then
      begin
        archiveZip.Entries.AddFileEntries(archiveFiles);
        archiveZip.FileComment := 'klock absolute';                          //  make archive smart.
      end;

      archiveZip.FileName := fname;
      archiveZip.ZipAllFiles;
    except
      on E: Exception do
      begin
        noError := false;
        ShowMessage('ERROR: Saving Archive file.' + LineEnding + E.Message);
      end;
    end;
  finally
    archiveZip.Free;
  end;

  if noError then
    ShowMessage('Archive file saved.');
end;

procedure LoadArchive(fname: String);
{  Loads a zip files and extracts all the files.
   The files can either have relative or absolute pathnames.
   The archive file comment is read to determine the mode.

   Should only load valid klock generated archives.
}
var
  archiveZip: TUnZipper;
  noError   : boolean;
begin
  klog.writeLog('Loading archive file : ' + fname);

  noError    := true;
  archiveZip := TUnZipper.Create;

  try
    try
      archiveZip.FileName := fname;

      if not AnsiStartsStr(archiveZip.FileComment, 'Klock')then  //  Not a Klock generated archive, abort.
      begin
        ShowMessage('Not a Klock archive');
        exit;
      end;

      if archiveZip.FileComment = 'Klock relative' then          //  zip filename with realtive pathnames
        archiveZip.OutputPath := ExtractFilePath(fname);

      archiveZip.Examine;
      archiveZip.UnZipAllFiles;
    except
      on E: Exception do
      begin
        noError := false;
        ShowMessage('ERROR: Loading Archive file.' + LineEnding + E.Message);
      end;
    end;
  finally
    archiveZip.Free;
  end;

  if noError then
  begin
    klog.writeLog('Reading options file');  //  In case loaded options is different
    userOptions.readOptions;                //  from current ones.
    ShowMessage('Archive file loaded.');
  end;

end;

end.



