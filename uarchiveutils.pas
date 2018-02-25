unit uArchiveUtils;

{  some helper routines to archive [load and save] data files.

   NB: All path are absolute.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Zipper, dialogs, LCLType, Controls, Forms;

function getArchiveFiles: TStringList;
procedure saveArchive(fname: String; archiveFiles: TStringList);
procedure LoadArchive(fname: String);

implementation

uses
  formklock;


function getArchiveFiles: TStringList;
{  Adds to the archive list, known files [if exist] that need to be backed up.
}
var
  optionsFile: String;
  memoFile: String;
  stickyFile: String;
  unitsFile: String;
  reminderFile: String;

begin
  optionsFile := userOptions.optionsName;
  memoFile := userOptions.memoName;
  stickyFile := userOptions.stickyName;
  unitsFile := userOptions.unitsName;
  reminderFile := userOptions.reminderName;

  result := TStringList.Create;

  result.add('Fonts Directory');

  if FileExists(optionsFile) then
    result.add(optionsFile);
  if FileExists(memoFile) then
    result.add(memoFile);
  if FileExists(stickyFile) then
    result.add(stickyFile);
  if FileExists(unitsFile) then
    result.add(unitsFile);
  if FileExists(reminderFile) then
    result.add(reminderFile);
end;

procedure saveArchive(fname: String; archiveFiles: TStringList);
{  Create a zip file [fname] and add all the files in archiveFiles.    }
var
  archiveZip: TZipper;
begin
  klog.writeLog('Saving archive file : ' + fname);

  if fileExists(fname) then
    if QuestionDlg ('Archive File', 'Do You Really Want To overwrite the archive file',
                     mtConfirmation, [mrYes,'yes', mrNo, 'No', 'IsDefault'],'')
                    = mrNo then exit;

  archiveZip := TZipper.Create;

  try
    archiveZip.FileName := fname;
    archiveZip.Entries.AddFileEntries(archiveFiles);
    archiveZip.ZipAllFiles;
  finally
    archiveZip.Free;
  end;
  ShowMessage('Archive file saved.');
end;

procedure LoadArchive(fname: String);
{  Loads a zip files and extracts all the files.
   The files where compressed with absolute file names, so the files will
   be placed in the same directory as the came from.
}
var
  archiveZip: TUnZipper;
begin
  klog.writeLog('Loading archive file : ' + fname);
  archiveZip := TUnZipper.Create;

  try
    archiveZip.FileName := fname;
    archiveZip.Examine;
    archiveZip.UnZipAllFiles;
  finally
    archiveZip.Free;
  end;

  klog.writeLog('Reading options file');  //  In case loaded options is different
  userOptions.readOptions;                //  from current ones.
  ShowMessage('Archive file loaded.');
end;

end.



