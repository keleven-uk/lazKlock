unit formOptions;

{  This form allows the amendment of all user options.
   The user options are held in the class Options which is held in uOptions.pas.
   The main user options is created withing formKlock.pas when the application is first started.
   This unit creates a secondary options objects to hold changes made wile the form is active.
   If ok is pressed the secondary options object with the changes are copied to the main
   options and the form is closed.
   If cancel is pressed, the form is closed and the changes are lost.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ECAccordion, Forms, Controls, Dialogs,
  UKlockUtils, Graphics, StdCtrls, ButtonPanel, Buttons, ComCtrls, ExtCtrls,
  Spin, ColorBox, uOptions;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    accItemGlobal: TAccordionItem;
    accItemLogging: TAccordionItem;
    accItemTime: TAccordionItem;
    accItemOtherKlocks: TAccordionItem;
    accOtherStuff: TAccordionItem;
    btrOptionsReset: TButton;
    btnGlobalVolumeTest: TButton;
    ButtonPanel1: TButtonPanel;
    ChckBxCullLogsFiles: TCheckBox;
    ChckBxLogging: TCheckBox;
    ChckGrpTimeOptions: TCheckGroup;
    ChckGrpTimerSettings: TCheckGroup;
    ChckGrpGlobalOptions: TCheckGroup;
    ChckGrpTimeChimes: TCheckGroup;
    ChckGrpAnalogueKlock: TCheckGroup;
    CmbBxDefaulTtab: TComboBox;
    CmbBxDefaultTime: TComboBox;
    AcrdnOptions: TECAccordion;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    lblCullFileDays: TLabel;
    lblSettingsFileName: TLabel;
    Label3: TLabel;
    LstBxLogFiles: TListBox;
    Settings: TGroupBox;
    SpnEdtCullDays: TSpinEdit;
    TrckBrGlobalVolume: TTrackBar;
    procedure btnGlobalVolumeTestClick(Sender: TObject);
    procedure btrOptionsResetClick(Sender: TObject);
    procedure ChckBxCullLogsFilesChange(Sender: TObject);
    procedure ChckBxLoggingChange(Sender: TObject);
    procedure ChckGrpAnalogueKlockItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpGlobalOptionsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimeChimesItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimeOptionsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimerSettingsItemClick(Sender: TObject; Index: integer);
    procedure CmbBxDefaulTtabChange(Sender: TObject);
    procedure CmbBxDefaultTimeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure TrckBrGlobalVolumeChange(Sender: TObject);
  private

  public

  end;


var
  frmOptions: TfrmOptions;
  userBacOptions: Options;          //  holds all the user options.

implementation

{$R *.lfm}

uses
  formklock;

{ TfrmOptions }

{....................................... Form Routines ......................................................}

procedure TfrmOptions.FormCreate(Sender: TObject);
{  Run things when the form is first created.  }
begin
  kLog.writeLog('FormOptions Create');

  //  Create tmp options file as c:\Users\<user>\AppData\Local\<app Name>\OptionsXX_tmp.xml
  //  This is used to store local amendments and only copied to the main options files
  //  if the OK button is clicked.
  {$ifdef WIN32}
    userBacOptions := Options.Create('Options32_temp.xml');
  {$else}
    userBacOptions := Options.Create('Options64_temp.xml');
  {$endif}

  lblSettingsFileName.Caption := userOptions.optionsName;
end;

procedure TfrmOptions.FormActivate(Sender: TObject);
{  Run things when ever the form is shown.

   See ChckGrpTimeOptionsItemClick for what the check Timer group index means.
   See ChckGrpTimerSettingsItemClick for what the check Timer group index means.
   See ChckGrpGlobalOptionsItemClick for what the check Global group index means.
}
var
  logFiles:TStringlist;
begin
  kLog.writeLog('FormOptions Activate');
  userBacOptions.Assign(userOptions);                       //  make a copy of the current user options
  // userBacOptions.writeCurrentOptions;                    //  write the copy back to disk.

  AcrdnOptions.ItemIndex := 0;                              //  Always start on Global Options.
  CmbBxDefaulTtab.ItemIndex := userBacOptions.defaultTab;

  CmbBxDefaultTime.Items.AddStrings(ft.fuzzyTypes);
  CmbBxDefaultTime.ItemIndex := userBacOptions.defaultTime;

  TrckBrGlobalVolume.Position := StrToInt(userBacOptions.volume);

  ChckGrpGlobalOptions.Checked[0] := userBacOptions.screenSave;
  ChckGrpGlobalOptions.Checked[1] := userBacOptions.runAtStartUp;

  ChckGrpTimeOptions.Checked[0] := userBacOptions.display24Hour;
  ChckGrpTimeOptions.Checked[1] := userBacOptions.netTimeSeconds;
  ChckGrpTimeOptions.Checked[2] := userBacOptions.swatchCentibeats;
  ChckGrpTimeOptions.Checked[3] := userBacOptions.fuzzyTimeBalloon;
  ChckGrpTimeOptions.Checked[4] := userBacOptions.displayIdleTime;

  ChckGrpTimeChimes.Checked[0] := userBacOptions.hourPips;
  ChckGrpTimeChimes.Checked[1] := userBacOptions.hourChimes;
  ChckGrpTimeChimes.Checked[2] := userBacOptions.halfChimes;
  ChckGrpTimeChimes.Checked[3] := userBacOptions.quarterChimes;
  ChckGrpTimeChimes.Checked[4] := userBacOptions.threeQuarterChimes;

  ChckGrpTimeChimes.CheckEnabled[1] := not ChckGrpTimeChimes.Checked[0];
  ChckGrpTimeChimes.CheckEnabled[2] := not ChckGrpTimeChimes.Checked[0];
  ChckGrpTimeChimes.CheckEnabled[3] := not ChckGrpTimeChimes.Checked[0];
  ChckGrpTimeChimes.CheckEnabled[4] := not ChckGrpTimeChimes.Checked[0];

  ChckGrpAnalogueKlock.Checked[0] := userBacOptions.analogueScreenSave;

  ChckGrpTimerSettings.Checked[0] := userBacOptions.timerMilliSeconds;

  ChckBxLogging.Checked := userBacOptions.logging;
  SpnEdtCullDays.Visible := ChckBxCullLogsFiles.Checked;
  lblCullFileDays.Visible := ChckBxCullLogsFiles.Checked;

  logFiles := TstringList.Create;             //  Scan for log files and load listbox.
    try
    kLog.readLogFile(logFiles);
    LstBxLogFiles.Items.Assign(logFiles) ;
  finally
      freeandnil(logFiles);
  end;
end;

procedure TfrmOptions.btrOptionsResetClick(Sender: TObject);
{  reset user settings to system default

   NOTE :: maybe should have a confirm dialog.
}
begin
  userOptions.writeDefaultOptions;
end;

{....................................... Options Routines ......................................................}
//
//....................................................................GLOBAL ....................................
//
procedure TfrmOptions.CmbBxDefaulTtabChange(Sender: TObject);
{  The default tab has changed, relect is user options.  }
begin
  userBacOptions.defaultTab := CmbBxDefaulTtab.ItemIndex;
end;

procedure TfrmOptions.ChckGrpGlobalOptionsItemClick(Sender: TObject; Index: integer);
{  Sets the use Global options according to the state of the radio group.

   index 0 - Save Screen Position
   index 1 - Run Kock on start up - HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\run
}
begin
  userBacOptions.screenSave := ChckGrpGlobalOptions.Checked[0];
  userBacOptions.runAtStartUp := ChckGrpGlobalOptions.Checked[1];
end;

procedure TfrmOptions.TrckBrGlobalVolumeChange(Sender: TObject);
{  Sets the global volume.
   NB  :: is held as string - easier in calling programme.
}
begin
  userBacOptions.volume := intToStr(TrckBrGlobalVolume.Position);
end;

procedure TfrmOptions.btnGlobalVolumeTestClick(Sender: TObject);
{  Plays a sound to chech volume.  }
begin
  doPlaySound('thepips.mp3', userBacOptions.volume);
end;
//
//....................................................................TIME ....................................
//
procedure TfrmOptions.ChckGrpTimeOptionsItemClick(Sender: TObject; Index: integer);
{  Sets the use Time options according to the state of the radio group.

index 0 - Display using 24 hour if true, else use 12 hour
      1 - New Earth Time to display seconds
      2 - SwatchTime to display Centibeats
      3 - Display Time in balloon
      4 - Display system idle time
}
begin
  userBacOptions.display24Hour := ChckGrpTimeOptions.Checked[0];
  userBacOptions.netTimeSeconds := ChckGrpTimeOptions.Checked[1];
  userBacOptions.swatchCentibeats := ChckGrpTimeOptions.Checked[2];
  userBacOptions.fuzzyTimeBalloon := ChckGrpTimeOptions.Checked[3];
  userBacOptions.displayIdleTime := ChckGrpTimeOptions.Checked[4];
end;

procedure TfrmOptions.ChckGrpTimeChimesItemClick(Sender: TObject; Index: integer);
{  Sets the Ciming options according to the state of the radio group.

index 0 - Sound "The Pips on the Hour"
   1 - Hourly Chimes
   2 - Half hourly Chimes
   3 - Quarter hourly Chimes
   4 - ThreeQuarter Horly Chimes
}
begin
  userBacOptions.hourPips := ChckGrpTimeChimes.Checked[0];
  userBacOptions.hourChimes := ChckGrpTimeChimes.Checked[1];
  userBacOptions.halfChimes := ChckGrpTimeChimes.Checked[2];
  userBacOptions.quarterChimes := ChckGrpTimeChimes.Checked[3];
  userBacOptions.threeQuarterChimes := ChckGrpTimeChimes.Checked[4];

  ChckGrpTimeChimes.CheckEnabled[1] := not ChckGrpTimeChimes.Checked[0];
  ChckGrpTimeChimes.CheckEnabled[2] := not ChckGrpTimeChimes.Checked[0];
  ChckGrpTimeChimes.CheckEnabled[3] := not ChckGrpTimeChimes.Checked[0];
  ChckGrpTimeChimes.CheckEnabled[4] := not ChckGrpTimeChimes.Checked[0];
end;

procedure TfrmOptions.CmbBxDefaultTimeChange(Sender: TObject);
begin
  userBacOptions.defaultTime := CmbBxDefaultTime.ItemIndex;
end;
//
//....................................................................ANALOGUE KLOCK ...........................
//
procedure TfrmOptions.ChckGrpAnalogueKlockItemClick(Sender: TObject; Index: integer);
{  Sets the Ciming options according to the state of the radio group.

   index 0 - Save Screen Position
}
begin
  userBacOptions.analogueScreenSave := ChckGrpAnalogueKlock.Checked[0];
end;
//
//....................................................................OTHER STUFF ..............................
//
procedure TfrmOptions.ChckGrpTimerSettingsItemClick(Sender: TObject; Index: integer);
{  Sets the use Timer options according to the state of the radio group.

   index 0 - Timer To Show MilliSeconds
}
begin
  userBacOptions.timerMilliSeconds := ChckGrpTimerSettings.Checked[0];
end;
//
//....................................................................LOGGING ...................................
//
procedure TfrmOptions.ChckBxLoggingChange(Sender: TObject);
{  Switch on/off logging.    }
begin
  userBacOptions.logging := ChckBxLogging.Checked;
end;

procedure TfrmOptions.ChckBxCullLogsFilesChange(Sender: TObject);
{  Only show cull days if log culling is enabled.    }
begin
  SpnEdtCullDays.Visible := ChckBxCullLogsFiles.Checked;
  lblCullFileDays.Visible := ChckBxCullLogsFiles.Checked;

  if ChckBxCullLogsFiles.Checked then
  begin
    userBacOptions.cullLogs := ChckBxCullLogsFiles.Checked;
    userBacOptions.CullLogsDays := SpnEdtCullDays.Value;
  end;
end;
{....................................... Pannel Buttons ......................................................}

procedure TfrmOptions.CancelButtonClick(Sender: TObject);
{  The cancel button has been pressed, so we forget any changes.
    Since the changes are made to userBacOptions - we don't need to do nowt.
}
begin
  close;
end;

procedure TfrmOptions.OKButtonClick(Sender: TObject);
{  The OK button has been pressed, Copy the amended changes to main user options.
    Write back the main user options.
}
begin
  userOptions.Assign(userBacOptions);                     //  Copy amended options to main user options.
  UserOptions.writeCurrentOptions;                        //  write back amended options back to disk.

  applyRunAtStartUp(userOptions.runAtStartUp);
end;

end.



