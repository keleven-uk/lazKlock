unit formOptions;

{  This form allows the amendment of all user options.
   The user options are held in the class Options which is held in uOptions.pas.
   The main user options is created withing formKlock.pas when the application is first started.
   This unit creates a secondary options objects to hold changes made wile the form is active.
   If OK is pressed the secondary options object with the changes are copied to the main
   options and the form is closed.
   If cancel is pressed, the form is closed and the changes are lost.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ECAccordion, Forms, Controls, Dialogs,
  UKlockUtils, Graphics, StdCtrls, ButtonPanel, Buttons, ComCtrls, ExtCtrls,
  Spin, uOptions;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    accItemGlobal: TAccordionItem;
    accItemLogging: TAccordionItem;
    accItemTime: TAccordionItem;
    accItemOtherKlocks: TAccordionItem;
    accItemStickyMemo: TAccordionItem;
    btrOptionsReset: TButton;
    btnGlobalVolumeTest: TButton;
    btnCullLogs: TButton;
    btnStickyNoteFont: TButton;
    ButtonPanel1: TButtonPanel;
    ChckBxCullLogsFiles: TCheckBox;
    ChckBxLogging: TCheckBox;
    ChckGrpTimeOptions: TCheckGroup;
    ChckGrpGlobalOptions: TCheckGroup;
    ChckGrpTimeChimes: TCheckGroup;
    ChckGrpAnalogueKlock: TCheckGroup;
    ChckGrpHolidayFonts: TCheckGroup;
    ChckGrpLEDKlock: TCheckGroup;
    ChckGrpBinaryKlock: TCheckGroup;
    ChckGrpSmallTextKlock: TCheckGroup;
    ChckGrpTimerSettings: TCheckGroup;
    ChckBxDefaultPassWord: TCheckBox;
    CmbBxDefaulTtab: TComboBox;
    CmbBxDefaultTime: TComboBox;
    AcrdnOptions: TECAccordion;
    clrBtnStickyNoteColour: TColorButton;
    ColorDialog1: TColorDialog;
    EdtDefaultPassWord: TEdit;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    LblStickyNoteColour: TLabel;
    lblCullFileDays: TLabel;
    lblSettingsFileName: TLabel;
    Label3: TLabel;
    LstBxLogFiles: TListBox;
    Settings: TGroupBox;
    SpnEdtMemoTimeOut: TSpinEdit;
    SpnEdtCullDays: TSpinEdit;
    lblStickyNoteFont: TStaticText;
    TrckBrGlobalVolume: TTrackBar;
    procedure btnCullLogsClick(Sender: TObject);
    procedure btnGlobalVolumeTestClick(Sender: TObject);
    procedure btnStickyNoteFontClick(Sender: TObject);
    procedure btrOptionsResetClick(Sender: TObject);
    procedure ChckBxCullLogsFilesChange(Sender: TObject);
    procedure ChckBxDefaultPassWordChange(Sender: TObject);
    procedure ChckBxLoggingChange(Sender: TObject);
    procedure ChckGrpAnalogueKlockItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpBinaryKlockItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpGlobalOptionsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpHolidayFontsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpLEDKlockItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpSmallTextKlockItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimeChimesItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimeOptionsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimerSettingsItemClick(Sender: TObject; Index: integer);
    procedure clrBtnStickyNoteColourColorChanged(Sender: TObject);
    procedure CmbBxDefaulTtabChange(Sender: TObject);
    procedure CmbBxDefaultTimeChange(Sender: TObject);
    procedure EdtDefaultPassWordExit(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SpnEdtMemoTimeOutChange(Sender: TObject);
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

//............................ Form Routines ...................................

procedure TfrmOptions.FormCreate(Sender: TObject);
{  Run things when the form is first created.  }
begin
  kLog.writeLog('FormOptions Create');

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
  optnFile: String;
begin
  kLog.writeLog('FormOptions Activate');

  //  Create tmp options file as c:\Users\<user>\AppData\Local\<app Name>\OptionsXX_tmp.xml
  //  This is used to store local amendments and only copied to the main options files
  //  if the OK button is clicked.

  {$IFDEF TEST}
    optnFile := 'TEST_Options';
  {$else}
    optnFile := 'Options';
  {$endif}
  {$ifdef WIN32}
    userBacOptions := Options.Create(optnFile + '32_temp.xml');
  {$else}
    userBacOptions := Options.Create(optnFile + '64_temp.xml');
  {$endif}

  userBacOptions.Assign(userOptions);                    //  make a copy of the current user options
  //userBacOptions.writeCurrentOptions;                    //  write the copy back to disk.

  AcrdnOptions.ItemIndex := 0;                              //  Always start on Global Options.
  CmbBxDefaulTtab.ItemIndex := userBacOptions.defaultTab;

  CmbBxDefaultTime.Items.AddStrings(ft.fuzzyTypes);
  CmbBxDefaultTime.ItemIndex := userBacOptions.defaultTime;

  TrckBrGlobalVolume.Position := StrToInt(userBacOptions.volume);

  ChckGrpGlobalOptions.Checked[0] := userBacOptions.screenSave;
  ChckGrpGlobalOptions.Checked[1] := userBacOptions.runAtStartUp;
  ChckGrpGlobalOptions.Checked[2] := userBacOptions.monitorClipboard;
  ChckGrpGlobalOptions.Checked[3] := userBacOptions.CB_ScreenSave;

  ChckGrpTimeOptions.Checked[0] := userBacOptions.display24Hour;
  ChckGrpTimeOptions.Checked[1] := userBacOptions.netTimeSeconds;
  ChckGrpTimeOptions.Checked[2] := userBacOptions.swatchCentibeats;
  ChckGrpTimeOptions.Checked[3] := userBacOptions.fuzzyTimeBalloon;
  ChckGrpTimeOptions.Checked[4] := userBacOptions.displayIdleTime;

  ChckGrpHolidayFonts.Checked[0] := userBacOptions.christmasFont;
  ChckGrpHolidayFonts.Checked[1] := userBacOptions.easterFont;

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

  ChckGrpLEDKlock.Checked[0] := userBacOptions.LEDScreenSave;
  ChckGrpLEDKlock.Checked[1] := userBacOptions.LEDlongDate ;

  ChckGrpBinaryKlock.Checked[0] := userBacOptions.BinaryScreenSave;
  ChckGrpBinaryKlock.Checked[1] := userBacOptions.BinaryFormat;

  ChckGrpSmallTextKlock.Checked[0] := userBacOptions.smallTextScreenSave;
  ChckGrpSmallTextKlock.Checked[1] := userBacOptions.smallTextTransparent;

  ChckGrpTimerSettings.Checked[0] := userBacOptions.timerMilliSeconds;

  ChckBxLogging.Checked := userBacOptions.logging;
  ChckBxCullLogsFiles.Checked := userBacOptions.cullLogs;
  SpnEdtCullDays.Value := userBacOptions.CullLogsDays;
  SpnEdtCullDays.Visible := ChckBxCullLogsFiles.Checked;
  lblCullFileDays.Visible := ChckBxCullLogsFiles.Checked;
  btnCullLogs.Visible := ChckBxCullLogsFiles.Checked;

  logFiles := TstringList.Create;             //  Scan for log files and load listbox.
    try
    kLog.readLogFile(logFiles);
    LstBxLogFiles.Items.Assign(logFiles) ;
  finally
      freeandnil(logFiles);
  end;

  edtDefaultPassWord.Caption := userBacOptions.DefaultpassWord;
  ChckBxDefaultPassWord.Checked := userBacOptions.usedefaultpassWord;
  edtDefaultPassWord.Visible := ChckBxDefaultPassWord.Checked;
  SpnEdtMemoTimeOut.Value := userBacOptions.decryptTimeOut;

  LblStickyNoteColour.Font.Color := userBacOptions.stickyColor;
  clrBtnStickyNoteColour.ButtonColor := userBacOptions.stickyColor;
  lblStickyNoteFont.Font := userBacOptions.stickyFont;
end;

procedure TfrmOptions.btrOptionsResetClick(Sender: TObject);
{  reset user settings to system default

   NOTE :: maybe should have a confirm dialog.
}
begin
  userOptions.writeDefaultOptions;
end;

//............................ Options Routines ................................
//
//...................................GLOBAL ....................................
//
procedure TfrmOptions.CmbBxDefaulTtabChange(Sender: TObject);
{  The default tab has changed, relect is user options.  }
begin
  userBacOptions.defaultTab := CmbBxDefaulTtab.ItemIndex;
end;

procedure TfrmOptions.ChckGrpGlobalOptionsItemClick(Sender: TObject; Index: integer);
{  Sets the user Global options according to the state of the radio group.

   index 0 - Save Screen Position.
   index 1 - Run Klock on start up - HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\run
   index 2 - Monitor Clipboard.
   index 4 - Save Screen Position [Clipboard Monitor].
}
begin
  userBacOptions.screenSave := ChckGrpGlobalOptions.Checked[0];
  userBacOptions.runAtStartUp := ChckGrpGlobalOptions.Checked[1];
  userBacOptions.monitorClipboard := ChckGrpGlobalOptions.Checked[2];
  userBacOptions.CB_ScreenSave := ChckGrpGlobalOptions.Checked[3];
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
//.....................................TIME ....................................
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
{  Sets the Chiming options according to the state of the radio group.

index 0 - Sound "The Pips on the Hour"
      1 - Hourly Chimes
      2 - Half hourly Chimes
      3 - Quarter hourly Chimes
      4 - ThreeQuarter Hourly Chimes
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

procedure TfrmOptions.ChckGrpHolidayFontsItemClick(Sender: TObject; Index: integer);
{  sets the user Global options for the holiday fonts.

    Index 0 - 12 days of Christmas [before and after]
    Index 1 - Easter Holidays [week before and after].
}
begin
  userBacOptions.christmasFont := ChckGrpHolidayFonts.Checked[0];
  userBacOptions.easterFont := ChckGrpHolidayFonts.Checked[1];
end;

procedure TfrmOptions.CmbBxDefaultTimeChange(Sender: TObject);
begin
  userBacOptions.defaultTime := CmbBxDefaultTime.ItemIndex;
end;
//
//....................................ANALOGUE KLOCK ...........................
//
procedure TfrmOptions.ChckGrpAnalogueKlockItemClick(Sender: TObject; Index: integer);
{  Sets the options for the Analogue Klock.

   Index 0 - Save Screen Position.
}
begin
  userBacOptions.analogueScreenSave := ChckGrpAnalogueKlock.Checked[0];
end;
//
//.........................................LED KLOCK ...........................
//
procedure TfrmOptions.ChckGrpLEDKlockItemClick(Sender: TObject; Index: integer);
{  Sets the options for the LED Klock.

   Index 0 - Save Screen Position.
   Index 1 - Long Date Format
}
begin
   userBacOptions.LEDScreenSave := ChckGrpLEDKlock.Checked[0];
   userBacOptions.LEDlongDate := ChckGrpLEDKlock.Checked[1];
end;
//
//......................................Binary KLOCK ...........................
//
procedure TfrmOptions.ChckGrpBinaryKlockItemClick(Sender: TObject; Index: integer);
{  Sets the options for the Binary Klock.

   Index 0 - Save Screen Position.
   Index 1 - Binary / BCD Format - true for Binary.
}
begin
  userBacOptions.BinaryScreenSave := ChckGrpBinaryKlock.Checked[0];
  userBacOptions.BinaryFormat := ChckGrpBinaryKlock.Checked[1];
end;
//
//......................................Small Text KLOCK .......................
//
procedure TfrmOptions.ChckGrpSmallTextKlockItemClick(Sender: TObject; Index: integer);
{  Sets the options for the Small Text Klock.

   Index 0 - Save Screen Position.
   Index 1 - Small Text Klock Transparent.
}
begin
  userBacOptions.smallTextScreenSave := ChckGrpSmallTextKlock.Checked[0];
  userBacOptions.smallTextTransparent := ChckGrpSmallTextKlock.Checked[1];
end;
//
//....................................OTHER STUFF ..............................
//
procedure TfrmOptions.ChckGrpTimerSettingsItemClick(Sender: TObject; Index: integer);
{  Sets the use Timer options according to the state of the radio group.

   index 0 - Timer To Show MilliSeconds
}
begin
  userBacOptions.timerMilliSeconds := ChckGrpTimerSettings.Checked[0];
end;
//
//................................... Sticky Notes and Memos ...................
//
procedure TfrmOptions.ChckBxDefaultPassWordChange(Sender: TObject);
begin
  userBacOptions.useDefaultpassWord := ChckBxDefaultPassWord.Checked;
  edtDefaultPassWord.Visible := ChckBxDefaultPassWord.Checked;
end;

procedure TfrmOptions.EdtDefaultPassWordExit(Sender: TObject);
begin
  userBacOptions.defaultpassWord := EdtDefaultPassWord.Caption;
end;

procedure TfrmOptions.SpnEdtMemoTimeOutChange(Sender: TObject);
begin
  userBacOptions.decryptTimeOut := SpnEdtMemoTimeOut.Value;
end;

procedure TfrmOptions.clrBtnStickyNoteColourColorChanged(Sender: TObject);
{  When the colour button is clicked, it runs the colour dialog chooser.
   When A colour is selected, this procedure is called.
   Sets the label and button colour to the chosen colour.
}
begin
  LblStickyNoteColour.Font.Color := clrBtnStickyNoteColour.ButtonColor;
  clrBtnStickyNoteColour.ButtonColor := clrBtnStickyNoteColour.ButtonColor;

  userBacOptions.stickyColor := clrBtnStickyNoteColour.ButtonColor;
end;

procedure TfrmOptions.btnStickyNoteFontClick(Sender: TObject);
begin
   if FontDialog1.Execute then
   begin
     lblStickyNoteFont.Font := FontDialog1.Font;
     userBacOptions.stickyFont := FontDialog1.Font;
   end;
end;
//
//...................................LOGGING ...................................
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
  btnCullLogs.Visible := ChckBxCullLogsFiles.Checked;

  if ChckBxCullLogsFiles.Checked then
  begin
    userBacOptions.cullLogs := ChckBxCullLogsFiles.Checked;
    userBacOptions.CullLogsDays := SpnEdtCullDays.Value;
  end;
end;

procedure TfrmOptions.btnCullLogsClick(Sender: TObject);
{  If enabled [cull logs is checked], then delete all logs files over due.
}
var
  logFiles:TStringlist;
begin
  if userOptions.cullLogs then         //  Removed old log files, if instructed.
  begin
    kLog.cullLogFile(userOptions.CullLogsDays);

    logFiles := TstringList.Create;    //  Scan for log files and load listbox.
    try
      kLog.readLogFile(logFiles);
      LstBxLogFiles.Items.Assign(logFiles) ;
    finally
      freeandnil(logFiles);
    end;
  end;
end;
{........ Pannel Buttons ......................................................}

procedure TfrmOptions.CancelButtonClick(Sender: TObject);
{  The cancel button has been pressed, so we forget any changes.
    Since the changes are made to userBacOptions - we don't need to do nowt.
}
begin
  FreeAndNil(userBacOptions);
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
  FreeAndNil(userBacOptions);
end;

procedure TfrmOptions.HelpButtonClick(Sender: TObject);
begin
  displayHelp('help\Klock.chm', '/Options.htm');
end;


end.



