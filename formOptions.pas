unit formOptions;

{  This form allows the amendment of all user options.
   The user options are held in the class Options which is held in uOptions.pas.
   The main user options is created with in formKlock.pas when the application is first started.
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
  Spin, CheckLst, EditBtn, uOptions, uArchiveUtils;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)

    AcrdnOptions               : TECAccordion;
    accItemGlobal              : TAccordionItem;
    accItemLogging             : TAccordionItem;
    accItemTime                : TAccordionItem;
    accItemOtherKlocks         : TAccordionItem;
    accItemStickyMemo          : TAccordionItem;
    accItemArchive             : TAccordionItem;
    btrOptionsReset            : TButton;
    btnGlobalVolumeTest        : TButton;
    btnCullLogs                : TButton;
    btnStickyNoteFont          : TButton;
    btnSaveArchive             : TButton;
    btnLoadArchive             : TButton;
    btnlblFloatingTextKlockFont: TButton;
    ButtonPanel1               : TButtonPanel;
    ChckGrpTimeOptions         : TCheckGroup;
    ChckGrpGlobalOptions       : TCheckGroup;
    ChckGrpTimeChimes          : TCheckGroup;
    ChckGrpAnalogueKlock       : TCheckGroup;
    ChckGrpHolidayFonts        : TCheckGroup;
    ChckGrpLEDKlock            : TCheckGroup;
    ChckGrpBinaryKlock         : TCheckGroup;
    ChckGrpSmallTextKlock      : TCheckGroup;
    ChckGrpTimerSettings       : TCheckGroup;
    ChckGrpFloatingTextKlock   : TCheckGroup;
    ChckLstBxArchive           : TCheckListBox;
    ChckBxDefaultPassWord      : TCheckBox;
    ChckBxCullLogsFiles        : TCheckBox;
    ChckBxLogging              : TCheckBox;
    ChckBxKeepMonitorAwake     : TCheckBox;
    ChckBxUseF15               : TCheckBox;
    ChckBxJiggleMouse          : TCheckBox;
    CmbBxDefaulTtab            : TComboBox;
    CmbBxDefaultTime           : TComboBox;
    clrBtnStickyNoteColour     : TColorButton;
    ColorDialog1               : TColorDialog;
    DtEdtBirthDate             : TDateEdit;
    EdtDefaultPassWord         : TEdit;
    edtLatitude                : TEdit;
    edtLongitude               : TEdit;
    FlNmEdtLoadArchiveName     : TFileNameEdit;
    FlNmEdtSaveArchiveName     : TFileNameEdit;
    FontDialog1                : TFontDialog;
    GroupBox1                  : TGroupBox;
    GroupBox10                 : TGroupBox;
    GroupBox11                 : TGroupBox;
    GroupBox12                 : TGroupBox;
    GroupBox2                  : TGroupBox;
    GroupBox3                  : TGroupBox;
    GroupBox4                  : TGroupBox;
    GroupBox5                  : TGroupBox;
    GroupBox6                  : TGroupBox;
    GroupBox7                  : TGroupBox;
    GroupBox8                  : TGroupBox;
    Settings                   : TGroupBox;
    GroupBox9                  : TGroupBox;
    Label1                     : TLabel;
    Label2                     : TLabel;
    lblBirthDate               : TLabel;
    lblCheckEvery              : TLabel;
    lblMinutes                 : TLabel;
    lblFloatingTextKlockFont   : TLabel;
    lblLatitude                : TLabel;
    lblLongitude               : TLabel;
    LblStickyNoteColour        : TLabel;
    lblCullFileDays            : TLabel;
    lblSettingsFileName        : TLabel;
    Label3                     : TLabel;
    LstBxLogFiles              : TListBox;
    SpnEdtMonitorMinites       : TSpinEdit;
    SpnEdtMemoTimeOut          : TSpinEdit;
    SpnEdtCullDays             : TSpinEdit;
    lblStickyNoteFont          : TStaticText;
    TrckBrGlobalVolume         : TTrackBar;

    procedure btnCullLogsClick(Sender: TObject);
    procedure btnGlobalVolumeTestClick(Sender: TObject);
    procedure btnlblFloatingTextKlockFontClick(Sender: TObject);
    procedure btnLoadArchiveClick(Sender: TObject);
    procedure btnSaveArchiveClick(Sender: TObject);
    procedure btnStickyNoteFontClick(Sender: TObject);
    procedure btrOptionsResetClick(Sender: TObject);
    procedure ChckBxCullLogsFilesChange(Sender: TObject);
    procedure ChckBxDefaultPassWordChange(Sender: TObject);
    procedure ChckBxJiggleMouseChange(Sender: TObject);
    procedure ChckBxKeepMonitorAwakeChange(Sender: TObject);
    procedure ChckBxLoggingChange(Sender: TObject);
    procedure ChckBxUseF15Change(Sender: TObject);
    procedure ChckGrpAnalogueKlockItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpBinaryKlockItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpFloatingTextKlockItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpGlobalOptionsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpHolidayFontsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpLEDKlockItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpSmallTextKlockItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimeChimesItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimeOptionsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimerSettingsItemClick(Sender: TObject; Index: integer);
    procedure ChckLstBxArchiveClickCheck(Sender: TObject);
    procedure clrBtnStickyNoteColourColorChanged(Sender: TObject);
    procedure CmbBxDefaulTtabChange(Sender: TObject);
    procedure CmbBxDefaultTimeChange(Sender: TObject);
    procedure EdtDefaultPassWordExit(Sender: TObject);
    procedure edtLatitudeChange(Sender: TObject);
    procedure edtLongitudeChange(Sender: TObject);
    procedure FlNmEdtLoadArchiveNameAcceptFileName(Sender: TObject; Var Value: String);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SpnEdtMemoTimeOutChange(Sender: TObject);
    procedure SpnEdtMonitorMinitesChange(Sender: TObject);
    procedure TrckBrGlobalVolumeChange(Sender: TObject);
  private
    procedure readLogFiles;
    procedure setKeepMonitorAwake;
  public

  end;


var
  frmOptions    : TfrmOptions;
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


  //  Before userBacOptions is assigned, so use userOptions.
  lblSettingsFileName.Caption := userOptions.optionsName;
  DtEdtBirthDate.Date         := userOptions.birthdate;
end;

procedure TfrmOptions.FormActivate(Sender: TObject);
{  Run things when ever the form is shown.

   See ChckGrpTimeOptionsItemClick for what the check Timer group index means.
   See ChckGrpTimerSettingsItemClick for what the check Timer group index means.
   See ChckGrpGlobalOptionsItemClick for what the check Global group index means.
}
var
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

  ChckBxKeepMonitorAwake.Checked := userBacOptions.keepMonitorAwake;
  chckBxUseF15.Checked           := userBacOptions.keepMonitorAwakeF15;
  ChckBxJiggleMouse.Checked      := userBacOptions.keepMonitorAwakeJiggle;
  SpnEdtMonitorMinites.Value     := userBacOptions.keepMonitorAwakeMinutes;
  setKeepMonitorAwake;

  ChckGrpTimeOptions.Checked[0] := userBacOptions.display24Hour;
  ChckGrpTimeOptions.Checked[1] := userBacOptions.netTimeSeconds;
  ChckGrpTimeOptions.Checked[2] := userBacOptions.swatchCentibeats;
  ChckGrpTimeOptions.Checked[3] := userBacOptions.fuzzyTimeBalloon;
  ChckGrpTimeOptions.Checked[4] := userBacOptions.displayIdleTime;
  ChckGrpTimeOptions.Checked[5] := userBacOptions.fuzzyTimeVerbose;

  ChckGrpHolidayFonts.Checked[0] := userBacOptions.christmasFont;
  ChckGrpHolidayFonts.Checked[1] := userBacOptions.easterFont;
  ChckGrpHolidayFonts.Checked[2] := userBacOptions.valentinesFont;
  ChckGrpHolidayFonts.Checked[3] := userBacOptions.haloweenFont;

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
  ChckGrpAnalogueKlock.Checked[1] := userBacOptions.analogueAlwaysOnTop;

  ChckGrpLEDKlock.Checked[0] := userBacOptions.LEDScreenSave;
  ChckGrpLEDKlock.Checked[1] := userBacOptions.LEDlongDate ;
  ChckGrpLEDKlock.Checked[2] := userBacOptions.LEDAlwaysOnTop;

  ChckGrpBinaryKlock.Checked[0] := userBacOptions.BinaryScreenSave;
  ChckGrpBinaryKlock.Checked[1] := userBacOptions.BinaryFormat;
  ChckGrpBinaryKlock.Checked[2] := userBacOptions.BinaryAlwaysOnTop;

  ChckGrpSmallTextKlock.Checked[0] := userBacOptions.smallTextScreenSave;
  ChckGrpSmallTextKlock.Checked[1] := userBacOptions.smallTextTransparent;
  ChckGrpSmallTextKlock.Checked[2] := userBacOptions.smallAlwaysOnTop;

  ChckGrpFloatingTextKlock.Checked[0] := userBacOptions.floatingTextScreenSave;
  ChckGrpFloatingTextKlock.Checked[1] := userBacOptions.floatingTextUseKlockFont;
  ChckGrpFloatingTextKlock.Checked[2] := userBacOptions.floatingAlwaysOnTop;

  lblFloatingTextKlockFont.Enabled    := not(ChckGrpFloatingTextKlock.Checked[1]);
  btnlblFloatingTextKlockFont.Enabled := not(ChckGrpFloatingTextKlock.Checked[1]);

  ChckGrpTimerSettings.Checked[0] := userBacOptions.timerMilliSeconds;

  ChckBxLogging.Checked       := userBacOptions.logging;
  ChckBxCullLogsFiles.Checked := userBacOptions.cullLogs;
  SpnEdtCullDays.Value        := userBacOptions.CullLogsDays;
  SpnEdtCullDays.Visible      := ChckBxCullLogsFiles.Checked;
  lblCullFileDays.Visible     := ChckBxCullLogsFiles.Checked;
  btnCullLogs.Visible         := ChckBxCullLogsFiles.Checked;

  readLogFiles;             //  Scan for log files and load listbox.

  edtDefaultPassWord.Caption    := userBacOptions.DefaultpassWord;
  ChckBxDefaultPassWord.Checked := userBacOptions.usedefaultpassWord;
  edtDefaultPassWord.Visible    := ChckBxDefaultPassWord.Checked;
  SpnEdtMemoTimeOut.Value       := userBacOptions.decryptTimeOut;

  LblStickyNoteColour.Font.Color     := userBacOptions.stickyColor;
  clrBtnStickyNoteColour.ButtonColor := userBacOptions.stickyColor;
  lblStickyNoteFont.Font             := userBacOptions.stickyFont;

  btnSaveArchive.Enabled            := false;
  btnLoadArchive.Enabled            := false;
  FlNmEdtSaveArchiveName.InitialDir := GetAppConfigDir(False);
  FlNmEdtSaveArchiveName.FileName   := format('%sKlock_%s.zip', [GetAppConfigDir(False),
                                                              FormatDateTime('DDMMMYYYY', now)]);
  FlNmEdtLoadArchiveName.FileName := '';
  ChckLstBxArchive.Items          := getArchiveFiles;

  //  when the date is accepted on a TDateEdit, it runs a form activate for some reason.
  //  So we set the values here.
  userBacOptions.birthdate := DtEdtBirthDate.Date;
  lblBirthDate.Caption     := format('Current BirthDate set to - %s', [DateTimeToStr(DtEdtBirthDate.Date)]);
end;

procedure TfrmOptions.btrOptionsResetClick(Sender: TObject);
{  reset user settings to system default

   NOTE :: maybe should have a confirm dialog.
}
begin
  userOptions.writeDefaultOptions;
  UserOptions.writeCurrentOptions;
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
   index 3 - Save Screen Position [Clipboard Monitor].
}
begin
  userBacOptions.screenSave       := ChckGrpGlobalOptions.Checked[0];
  userBacOptions.runAtStartUp     := ChckGrpGlobalOptions.Checked[1];
  userBacOptions.monitorClipboard := ChckGrpGlobalOptions.Checked[2];
  userBacOptions.CB_ScreenSave    := ChckGrpGlobalOptions.Checked[3];
end;

procedure TfrmOptions.TrckBrGlobalVolumeChange(Sender: TObject);
{  Sets the global volume.
   NB  :: is held as string - easier in calling programme.
}
begin
  userBacOptions.volume := intToStr(TrckBrGlobalVolume.Position);
end;

procedure TfrmOptions.btnGlobalVolumeTestClick(Sender: TObject);
{  Plays a sound to test volume.  }
begin
  doPlaySound('thepips.mp3', userBacOptions.volume);
end;

procedure TfrmOptions.edtLatitudeChange(Sender: TObject);
{  Save edited latitude.    }
begin
  userBacOptions.Latitude := StrToFloat(edtLatitude.Caption);
end;

procedure TfrmOptions.edtLongitudeChange(Sender: TObject);
{  Save edited Longitude.    }
begin
  userBacOptions.Latitude := StrToFloat(edtLongitude.Caption);
end;

procedure TfrmOptions.ChckBxKeepMonitorAwakeChange(Sender: TObject);
{  Enable Keep Monitor Awake.    }
begin
  userBacOptions.keepMonitorAwake := ChckBxKeepMonitorAwake.Checked;

  setKeepMonitorAwake;
end;

procedure TfrmOptions.ChckBxUseF15Change(Sender: TObject);
{  Use simulate pressing F15 to keep monitor awake.    }
begin
  userBacOptions.keepMonitorAwakeF15 := chckBxUseF15.Checked;
  ChckBxJiggleMouse.Checked          := not chckBxUseF15.Checked;
end;

procedure TfrmOptions.ChckBxJiggleMouseChange(Sender: TObject);
{  Use mouse movements to keep monitor awake.    }
begin
  userBacOptions.keepMonitorAwakeJiggle := ChckBxJiggleMouse.Checked;
  chckBxUseF15.Checked                  := not ChckBxJiggleMouse.Checked;
end;

procedure TfrmOptions.SpnEdtMonitorMinitesChange(Sender: TObject);
{  Set the time intewrval for the keep monitor awake routine.    }
begin
  userBacOptions.keepMonitorAwakeMinutes := SpnEdtMonitorMinites.Value;
end;

procedure TfrmOptions.setKeepMonitorAwake;
{  if Keep Monitor Awake is use then enable options, if not disable.    }
begin
  ChckBxUseF15.Enabled         := ChckBxKeepMonitorAwake.Checked;
  ChckBxJiggleMouse.Enabled    := ChckBxKeepMonitorAwake.Checked;
  lblCheckEvery.Enabled        := ChckBxKeepMonitorAwake.Checked;
  SpnEdtMonitorMinites.Enabled := ChckBxKeepMonitorAwake.Checked;
  lblMinutes.Enabled           := ChckBxKeepMonitorAwake.Checked;
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
      5 - Verbose Fuzzy Time.
}
begin
  userBacOptions.display24Hour    := ChckGrpTimeOptions.Checked[0];
  userBacOptions.netTimeSeconds   := ChckGrpTimeOptions.Checked[1];
  userBacOptions.swatchCentibeats := ChckGrpTimeOptions.Checked[2];
  userBacOptions.fuzzyTimeBalloon := ChckGrpTimeOptions.Checked[3];
  userBacOptions.displayIdleTime  := ChckGrpTimeOptions.Checked[4];
  userBacOptions.fuzzyTimeVerbose := ChckGrpTimeOptions.Checked[5];
end;

procedure TfrmOptions.ChckGrpTimeChimesItemClick(Sender: TObject; Index: integer);
{  Sets the Chiming options according to the state of the radio group.

index 0 - Sound "The Pips on the Hour"
      1 - Hourly Chimes
      2 - Half hourly Chimes
      3 - Quarter hourly Chimes
      4 - Three-quarter Hourly Chimes
}
begin
  userBacOptions.hourPips           := ChckGrpTimeChimes.Checked[0];
  userBacOptions.hourChimes         := ChckGrpTimeChimes.Checked[1];
  userBacOptions.halfChimes         := ChckGrpTimeChimes.Checked[2];
  userBacOptions.quarterChimes      := ChckGrpTimeChimes.Checked[3];
  userBacOptions.threeQuarterChimes := ChckGrpTimeChimes.Checked[4];

  ChckGrpTimeChimes.CheckEnabled[1] := not ChckGrpTimeChimes.Checked[0];
  ChckGrpTimeChimes.CheckEnabled[2] := not ChckGrpTimeChimes.Checked[0];
  ChckGrpTimeChimes.CheckEnabled[3] := not ChckGrpTimeChimes.Checked[0];
  ChckGrpTimeChimes.CheckEnabled[4] := not ChckGrpTimeChimes.Checked[0];
end;

procedure TfrmOptions.ChckGrpHolidayFontsItemClick(Sender: TObject; Index: integer);
{  sets the user Global options for the holiday fonts.

    Index 0 - 12 days of Christmas [before and after]
    Index 1 - Easter Holidays [week before and after]
    Index 2 - Valentines day [on that day only]
    Index 3 - Halloween [on that day only]
}
begin
  userBacOptions.christmasFont  := ChckGrpHolidayFonts.Checked[0];
  userBacOptions.easterFont     := ChckGrpHolidayFonts.Checked[1];
  userBacOptions.valentinesFont := ChckGrpHolidayFonts.Checked[2];
  userBacOptions.haloweenFont   := ChckGrpHolidayFonts.Checked[3];
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
   Index 1 - Always On Top.
}
begin
  userBacOptions.analogueScreenSave  := ChckGrpAnalogueKlock.Checked[0];
  userBacOptions.analogueAlwaysOnTop := ChckGrpAnalogueKlock.Checked[1];
end;
//
//.........................................LED KLOCK ...........................
//
procedure TfrmOptions.ChckGrpLEDKlockItemClick(Sender: TObject; Index: integer);
{  Sets the options for the LED Klock.

   Index 0 - Save Screen Position.
   Index 1 - Long Date Format.
   Index 2 - Always On Top.
}
begin
   userBacOptions.LEDScreenSave  := ChckGrpLEDKlock.Checked[0];
   userBacOptions.LEDlongDate    := ChckGrpLEDKlock.Checked[1];
   userBacOptions.LEDAlwaysOnTop := ChckGrpLEDKlock.Checked[2];
end;
//
//......................................Binary KLOCK ...........................
//
procedure TfrmOptions.ChckGrpBinaryKlockItemClick(Sender: TObject; Index: integer);
{  Sets the options for the Binary Klock.

   Index 0 - Save Screen Position.
   Index 1 - Binary / BCD Format - true for Binary.
   Index 2 - Always On Top.
}
begin
  userBacOptions.BinaryScreenSave  := ChckGrpBinaryKlock.Checked[0];
  userBacOptions.BinaryFormat      := ChckGrpBinaryKlock.Checked[1];
  userBacOptions.BinaryAlwaysOnTop := ChckGrpBinaryKlock.Checked[2];
end;
//
//......................................Small Text KLOCK .......................
//
procedure TfrmOptions.ChckGrpSmallTextKlockItemClick(Sender: TObject; Index: integer);
{  Sets the options for the Small Text Klock.

   Index 0 - Save Screen Position.
   Index 1 - Small Text Klock Transparent.
   Index 2 - Always On Top.
}
begin
  userBacOptions.smallTextScreenSave  := ChckGrpSmallTextKlock.Checked[0];
  userBacOptions.smallTextTransparent := ChckGrpSmallTextKlock.Checked[1];
  userBacOptions.smallAlwaysOnTop     := ChckGrpSmallTextKlock.Checked[2];
end;
//
//...................................Floating Text KLOCK .......................
//
procedure TfrmOptions.ChckGrpFloatingTextKlockItemClick(Sender: TObject; Index: integer);
{  Sets the options for the Floating Text Klock.

   Index 0 - Save Screen Position.
   Index 1 - Use Main Klock font.
   Index 2 - Always On Top.
}
begin
  userBacOptions.floatingTextScreenSave   := ChckGrpFloatingTextKlock.Checked[0];
  userBacOptions.floatingTextUseKlockFont := ChckGrpFloatingTextKlock.Checked[1];
  userBacOptions.floatingAlwaysOnTop      := ChckGrpFloatingTextKlock.Checked[2];

  lblFloatingTextKlockFont.Enabled    := not(ChckGrpFloatingTextKlock.Checked[1]);
  btnlblFloatingTextKlockFont.Enabled := not(ChckGrpFloatingTextKlock.Checked[1]);
end;

procedure TfrmOptions.btnlblFloatingTextKlockFontClick(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    lblFloatingTextKlockFont.Font   := FontDialog1.Font;
    userBacOptions.floatingTextFont := FontDialog1.Font;
  end;
end;
//
//....................................OTHER STUFF ..............................
//
procedure TfrmOptions.ChckGrpTimerSettingsItemClick(Sender: TObject; Index: integer);
{  Sets the use Timer options according to the state of the radio group.

   index 0 - Timer To Show Milliseconds
}
begin
  userBacOptions.timerMilliSeconds := ChckGrpTimerSettings.Checked[0];
end;

//
//................... Sticky Notes, Memo, Monitor Sleeping and BirthDate .......
//
procedure TfrmOptions.ChckBxDefaultPassWordChange(Sender: TObject);
begin
  userBacOptions.useDefaultpassWord := ChckBxDefaultPassWord.Checked;
  edtDefaultPassWord.Visible        := ChckBxDefaultPassWord.Checked;
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
  LblStickyNoteColour.Font.Color     := clrBtnStickyNoteColour.ButtonColor;
  clrBtnStickyNoteColour.ButtonColor := clrBtnStickyNoteColour.ButtonColor;

  userBacOptions.stickyColor := clrBtnStickyNoteColour.ButtonColor;
end;

procedure TfrmOptions.btnStickyNoteFontClick(Sender: TObject);
begin
   if FontDialog1.Execute then
   begin
     lblStickyNoteFont.Font    := FontDialog1.Font;
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
  SpnEdtCullDays.Visible  := ChckBxCullLogsFiles.Checked;
  lblCullFileDays.Visible := ChckBxCullLogsFiles.Checked;
  btnCullLogs.Visible     := ChckBxCullLogsFiles.Checked;

  if ChckBxCullLogsFiles.Checked then
  begin
    userBacOptions.cullLogs     := ChckBxCullLogsFiles.Checked;
    userBacOptions.CullLogsDays := SpnEdtCullDays.Value;
  end;
end;

procedure TfrmOptions.btnCullLogsClick(Sender: TObject);
{  If enabled [cull logs is checked], then delete all logs files over due.
}
begin
  if userOptions.cullLogs then         //  Removed old log files, if instructed.
  begin
    kLog.cullLogFile(userOptions.CullLogsDays);
    LstBxLogFiles.Items.Clear;
    readLogFiles;
  end;
end;

procedure TfrmOptions.readLogFiles;
{  Scan for log files and load listbox.    }
var
  logFiles:TStringlist;
begin
    logFiles := TstringList.Create;
    try
      kLog.readLogFile(logFiles);
      LstBxLogFiles.Items.Assign(logFiles) ;
    finally
      freeandnil(logFiles);
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
//
//...................................ARCHIVE ...................................
//
procedure TfrmOptions.btnSaveArchiveClick(Sender: TObject);
{  Save all files that have been clicked in the listbox.
   The zip file name is taken from the save file edit box.

   If the fonts directory is checked, the fonts have to be removed
   for archive, Klock has then locked.  The added again after archive.
}
var
  f           : integer;
  files       : TStringList;
  theFontFiles: TStringList;
  fontsRemoved: boolean;
begin
  fontsRemoved := false;
  files := TStringList.Create;

  try
    for f := 0 to ChckLstBxArchive.Items.Count - 1 do
      if ChckLstBxArchive.Checked[f] then
        if ChckLstBxArchive.Items.Strings[f] = 'Fonts Directory' then
          begin
            klog.writeLog('*** Removing Fonts For Archive ***');
            fontsRemoved := true;
            fs.removeFonts;
            theFontFiles := TStringList.Create;
            try
              FindAllFiles(theFontFiles, ExtractFilePath(Application.ExeName) + 'fonts');
              files.AddStrings(theFontFiles);
            finally
              theFontFiles.Free;
            end;  //  end of try
          end     //  if ChckLstBxArchive.Items.Strings[f] = 'Fonts Directory' then
        else
          files.Add(ChckLstBxArchive.Items.Strings[f]);

    saveArchive(FlNmEdtSaveArchiveName.FileName, files);
  finally
    if fontsRemoved then  //  Fonts where archived, so need re-loading.
      fs.addFonts;
    files.free;
  end;
end;

procedure TfrmOptions.ChckLstBxArchiveClickCheck(Sender: TObject);
{  Loops through the list box and count the clicked items.
   If count is 0 [no item clicked] the save button is disabled.
   If the count is not 0 then enable the save button.
}
var
  f    : integer;
  count: integer;
begin
  count := 0;
  for f := 0 to ChckLstBxArchive.Items.Count - 1 do
    if ChckLstBxArchive.Checked[f] then
      inc(count);

  if count <> 0 then
    btnSaveArchive.Enabled := true
  else
    btnSaveArchive.Enabled := false;
end;

procedure TfrmOptions.btnLoadArchiveClick(Sender: TObject);
{  Loads the zip file.    }
begin
  LoadArchive(FlNmEdtLoadArchiveName.FileName);
end;

procedure TfrmOptions.FlNmEdtLoadArchiveNameAcceptFileName(Sender: TObject; Var Value: String);
{  If the file name edit contains text then enable the load button.
   Does not check for a valid zip file [yet].
}
begin
  if value <> '' then
    btnLoadArchive.Enabled := true
  else
    btnLoadArchive.Enabled := false;
end;

end.



