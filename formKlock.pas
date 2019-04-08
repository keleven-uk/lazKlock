unit formklock;

{
Klock :: A Clock with a K.
Copyright (C) 2012 - 2018 :: Kevin Scott

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.


  To compile, the following components must be installed into Lazarus.
     BGRA controls, which installs BGRA bitmap.
       MouseAndKeyInput, needs to be ticked to use from this package.
     EC-contols - Eye Candy - used for the Accordion on the options screen.
     VisualPlanit - L.E.D. control.
     DCPciphers - Encryption and Decryption stuff.
     DelphiMoon - Moon image and Moon/Sun stuff.
     PascalTZ - Time Zone stuff [go to web site].

     Most from the Online Package manager.
}

{ TODO : Look at up time in formAbout. }
{ TODO : Combine onChange routines in both memo and events }
{ TODO : Scrolling and floating text klocks need all option on options screen. }
{ TODO : uEvents.pas should use userOptions for file location, possibly passed into create. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, uFonts, ComCtrls, Menus, Buttons, StdCtrls, Spin,
  PopupNotifier, EditBtn, uMemos, uMemo, formAbout, formOptions, formLicense,
  UFuzzyTime, dateutils, LCLIntf, LCLType, uPascalTZ, DCPrijndael, DCPsha256,
  UKlockUtils, uEvent, uEvents, UConversion, uOptions, Windows, ULogging,
  ustickyNotes, formInfo, Graph, formClipBoard, formLEDKlock, formBinaryKlock,
  formAnalogueKlock, formSmallTextKlock, formFloatingKlock, formSplashScreen,
  formBiorhythm, uFriends, formFriendsInput, uFriend, formTimePositions,
  formScrollingKlock;

type
  FourStrings = array [0..3] of string;

  { TfrmMain }

  TfrmMain = class(TForm)
    BitBtnHide               : TBitBtn;
    BitBtnClose              : TBitBtn;
    BitBtnHelp               : TBitBtn;
    btnConverionConvert      : TButton;
    btnConversionAddUnits    : TButton;
    btnCountdownLoadCommand  : TButton;
    btnCountdownLoadSound    : TButton;
    btnCountdownShutdownAbort: TButton;
    btnCountdownStart        : TButton;
    btnCountdownStop         : TButton;
    btnEventAdd              : TButton;
    btnEventClear            : TButton;
    btnEventDelete           : TButton;
    btnEventEdit             : TButton;
    btnEventNew              : TButton;
    btnEventPrint            : TButton;
    btnFriendsNew            : TButton;
    btnFriendsEdit           : TButton;
    btnReminderAbort         : TButton;
    btnReminderClear         : TButton;
    btnReminderLoadCommand   : TButton;
    btnReminderLoadSound     : TButton;
    btnReminderSet           : TButton;
    btnReminderTestSound     : TButton;
    btnMemoDecrypt           : TButton;
    btnMemoNew               : TButton;
    btnMemoAdd               : TButton;
    btnMemoClear             : TButton;
    btnMemoEdit              : TButton;
    btnMemoDelete            : TButton;
    btnMemoPrint             : TButton;
    btnSoundTest             : TButton;
    btnTimerClear            : TButton;
    btnTimerSplit            : TButton;
    btnTimerStart            : TButton;
    btnTimerStop             : TButton;
    btnFriendsDelete         : TButton;
    chckBxCountdownCommand   : TCheckBox;
    chckBxCountdownEvent     : TCheckBox;
    chckBxCountdownReminder  : TCheckBox;
    chckBxCountdownSound     : TCheckBox;
    ChckBxReminderCommand    : TCheckBox;
    ChckBxReminderReminder   : TCheckBox;
    ChckBxReminderSound      : TCheckBox;
    ChckBxReminderSystem     : TCheckBox;
    ChckBxEventFloating      : TCheckBox;
    CmbBxCategory            : TComboBox;
    CmbBxConvertTo           : TComboBox;
    CmbBxCountdownAction     : TComboBox;
    CmbBxCountdownEvent      : TComboBox;
    cmbBxEventType           : TComboBox;
    CmbBxReminderAction      : TComboBox;
    CmbBxReminderSystem      : TComboBox;
    CmbBxTZFonts             : TComboBox;
    CmbBxTime                : TComboBox;
    CmbBxName                : TComboBox;
    CmbBxTimeZones           : TComboBox;
    dtEdtEventDate           : TDateTimePicker;
    DCP_rijndael1            : TDCP_rijndael;
    DCP_sha256_1             : TDCP_sha256;
    DtReminderEvent          : TDateEdit;
    edtConverionResult       : TEdit;
    edtConverionValue        : TEdit;
    EdtCountdownCommand      : TEdit;
    EdtCountdownReminder     : TEdit;
    EdtCountdownSound        : TEdit;
    edtEventName             : TEdit;
    EdtReminderCommand       : TEdit;
    EdtReminderSound         : TEdit;
    EdtReminderText          : TEdit;
    edtMemoKey               : TEdit;
    Label1                   : TLabel;
    Label2                   : TLabel;
    Label3                   : TLabel;
    Label4                   : TLabel;
    Label5                   : TLabel;
    Label6                   : TLabel;
    Label7                   : TLabel;
    Label8                   : TLabel;
    LblCountdownTime         : TLabel;
    lblReminder              : TLabel;
    lblfuzzy                 : TLabel;
    lblTZTime                : TLabel;
    LblMemoName              : TLabel;
    lblRadix                 : TLabel;
    lblSplitLap              : TLabel;
    lblTimer                 : TLabel;
    lstBxEvents              : TListBox;
    lstBxFriends             : TListBox;
    LstBxMemoName            : TListBox;
    mnuItmScrollingTextKlock : TMenuItem;
    mEventNotes              : TMemo;
    MmMemoData               : TMemo;
    PascalTZ1                : TPascalTZ;
    RdBttnMemoEncrypt        : TRadioButton;
    SaveDialog1              : TSaveDialog;
    ScrollBox2               : TScrollBox;
    SpdBtn120                : TSpeedButton;
    SpdBtn90                 : TSpeedButton;
    SpdBtn60                 : TSpeedButton;
    SpdBtn30                 : TSpeedButton;
    SpdBtnClipboard          : TSpeedButton;
    SpnEdtCountdown          : TSpinEdit;
    SpnReminderHour          : TSpinEdit;
    SpnReminderMins          : TSpinEdit;
    SpnEdtTimeBase           : TSpinEdit;
    PageControl1             : TPageControl;
    TbShtFriends             : TTabSheet;
    TbShtEvents              : TTabSheet;
    TbShtWorldKlock          : TTabSheet;
    TbShtConversion          : TTabSheet;
    TbShtCountdown           : TTabSheet;
    TbShtReminder            : TTabSheet;
    TbShtFuzzy               : TTabSheet;
    TbShtMemo                : TTabSheet;
    TbShtTimer               : TTabSheet;
    MnuItmEnhancedBiorhythm  : TMenuItem;
    MnuItmSimpleBiorhythm    : TMenuItem;
    MnItmBiorhythm           : TMenuItem;
    mnuItmMonitorStuff       : TMenuItem;
    mnuItmChineseYear        : TMenuItem;
    mnuItmPowerStuff         : TMenuItem;
    mnuItmSunStuff           : TMenuItem;
    mnuItmFloatingTextKlock  : TMenuItem;
    mnuItmMoonStuff          : TMenuItem;
    mnuItmNewStickyNote      : TMenuItem;
    mnuItmStickyNote         : TMenuItem;
    mnuItmSmallTextKlock     : TMenuItem;
    mnuItmBinaryKlock        : TMenuItem;
    mnuItmLEDKlock           : TMenuItem;
    mnuItmTimePositions      : TMenuItem;
    mnuItmLentDates          : TMenuItem;
    mnuItmEasterDates        : TMenuItem;
    mnuItmDaylightSaving     : TMenuItem;
    mnuInfo                  : TMenuItem;
    mnuTime                  : TMenuItem;
    mnuItmAnalogueKlock      : TMenuItem;
    ppMnItmTime              : TMenuItem;
    ppMnItmExit              : TMenuItem;
    ppMnItmShow              : TMenuItem;
    mnuItmLicense            : TMenuItem;
    mnuItmOptions            : TMenuItem;
    mnuItmHelp               : TMenuItem;
    mnuItmAbout              : TMenuItem;
    mnuItmExit               : TMenuItem;
    mnuBiorhythm             : TMenuItem;
    mnuFile                  : TMenuItem;
    mnuMain                  : TMainMenu;
    Panel1                   : TPanel;
    Panel2                   : TPanel;
    Panel3                   : TPanel;
    Panel4                   : TPanel;
    Panel5                   : TPanel;
    Panel6                   : TPanel;
    Panel7                   : TPanel;
    Panel8                   : TPanel;
    Panel9                   : TPanel;
    Panel10                  : TPanel;
    Panel11                  : TPanel;
    Panel12                  : TPanel;
    Panel13                  : TPanel;
    Panel14                  : TPanel;
    Panel15                  : TPanel;
    Panel16                  : TPanel;
    Panel17                  : TPanel;
    Panel18                  : TPanel;
    Panel19                  : TPanel;
    Panel20                  : TPanel;
    Panel21                  : TPanel;
    Panel22                  : TPanel;
    Panel23                  : TPanel;
    Panel24                  : TPanel;
    Panel25                  : TPanel;
    Panel26                  : TPanel;
    Panel27                  : TPanel;
    Panel28                  : TPanel;
    Panel29                  : TPanel;
    Panel30                  : TPanel;
    PpMnTray                 : TPopupMenu;
    PopupNotifier1           : TPopupNotifier;
    SpdBtnNewStickyNote      : TSpeedButton;
    stsBrInfo                : TStatusBar;
    mainIdleTimer            : TIdleTimer;
    mainTimer                : TTimer;
    CountdownTimer           : TTimer;
    ReminderTimer            : TTimer;
    tmrMemo                  : TTimer;
    timerTimer               : TTimer;
    ballonTimer              : TTimer;
    TrayIcon                 : TTrayIcon;

    procedure ballonTimerTimer(Sender: TObject);
    procedure btnConverionConvertClick(Sender: TObject);
    procedure btnConversionAddUnitsClick(Sender: TObject);
    procedure btnEventAddClick(Sender: TObject);
    procedure btnEventClearClick(Sender: TObject);
    procedure btnEventDeleteClick(Sender: TObject);
    procedure btnEventEditClick(Sender: TObject);
    procedure btnEventNewClick(Sender: TObject);
    procedure btnFriendsClick(Sender: TObject);
    procedure btnMemoAddClick(Sender: TObject);
    procedure btnMemoClearClick(Sender: TObject);
    procedure btnMemoDecryptClick(Sender: TObject);
    procedure btnMemoDeleteClick(Sender: TObject);
    procedure btnMemoEditClick(Sender: TObject);
    procedure btnMemoNewClick(Sender: TObject);
    procedure btnMemoPrintClick(Sender: TObject);
    procedure btnCountdownLoadCommandClick(Sender: TObject);
    procedure btnCountdownLoadSoundClick(Sender: TObject);
    procedure btnCountdownShutdownAbortClick(Sender: TObject);
    procedure btnCountdownStartClick(Sender: TObject);
    procedure btnCountdownStopClick(Sender: TObject);
    procedure btnReminderClearClick(Sender: TObject);
    procedure btnReminderLoadCommandClick(Sender: TObject);
    procedure btnReminderLoadSoundClick(Sender: TObject);
    procedure btnReminderSetClick(Sender: TObject);
    procedure btnReminderAbortClick(Sender: TObject);
    procedure btnReminderTestSoundClick(Sender: TObject);
    procedure btnSoundTestClick(Sender: TObject);
    procedure btnTimerClearClick(Sender: TObject);
    procedure btnTimerStartClick(Sender: TObject);
    procedure btnTimerStopClick(Sender: TObject);
    procedure btnTimerSplitClick(Sender: TObject);
    procedure chckBxCountdownCommandChange(Sender: TObject);
    procedure chckBxCountdownEventChange(Sender: TObject);
    procedure chckBxCountdownReminderChange(Sender: TObject);
    procedure ChckBxCountdownSoundChange(Sender: TObject);
    procedure ChckBxReminderCommandChange(Sender: TObject);
    procedure ChckBxReminderReminderChange(Sender: TObject);
    procedure ChckBxReminderSoundChange(Sender: TObject);
    procedure ChckBxReminderSystemChange(Sender: TObject);
    procedure CmbBxCategoryChange(Sender: TObject);
    procedure CmbBxConvertToChange(Sender: TObject);
    procedure CmbBxReminderActionChange(Sender: TObject);
    procedure CmbBxTimeChange(Sender: TObject);
    procedure CmbBxCountdownActionChange(Sender: TObject);
    procedure CountdownTimerTimer(Sender: TObject);
    procedure dtEdtEventDateChange(Sender: TObject);
    procedure DtReminderEventChange(Sender: TObject);
    procedure edtConverionValueChange(Sender: TObject);
    procedure edtEventNameChange(Sender: TObject);
    procedure edtMemoKeyChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstBxEventsClick(Sender: TObject);
    procedure LstBxMemoNameClick(Sender: TObject);
    procedure mainIdleTimerStartTimer(Sender: TObject);
    procedure mainIdleTimerTimer(Sender: TObject);
    procedure MmMemoDataChange(Sender: TObject);
    procedure mnuItmClick(Sender: TObject);
    procedure mnuItmOptionsClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure mainTimerTimer(Sender: TObject);
    procedure PopupNotifier1Close(Sender: TObject; var CloseAction: TCloseAction);
    procedure ppMnItmExitClick(Sender: TObject);
    procedure ppMnItmShowClick(Sender: TObject);
    procedure ppMnItmTimeClick(Sender: TObject);
    procedure ReminderTimerTimer(Sender: TObject);
    procedure spdBtnClick(Sender: TObject);
    procedure SpnEdtCountdownChange(Sender: TObject);
    procedure SpnReminderHourChange(Sender: TObject);
    procedure SpnReminderMinsChange(Sender: TObject);
    procedure SpnEdtTimeBaseChange(Sender: TObject);
    procedure timerTimerTimer(Sender: TObject);
    procedure tmrMemoTimer(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    procedure DisplayMessage;
    procedure StopCountDown(Sender: TObject);
    procedure enableSpeedButtons(mode: boolean);
    procedure SetDefaults;
    procedure resetReminder;
    procedure ReminderTimerStop(Sender: TObject);
    procedure ReminderValid;
    procedure UpdateStatusBar(KTime: TDateTime);
    procedure UpdateTime;
    procedure UpdateWorldKlock(KTime: TDateTime);
    procedure setMemoButtons(mode: Boolean);
    procedure setFriendsButtons(mode: Boolean);
    procedure setEventButtons(mode: Boolean);
    procedure displayEvent(pos: integer);
    procedure displayMemo(pos: integer);
    procedure displayFriends(pos: integer);
    procedure displayEncryptedMemo;
    procedure loadevents;
    procedure loadMemos;
    procedure loadFriends;
    procedure callInfo(mode: string);
    procedure EnhancedBiorhythmClick;
    procedure setUpFuzzyOptions;
    procedure setUpEventsOptions;
    procedure parseTimeZoneData;

  public

  end;

CONST
  PASSWORD = 'KLOCK';          //  Default password used to decypt memos.

var
  frmMain          : TfrmMain;
  userOptions      : Options;     //  used to hold all the user options.
  ft               : FuzzyTime;   //  the object to give the different times.
  fs               : fontStore;   //  used to handle custom fonts i.e. load & remove
  kLog             : Logger;      //  used to log errors, debug statements etc.
  stickies         : stickyNotes; //  used to store the Sticky Notes.
  memorandum       : Memos;       //  used to store memos.
  ev               : Events;      //  used to store events.
  fr               : Friends;     //  used to store friends.
  timeZone         : TPascalTZ;   //  used for world klock time zones.
  ConversionUnits  : TStrings;    //  used to hold the conversions units - read from file.
  unitConvertVal   : double;      //  used to hold the conversion value.
  unitConvertfactor: double;      //  used to hold the conversion factor.
  appStartTime     : int64;       //  used by formAbout to determine how long the app has been running.
  countdownTicks   : integer;
  timerStart       : TDateTime;
  timerPaused      : TdateTime;
  popupMessages    : FourStrings;
  popupTitle       : FourStrings;
  idleTime         : TdateTime;
  nowTime          : string;
  prvTime          : string;

implementation

{$R *.lfm}

{ TfrmMain }
//
// *********************************************************** Global **********
//

procedure TfrmMain.FormCreate(Sender: TObject);
{  Called at start - sets up fuzzy time and default sound files.
}
begin
  kLog := Logger.Create;
  userOptions := Options.Create;   //  create options file as c:\Users\<user>\AppData\Local\Stub\Options.xml

  logHeader;

  logMessage('Klock Create');      //  Write to log file and splash screen.

  mainTimer.Enabled := False;  //  disable main timer until all options and fuzzy time are set up.

  EdtCountdownSound.Text := 'alarm-fatal.mp3';
  EdtReminderSound.Text  := 'alarm-fatal.mp3';

  appStartTime    := GetTickCount64;      //  tick count when application starts.
  ft              := FuzzyTime.Create;    //  Create the fuzzyTime object
  ev              := Events.Create;       //  Create the event store object.
  fr              := Friends.Create;      //  Create the friends store object.
  ConversionUnits := TStringList.Create;  //  Create the conversions object.
  stickies        := stickyNotes.Create;  //  Create the sticky nore store object.
  memorandum      := Memos.Create;        //  Create the memos store object.

  if userOptions.useCustomFonts then
    fs := fontStore.Create;  //  Create the font store objects, if needed.

  if userOptions.cullLogs then     //  Removed old log files, if instructed.
    kLog.cullLogFile(userOptions.CullLogsDays);

  frmClipBoard.cullTmpFiles;       //  Remove old .tmp files left over from clipboard operations.

  with mainIdleTimer do            //  set up the idle timer.
  begin
    AutoEnabled    := True;
    AutoStartEvent := itaOnIdle;
    AutoEndEvent   := itaOnUserInput;
    Interval       := 1000;
    Enabled        := False;
  end;

  parseTimeZoneData;

  stickies.restoreStickyNotes;   //  Reload Sticky Notes, if any.
  memorandum.restoreMemos;       //  Reload Memos into memo store, if any.
  ev.restoreEvents;              //  Reload Events into event store, if any.
  fr.restoreFriends;             //  Reload Friends into event store, if any.

  DoubleBuffered := true;
  stsBrInfo.DoubleBuffered := true;
end;

procedure TfrmMain.parseTimeZoneData;
var
  s : TStringList;
begin
  //  Load and parse the time zone data base.
  logMessage('Load and parse the time zone data base.');  //  Write to log file and splash screen.
  timeZone              := TPascalTZ.Create;
  timeZone.DatabasePath := 'tzdata';
  timeZone.ParseDatabaseFromDirectory('tzdata');

  //  Load the time zones into the combo box.
  logMessage('Load the time zones into the combo box');  //  Write to log file and splash screen.
  s := TStringList.Create;
  timeZone.GetTimeZoneNames(s, false);
  CmbBxTimeZones.Items.AddStrings(s);
  CmbBxTimeZones.ItemIndex := 0;
  s.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  kLog.writeLog('FormKlock Showing');

  SetDefaults;

  mainTimer.Enabled := True;        //  Now safe to enable main timer.

  nowTime := ft.getTime;
  prvTime := 'Klock';               //  so times are different first time

  frmClipBoard.DoubleBuffered   := true;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{  called on form close, save screen position if needed.
}
begin
  kLog.writeLog('FormKlock Closing');

  frmSplashScreen := TfrmSplashScreen.Create(nil);
  frmSplashScreen.Show;
  frmSplashScreen.Update;

  Visible := False;

  if frmClipBoard.Visible then frmClipBoard.Visible := false;

  logSplashFooter;            // to populate splash screen top memo

  if userOptions.screenSave then
  begin
    userOptions.formTop  := frmMain.Top;
    userOptions.formLeft := frmMain.Left;
    userOptions.writeCurrentOptions;
  end;

  //  if clipboard manager active, we need to save its position - if needed.
  if userOptions.CB_ScreenSave then
  begin
    userOptions.CB_formTop  := frmClipBoard.Top;
    userOptions.CB_formLeft := frmClipBoard.Left;
    userOptions.writeCurrentOptions;
  end;

  klog.writeLog('Killing Events');
  ev.killEvents;

  kLog.writeLog('Updated Sticky Note File');
  stickies.updateStickyNotes;

  if userOptions.useCustomFonts and Assigned(fs) then
  begin
    fs.removeFonts;                   //  Remove all fonts from system.
    fs.Free;                          //  Release the font store object.
  end;

  ft.Free;                            //  Release the fuzzy time object.
  ev.Free;                            //  Release the events store object.
  fr.Free;                            //  release the friends store objects.
  stickies.Free;                      //  Release the Sticky Note store.
  memorandum.Free;                    //  Release the Memo store.
  userOptions.Free;                   //  Release the user options.
  timeZone.Free;                      //  Release the Time Lone object.
  ConversionUnits.Free;               //  release the Conversion string list.

  logFooter;

  frmSplashScreen.Hide;
  frmSplashScreen.Free;

  CloseAction := caFree;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  kLog.Free;                          //  Release the logger object.
end;

procedure TfrmMain.SetDefaults;
{  called to set defaults on start-up.
   Set things that can be changed in the options screen, to the values in the options screen.
}
begin
  kLog.writeLog('FormKlock SetDefaults');

  mainIdleTimer.Enabled := userOptions.displayIdleTime;  //  switch on main timer.

  PageControl1.TabIndex := userOptions.defaultTab;

  DtReminderEvent.Date   := now;                       //  set up reminder stuff.
  SpnReminderMins.Value  := MinuteOf(time);
  SpnReminderHour.Value  := HourOf(time);
  btnReminderSet.Enabled := False;

  SpnEdtTimeBase.Visible := False;
  lblRadix.Visible       := False;

  setMemoButtons(false);            //  set up memo buttons.
  setFriendsButtons(false);         //  set up friends buttons.
  setEventButtons(false);           //  set up event buttons.

  setUpFuzzyOptions;                //  Set up all things fuzzy.

  setUpEventsOptions;               //  Set up events user options - might have changed?
  ev.updateEvents;                  //  Update due dates, needs to be done after set aged days. .

  if userOptions.screenSave then
  begin
    frmMain.Top  := userOptions.formTop;
    frmMain.Left := userOptions.formLeft;
  end;

  CmbBxName.Enabled := userOptions.useCustomFonts; //  Only enable font combo box if needed.

  if userOptions.netTimeSeconds and (CmbBxTime.Items[CmbBxTime.ItemIndex] = 'NET Time') then
    mainTimer.Interval := 1
  else
    mainTimer.Interval := 1000;

  klog.writeLog(format('Main timer interval set to %D milliseconds', [mainTimer.Interval]));
end;

procedure TfrmMain.setUpFuzzyOptions;
{  set up defaults for fuzzyTime and other stuff.  }
begin
  ft.displayFuzzy     := userOptions.defaultTime;
  ft.fuzzyTimeVerbose := userOptions.fuzzyTimeVerbose;
  ft.display24Hour    := userOptions.display24Hour;
  ft.netTimeSeconds   := userOptions.netTimeSeconds;
  ft.swatchCentibeats := userOptions.swatchCentibeats;
  ft.speakTimeVolume  := userOptions.speakTimeVolume;
  ft.fuzzyBase        := 2;

  CmbBxTime.Items     := ft.fuzzyTypes;                 //  set up time combo box.
  CmbBxTime.ItemIndex := userOptions.defaultTime;

  if userOptions.useCustomFonts then
  begin
    CmbBxName.Items     := fs.fontTypes;                //  set up font combo box.
    CmbBxName.ItemIndex := 0;
  end;

  lblfuzzy.Top       := 8;                              //  defaults for fuzzy time label.
  lblfuzzy.Left      := 4;
  lblfuzzy.Font.Size := 22;
  lblfuzzy.AutoSize  := true;

  lblTZTime.Top       := 8;                             //  defaults for world klock time label.
  lblTZTime.Left      := 4;
  lblTZTime.Font.Size := 22;
  lblTZTime.AutoSize  := true;
end;

procedure TfrmMain.DisplayMessage;
{  display a message as a pop-up notifier.
   If this procedure is called with an empty message array,
   the pop-up notifier is cancelled.
}
var
  f      : integer;
  title  : string;       //  do we need to set title?
  message: string;
begin
  title   := '';
  message := '';

  for f := 0 to 3 do
  begin
    if (popupMessages[f] <> '') then
    begin
      message := message + popupMessages[f] + LineEnding;
    end;
    if (popupTitle[f] <> '') then
    begin
      Title := Title + popupTitle[f] + ' : ';
    end;
  end;

  with PopupNotifier1 do
  begin
    ShowAtPos(100, 100);
    Color := clyellow;
    Title := title;
    Text  := message;

    if (Visible = False) then  // if not currently shown, show
      Visible := True;

    if (message = '') then
    begin    //  empty message array, close pop-up notifier
      Visible := False;
      Free;
    end;
  end;  //  with PopupNotifier1 do

end;

procedure TfrmMain.PageControl1Change(Sender: TObject);
{   called when tabs is changed on the main tab control.
    Sets the appropriate information for each tab.
        0 = Time
        1 = World Klock
        2 = Countdown
        3 = Timer
        4 = Reminder
        5 = Friends
        6 = Events
        7 = memo
        8 = conversion
}
begin
  setMemoButtons(false);
  setFriendsButtons(false);
  setEventButtons(false);

  case PageControl1.TabIndex of
    0:
    begin                               //  fuzzy page.
      stsBrInfo.Panels.Items[4].Text := '';
    end;
    //1:                                //  world klock.
    2:
    begin                               //  countdown page
      if CountdownTimer.Enabled = False then
        stsBrInfo.Panels.Items[4].Text := ''
      else
        stsBrInfo.Panels.Items[4].Text := format(' Counting down from %2.d minute[s]', [SpnEdtCountdown.Value]);
    end;
    3:
    begin                               //  timer page.
      stsBrInfo.Panels.Items[4].Text := '';

      if timerTimer.Enabled = False then
      begin

        if btnTimerStart.Caption = 'Resume' then
          stsBrInfo.Panels.Items[4].Text := 'Timer :: Paused';

        if btnTimerStart.Caption = 'Start' then
        begin
          if userOptions.timerMilliSeconds then
          begin
            lblTimer.Caption    := '00:00:00:00';
            lblSplitLap.Caption := '00:00:00:00';
          end
          else
          begin
            lblTimer.Caption    := '00:00:00';
            lblSplitLap.Caption := '00:00:00';
          end;  //  if OptionsRec.TimerMilliSeconds
        end;    //  btnTimerStart.Caption = 'Start'
      end;      //  if timerTimer.Enabled = false
    end;
    4:
    begin                               //  Reminder page.
      stsBrInfo.Panels.Items[4].Text := '';
      if ReminderTimer.Enabled = False then
      begin   // only set display to current
        DtReminderEvent.Date   := now;
        SpnReminderMins.Value  := MinuteOf(time);
        SpnReminderHour.Value  := HourOf(time);
        btnReminderSet.Enabled := False;
      end
      else
      begin
        stsBrInfo.Panels.Items[4].Text := format('Reminder set for %.2d:%.2d - %s', [SpnReminderHour.Value,
                                                        SpnReminderMins.Value, DatetoStr(DtReminderEvent.Date)]);
      end;  //  if btnReminderSet.Enabled
    end;
    5:                                  //  friends page
    begin
      loadFriends;                      //  Load Friends store into listbox, if any.
      setFriendsButtons(true);
    end;
    6:                                  //  events page
    begin
      loadEvents;                       //  Load Events store into listbox, if any.
      setEventButtons(true);
    end;
    7:                                  //  memo page.
    begin
      loadMemos;                        //  Load Memos store into listbox, if any.
      setMemoButtons(true);
    end;
    8:
    begin                               //  Conversion Page.
      checkConversionUnitsFile;         //  Create conversion Units file is it does not exist.
      readConversionUnitsFile;
      parseConversionUnitsFile('LoadCategory');
      parseConversionUnitsFile('LoadUnits');
      cleartextFiles;
    end;
  end;

end;
//
// *********************************************************** Main Timer ******
//
procedure TfrmMain.mainTimerTimer(Sender: TObject);
{  on every tick on the clock, update the system.
      Update real time to status panel.
      Update desired time to either to main program, tray icon hint or pop-up notifier.
}
var
  myNow: TdateTime;
  strTime: string;
begin
  myNow := now;

  if isMinute(myNow, 0) then        //  Every hour
  begin
    stickies.updateStickyNotes;     //  Update Sticky Notes file.
    ev.updateEvents;                //  Update due dates.
  end;

  //  if switched on, speak time every n minutes.
  //  where n is set in userOptions.speakTimeDuration.
  if userOptions.speakTime and everyMinute(myNow, userOptions.speakTimeDuration) then
    ft.speakTime;

  if userOptions.HourPips and isMinute(myNow, 0) then
    playChime('pips')
  else                                       //  only play chimes if pips turned off.
  begin
    if userOptions.HourChimes         and isMinute(myNow, 0)  then playChime('hour');
    if userOptions.quarterChimes      and isMinute(myNow, 15) then playChime('quarter');
    if userOptions.HalfChimes         and isMinute(myNow, 30) then playChime('half');
    if userOptions.threeQuarterChimes and isMinute(myNow, 45) then playChime('threequarter');
  end;

  if TrayIcon.Visible then
   begin
     strTime := CmbBxTime.Items.Strings[CmbBxTime.ItemIndex] + ' time :: ' + ft.getTime;
     TrayIcon.Hint := strTime;

     if userOptions.fuzzyTimeBalloon and everyMinute(myNow, 10) then
     begin  //  only display on the five minutes.
       TrayIcon.BalloonHint := strTime;
       trayIcon.ShowBalloonHint;
       ballonTimer.Enabled := True;                  //  balloon hint time-out bug - see below.
     end;  //  if OptionsRec.FuzzyTimeBalloon then

     if ppMnItmTime.Checked and isMinute(myNow, 10) then
     begin
       popupTitle[0]    := 'Time';
       popupMessages[0] := strTime;
       DisplayMessage;
     end;      //  if ppMnItmTime.Checked then begin
   end         //  if TrayIcon.Visible then
   else
   begin  //  normal display i.e. not trayicon
     if PageControl1.TabIndex = 0 then        //  Display FuzzyTime.
       UpdateTime;
     if PageControl1.TabIndex = 1 then        //  Display World Klock.
       UpdateWorldKlock(myNow);

     UpdateStatusBar(myNow);
   end;  //  if TrayIcon.Visible then

end;

procedure TfrmMain.UpdateTime;
{  Updates the time in the correct font.    }
begin
  nowTime := ft.getTime;      //  No need to update ever second in showing
                              //  time in words or fuzzytime etc
                              //  i.e. time changes every one or five minutes.

  if nowTime = prvTime then   //  if times are the same, no need to do owt.
    exit
  else
    prvTime := nowTime;

  lblfuzzy.Caption   := ft.getTime;

  if userOptions.useCustomFonts then
  begin
    lblfuzzy.Font.Name := fs.getFont(CmbBxName.ItemIndex);
    if userOptions.christmasFont       and isChristmas  then lblfuzzy.Font.Name := 'Christmas'
    else if userOptions.easterFont     and isEaster     then lblfuzzy.Font.Name := 'RMBunny'
    else if userOptions.valentinesFont and isValentines then lblfuzzy.Font.Name := 'Sweet Hearts BV'
    else if userOptions.haloweenFont   and isHalloween  then lblfuzzy.Font.Name := 'Groovy Ghosties';
  end;

  lblfuzzy.AdjustFontForOptimalFill;
end;

procedure TfrmMain.UpdateWorldKlock(KTime: TDateTime);
{  Updates the World Klock time in the correct font.    }
var
  DateTime: TDateTime;
  s: string;
begin
  s := CmbBxTimeZones.Items[CmbBxTimeZones.ItemIndex];
  DateTime            := timeZone.GMTToLocalTime(KTime, s);
  lblTZTime.Caption   := DateTimeToStr(DateTime);
  if userOptions.useCustomFonts then
    lblTZTime.Font.Name := fs.getFont(CmbBxTZFonts.ItemIndex);

  lblTZTime.AdjustFontForOptimalFill;
end;

procedure TfrmMain.UpdateStatusBar(KTime: TDateTime);
{  Updates the status bar.    }
VAR
  keyResult: string;
begin
  stsBrInfo.BeginUpdate;

  keyResult := ' cns ';
  if LCLIntf.GetKeyState(VK_CAPITAL) <> 0 then keyResult[2] := 'C';
  if LCLIntf.GetKeyState(VK_NUMLOCK) <> 0 then keyResult[3] := 'N';
  if LCLIntf.GetKeyState(VK_SCROLL)  <> 0 then keyResult[4] := 'S';

  if userOptions.display24Hour then
    stsBrInfo.Panels.Items[0].Text := FormatDateTime('hh:nn:ss', KTime)
  else
    stsBrInfo.Panels.Items[0].Text := FormatDateTime('hh:nn:ss am/pm', KTime);

  stsBrInfo.Panels.Items[1].Text := FormatDateTime('DD MMM YYYY', KTime);
  stsBrInfo.Panels.Items[2].Text := keyResult;

  if userOptions.displayIdleTime then
    stsBrInfo.Panels.Items[3].Text := 'Idle Time :: ' + FormatDateTime('hh:nn:ss', idleTime)
  else
    stsBrInfo.Panels.Items[3].Text := '';

  stsBrInfo.EndUpdate;
end;

procedure TfrmMain.ballonTimerTimer(Sender: TObject);
{  There seems to be a bug in lcl, the balloon time-out does not work.
   So, a timer is fired when the balloon hint is displayed and then
   10 seconds later this kludge is performed.
}
begin
  ballonTimer.Enabled := False;
  TrayIcon.Visible    := False;
  TrayIcon.Visible    := True;
end;
//
// *********************************************************** Idle Timer ******
//
procedure TfrmMain.mainIdleTimerTimer(Sender: TObject);
{  runs when the system is idle.
   Adds a second to the previously create zero TDateTime.
   Checks for, and if needed, Keeps monitor Awake.
}
begin
  idleTime := IncSecond(idleTime,1);

  {  if system is idle, then keep monitor awake if required.    }
  if userOptions.keepMonitorAwake and
    everyMinute(idleTime, userOptions.keepMonitorAwakeMinutes) then
    keepMonitorAwake;
end;

procedure TfrmMain.mainIdleTimerStartTimer(Sender: TObject);
{  On start of system idle, create a zero tDateTime.    }
begin
  idleTime := EncodeDateTime(2018, 1, 1, 0, 0, 0, 0);
end;
//
// *********************************************************** Fuzzy Time ******
//
procedure TfrmMain.CmbBxTimeChange(Sender: TObject);
{  called to set the different format of time.
   If index = 9 then radix time is chosen, so display choice of bases.
}
begin
  klog.writeLog('Fuzzy time selected : ' + CmbBxTime.Items[CmbBxTime.ItemIndex]);

  ft.displayFuzzy  := CmbBxTime.ItemIndex;
  lblfuzzy.Caption := ft.getTime;

  if CmbBxTime.Items[CmbBxTime.ItemIndex] = 'Radix Time' then   //  hard coded for radix time.
  begin
    SpnEdtTimeBase.Visible := True;
    lblRadix.Visible       := True;
    ft.FuzzyBase           := SpnEdtTimeBase.Value;
  end
  else
  begin
    SpnEdtTimeBase.Visible := False;
    lblRadix.Visible       := False;
  end;

  if userOptions.netTimeSeconds and (CmbBxTime.Items[CmbBxTime.ItemIndex] = 'NET Time') then
    mainTimer.Interval := 1
  else
    mainTimer.Interval := 1000;

  klog.writeLog(format('Main timer interval set to %D milliseconds', [mainTimer.Interval]));
end;

procedure TfrmMain.SpnEdtTimeBaseChange(Sender: TObject);
begin
  ft.FuzzyBase := SpnEdtTimeBase.Value;
end;
//
// *********************************************************** Countdown *******
//
procedure TfrmMain.CmbBxCountdownActionChange(Sender: TObject);
{  Set the desired action, for when the countdown is completed.
}
begin
  if CmbBxCountdownAction.ItemIndex = 0 then
  begin  //  Sound chosen
    chckBxCountdownSound.Visible  := True;
    EdtCountdownSound.Visible     := True;
    btnCountdownLoadSound.Visible := True;
    btnSoundTest.Visible          := True;
  end
  else
  begin
    chckBxCountdownSound.Visible  := False;
    EdtCountdownSound.Visible     := False;
    btnCountdownLoadSound.Visible := False;
    btnSoundTest.Visible          := False;
  end;

  if CmbBxCountdownAction.ItemIndex = 1 then
  begin  //  Reminder chosen
    chckBxCountdownReminder.Visible := True;
    EdtCountdownReminder.Visible    := True;
  end
  else
  begin
    chckBxCountdownReminder.Visible := False;
    EdtCountdownReminder.Visible    := False;
  end;

  if CmbBxCountdownAction.ItemIndex = 2 then
  begin  //  System event chosen
    chckBxCountdownEvent.Visible := True;
    CmbBxCountdownEvent.Visible  := True;
  end
  else
  begin
    chckBxCountdownEvent.Visible := False;
    CmbBxCountdownEvent.Visible  := False;
  end;

  if CmbBxCountdownAction.ItemIndex = 3 then
  begin  //  command chosen
    chckBxCountdownCommand.Visible  := True;
    btnCountdownLoadCommand.Visible := True;
    EdtCountdownCommand.Visible     := True;
  end
  else
  begin
    chckBxCountdownCommand.Visible  := False;
    btnCountdownLoadCommand.Visible := False;
    EdtCountdownCommand.Visible     := False;
  end;
end;

procedure TfrmMain.btnCountdownStartClick(Sender: TObject);
{ called when start button is clicked, can have three modes
      Start  :: Start countdown
      Pause  :: Pause countdown
      Resume :: Resume a paused countdown.
}
var
  val: integer;

begin
  enableSpeedButtons(false);
  if btnCountdownStart.Caption = 'Start' then
  begin
    btnCountdownStop.Enabled       := True;
    CountdownTimer.Enabled         := True;
    SpnEdtCountdown.Enabled        := False;
    val                            := CountdownTicks div 60;    //  in case the status message has changed
    stsBrInfo.Panels.Items[4].Text := format(' Counting down from %d minute[s]', [val]);
    btnCountdownStart.Caption      := 'Pause';

    if (chckBxCountdownCommand.Checked) and (EdtCountdownCommand.Text = '') then
    begin
      popupTitle[1] := 'Countdown';
      popupMessages[1] := 'er, need to give Klock a command.';
      DisplayMessage;
      btnCountdownStopClick(Sender);  //  pretend the stop button has been pressed.
    end;
  end
  else if btnCountdownStart.Caption = 'Pause' then
  begin
    CountdownTimer.Enabled    := False;
    btnCountdownStart.Caption := 'Resume';
    frmMain.Caption           := 'Countdown :: PAUSED';
    application.Title         := 'Paused';
  end
  else if btnCountdownStart.Caption = 'Resume' then
  begin
    CountdownTimer.Enabled    := True;
    btnCountdownStart.Caption := 'Pause';
  end;

end;

procedure TfrmMain.btnCountdownLoadSoundClick(Sender: TObject);
{  if the text box is clicked, allow the sound file to be changed.
}
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter     := '*.wav; *.mp3';
    InitialDir := getCurrentDir + '\sounds';
    Title      := 'Choose a sound file.';
    if Execute then
    begin
      EdtCountdownSound.Text         := ExtractFileName(FileName);
      stsBrInfo.Panels.Items[4].Text := Filename + ' Chosen';
    end;    //  if Execute
    Free;
  end;
end;

procedure TfrmMain.btnCountdownLoadCommandClick(Sender: TObject);
{  if the command box is clicked, allow the command file to be loaded.
}
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter     := '*.*';
    InitialDir := getCurrentDir;
    Title      := 'Choose a executable';
    if Execute then
    begin
      EdtCountdownCommand.Text       := (FileName);
      stsBrInfo.Panels.Items[4].Text := Filename + ' Chosen';
    end;    //  if Execute
    Free;
  end;
end;

procedure TfrmMain.btnCountdownStopClick(Sender: TObject);
{  Called when the countdown stop button is clicked.
}
begin
  btnCountdownStart.Enabled := True;
  btnCountdownStart.Caption := 'Start';
  btnCountdownStop.Enabled  := False;
  CountdownTimer.Enabled    := False;
  SpnEdtCountdown.Enabled   := True;

  frmMain.Caption          := 'Countdown';
  application.Title        := 'Countdown';
  LblCountdownTime.Caption := '00:00';

  enableSpeedButtons(true);
end;

procedure TfrmMain.SpnEdtCountdownChange(Sender: TObject);
{    called when the time is entered - only allow 1 - 120 minutes.
}
var
  val: integer;                 //  used to hold value from spin edit
                                //  can't pass this to the function directly
begin
  val := SpnEdtCountdown.Value;

  if (val > 0) and (val <= 120) then
  begin
    LblCountdownTime.Caption  := format('%2.2d:00', [val]);
    btnCountdownStart.Enabled := True;
    countdownTicks            := val * 60;

    stsBrInfo.Panels.Items[4].Text := format(' Counting down from %d minute[s]', [val]);
  end
  else
  begin
    LblCountdownTime.Caption  := '00:00';
    btnCountdownStart.Enabled := False;
    countdownTicks            := 0;

    stsBrInfo.Panels.Items[4].Text := ' Only allow 1 - 90 minutes';
  end;
end;

procedure TfrmMain.StopCountDown(Sender: TObject);
{    Called when the timer has finished.    }
begin
  LblCountdownTime.Caption := '00:00';

  btnCountdownStart.Enabled := True;       //  reset buttons
  btnCountdownStart.Caption := 'Start';
  btnCountdownStop.Enabled  := False;
  SpnEdtCountdown.Enabled   := True;
  CountdownTimer.Enabled    := False;

  stsBrInfo.Panels.Items[4].Text := ' Finished counting, now!';
  frmMain.Caption                := 'Countdown';
  application.Title              := 'Countdown';

  if chckBxCountdownSound.Checked then
  begin      //  only play sound if checked
    doPlaySound(EdtCountdownSound.Text, userOptions.volume);
    chckBxCountdownSound.Checked := False;
    ChckBxCountdownSoundChange(Sender);    //  now box is unchecked, call change procedure
  end;

  if chckBxCountdownReminder.Checked then
  begin   //  only display reminder if checked
    popupTitle[1]    := 'Countdown';
    popupMessages[1] := EdtCountdownReminder.Text;
    DisplayMessage;
    chckBxCountdownReminder.Checked := False;
    chckBxCountdownReminderChange(Sender);    //  now box is unchecked, call change procedure
  end;

  if chckBxCountdownEvent.Checked then
  begin      //  only do event if checked
    btnCountdownShutdownAbort.Visible := True;
    doSystemEvent(CmbBxCountdownEvent.ItemIndex);
    chckBxCountdownEvent.Checked := False;
    chckBxCountdownEventChange(Sender);    //  now box is unchecked, call change procedure

    if TrayIcon.Visible then
    begin               //  if running in the system tray,
      ppMnItmShowClick(Sender);                  //  select the reminder tab and
      PageControl1.TabIndex := 1;                //  display main application so
    end;                                         //  the abort button can be used.
  end;

  if chckBxCountdownCommand.Checked then
  begin   //  only do command if checked
    doCommandEvent(EdtCountdownCommand.Text, '');
    chckBxCountdownCommand.Checked := False;
    chckBxCountdownCommandChange(Sender);    //  now box is unchecked, call change procedure
  end;

  //  reset the noOfTicks, so we start the timer again without changing the time.
  //  should be okay, already validated [if time is changed will be revalidated]
  countdownTicks := SpnEdtCountdown.Value * 60;

  enableSpeedButtons(true);
end;

procedure TfrmMain.btnCountdownShutdownAbortClick(Sender: TObject);
{  button only visible during delay prior to a system shutdown/reboot.
   Allows user to abort action.
   Also tidies up application = a bit messy I'm afraid.
}
begin
  abortSystemEvent;

  PopupNotifier1.Visible := False;

  btnCountdownShutdownAbort.Visible := False;
  CmbBxCountdownEvent.Visible       := False;
  chckBxCountdownEvent.Checked      := False;
end;

procedure TfrmMain.CountdownTimerTimer(Sender: TObject);
{ tick of countdown timer - called every 1 second.
}
var
  minutes: integer;
  seconds: integer;
  message: string;

begin
  countdownTicks := countdownTicks - 1;

  if countdownTicks = 0 then
    StopCountDown(Sender);

  if countdownTicks < 60 then
    message := format('00:%2.2d', [countdownTicks])
  else
  begin
    minutes := countdownTicks div 60;
    seconds := countdownTicks mod 60;
    message := format('%2.2d:%2.2d', [minutes, seconds]);
  end;

  LblCountdownTime.Caption := message;
  application.Title        := message;
  frmMain.Caption          := 'Countdown :: ' + message;
end;

procedure TfrmMain.btnSoundTestClick(Sender: TObject);
{  Called to test the sound file.    }
begin
  doPlaySound(EdtCountdownSound.Text, userOptions.volume);
end;

procedure TfrmMain.ChckBxCountdownSoundChange(Sender: TObject);
{  Called to enable/disable the sound - from a check box.    }
begin
  if chckBxCountdownSound.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'Sound Enabled';
    EdtCountdownSound.Enabled      := True;
    btnCountdownLoadSound.Enabled  := True;
    btnSoundTest.Enabled           := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Sound Disabled';
    EdtCountdownSound.Enabled      := False;
    btnCountdownLoadSound.Enabled  := False;
    btnSoundTest.Enabled           := False;
  end;
end;

procedure TfrmMain.chckBxCountdownReminderChange(Sender: TObject);
{  enable or disable reminders.    }
begin
  if chckBxCountdownReminder.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'Reminder Enabled';
    EdtCountdownReminder.Enabled   := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Reminder Disabled';
    EdtCountdownReminder.Enabled   := False;
  end;
end;

procedure TfrmMain.chckBxCountdownEventChange(Sender: TObject);
{  enable or disable system events.    }
begin
  if chckBxCountdownEvent.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'System Event Enabled';
    CmbBxCountdownEvent.Enabled    := True;
    CmbBxCountdownEvent.ItemIndex  := 0;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'System Event Disabled';
    CmbBxCountdownEvent.Enabled    := False;
  end;
end;

procedure TfrmMain.chckBxCountdownCommandChange(Sender: TObject);
{  enable or disable commands.    }
begin
  if chckBxCountdownCommand.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text  := 'Command Enabled';
    btnCountdownLoadCommand.Enabled := True;
    EdtCountdownCommand.Enabled     := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text  := 'Command Disabled';
    btnCountdownLoadCommand.Enabled := False;
    EdtCountdownCommand.Enabled     := False;
  end;
end;

procedure Tfrmmain.spdBtnClick(Sender: TObject);
{  Set and run the countdown time.

   A generic handler for the countdown speed buttons.
   Each button is named spdBtnNNN, where NNN is the time interval in minutes.
}
VAR
  btnName  : string;
  btnValue : integer;
begin
  //  if not called by a click on a speed button then exit.
  if not (Sender is TSpeedButton) then Exit;

  //  Must be a TSpeedButton, grab name.
  btnName := TSpeedButton(Sender).Name;
  Delete(btnName, 1, 6);            //  delete spdBtn from start of name.
  btnValue  := strToInt(btnName);

  SpnEdtCountdown.Value := btnValue;
  btnCountdownStart.Click;
  enableSpeedButtons(false);
end;

procedure TfrmMain.enableSpeedButtons(mode: boolean);
{  Set the enable property of the speed buttons as desired.    }
begin
  SpdBtn120.Enabled := mode;
  SpdBtn90.Enabled  := mode;
  SpdBtn60.Enabled  := mode;
  SpdBtn30.Enabled  := mode;
end;

//
// *********************************************************** Timer ***********
//
procedure TfrmMain.timerTimerTimer(Sender: TObject);
{  if time is enables, this will be the timer tick.    }
var
  hh, mm, ss, ms: word;
  timerInterval: TDateTime;
begin
  timerInterval := timerPaused + (time - timerStart);
  DecodeTime(timerInterval, hh, mm, ss, ms);
  if userOptions.timerMilliSeconds then
    lblTimer.Caption := format('%.2d:%.2d:%.2d:%.2d', [hh, mm, ss, ms])
  else
    lblTimer.Caption := format('%.2d:%.2d:%.2d', [hh, mm, ss]);
end;

procedure TfrmMain.btnTimerStartClick(Sender: TObject);
{ called when start button is clicked, can have three modes
      Start  :: Start timer
      Pause  :: Pause timer
      Resume :: Resume a paused timer.
}
begin
  if btnTimerStart.Caption = 'Start' then
  begin

    if userOptions.timerMilliSeconds then
      timerTimer.Interval          := 100;
    timerStart                     := time;
    timerPaused                    := 0;
    btnTimerStop.Enabled           := True;
    timerTimer.Enabled             := True;
    btnTimerClear.Enabled          := False;
    btnTimerStart.Caption          := 'Pause';
    btnTimerSplit.Enabled          := True;
    lblSplitLap.Enabled            := True;
    frmMain.Caption                := 'Timer :: Started';
    stsBrInfo.Panels.Items[4].Text := 'Timer Running';
  end
  else if btnTimerStart.Caption = 'Pause' then
  begin
    timerPaused                    := timerPaused + (time - timerStart);
    btnTimerStart.Caption          := 'Resume';
    timerTimer.Enabled             := False;
    btnTimerSplit.Enabled          := False;
    lblSplitLap.Enabled            := False;
    frmMain.Caption                := 'Timer :: Paused';
    stsBrInfo.Panels.Items[4].Text := 'Timer :: Paused';
  end
  else if btnTimerStart.Caption = 'Resume' then
  begin
    timerStart                     := time;
    btnTimerStart.Caption          := 'Pause';
    timerTimer.Enabled             := True;
    btnTimerSplit.Enabled          := True;
    lblSplitLap.Enabled            := True;
    frmMain.Caption                := 'Timer :: Started';
    stsBrInfo.Panels.Items[4].Text := 'Timer Running';
  end;
end;

procedure TfrmMain.btnTimerStopClick(Sender: TObject);
{  Stop the timer.    }
begin
  btnTimerStop.Enabled           := False;
  timerTimer.Enabled             := False;
  btnTimerSplit.Enabled          := False;
  lblSplitLap.Enabled            := False;
  btnTimerClear.Enabled          := True;
  btnTimerStart.Caption          := 'Start';
  frmMain.Caption                := 'Timer :: Stoped';
  stsBrInfo.Panels.Items[4].Text := 'Timer :: Stoped';
end;

procedure TfrmMain.btnTimerSplitClick(Sender: TObject);
begin
  lblSplitLap.Caption := lblTimer.Caption;
end;

procedure TfrmMain.btnTimerClearClick(Sender: TObject);
{  Reset [clear] the timer.    }
begin
  if userOptions.timerMilliSeconds then
  begin
    lblTimer.Caption    := '00:00:00:00';
    lblSplitLap.Caption := '00:00:00:00';
  end
  else
  begin
    lblTimer.Caption    := '00:00:00';
    lblSplitLap.Caption := '00:00:00';
  end;  //  if userOptions.timerMilliSeconds = 'True' then

  stsBrInfo.Panels.Items[4].Text := '';

  btnTimerSplit.Enabled := False;
  lblSplitLap.Enabled   := False;
end;
//
// *********************************************************** Reminder ********
//
procedure TfrmMain.SpnReminderHourChange(Sender: TObject);
{  will one day be used to validate the hours set.    }
begin
  ReminderValid;
end;

procedure TfrmMain.SpnReminderMinsChange(Sender: TObject);
{  will one day be used to validate the minute set.    }
begin
  ReminderValid;
end;

procedure TfrmMain.DtReminderEventChange(Sender: TObject);
{  will one day be used to validate the date set.    }
begin
  ReminderValid;
end;

procedure TfrmMain.ReminderValid;
{  only allow the reminder set button to be enabled,
   if the reminder date is in the future.
}
var
  EvntDt: TDateTime;
begin
  EvntDt := EncodeDateTime(YearOf(DtReminderEvent.Date), MonthOf(DtReminderEvent.Date),
            DayOf(DtReminderEvent.Date), SpnReminderHour.Value, SpnReminderMins.Value, 0, 0);

  if (EvntDt > Now) then
    btnReminderSet.Enabled := True
  else
    btnReminderSet.Enabled := False;

end;

procedure TfrmMain.btnReminderSetClick(Sender: TObject);
{  Set the reminder.    }
begin
  lblReminder.Caption := format('Reminder set for %.2d:%.2d - %s', [SpnReminderHour.Value, SpnReminderMins.Value, DatetoStr(DtReminderEvent.Date)]);
  stsBrInfo.Panels.Items[4].Text := format('Reminder set for %.2d:%.2d - %s', [SpnReminderHour.Value, SpnReminderMins.Value, DatetoStr(DtReminderEvent.Date)]);

  SpnReminderMins.Visible := False;
  SpnReminderHour.Visible := False;
  DtReminderEvent.Visible := False;

  btnReminderClear.Enabled := True;
  btnReminderSet.Enabled   := False;

  ReminderTimer.Enabled := True;
end;

procedure TfrmMain.btnReminderClearClick(Sender: TObject);
begin
  resetReminder;
end;

procedure TfrmMain.ReminderTimerTimer(Sender: TObject);
{  if reminders are set, this will be ticking and tested to
   see if the reminder is due.
}
var
  reminderDt: TDateTime;
begin
  reminderDt := EncodeDateTime(YearOf(DtReminderEvent.Date), MonthOf(DtReminderEvent.Date),
    DayOf(DtReminderEvent.Date), SpnReminderHour.Value, SpnReminderMins.Value, 0, 0);

  if Now > reminderDt then
    ReminderTimerStop(Sender);

end;

procedure Tfrmmain.ReminderTimerStop(Sender: TObject);
{  Called when the reminder date/time is passed - calls any actions required.    }
begin
  ReminderTimer.Enabled  := False;
  btnReminderSet.Enabled := False;

  if ChckBxReminderSound.Checked then
  begin       //  only play sound if checked
    ChckBxReminderSound.Checked := False;
    doPlaySound(EdtReminderSound.Text, userOptions.volume);
  end;

  if ChckBxReminderReminder.Checked then
  begin    //  only display reminder if checked
    ChckBxReminderReminder.Checked := False;
    popupTitle[3]    := 'Reminder';
    popupMessages[3] := EdtReminderText.Text;
    DisplayMessage;
  end;

  if ChckBxReminderCommand.Checked then
  begin     //  only execute command if checked
    ChckBxReminderCommand.Checked := False;
    doCommandEvent(EdtReminderCommand.Text, '');
  end;

  if ChckBxReminderSystem.Checked then
  begin      //  only do system event if checked
    ChckBxReminderSystem.Checked := False;
    btnReminderAbort.Visible     := True;
    doSystemEvent(CmbBxReminderSystem.ItemIndex);

    if TrayIcon.Visible then
    begin               //  if running in the system tray,
      ppMnItmShowClick(Sender);                  //  select the reminder tab and
      PageControl1.TabIndex := 3;                //  display main application so
    end;                                         //  the abort button can be used.
  end;  //  if ChckBxReminderSystem.Checked then begin

  resetReminder;
end;

procedure TfrmMain.btnReminderAbortClick(Sender: TObject);
{  button only visible during delay prior to a system shutdown/reboot.
   Allows user to abort action.
   Also tidies up application - a bit messy I'm afraid.
}
var
  f: integer;
begin

  abortSystemEvent;

  for f := 1 to 3 do
  begin      //  clear the message array, but leave time.
    popupTitle[f]    := '';
    popupMessages[f] := '';
  end;

  DisplayMessage;               //  will clear popup if currently displayed.

  btnCountdownShutdownAbort.Visible := False;
  CmbBxReminderSystem.Visible       := False;
  ChckBxReminderSystem.Checked      := False;
  btnReminderAbort.Visible          := False;

  resetReminder;
end;

procedure TfrmMain.resetReminder;
{  performs reminder reset.    }
begin
  lblReminder.Caption := 'Reminder not set';
  stsBrInfo.Panels.Items[4].Text := '';

  ReminderTimer.Enabled    := False;
  btnReminderClear.Enabled := False;

  DtReminderEvent.Date  := now;
  SpnReminderMins.Value := MinuteOf(time);
  SpnReminderHour.Value := HourOf(time);

  DtReminderEvent.Enabled := True;
  SpnReminderHour.Enabled := True;
  SpnReminderMins.Enabled := True;

  SpnReminderMins.Visible := True;
  SpnReminderHour.Visible := True;
  DtReminderEvent.Visible := True;

end;

procedure TfrmMain.CmbBxReminderActionChange(Sender: TObject);
begin
  if CmbBxReminderAction.ItemIndex = 0 then
  begin  //  Sound chosen
    ChckBxReminderSound.Visible  := True;
    EdtReminderSound.Visible     := True;
    btnReminderLoadSound.Visible := True;
    btnReminderTestSound.Visible := True;
  end
  else
  begin
    ChckBxReminderSound.Visible  := False;
    EdtReminderSound.Visible     := False;
    btnReminderLoadSound.Visible := False;
    btnReminderTestSound.Visible := False;
  end;

  if CmbBxReminderAction.ItemIndex = 1 then
  begin  //  Reminder chosen
    ChckBxReminderReminder.Visible := True;
    EdtReminderText.Visible        := True;
  end
  else
  begin
    ChckBxReminderReminder.Visible := False;
    EdtReminderText.Visible        := False;
  end;

  if CmbBxReminderAction.ItemIndex = 2 then
  begin  //  System chosen
    ChckBxReminderSystem.Visible      := True;
    CmbBxReminderSystem.Visible       := True;
    btnCountdownShutdownAbort.Visible := True;
  end
  else
  begin
    ChckBxReminderSystem.Visible      := False;
    CmbBxReminderSystem.Visible       := False;
    btnCountdownShutdownAbort.Visible := False;
  end;

  if CmbBxReminderAction.ItemIndex = 3 then
  begin  //  Command chosen
    ChckBxReminderCommand.Visible  := True;
    btnReminderLoadCommand.Visible := True;
    EdtReminderCommand.Visible     := True;
  end
  else
  begin
    ChckBxReminderCommand.Visible  := False;
    btnReminderLoadCommand.Visible := False;
    EdtReminderCommand.Visible     := False;
  end;
end;

procedure TfrmMain.ChckBxReminderSoundChange(Sender: TObject);
begin
  if ChckBxReminderSound.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'Sound Enabled';
    EdtReminderSound.Enabled       := True;
    btnReminderLoadSound.Enabled   := True;
    btnReminderTestSound.Enabled   := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Sound Disabled';
    EdtReminderSound.Enabled       := False;
    btnReminderLoadSound.Enabled   := False;
    btnReminderTestSound.Enabled   := False;
  end;
end;

procedure TfrmMain.ChckBxReminderReminderChange(Sender: TObject);
begin
  if ChckBxReminderReminder.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'Reminder Enabled';
    EdtReminderText.Enabled        := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Reminder Disabled';
    EdtReminderText.Enabled        := False;
  end;
end;

procedure TfrmMain.ChckBxReminderSystemChange(Sender: TObject);
begin
  if ChckBxReminderSystem.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'System Events Enabled';
    CmbBxReminderSystem.Enabled    := True;
    CmbBxReminderSystem.ItemIndex  := 0;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'System Events Disabled';
    CmbBxReminderSystem.Enabled    := False;
  end;
end;



procedure TfrmMain.ChckBxReminderCommandChange(Sender: TObject);
begin
  if ChckBxReminderCommand.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'Command Enabled';
    EdtReminderCommand.Enabled     := True;
    btnReminderLoadCommand.Enabled := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Command Disabled';
    EdtReminderCommand.Enabled     := False;
    btnReminderLoadCommand.Enabled := False;
  end;
end;

procedure TfrmMain.btnReminderLoadCommandClick(Sender: TObject);
{  if the command box is clicked, allow the command file to be loaded.    }
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.*';
    InitialDir := getCurrentDir;
    Title      := 'Choose a executable';
    if Execute then
    begin
      EdtReminderCommand.Text        := FileName;
      stsBrInfo.Panels.Items[4].Text := Filename + ' Chosen';
    end;    //  if Exectute
    Free;
  end;
end;

procedure TfrmMain.btnReminderLoadSoundClick(Sender: TObject);
{  if the text box is clicked, allow the sound file to be changed.    }
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter     := '*.wav; *.mp3';
    InitialDir := getCurrentDir + '\sounds';
    Title      := 'Choose a sound file.';
    if Execute then
    begin
      EdtReminderSound.Text          := ExtractFileName(FileName);
      stsBrInfo.Panels.Items[4].Text := Filename + ' Chosen';
    end;    //  if Execute
    Free;
  end;
end;

procedure TfrmMain.btnReminderTestSoundClick(Sender: TObject);
begin
  doPlaySound(EdtReminderSound.Text, userOptions.volume);
end;
//
// *********************************************************** Friends **********
//
procedure TfrmMain.setFriendsButtons(mode: Boolean);
{  Configure the Fiends buttons.
   Edit and Delete are only visible if friends already exist.
}
begin
  btnFriendsNew.Visible    := mode;
  btnFriendsEdit.Visible   := false;
  btnFriendsDelete.Visible := false;

  if (PageControl1.TabIndex = 5) and (lstBxFriends.Items.Count <> 0) then
  begin
    btnFriendsEdit.Visible   := mode;
    btnFriendsDelete.Visible := mode;
  end;
end;

procedure TfrmMain.btnFriendsClick(Sender: TObject);
{  A generic click routine called for friends actions.

   The sender should either be a TButton ot TListBox.
}
VAR
  itemName   : string;
begin
  itemName := '';

  //  set the appropiate name.
  if (Sender is TButton) then
    itemName := TButton(Sender).Name
  else if (Sender is TListBox) then
    itemName := TListBox(Sender).Name;

  if itemName = '' then exit;                                //  not called by a TButton or TListBox.

  frmFriendsInput := TfrmFriendsInput.Create(Nil);           //  frmFriendsInput is created
  case itemName of
    'lstBxFriends'    : formFriendsInput.Mode := 'VIEW';     //  show form in NEW mode.
    'btnFriendsNew'   : formFriendsInput.Mode := 'NEW';      //  show form in NEW mode.
    'btnFriendsEdit'  : formFriendsInput.Mode := 'EDIT';     //  show form in NEW mode.
    'btnFriendsDelete': formFriendsInput.Mode := 'DELETE';   //  show form in NEW mode.
  end;

  formFriendsInput.pos  := lstBxFriends.ItemIndex;           //  position of the friend in the store.
  frmFriendsInput.ShowModal;                                 //  frmFriendsInput is displayed
  FreeAndNil(frmFriendsInput);                               //  frmFriendsInput is released

  loadFriends;
  setFriendsButtons(true);
end;

procedure TfrmMain.loadFriends;
{ Load the contents of the events file into the listbox.    }
var
  f: integer;
begin
  klog.writeLog(format('Loading %d friends', [fr.friendsCount]));

  lstBxFriends.Clear;

  for f := 0 to fr.friendsCount -1 do
  begin
    lstBxFriends.Items.Add('');    //  insert a blank into the listbox, so can be amended later.
    displayFriends(f);
  end;
end;

procedure TfrmMain.displayFriends(pos: integer);
{  Display a event at position pos.
   If the event count is zero i.e. no events - then just exit.
}
VAR
  f: friend;                    //  Friend.
begin
  if fr.friendsCount = 0 then exit;

  f := Friend.Create(0);
  f := fr.retrieve(pos);

  lstBxFriends.Items[pos] := format('%s %s %s : %s :: %s', [f.fName, f.mName, f.sName, f.email1, f.telNo1]);
  lstBxFriends.ItemIndex  := 0;

  f.Free;
end;
//
// *********************************************************** Events **********
//
procedure TfrmMain.setEventButtons(mode: Boolean);
{  Configure the Event buttons and Event fields.
   Edit, Delete and Print are only visible if events already exist.

   We overload the clear button.
   If events exist, the clear button will display CSV - save a csh version of the events.
   if adding a new event - button will display clear and clear all fields.
}
begin
  btnEventNew.Visible   := mode;
  btnEventAdd.Visible   := false;

  edtEventName.Enabled        := false;
  edtEventName.ReadOnly       := true;
  dtEdtEventDate.Enabled      := false;
  cmbBxEventType.Enabled      := false;
  ChckBxEventFloating.Enabled := false;
  mEventNotes.Enabled         := false;
  mEventNotes.ReadOnly        := false;
  mEventNotes.Text            := '';

  btnEventEdit.Caption := 'Edit';

  if (PageControl1.TabIndex = 6) and (lstBxEvents.Items.Count <> 0)then
  begin
    btnEventEdit.Visible   := true;
    btnEventDelete.Visible := true;
    btnEventPrint.Visible  := true;
    btnEventClear.Caption  := 'CSV';
    btnEventClear.Visible  := true;
  end
  else
  begin                                //  No events, don't need the buttons yet.
    btnEventEdit.Visible   := false;
    btnEventDelete.Visible := false;
    btnEventPrint.Visible  := false;
    btnEventClear.Visible  := false;
  end;

  btnEventPrint.Enabled := false;       //  enable when print function is completed.
end;

procedure TfrmMain.btnEventNewClick(Sender: TObject);
{  Add a new event.    }
begin
  edtEventName.Enabled  := true;
  edtEventName.ReadOnly := false;
  edtEventName.Text     := '';
  edtEventName.SetFocus;

  btnEventClear.Caption       := 'Clear';
  btnEventClear.Visible       := true;
  dtEdtEventDate.Enabled      := true;
  dtEdtEventDate.Date         := today;
  dtEdtEventDate.Time         := EnCodeTime (0, 0, 0, 0);      //  Set to midnight.
  ChckBxEventFloating.Checked := false;
  cmbBxEventType.Enabled      := true;
  cmbBxEventType.ItemIndex    := 0;
  ChckBxEventFloating.Enabled := true;
  mEventNotes.Enabled         := true;
  mEventNotes.ReadOnly        := false;
  mEventNotes.Text            := '';

  btnEventNew.Visible    := false;
  btnEventEdit.Visible   := false;
  btnEventDelete.Visible := false;
  btnEventPrint.Visible  := false;
end;

procedure TfrmMain.btnEventClearClick(Sender: TObject);
{  Clear all fields and return to new mode.

   We overload the clear button.
   If events exist, the clear button will display CSV - save a csh version of the events.
   if adding a new event - button will display clear and clear all fields.
}
begin
  if btnEventClear.Caption = 'Clear' then  //  button is in clear mode.
  begin
    edtEventName.Text        := '';
    dtEdtEventDate.date      := date;
    dtEdtEventDate.Time      := time;
    cmbBxEventType.ItemIndex := 0;
  end
  else                                     //  button is in CSV mode, save events in CSV file
  begin
    SaveDialog1.DefaultExt := '*.csv';
    Savedialog1.Filter     := 'Comma seperated variables *.csv|*.csv';
    Savedialog1.Options    := [ofOverwritePrompt];

    if SaveDialog1.Execute then
    begin
      stsBrInfo.Panels.Items[4].Text := 'Saving CSV File';
      ev.saveEventsCSV(SaveDialog1.FileName);
      stsBrInfo.Panels.Items[4].Text := '';
    end;
  end;

  setEventButtons(true);
end;

procedure TfrmMain.btnEventAddClick(Sender: TObject);
{  Add a event to the store.
   This is achieved by calling event store new function and
   passing in the data.  The listbox is then re-populated.
   The eventCount is one more then the actual count.
}
VAR
  sDate: string;
  sTime: string;
  Var HH,MM,SS,MS: Word;
begin

  sDate := DateToStr(dtEdtEventDate.Date);

  DecodeTime(dtEdtEventDate.Time, HH, MM, SS,MS);   //  We don't need seconds or milli seconds.
  ss    := 0;                                       //  So, we set them to zero.
  ms    := 0;
  sTime := TimeToStr(EnCodeTime (HH, MM, SS, MS));

  if mEventNotes.Text = '' then mEventNotes.Text := ' ';

  ev.new(edtEventName.Text, sDate, sTime, cmbBxEventType.ItemIndex, mEventNotes.Text, ChckBxEventFloating.Checked);

  ev.updateEvents;
  loadEvents;

  seteventButtons(true);
end;

procedure TfrmMain.loadEvents;
{ Load the contents of the events file into the listbox.    }
var
  f: integer;
begin
  klog.writeLog(format('Loading %d events', [ev.eventsCount]));

  lstBxEvents.Clear;
  edtEventName.Clear;
  dtEdtEventDate.Date := Date;
  dtEdtEventDate.Time := Time;
  mEventNotes.Clear;

  for f := 0 to ev.EventsCount -1 do
  begin
    lstBxEvents.Items.Add('');    //  insert a blank into the listbox, so can be amended later.
    displayEvent(f);
  end;

  displayEvent(0);
end;

procedure TfrmMain.btnEventDeleteClick(Sender: TObject);
begin
  if QuestionDlg ('Event Delete',
                  'Do You Really Want To Delete This Event',
                   mtCustom, [mrYes,'yes', mrNo, 'No', 'IsDefault'],'')  = mrYes then
  begin
    ev.Remove(lstBxEvents.ItemIndex);
    loadevents;
    seteventButtons(true);
  end;
end;

procedure TfrmMain.btnEventEditClick(Sender: TObject);
VAR
  sDate: string;
  sTime: string;
begin
  btnEventNew.Visible    := false;
  btnEventDelete.Visible := false;
  btnEventPrint.Visible  := false;

  if btnEventEdit.Caption = 'Edit' then          //  Edit Event.
  begin
    mEventNotes.Enabled         := true;
    mEventNotes.ReadOnly        := false;
    ChckBxEventFloating.Enabled := true;
    dtEdtEventDate.Enabled      := true;
    cmbBxEventType.Enabled      := true;
    btnEventEdit.Caption        := 'Save';
    btnEventClear.Caption       := 'Clear';
    btnEventClear.Visible       := true;
  end
  else                                          //  save Event.
  begin
    btnEventEdit.Caption  := 'Edit';

    sDate := DateToStr(dtEdtEventDate.date);
    sTime := TimeToStr(dtEdtEventDate.Time);

    if mEventNotes.Text = '' then mEventNotes.Text := ' ';
    ev.amend(lstBxEvents.ItemIndex, sDate, sTime, cmbBxEventType.ItemIndex, mEventNotes.Text, ChckBxEventFloating.Checked);
    loadevents;
    seteventButtons(true);
  end;
end;

procedure TfrmMain.displayEvent(pos: integer);
{  Display a event at position pos.
   If the event count is zero i.e. no events - then just exit.
}
VAR
  e: Event;                   //  Event.
  d: TDate;
  t: TDateTime;
begin
  if ev.EventsCount = 0 then exit;

  e := Event.Create(0);
  e := ev.retrieve(pos);

  d := StrToDate(e.date);
  t := StrToTime(e.time);

  edtEventName.Text           := e.name;
  dtEdtEventDate.Date         := d;
  dtEdtEventDate.Time         := t;
  cmbBxEventType.ItemIndex    := e.etype;
  ChckBxEventFloating.Checked := e.float;
  mEventNotes.Text            := e.notes;

  lstBxEvents.Items[pos] := e.dueShort;  //  Amend the listbox entry to include the days due.
  LstBxEvents.ItemIndex  := pos;

  e.Free;
end;

procedure TfrmMain.edtEventNameChange(Sender: TObject);
{  makes the add button visible when both name and date contain text.

   Was also being fired when loading a previous event.
   Since the Clear button is made visible when the new button
   is clicked, this used as a flag to indicate a new event is being added
   and not a previous event being displayed.

   NB : A date of 0 is 1/1/1989 - so an event can not be set for that date.
}
begin
 if (edtEventName.Text <> '') and (dtEdtEventDate.Date <> 0) then
   btnEventAdd.Visible := btnEventClear.Visible;
end;

procedure TfrmMain.dtEdtEventDateChange(Sender: TObject);
{  We now have an event name and an event date, so enable the event add button.
}
begin
 if (edtEventName.Text <> '') and (dtEdtEventDate.Date <> 0) then
   btnEventAdd.Visible := btnEventClear.Visible;
end;

procedure TfrmMain.lstBxEventsClick(Sender: TObject);
{  Display the item clicked.  }
begin
  displayEvent(lstBxEvents.ItemIndex);
end;

procedure TfrmMain.setUpEventsOptions;
{  Load Event Types and set up the aged days for event prompts.  }
begin
  cmbBxEventType.Items     := ev.eventsTypes;
  cmbBxEventType.ItemIndex := 0;

  ev.stage1Days       := userOptions.eventsStage1Days;
  ev.stage2Days       := userOptions.eventsStage2Days;
  ev.stage3Days       := userOptions.eventsStage3Days;
  ev.stage1Mess       := userOptions.eventsStage1Mess;
  ev.stage2Mess       := userOptions.eventsStage2Mess;
  ev.stage3Mess       := userOptions.eventsStage3Mess;
  ev.stage1BackColour := userOptions.eventsStage1BackColour;
  ev.stage2BackColour := userOptions.eventsStage2BackColour;
  ev.stage3BackColour := userOptions.eventsStage3BackColour;
  ev.stage1ForeColour := userOptions.eventsStage1ForeColour;
  ev.stage1ForeColour := userOptions.eventsStage1ForeColour;
  ev.stage1ForeColour := userOptions.eventsStage1ForeColour;
end;
//
// *********************************************************** Conversion ******
//
procedure TfrmMain.CmbBxCategoryChange(Sender: TObject);
{  A new category has been chosen, re-load the categories.    }
begin
  parseConversionUnitsFile('LoadUnits');
  cleartextFiles;
end;

procedure TfrmMain.CmbBxConvertToChange(Sender: TObject);
{  A new convert to has been chosen, clear the edit boxes.
   The new choice is still within the same category - so no need to reload the units file.
}
begin
  cleartextFiles;
end;

procedure TfrmMain.edtConverionValueChange(Sender: TObject);
{  Entry into the value field, if numeric then enable the convert button.    }
begin
  if tryStrToFloat(edtConverionValue.Text, unitConvertVal) then
    btnConverionConvert.Enabled := true
  else
    btnConverionConvert.Enabled := false;
end;

procedure TfrmMain.btnConverionConvertClick(Sender: TObject);
{  carry out the conversion.    }
begin
  parseConversionUnitsFile('SelectUnit');

  edtConverionResult.text := floatToStr(unitConvertVal * unitConvertfactor);
end;

procedure TfrmMain.btnConversionAddUnitsClick(Sender: TObject);
{  This allows new conversion units to be added,
   loads file into notepad.
}
begin
  EditConversionUnitsFile;

  //  reload everything - in case anything has been added.
  readConversionUnitsFile;
  parseConversionUnitsFile('LoadCategory');
  parseConversionUnitsFile('LoadUnits');
  cleartextFiles;
end;

//
// *********************************************************** Memo ************
//
procedure TfrmMain.btnMemoNewClick(Sender: TObject);
{  Add a new memo.    }
begin
  edtMemoKey.Enabled  := true;
  edtMemoKey.ReadOnly := false;
  edtMemoKey.Text     := '';
  edtMemoKey.SetFocus;

  btnMemoClear.Visible      := true;
  RdBttnMemoEncrypt.Enabled := true;
  MmMemoData.Enabled        := true;
  MmMemoData.ReadOnly       := false;
  MmMemoData.Text           := '';

  btnMemoEdit.Visible   := false;
  btnMemoDelete.Visible := false;
  btnMemoPrint.Visible  := false;
end;

procedure TfrmMain.btnMemoPrintClick(Sender: TObject);
begin

end;

procedure TfrmMain.btnMemoClearClick(Sender: TObject);
{  Clear all fields and return to new mode.    }
begin
  setMemoButtons(true);
end;

procedure TfrmMain.btnMemoDecryptClick(Sender: TObject);
begin
  displayEncryptedMemo;
  tmrMemo.Enabled := true;                     //  only display for 30 seconds.
end;

procedure TfrmMain.btnMemoDeleteClick(Sender: TObject);
begin
  if QuestionDlg ('Memo Delete',
                  'Do You Really Want To Delete This memo',
                   mtCustom, [mrYes,'yes', mrNo, 'No', 'IsDefault'],'')  = mrYes then
  begin
    klog.writeLog(format('Deleting memo at pos %d', [LstBxMemoName.ItemIndex]));
    memorandum.Remove(LstBxMemoName.ItemIndex);
    loadmemos;
    setMemoButtons(true);
  end;
end;

procedure TfrmMain.btnMemoEditClick(Sender: TObject);
{  Edit a selected memo and re save file.    }
var
  passWord: string;
begin
  btnMemoNew.Visible    := false;
  btnMemoDelete.Visible := false;
  btnMemoPrint.Visible  := false;

  if btnMemoEdit.Caption = 'Edit' then          //  Edit memo.
  begin
    MmMemoData.ReadOnly       := false;
    btnMemoEdit.Caption       := 'Save';
    btnMemoClear.Visible      := true;
    RdBttnMemoEncrypt.Enabled := true;
    if RdBttnMemoEncrypt.checked then
      displayEncryptedMemo;   //  no timer on edit.
  end
  else                                          //  save memo
  begin
    MmMemoData.ReadOnly  := true;
    btnMemoEdit.Caption  := 'Edit';
    btnMemoClear.Visible := false;

    if RdBttnMemoEncrypt.checked then
    begin
      passWord := PasswordBox('Memo Password',
                              'Input a password to encrypt memo, or return to use default.');

      if passWord = '' then   //  if password is blank then use default.
        passWord := PASSWORD;

      MmMemoData.Text := encrypt(MmMemoData.Text, passWord);
    end;
    memorandum.amend(LstBxMemoName.ItemIndex, MmMemoData.Text, RdBttnMemoEncrypt.Checked);
    setMemoButtons(true);
  end;
end;

procedure TfrmMain.displayEncryptedMemo;
{  Display an encrypted memo - used by edit and decrypt.    }
var
  m: Memo;                   //  Memo.
  passWord: string = '';
begin
  m := Memo.Create(0);
  m := memorandum.retrieve(LstBxMemoName.ItemIndex);

  btnMemoNew.Visible    := false;
  btnMemoDelete.Visible := false;
  btnMemoPrint.Visible  := false;
  btnMemoEdit.Visible   := false;

  if InputQuery('Memo Password',
                'Input a password to decrypt memo, or return to use default.',
                TRUE, passWord) then
  begin
    if passWord = '' then   //  if password is blank then use default.
      passWord := PASSWORD;

    MmMemoData.Text := decrypt(m.body, passWord);
  end;  //  if InputQuery('Memo Password',

  m.Free;
end;

procedure TfrmMain.btnMemoAddClick(Sender: TObject);
{  Add a memo to the store.
   This is achieved by calling memo store [memorandum] new function and
   passing in the data.  The listbox is then re-populated.
   The memoCount is one more then the actual count.
}
var
  passWord: string = '';
begin
  klog.writeLog('Adding memo');
  if RdBttnMemoEncrypt.checked then
  begin
    if InputQuery('Memo Password',
                  'Input a password to decrypt memo, or return to use default.',
                  TRUE, passWord) then

      if passWord = '' then   //  if password is blank then use default.
        passWord := PASSWORD;

      MmMemoData.Text := decrypt(MmMemoData.Text, passWord);
  end;  //  if RdBttnMemoEncrypt.checked then

  memorandum.new(edtMemoKey.Text, MmMemoData.Text, RdBttnMemoEncrypt.Checked);
  loadMemos;
  setMemoButtons(true);
end;

procedure TfrmMain.edtMemoKeyChange(Sender: TObject);
{  makes the add button visible when both name and date contain text.

   Was also being fired when loading a previous memo.
   Since the Clear button is made visible when the new button
   is clicked, this used as a flag to indicate a new memo is being added
   and not a previous memo being displayed.
}
begin
 if (edtMemoKey.Text <> '') and (MmMemoData.Text <> '') then
   btnMemoAdd.Visible := btnMemoClear.Visible;
end;

procedure TfrmMain.MmMemoDataChange(Sender: TObject);
begin
 if (edtMemoKey.Text <> '') and (MmMemoData.Text <> '') then
   btnMemoAdd.Visible := btnMemoClear.Visible;
end;

procedure TfrmMain.LstBxMemoNameClick(Sender: TObject);
{  Display the memo at position of the mouse click.    }
begin
  displayMemo(LstBxMemoName.ItemIndex);
end;

procedure TfrmMain.loadMemos;
{ Load the contents of the memo file into the listbox.    }
var
  f: integer;
begin
  klog.writeLog(format('Loading %d memos', [memorandum.MemosCount]));
  LstBxMemoName.Clear;
  for f := 0 to memorandum.MemosCount -1 do
  begin
    displayMemo(f);
    LstBxMemoName.Items.Add(edtMemoKey.Text);
  end;
end;

procedure TfrmMain.displayMemo(pos: integer);
{  Display a memo at position pos.
   If the memo count is zero i.e. no memos - then just exit.
   If the memo is encrypted, the display the Decrypt button.
   If the memo is encrypted, then display 'Secret' instead of the encrypted text.
}
VAR
  m: Memo;                   //  Memo.
begin
  if memorandum.MemosCount = 0 then exit;
  m := Memo.Create(0);
  m := memorandum.retrieve(pos);

  edtMemoKey.Text           := m.name;
  RdBttnMemoEncrypt.Checked := m.encrypt;

  if m.encrypt then
  begin
    MmMemoData.Text        := 'Shhh it''s a secret';
    btnMemoDecrypt.visible := true
  end
  else
  begin
    MmMemoData.Text        := m.body;
    btnMemoDecrypt.visible := false;
  end;

  m.Free;
end;

procedure TfrmMain.setMemoButtons(mode: Boolean);
{  Configure the memo buttons and memo fields.
   Edit, Delete and Print are only visible if events already exist.
}
begin
  btnMemoNew.Visible     := mode;
  btnMemoAdd.Visible     := false;
  btnMemoClear.Visible   := false;
  btnMemoDecrypt.visible := false;       //  Always hidden, unless needed.

  btnMemoEdit.Caption := 'Edit';

  if (PageControl1.TabIndex = 7) and (memorandum.MemosCount <> 0) then
  begin
    btnMemoEdit.Visible       := true;
    btnMemoDelete.Visible     := true;
    btnMemoPrint.Visible      := true;
  end
  else
  begin                                //  No memos, don't need the buttons yet.
    btnMemoEdit.Visible       := false;
    btnMemoDelete.Visible     := false;
    btnMemoPrint.Visible      := false;
    RdBttnMemoEncrypt.Checked := false;
    edtMemoKey.Caption        := '';
  end;

  RdBttnMemoEncrypt.Enabled := false;
  MmMemoData.ReadOnly       := true;
  edtMemoKey.ReadOnly       := true;
  edtMemoKey.Enabled        := false;

  btnMemoPrint.Enabled := false;       //  enable when print fuction is completed.
end;

procedure TfrmMain.tmrMemoTimer(Sender: TObject);
begin
  tmrMemo.Enabled := false;
  MmMemoData.Text := 'Shhh it''s a secret';
  setMemoButtons(true);
end;

//
// *********************************************************** Menu procs ******
//
procedure TfrmMain.mnuItmOptionsClick(Sender: TObject);
{  if clicked, call the option screen, reapply options after.    }
var
  frmTop  : integer;
  frmLeft : integer;
  res     : integer;         //  return value from option screen.
  useFonts: boolean;
begin
  if userOptions.screenSave then
  begin
    userOptions.formTop  := frmMain.Top;
    userOptions.formLeft := frmMain.Left;
  end
  else
  begin
    frmTop  := frmMain.Top;    //  return to same place, after option screen.
    frmLeft := frmMain.Left;
  end;

  useFonts := userOptions.useCustomFonts;      //  Store, so we know if has been changed.

  frmOptions := TfrmOptions.Create(Nil);       //  frmOptions is created
  res        := frmOptions.ShowModal;          //  frmOptions is displayed
  FreeAndNil(frmOptions);                      //  frmOptions is released

  if res = 1 then                              //  1 = OK button press, 2 = cancel button pressed.
  begin
    if useFonts <> userOptions.useCustomFonts then           //  the use fonts options has changed,
    begin                                                    //  we my have to create or free the font object.
      if (not useFonts) and userOptions.useCustomFonts then  //  Use fonts has been turned on, so we create.
      begin                                                  //  Create the font store objects, if needed.
        stsBrInfo.Panels.Items[4].Text := 'Creating Fonts';
        fs := fontStore.Create;
        stsBrInfo.Panels.Items[4].Text := '';
      end;

      if useFonts and (not userOptions.useCustomFonts) then  //  use fonts has been turned off, so we free.
      begin
        stsBrInfo.Panels.Items[4].Text := 'Removing Fonts';
        fs.removeFonts;                                      //  Remove all fonts from system.
        fs.Free;                                             //  Release the font store object.
        stsBrInfo.Panels.Items[4].Text := '';
      end;  //  if useFonts and (not userOptions.useCustomFonts) then
    end;    //  if useFonts <> userOptions.useCustomFonts then

    SetDefaults;

  end;  //  if res = 1 then

  if not userOptions.screenSave then
  begin  //  not done in SetDefaults
    frmMain.Top  := frmTop;
    frmMain.Left := frmLeft;
  end;
end;
//
// ********************************************************* Menu Items *********
//
procedure TfrmMain.mnuItmClick(Sender: TObject);
{  A generic click routine called by each menu item.
   Also called by the TbitBtn and TSpeedButton on front panel.

   The action of the menu is determined from the item name.
}
VAR
  itemName   : string;
begin
  itemName := '';

  //  set the appropiate name.
  if (Sender is TMenuItem) then
    itemName := TMenuItem(Sender).Name
  else if (Sender is TBitBtn) then
    itemName := TBitBtn(Sender).Name
  else if (Sender is TSpeedButton) then
    itemName := TSpeedButton(Sender).Name;

  if itemName = '' then exit;    //  not called by a TMenuItem, TSpeedButton or TBitBtn.

  case itemName of
    // ********************************************************* Front Pannel ******
    'BitBtnHide':  //  if clicked will hide the main form and display the tray icon.
    begin
      TrayIcon.Visible := True;
      TrayIcon.Show;

      frmMain.Visible := False;
    end;
    // ********************************************************* File Menu *********
    'mnuItmExit',
    'BitBtnClose' : close;
    // ********************************************************* Help Menu *********
    'mnuItmHelp',
    'BitBtnHelp' : displayHelp('help\Klock.chm', '/Introduction.htm');  //  Calls the Help file.
    'mnuItmAbout':                                                      //  Calls the About screen.
    begin
      frmAbout := TfrmAbout.Create(Nil);  //frmAbout is created
      frmAbout.ShowModal;                 //frmAbout is displayed
      FreeAndNil(frmAbout);               //frmAbout is released
    end;
    'mnuItmLicense':                                                      //  Calls the License screen.
    begin
      frmLicense := TfrmLicense.Create(Nil);
      frmLicense.ShowModal;
      FreeAndNil(frmLicense);
    end;
    // ********************************************************* Time Menu *********  frmFloatingKlock: TfrmFloatingKlock;
    'mnuItmAnalogueKlock'     :                                  //  Calls the Analogue Klock.
    begin
      frmAnalogueKlock := TfrmAnalogueKlock.Create(Nil);
      frmAnalogueKlock.ShowModal;
      FreeAndNil(frmAnalogueKlock);
    end;
    'mnuItmLEDKlock'          :                                  //  Calls the LED Klock.
    begin
      frmLEDKlock := TfrmLEDKlock.Create(Nil);
      frmLEDKlock.ShowModal;
      FreeAndNil(frmLEDKlock);
    end;
    'mnuItmBinaryKlock'       :                                  //  Calls the Binary Klock.
    begin
      frmBinaryKlock := TfrmBinaryKlock.Create(Nil);
      frmBinaryKlock.ShowModal;
      FreeAndNil(frmBinaryKlock);
    end;
    'mnuItmSmallTextKlock'    :                                  //  Calls the Small Text Klock.
     begin
      frmSmallTextKlock := TfrmSmallTextKlock.Create(Nil);
      frmSmallTextKlock.ShowModal;
      FreeAndNil(frmSmallTextKlock);
    end;
    'mnuItmFloatingTextKlock' :                                  //  Calls the Floating Text Klock.
     begin
      frmFloatingKlock := TfrmFloatingKlock.Create(Nil);
      frmFloatingKlock.ShowModal;
      FreeAndNil(frmFloatingKlock);
    end;
    'mnuItmScrollingTextKlock':                                  //  calls the Scrolling Text Klock.
    begin
      frmScrollingKlock := TfrmScrollingKlock.Create(Nil);
      frmScrollingKlock.ShowModal;
      FreeAndNil(frmScrollingKlock);
    end;
    'mnuItmTimePositions'     :                                  //  Calls the time Positions form.
    begin
      frmTimePositions := TfrmTimePositions.Create(Nil);
      frmTimePositions.ShowModal;
      FreeAndNil(frmTimePositions);
    end;
    // ********************************************************* Info Menu *********
    'mnuItmDaylightSaving'   : callInfo('Daylight Saving');     //  Calls the Daylight Saving Info screen.
    'mnuItmEasterDates'      : callInfo('Easter Dates');        //  Calls the Easter Dates Info screen.
    'mnuItmLentDates'        : callInfo('Lent Dates');          //  Calls the Lent Dates Info screen.
    'mnuItmChineseYear'      : callInfo('Chinese Year');        //  Calls the Chinese Year Info screen.
    'mnuItmMoonStuff'        : callInfo('Moon Stuff');          //  Calls the Moon Stuff Info screen.
    'mnuItmSunStuff'         : callInfo('Sun Stuff');           //  Calls the Sun Stuff Info screen.
    'mnuItmPowerStuff'       : callInfo('Power Source');        //  Calls the Power Source Saving Info screen.
    'mnuItmMonitorStuff'     : callInfo('Monitor Stuff');       //  Calls the Monitor Stuff Info screen.
    // ************************************************** Clipboard manager ********
    'SpdBtnClipboard'        : frmClipBoard.Visible := true;
    // ************************************************** Sticky Note Menu *********
    'SpdBtnNewStickyNote',                                      //  Creates a new sticky note, will appear on the screen.
    'mnuItmNewStickyNote'    : stickies.new(userOptions.stickyColor, userOptions.stickyFont);
    // ************************************************** Biorhythm Menu ***********
    //  Display a simple Biorhythm chart, using the Birth date set up in user options.
    //  NB  This form is not shown model.
    'MnuItmSimpleBiorhythm'  :
    begin
      frmBiorhythm := TfrmBiorhythm.Create(Nil);
      frmBiorhythm.ShowModal;
      FreeAndNil(frmBiorhythm);
    end;
    'MnuItmEnhancedBiorhythm': EnhancedBiorhythmClick;
  end;
end;

procedure TfrmMain.callInfo(mode: string);
{  Calls the actual info form, passing to mode.    }
begin
  frmInfo      := TfrmInfo.Create(Nil);
  frmInfo.Info := mode;
  frmInfo.ShowModal;
  FreeAndNil(frmInfo);
end;

procedure TfrmMain.EnhancedBiorhythmClick;
{  Displays an enhanced version of the Biorhythm chart, this allows the rhyms
   of two user to be displayed on one chart.  Also, a secdondary series
   of plots can be displayed.

   This is an external application which is called - can be either 32 or 64 bit
   depending upon calling application.
}
VAR
  dirName : string;
  fileName: String;
begin
  dirName := ExtractFilePath(Application.ExeName);
  {$ifdef WIN32}
    fileName := dirName +'\LazBiorhythms\LazBiorhythms_32.exe';
  {$else}
    fileName := dirName +'\LazBiorhythms\LazBiorhythms_64.exe';
  {$endif}

  doCommandEvent(fileName, '');
end;
//
// ******************************************************* pop menu ************
//
procedure TfrmMain.ppMnItmShowClick(Sender: TObject);
{  on menu show, hides the tray icon and redisplays the main form.
   Unchecks time menu item.
}
begin
  TrayIcon.Visible := False;
  TrayIcon.Hide;

  frmMain.Visible := True;

  KillOtherKlocks;    //  if made visible from the tray, kill any other klocks that are visible.

  if ppMnItmTime.Checked then
    ppMnItmTime.Checked := False;
end;

procedure TfrmMain.ppMnItmTimeClick(Sender: TObject);
{  on menu click, toggle the checked status.
   If checked becomes false, clear time message and tries to kill pop-up notifier.
}
begin
  if ppMnItmTime.Checked then
  begin
    ppMnItmTime.Checked := False;
    popupTitle[1]       := '';
    popupMessages[0]    := '';
    DisplayMessage;
  end
  else
    ppMnItmTime.Checked := True;
end;

procedure TfrmMain.ppMnItmExitClick(Sender: TObject);
begin
  close;
end;
//
// ******************************************************* tray icon ************
//
procedure TfrmMain.TrayIconDblClick(Sender: TObject);
{  double clicking the tray icon, will clear all messages and kill the pop-up notifier.
}
var
  f: integer;
begin
  for f := 0 to 3 do
  begin
    popupTitle[f]    := '';
    popupMessages[f] := '';
  end;

  ppMnItmTime.Checked := False;
  DisplayMessage;
end;
//
// ******************************************************* pop up notifier ******
//
procedure TfrmMain.PopupNotifier1Close(Sender: TObject; var CloseAction: TCloseAction);
{  if the pop-up is closed manually, assume closed by user after countdown
   of time - so clear these messages.
   NB  but leave time, just in case.
}
var
  f: integer;
begin
  for f := 1 to 3 do
  begin
    popupTitle[f]    := '';
    popupMessages[f] := '';
  end;

  CloseAction := caFree;
end;
//
// ******************************************************** KLOCK END ***********
//
end.
