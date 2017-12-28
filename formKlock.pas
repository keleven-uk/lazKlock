unit formklock;

{
Klock :: A Clock with a K.
Copyright (C) 2012 - 2017 :: Kevin Scott

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

  To compile, the following components must be instaled into Lazarus.
     BGRA comtorls, which installs BGRA bitmap.
     EC-contols - Eye Candy - used for the accordion on the options screen.
     VisualPlanit - L.E.D. control.

     All from the Online Package manager.
}

{ TODO : Check if options file has new options. }
{ TODO : Check out Ballon Time. }
{ TODO : Check out Reminders. }
{ TODO : Look at up time in formAbout. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, uFonts,
  ComCtrls, Menus, Buttons, StdCtrls, Spin, PopupNotifier, EditBtn, ButtonPanel,
  formAbout, formHelp, formOptions, formLicense, UFuzzyTime, dateutils, LCLIntf, LCLType,
  CheckLst, UKlockUtils, formReminderInput, AvgLvlTree, uOptions, Windows, formAnalogueKlock,
  ULogging, formInfo, Graph, formClipBoard, formLEDKlock, formBinaryKlock, formSmallTextKlock;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnCountdownStart: TButton;
    btnCountdownStop: TButton;
    btnCountdownLoadSound: TButton;
    btnEventAbort: TButton;
    btnTimerStart: TButton;
    btnTimerStop: TButton;
    btnTimerClear: TButton;
    btnEventSet: TButton;
    btnSoundTest: TButton;
    btnEventClear: TButton;
    btnTimerSplit: TButton;
    btnCountdownShutdownAbort: TButton;
    btnCountdownLoadCommand: TButton;
    btnEventrLoadSound: TButton;
    btnEventTestSound: TButton;
    btnEventLoadCommand: TButton;
    btnReminderNew: TButton;
    btnReminderEdit: TButton;
    btnReminderDelete: TButton;
    ButtonPanel1: TButtonPanel;
    ChckBxCountdownSound: TCheckBox;
    chckBxCountdownEvent: TCheckBox;
    chckBxCountdownReminder: TCheckBox;
    chckBxCountdownCommand: TCheckBox;
    ChckBxEventSound: TCheckBox;
    ChckBxEventReminder: TCheckBox;
    ChckBxEventSystem: TCheckBox;
    ChckBxEventCommand: TCheckBox;
    ChckLstBxReminder: TCheckListBox;
    CmbBxTime: TComboBox;
    CmbBxCountdownAction: TComboBox;
    CmbBxCountdownEvent: TComboBox;
    CmbBxEventAction: TComboBox;
    CmbBxEventSystem: TComboBox;
    DtEdtEvent: TDateEdit;
    EdtEventCommand: TEdit;
    EdtEventText: TEdit;
    EdtEventSound: TEdit;
    EdtCountdownCommand: TEdit;
    EdtCountdownReminder: TEdit;
    EdtCountdownSound: TEdit;
    mainIdleTimer: TIdleTimer;
    lblRadix: TLabel;
    lblSplitLap: TLabel;
    lblfuzzy: TLabel;
    lblEvent: TLabel;
    lblTimer: TLabel;
    LblCountdownTime: TLabel;
    mnuItmSmallTextKlock: TMenuItem;
    mnuItmBinaryKlock: TMenuItem;
    mnuItmLEDKlock: TMenuItem;
    mnuItmPowerSource: TMenuItem;
    mnuItmLentDates: TMenuItem;
    mnuItmEasterDates: TMenuItem;
    mnuItmDaylightSaving: TMenuItem;
    mnuInfo: TMenuItem;
    mnuTime: TMenuItem;
    mnuItmAnalogueKlock: TMenuItem;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    ppMnItmTime: TMenuItem;
    ppMnItmExit: TMenuItem;
    ppMnItmShow: TMenuItem;
    mnuItmLicense: TMenuItem;
    mnuItmOptions: TMenuItem;
    mnuItmHelp: TMenuItem;
    mnuItmAbout: TMenuItem;
    mnuItmExit: TMenuItem;
    mnuHelp: TMenuItem;
    mnuFile: TMenuItem;
    mnuMain: TMainMenu;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PpMnTray: TPopupMenu;
    PopupNotifier1: TPopupNotifier;
    SpnEdtTimeBase: TSpinEdit;
    SpnEdtHour: TSpinEdit;
    SpnEdtMins: TSpinEdit;
    SpnEdtCountdown: TSpinEdit;
    stsBrInfo: TStatusBar;
    TbShtReminder: TTabSheet;
    TbShtFuzzy: TTabSheet;
    TbShtCountdown: TTabSheet;
    TbShtTimer: TTabSheet;
    TbShtEvent: TTabSheet;
    mainTimer: TTimer;
    CountdownTimer: TTimer;
    EventTimer: TTimer;
    timerTimer: TTimer;
    ballonTimer: TTimer;
    TrayIcon: TTrayIcon;
    procedure ballonTimerTimer(Sender: TObject);
    procedure btnReminderNewClick(Sender: TObject);
    procedure btnCountdownLoadCommandClick(Sender: TObject);
    procedure btnCountdownLoadSoundClick(Sender: TObject);
    procedure btnCountdownShutdownAbortClick(Sender: TObject);
    procedure btnCountdownStartClick(Sender: TObject);
    procedure btnCountdownStopClick(Sender: TObject);
    procedure btnEventClearClick(Sender: TObject);
    procedure btnEventLoadCommandClick(Sender: TObject);
    procedure btnEventrLoadSoundClick(Sender: TObject);
    procedure btnEventSetClick(Sender: TObject);
    procedure btnEventAbortClick(Sender: TObject);
    procedure btnEventTestSoundClick(Sender: TObject);
    procedure btnSoundTestClick(Sender: TObject);
    procedure btnTimerClearClick(Sender: TObject);
    procedure btnTimerStartClick(Sender: TObject);
    procedure btnTimerStopClick(Sender: TObject);
    procedure btnTimerSplitClick(Sender: TObject);
    procedure chckBxCountdownCommandChange(Sender: TObject);
    procedure chckBxCountdownEventChange(Sender: TObject);
    procedure chckBxCountdownReminderChange(Sender: TObject);
    procedure ChckBxCountdownSoundChange(Sender: TObject);
    procedure ChckBxEventCommandChange(Sender: TObject);
    procedure ChckBxEventReminderChange(Sender: TObject);
    procedure ChckBxEventSoundChange(Sender: TObject);
    procedure ChckBxEventSystemChange(Sender: TObject);
    procedure CmbBxEventActionChange(Sender: TObject);
    procedure CmbBxTimeChange(Sender: TObject);
    procedure CmbBxCountdownActionChange(Sender: TObject);
    procedure CountdownTimerTimer(Sender: TObject);
    procedure DtEdtEventChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure mainIdleTimerStopTimer(Sender: TObject);
    procedure mainIdleTimerTimer(Sender: TObject);
    procedure mnuItmAnalogueKlockClick(Sender: TObject);
    procedure mnuItmAboutClick(Sender: TObject);
    procedure mnuItmBinaryKlockClick(Sender: TObject);
    procedure mnuItmDaylightSavingClick(Sender: TObject);
    procedure mnuItmEasterDatesClick(Sender: TObject);
    procedure mnuItmExitClick(Sender: TObject);
    procedure mnuItmHelpClick(Sender: TObject);
    procedure mnuItmLEDKlockClick(Sender: TObject);
    procedure mnuItmLentDatesClick(Sender: TObject);
    procedure mnuItmLicenseClick(Sender: TObject);
    procedure mnuItmOptionsClick(Sender: TObject);
    procedure mnuItmPowerSourceClick(Sender: TObject);
    procedure mnuItmSmallTextKlockClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure mainTimerTimer(Sender: TObject);
    procedure PopupNotifier1Close(Sender: TObject; var CloseAction: TCloseAction);
    procedure ppMnItmShowClick(Sender: TObject);
    procedure ppMnItmTimeClick(Sender: TObject);
    procedure EventTimerTimer(Sender: TObject);
    procedure SpnEdtCountdownChange(Sender: TObject);
    procedure SpnEdtHourChange(Sender: TObject);
    procedure SpnEdtMinsChange(Sender: TObject);
    procedure SpnEdtTimeBaseChange(Sender: TObject);
    procedure timerTimerTimer(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    procedure DisplayMessage;
    procedure StopCountDown(Sender: TObject);
    procedure SetDefaults;
    procedure resetEvent;
    procedure EventTimerStop(Sender: TObject);
    procedure EventValid;
    procedure readReminderFile;
    procedure UpdateStatusBar(KTime: TDateTime);
    procedure UpdateTime(KTime: TDateTime);
  public

  end;

var
  frmMain: TfrmMain;
  rmndrStore: TAvgLvlTree;      //  to store all the reminders.
  userOptions: Options;         //  holds all the user options.
  ft: FuzzyTime;                //  the object to give the different times.
  fs: fontStore;                //  used to handle custom fonts i.e. load & remove
  kLog: Logger;                 //  used to log erors, debug statements etc.
  appStartTime: int64;          //  used by formAbout to determine how long the app has been running.
  countdownTicks: integer;
  timerStart: TDateTime;
  timerPaused: TdateTime;
  popupMessages: array [0..3] of string;
  popupTitle: array [0..3] of string;
  noReminder: integer;
  tick: integer = 0;

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
  mainTimer.Enabled := False;  //  disable main timer until all options and fuzzy time are set up.

  EdtCountdownSound.Text := 'alarm-fatal.mp3';
  EdtEventSound.Text := 'alarm-fatal.mp3';

  DtEdtEvent.Date := now;
  SpnEdtMins.Value := MinuteOf(time);
  SpnEdtHour.Value := HourOf(time);
  btnEventSet.Enabled := False;

  noReminder := 0;
  appStartTime := GetTickCount64;  //  tick count when application starts.

  rmndrStore := TAvgLvlTree.Create;
  userOptions := Options.Create;   //  create options file as c:\Users\<user>\AppData\Local\Stub\Options.xml
  ft := FuzzyTime.Create;
  fs := fontStore.Create;
  kLog := Logger.Create(Application.MainFormHandle);

  logHeader;
  kLog.cullLogFile;

  frmClipBoard.cullTmpFiles;                     //  Remove old .tmp files left over from clipboard operations.

  fs.addFonts;                     //  Add custom fonts.

  with mainIdleTimer do            //  set up the idle timer.
  begin
    AutoEnabled := True;
    AutoStartEvent := itaOnIdle;
    AutoEndEvent := itaOnUserInput;
    Interval := 1000;
    Enabled := False;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  kLog.writeLog('FormKlock Showing');
  SetDefaults;
  mainTimer.Enabled := True;        //  Now safe to enable main timer.
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{  called on form close, save screen position if needed.
}
begin
  kLog.writeLog('FormKlock Closing');

  if userOptions.screenSave then
  begin
    userOptions.formTop := frmMain.Top;
    userOptions.formLeft := frmMain.Left;
    userOptions.writeCurrentOptions;
  end;

  //  if clipboard manager active, we need to save its position - if needed.
  if userOptions.CB_ScreenSave then
  begin
    userOptions.CB_formTop := frmClipBoard.Top;
    userOptions.CB_formLeft := frmClipBoard.Left;
  end;

  fs.removeFonts;                   //  Remove custom fonts.

  logFooter;

  FreeAndNil(fs);                   //  Release the font store object.
  FreeAndNil(ft);                   //  Release the fuzzy time object.
  FreeAndNil(userOptions);          //  Release the user options
  FreeAndNil(kLog);                 //  Release the logger object.

  CloseAction := caFree;
end;

procedure TfrmMain.SetDefaults;
{  called to set defaults on start-up.
   Set things that can be changed in the options screen, to the values in the options screen.
}
begin
  kLog.writeLog('FormKlock SetDefaults');

  PageControl1.TabIndex := userOptions.defaultTab;
  CmbBxTime.Items := ft.fuzzyTypes;
  CmbBxTime.ItemIndex := userOptions.defaultTime;
  mainIdleTimer.Enabled := userOptions.displayIdleTime;

  ft.displayFuzzy := userOptions.defaultTime;
  ft.display24Hour:= userOptions.display24Hour;
  ft.fuzzyBase := 2;

  if userOptions.screenSave then
  begin
    frmMain.Top := userOptions.formTop;
    frmMain.Left := userOptions.formLeft;
  end;

  if userOptions.netTimeSeconds and (CmbBxTime.Items[CmbBxTime.ItemIndex] = 'NET Time') then
    mainTimer.Interval := 1
  else
    mainTimer.Interval := 1000;

  klog.writeLog(format('Main timer inerval set to %D milliseconds', [mainTimer.Interval]));
end;

procedure TfrmMain.DisplayMessage;
{  display a message as a pop-up notifier.
   If this procedure is called with an empty message array,
   the pop-up notifier is cancelled.
}
var
  f: integer;
  title: string;       //  do we need to set title?
  message: string;
begin

  title := '';
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
    Text := message;

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
        0 = time
        1 = countdown
        2 = timer
        3 = event
        4 = reminder
}
begin

  case PageControl1.TabIndex of
    0:
    begin                     //  fuzzy page
      stsBrInfo.Panels.Items[4].Text := '';
    end;
    1:
    begin                     //  countdown page
      if CountdownTimer.Enabled = False then
        stsBrInfo.Panels.Items[4].Text := ''
      else
        stsBrInfo.Panels.Items[4].Text := format(' Counting down from %2.d minute[s]', [SpnEdtCountdown.Value]);
    end;
    2:
    begin                     //  timer page
      stsBrInfo.Panels.Items[4].Text := '';

      if timerTimer.Enabled = False then
      begin

        if btnTimerStart.Caption = 'Resume' then
          stsBrInfo.Panels.Items[4].Text := 'Timer :: Paused';

        if btnTimerStart.Caption = 'Start' then
        begin
          if userOptions.timerMilliSeconds then
          begin
            lblTimer.Caption := '00:00:00:00';
            lblSplitLap.Caption := '00:00:00:00';
          end
          else
          begin
            lblTimer.Caption := '00:00:00';
            lblSplitLap.Caption := '00:00:00';
          end;  //  if OptionsRec.TimerMilliSeconds
        end;    //  btnTimerStart.Caption = 'Start'
      end;      //  if timerTimer.Enabled = false
    end;
    3:
    begin                    //  event page
      stsBrInfo.Panels.Items[4].Text := '';
      if EventTimer.Enabled = False then
      begin   // only set display to current
        DtEdtEvent.Date := now;
        SpnEdtMins.Value := MinuteOf(time);
        SpnEdtHour.Value := HourOf(time);
        btnEventSet.Enabled := False;
      end
      else
      begin
        stsBrInfo.Panels.Items[4].Text := format('Reminder set for %.2d:%.2d - %s', [SpnEdtHour.Value,
          SpnEdtMins.Value, DatetoStr(DtEdtEvent.Date)]);
      end;  //  if btnEventSet.Enabled
    end;
    4:
    begin                    //   reminder Page
      readReminderFile;
    end;
  end;

end;

procedure TfrmMain.mainTimerTimer(Sender: TObject);
{  on every tick on the clock, update the system.
      Update real time to status panel.
      Update desired time to either to main program, tray icon hint or pop-up notifier.
}
var
  myNow: TdateTime;
  mySecs: integer;
  strTime: string;
begin
  myNow := now;
  mySecs := SecondOfTheDay(myNow);

  if userOptions.HourPips and isTime(myNow, 0) then
    playChime('pips')
  else                                       //  only play chimes if pips turned off.
  begin
    if userOptions.HourChimes and isTime(myNow, 0) then playChime('hour');
    if userOptions.quarterChimes and isTime(myNow, 15) then playChime('quarter');
    if userOptions.HalfChimes and isTime(myNow, 30) then playChime('half');
    if userOptions.threeQuarterChimes and isTime(myNow, 45) then playChime('threequarter');
  end;

  if TrayIcon.Visible then
   begin
     strTime := CmbBxTime.Items.Strings[CmbBxTime.ItemIndex] + ' time :: ' + ft.getTime;
     TrayIcon.Hint := strTime;

     if userOptions.fuzzyTimeBalloon then
     begin
       if (mySecs mod 300 = 0) then
       begin  //  only display on the five minutes.
         TrayIcon.BalloonHint := strTime;
         trayIcon.ShowBalloonHint;
         ballonTimer.Enabled := True;                  //  balloon hint time-out bug - see below.
       end;
     end;  //  if OptionsRec.FuzzyTimeBalloon then

     if ppMnItmTime.Checked then
     begin
       popupTitle[0] := 'Time';
       popupMessages[0] := strTime;
       DisplayMessage;
     end;      //  if ppMnItmTime.Checked then begin
   end         //  if TrayIcon.Visible then
   else
   begin  //  normal display i.e. not trayicon
     UpdateTime(myNow);
     UpdateStatusBar(myNow);
   end;  //  if TrayIcon.Visible then

end;

procedure TfrmMain.UpdateTime(KTime: TDateTime);
{  Updates the time in the correct font.    }
begin
  case CmbBxTime.Items[CmbBxTime.ItemIndex] of
    'Bar Code Time':
    begin
      lblfuzzy.Font.Name := 'Bar Code 39';
      lblfuzzy.Caption := FormatDateTime('hh  nn  ss', KTime);
      lblfuzzy.Font.Size := 22;
      lblfuzzy.AutoSize := true;
    end;
    'Nancy Blackett Time':
    begin
      lblfuzzy.Font.Name := 'Nancy Blackett semaphore';
      lblfuzzy.Caption := FormatDateTime('hh  nn  ss', KTime);
      lblfuzzy.Font.Size := 22;
      lblfuzzy.AutoSize := true;
    end;
    'Semaphore Time':
    begin
      lblfuzzy.Font.Name := 'Semaphore';
      lblfuzzy.Caption := FormatDateTime('hh  nn  ss', KTime);
      lblfuzzy.Font.Size := 22;
      lblfuzzy.AutoSize := true;
    end;
    'Braille Time':
    begin
      lblfuzzy.Font.Name := 'BrailleLatin';
      lblfuzzy.Caption := FormatDateTime('hh  nn  ss', KTime);
      lblfuzzy.Font.Size := 22;
      lblfuzzy.AutoSize := true;
    end;
    'Christmas':
    begin
      lblfuzzy.Top := 1;
      lblfuzzy.Font.Name := 'Christmas';
      lblfuzzy.Font.Size := 28;
      lblfuzzy.Caption := FormatDateTime('hh  nn  ss', KTime);
      lblfuzzy.AutoSize := true;
    end;
    'Fuzzy Time', 'Word Time':
      begin
        if userOptions.christmasFont and isChristmas then
          lblfuzzy.Font.Name := 'Christmas'
        else
          lblfuzzy.Font.Name := 'default';
        lblfuzzy.Font.Size := 22;          //  seems to neede this, or changes size on each call.
        lblfuzzy.Font.Size := Trunc( 22 * (490 / GetTextWidth(ft.getTime, lblfuzzy.Font)));
        lblfuzzy.Caption := ft.getTime;
        lblfuzzy.Top := 0;
        lblfuzzy.AutoSize := true;
      end;
    else   //  no font substitution, use default font.
      begin
        lblfuzzy.Font.Name := 'default';
        lblfuzzy.Font.Size := 22;
        lblfuzzy.Caption := ft.getTime;
      end;
  end;

end;

procedure TfrmMain.UpdateStatusBar(KTime: TDateTime);
{  Updates the status bar.    }
VAR
  keyResult: string;
begin
  keyResult := ' cns ';
    if LCLIntf.GetKeyState(VK_CAPITAL) <> 0 then
      keyResult[2] := 'C';
    if LCLIntf.GetKeyState(VK_NUMLOCK) <> 0 then
      keyResult[3] := 'N';
    if LCLIntf.GetKeyState(VK_SCROLL) <> 0 then
      keyResult[4] := 'S';

    if userOptions.display24Hour then
      stsBrInfo.Panels.Items[0].Text := FormatDateTime('hh:nn:ss', KTime)
    else
      stsBrInfo.Panels.Items[0].Text := FormatDateTime('hh:nn:ss am/pm', KTime);

    stsBrInfo.Panels.Items[1].Text := FormatDateTime('DD MMM YYYY', KTime);
    stsBrInfo.Panels.Items[2].Text := keyResult;

    if userOptions.displayIdleTime then
      stsBrInfo.Panels.Items[3].Text := 'Idle Time :: ' + FormatDateTime('hh:nn:ss', tick / SecsPerDay)
    else
      stsBrInfo.Panels.Items[3].Text := '';
end;

procedure TfrmMain.ballonTimerTimer(Sender: TObject);
{  There seems to be a bug in lcl, the balloon time-out does not work.
   So, a timer is fired when the balloon hint is displayed and then
   10 seconds later this kludge is performed.
}
begin
  ballonTimer.Enabled := False;
  TrayIcon.Visible := False;
  TrayIcon.Visible := True;
end;

procedure TfrmMain.mainIdleTimerTimer(Sender: TObject);

begin
  tick += 1;
end;

procedure TfrmMain.mainIdleTimerStopTimer(Sender: TObject);
begin
  tick := 0;
end;
//
// *********************************************************** Fuzzy Time ******
//
procedure TfrmMain.CmbBxTimeChange(Sender: TObject);
{  called to set the different format of time.
   If index = 9 then radix time is chosen, so display choice of bases.
}
begin
  ft.displayFuzzy := CmbBxTime.ItemIndex;
  lblfuzzy.Caption := ft.getTime;

  if CmbBxTime.Items[CmbBxTime.ItemIndex] = 'Radix Time' then   //  hard coded for radix time.
  begin
    SpnEdtTimeBase.Visible := True;
    lblRadix.Visible := True;
    ft.FuzzyBase := SpnEdtTimeBase.Value;
  end
  else
  begin
    SpnEdtTimeBase.Visible := False;
    lblRadix.Visible := False;
  end;

  if userOptions.netTimeSeconds and (CmbBxTime.Items[CmbBxTime.ItemIndex] = 'NET Time') then
    mainTimer.Interval := 1
  else
    mainTimer.Interval := 1000;

  klog.writeLog(format('Main timer inerval set to %D milliseconds', [mainTimer.Interval]));
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
    chckBxCountdownSound.Visible := True;
    EdtCountdownSound.Visible := True;
    btnCountdownLoadSound.Visible := True;
    btnSoundTest.Visible := True;
  end
  else
  begin
    chckBxCountdownSound.Visible := False;
    EdtCountdownSound.Visible := False;
    btnCountdownLoadSound.Visible := False;
    btnSoundTest.Visible := False;
  end;

  if CmbBxCountdownAction.ItemIndex = 1 then
  begin  //  Reminder chosen
    chckBxCountdownReminder.Visible := True;
    EdtCountdownReminder.Visible := True;
  end
  else
  begin
    chckBxCountdownReminder.Visible := False;
    EdtCountdownReminder.Visible := False;
  end;

  if CmbBxCountdownAction.ItemIndex = 2 then
  begin  //  System event chosen
    chckBxCountdownEvent.Visible := True;
    CmbBxCountdownEvent.Visible := True;
  end
  else
  begin
    chckBxCountdownEvent.Visible := False;
    CmbBxCountdownEvent.Visible := False;
  end;

  if CmbBxCountdownAction.ItemIndex = 3 then
  begin  //  command chosen
    chckBxCountdownCommand.Visible := True;
    btnCountdownLoadCommand.Visible := True;
    EdtCountdownCommand.Visible := True;
  end
  else
  begin
    chckBxCountdownCommand.Visible := False;
    btnCountdownLoadCommand.Visible := False;
    EdtCountdownCommand.Visible := False;
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
  if btnCountdownStart.Caption = 'Start' then
  begin
    btnCountdownStop.Enabled := True;
    CountdownTimer.Enabled := True;
    SpnEdtCountdown.Enabled := False;
    VAL := CountdownTicks div 60;           //  in case the status message has changed
    stsBrInfo.Panels.Items[4].Text := format(' Counting down from %d minute[s]', [val]);

    btnCountdownStart.Caption := 'Pause';

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
    CountdownTimer.Enabled := False;
    btnCountdownStart.Caption := 'Resume';
    frmMain.Caption := 'Countdown :: PAUSED';
    application.Title := 'Paused';
  end
  else if btnCountdownStart.Caption = 'Resume' then
  begin
    CountdownTimer.Enabled := True;
    btnCountdownStart.Caption := 'Pause';
  end;

end;

procedure TfrmMain.btnCountdownLoadSoundClick(Sender: TObject);
{  if the text box is clicked, allow the sound file to be changed.
}
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.wav; *.mp3';
    InitialDir := getCurrentDir + '\sounds';
    Title := 'Choose a sound file.';
    if Execute then
    begin
      EdtCountdownSound.Text := ExtractFileName(FileName);
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
    Filter := '*.*';
    InitialDir := getCurrentDir;
    Title := 'Choose a executable';
    if Execute then
    begin
      EdtCountdownCommand.Text := (FileName);
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
  btnCountdownStop.Enabled := False;
  CountdownTimer.Enabled := False;
  SpnEdtCountdown.Enabled := True;

  frmMain.Caption := 'Countdown';
  application.Title := 'Countdown';
  LblCountdownTime.Caption := '00:00';
end;

procedure TfrmMain.SpnEdtCountdownChange(Sender: TObject);
{    called when the time is entered - only allow 1 - 90 minutes.
}
var
  val: integer;                 //  used to hold value from spin edit
                                //  can't pass this to the function directly
begin
  val := SpnEdtCountdown.Value;

  if (val > 0) and (val <= 90) then
  begin
    LblCountdownTime.Caption := format('%2.2d:00', [val]);
    btnCountdownStart.Enabled := True;
    countdownTicks := val * 60;

    stsBrInfo.Panels.Items[4].Text := format(' Counting down from %d minute[s]', [val]);
  end
  else
  begin
    LblCountdownTime.Caption := '00:00';
    btnCountdownStart.Enabled := False;
    countdownTicks := 0;

    stsBrInfo.Panels.Items[4].Text := ' Only allow 1 - 90 minutes';
  end;
end;

procedure TfrmMain.StopCountDown(Sender: TObject);
{    Called when the timer has finished.    }
begin
  LblCountdownTime.Caption := '00:00';

  btnCountdownStart.Enabled := True;       //  reset buttons
  btnCountdownStart.Caption := 'Start';
  btnCountdownStop.Enabled := False;
  SpnEdtCountdown.Enabled := True;
  CountdownTimer.Enabled := False;

  stsBrInfo.Panels.Items[4].Text := ' Finished counting, now!';
  frmMain.Caption := 'Countdown';
  application.Title := 'Countdown';

  if chckBxCountdownSound.Checked then
  begin      //  only play sound if checked
    doPlaySound(EdtCountdownSound.Text, userOptions.volume);
    chckBxCountdownSound.Checked := False;
    ChckBxCountdownSoundChange(Sender);    //  now box is unchecked, call change procedure
  end;

  if chckBxCountdownReminder.Checked then
  begin   //  only display reminder if checked
    popupTitle[1] := 'Countdown';
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
    doCommandEvent(EdtCountdownCommand.Text);
    chckBxCountdownCommand.Checked := False;
    chckBxCountdownCommandChange(Sender);    //  now box is unchecked, call change procedure
  end;

  //  reset the noOfTicks, so we start the timer again without changing the time.
  //  should be okay, already validated [if time is changed will be revalidated]
  countdownTicks := SpnEdtCountdown.Value * 60;

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
  CmbBxCountdownEvent.Visible := False;
  chckBxCountdownEvent.Checked := False;
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
  application.Title := message;
  frmMain.Caption := 'Countdown :: ' + message;
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
    EdtCountdownSound.Enabled := True;
    btnCountdownLoadSound.Enabled := True;
    btnSoundTest.Enabled := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Sound Disabled';
    EdtCountdownSound.Enabled := False;
    btnCountdownLoadSound.Enabled := False;
    btnSoundTest.Enabled := False;
  end;
end;

procedure TfrmMain.chckBxCountdownReminderChange(Sender: TObject);
{  enable or disable reminders.    }
begin
  if chckBxCountdownReminder.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'Reminder Enabled';
    EdtCountdownReminder.Enabled := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Reminder Disabled';
    EdtCountdownReminder.Enabled := False;
  end;
end;

procedure TfrmMain.chckBxCountdownEventChange(Sender: TObject);
{  enable or disable system events.    }
begin
  if chckBxCountdownEvent.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'System Event Enabled';
    CmbBxCountdownEvent.Enabled := True;
    CmbBxCountdownEvent.ItemIndex := 0;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'System Event Disabled';
    CmbBxCountdownEvent.Enabled := False;
  end;
end;

procedure TfrmMain.chckBxCountdownCommandChange(Sender: TObject);
{  enable or disable commands.    }
begin
  if chckBxCountdownCommand.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'Command Enabled';
    btnCountdownLoadCommand.Enabled := True;
    EdtCountdownCommand.Enabled := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Command Disabled';
    btnCountdownLoadCommand.Enabled := False;
    EdtCountdownCommand.Enabled := False;
  end;
end;
//
// *********************************************************** Timer ***********
//
procedure TfrmMain.timerTimerTimer(Sender: TObject);
{  is time is enables, this will be the timer tick.    }
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
      timerTimer.Interval := 100;
    timerStart := time;
    timerPaused := 0;
    btnTimerStop.Enabled := True;
    timerTimer.Enabled := True;
    btnTimerClear.Enabled := False;
    btnTimerStart.Caption := 'Pause';
    btnTimerSplit.Enabled := True;
    lblSplitLap.Enabled := True;
    frmMain.Caption := 'Timer :: Started';
    stsBrInfo.Panels.Items[4].Text := 'Timer Running';
  end
  else if btnTimerStart.Caption = 'Pause' then
  begin
    timerPaused := timerPaused + (time - timerStart);
    btnTimerStart.Caption := 'Resume';
    timerTimer.Enabled := False;
    btnTimerSplit.Enabled := False;
    lblSplitLap.Enabled := False;
    frmMain.Caption := 'Timer :: Paused';
    stsBrInfo.Panels.Items[4].Text := 'Timer :: Paused';
  end
  else if btnTimerStart.Caption = 'Resume' then
  begin
    timerStart := time;
    btnTimerStart.Caption := 'Pause';
    timerTimer.Enabled := True;
    btnTimerSplit.Enabled := True;
    lblSplitLap.Enabled := True;
    frmMain.Caption := 'Timer :: Started';
    stsBrInfo.Panels.Items[4].Text := 'Timer Running';
  end;
end;

procedure TfrmMain.btnTimerStopClick(Sender: TObject);
{  Stop the timer.    }
begin
  btnTimerStop.Enabled := False;
  timerTimer.Enabled := False;
  btnTimerSplit.Enabled := False;
  lblSplitLap.Enabled := False;
  btnTimerClear.Enabled := True;
  btnTimerStart.Caption := 'Start';
  frmMain.Caption := 'Timer :: Stoped';
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
    lblTimer.Caption := '00:00:00:00';
    lblSplitLap.Caption := '00:00:00:00';
  end
  else
  begin
    lblTimer.Caption := '00:00:00';
    lblSplitLap.Caption := '00:00:00';
  end;  //  if userOptions.timerMilliSeconds = 'True' then

  stsBrInfo.Panels.Items[4].Text := '';

  btnTimerSplit.Enabled := False;
  lblSplitLap.Enabled := False;
end;
//
// ************************************************************* Event ********
//
procedure TfrmMain.SpnEdtHourChange(Sender: TObject);
{  will one day be used to validate the hours set.    }
begin
  EventValid;
end;

procedure TfrmMain.SpnEdtMinsChange(Sender: TObject);
{  will one day be used to validate the minute set.    }
begin
  EventValid;
end;

procedure TfrmMain.DtEdtEventChange(Sender: TObject);
{  will one day be used to validate the date set.    }
begin
  EventValid;
end;

procedure TfrmMain.EventValid;
{  only allow the reminder set button to be enabled,
   if the reminder date is in the future.
}
var
  EvntDt: TDateTime;
begin
  EvntDt := EncodeDateTime(YearOf(DtEdtEvent.Date), MonthOf(DtEdtEvent.Date),
            DayOf(DtEdtEvent.Date), SpnEdtHour.Value, SpnEdtMins.Value, 0, 0);

  if (EvntDt > Now) then
    btnEventSet.Enabled := True
  else
    btnEventSet.Enabled := False;

end;

procedure TfrmMain.btnEventSetClick(Sender: TObject);
{  Set the reminder.    }
begin
  lblEvent.Caption := format('Event set for %.2d:%.2d - %s', [SpnEdtHour.Value, SpnEdtMins.Value, DatetoStr(DtEdtEvent.Date)]);
  stsBrInfo.Panels.Items[4].Text := format('Event set for %.2d:%.2d - %s', [SpnEdtHour.Value, SpnEdtMins.Value, DatetoStr(DtEdtEvent.Date)]);

  SpnEdtMins.Visible := False;
  SpnEdtHour.Visible := False;
  DtEdtEvent.Visible := False;

  btnEventClear.Enabled := True;
  btnEventSet.Enabled := False;

  EventTimer.Enabled := True;
end;

procedure TfrmMain.btnEventClearClick(Sender: TObject);
begin
  resetEvent;
end;

procedure TfrmMain.EventTimerTimer(Sender: TObject);
{  if reminders are set, this will be ticking and tested to
   see if the reminder is due.
}
var
  EvntDt: TDateTime;
begin
  EvntDt := EncodeDateTime(YearOf(DtEdtEvent.Date), MonthOf(DtEdtEvent.Date),
    DayOf(DtEdtEvent.Date), SpnEdtHour.Value, SpnEdtMins.Value, 0, 0);

  if Now > EvntDt then
    EventTimerStop(Sender);

end;

procedure Tfrmmain.EventTimerStop(Sender: TObject);
{  Called when the reminder date/time is passed - calls any actions required.    }
begin
  EventTimer.Enabled := False;
  btnEventSet.Enabled := False;

  if ChckBxEventSound.Checked then
  begin       //  only play sound if checked
    ChckBxEventSound.Checked := False;
    doPlaySound(EdtEventSound.Text, userOptions.volume);
  end;

  if ChckBxEventReminder.Checked then
  begin    //  only display reminder if checked
    ChckBxEventReminder.Checked := False;
    popupTitle[3] := 'Reminder';
    popupMessages[3] := EdtEventText.Text;
    DisplayMessage;
  end;

  if ChckBxEventCommand.Checked then
  begin     //  only execute command if checked
    ChckBxEventCommand.Checked := False;
    doCommandEvent(EdtEventCommand.Text);
  end;

  if ChckBxEventSystem.Checked then
  begin      //  only do system event if checked
    ChckBxEventSystem.Checked := False;
    btnEventAbort.Visible := True;
    doSystemEvent(CmbBxEventSystem.ItemIndex);

    if TrayIcon.Visible then
    begin               //  if running in the system tray,
      ppMnItmShowClick(Sender);                  //  select the reminder tab and
      PageControl1.TabIndex := 3;                //  display main application so
    end;                                         //  the abort button can be used.
  end;  //  if ChckBxEventSystem.Checked then begin

  resetEvent;
end;

procedure TfrmMain.btnEventAbortClick(Sender: TObject);
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
    popupTitle[f] := '';
    popupMessages[f] := '';
  end;

  DisplayMessage;               //  will clear popup if currently displayed.

  btnCountdownShutdownAbort.Visible := False;
  CmbBxEventSystem.Visible := False;
  ChckBxEventSystem.Checked := False;
  btnEventAbort.Visible := False;

  resetEvent;
end;

procedure TfrmMain.resetEvent;
{  performs reminder reset.    }
begin
  lblEvent.Caption := 'Event not set';
  stsBrInfo.Panels.Items[4].Text := '';

  EventTimer.Enabled := False;
  btnEventClear.Enabled := False;

  DtEdtEvent.Date := now;
  SpnEdtMins.Value := MinuteOf(time);
  SpnEdtHour.Value := HourOf(time);

  DtEdtEvent.Enabled := True;
  spnEdtHour.Enabled := True;
  spnEdtMins.Enabled := True;

  SpnEdtMins.Visible := True;
  SpnEdtHour.Visible := True;
  DtEdtEvent.Visible := True;

end;

procedure TfrmMain.CmbBxEventActionChange(Sender: TObject);
begin
  if CmbBxEventAction.ItemIndex = 0 then
  begin  //  Sound chosen
    ChckBxEventSound.Visible := True;
    EdtEventSound.Visible := True;
    btnEventrLoadSound.Visible := True;
    btnEventTestSound.Visible := True;
  end
  else
  begin
    ChckBxEventSound.Visible := False;
    EdtEventSound.Visible := False;
    btnEventrLoadSound.Visible := False;
    btnEventTestSound.Visible := False;
  end;

  if CmbBxEventAction.ItemIndex = 1 then
  begin  //  Reminder chosen
    ChckBxEventReminder.Visible := True;
    EdtEventText.Visible := True;
  end
  else
  begin
    ChckBxEventReminder.Visible := False;
    EdtEventText.Visible := False;
  end;

  if CmbBxEventAction.ItemIndex = 2 then
  begin  //  System chosen
    ChckBxEventSystem.Visible := True;
    CmbBxEventSystem.Visible := True;
    btnCountdownShutdownAbort.Visible := True;
  end
  else
  begin
    ChckBxEventSystem.Visible := False;
    CmbBxEventSystem.Visible := False;
    btnCountdownShutdownAbort.Visible := False;
  end;

  if CmbBxEventAction.ItemIndex = 3 then
  begin  //  Command chosen
    ChckBxEventCommand.Visible := True;
    btnEventLoadCommand.Visible := True;
    EdtEventCommand.Visible := True;
  end
  else
  begin
    ChckBxEventCommand.Visible := False;
    btnEventLoadCommand.Visible := False;
    EdtEventCommand.Visible := False;
  end;
end;

procedure TfrmMain.ChckBxEventSoundChange(Sender: TObject);
begin
  if ChckBxEventSound.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'Sound Enabled';
    EdtEventSound.Enabled := True;
    btnEventrLoadSound.Enabled := True;
    btnEventTestSound.Enabled := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Sound Disabled';
    EdtEventSound.Enabled := False;
    btnEventrLoadSound.Enabled := False;
    btnEventTestSound.Enabled := False;
  end;
end;

procedure TfrmMain.ChckBxEventReminderChange(Sender: TObject);
begin
  if ChckBxEventReminder.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'Reminder Enabled';
    EdtEventText.Enabled := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Reminder Disabled';
    EdtEventText.Enabled := False;
  end;
end;

procedure TfrmMain.ChckBxEventSystemChange(Sender: TObject);
begin
  if ChckBxEventSystem.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'System Events Enabled';
    CmbBxEventSystem.Enabled := True;
    CmbBxEventSystem.ItemIndex := 0;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'System Events Disabled';
    CmbBxEventSystem.Enabled := False;
  end;
end;

procedure TfrmMain.ChckBxEventCommandChange(Sender: TObject);
begin
  if ChckBxEventCommand.Checked then
  begin
    stsBrInfo.Panels.Items[4].Text := 'Command Enabled';
    EdtEventCommand.Enabled := True;
    btnEventLoadCommand.Enabled := True;
  end
  else
  begin
    stsBrInfo.Panels.Items[4].Text := 'Command Disabled';
    EdtEventCommand.Enabled := False;
    btnEventLoadCommand.Enabled := False;
  end;
end;

procedure TfrmMain.btnEventLoadCommandClick(Sender: TObject);
{  if the command box is clicked, allow the command file to be loaded.    }
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.*';
    InitialDir := getCurrentDir;
    Title := 'Choose a executable';
    if Execute then
    begin
      EdtEventCommand.Text := FileName;
      stsBrInfo.Panels.Items[4].Text := Filename + ' Chosen';
    end;    //  if Exectute
    Free;
  end;
end;

procedure TfrmMain.btnEventrLoadSoundClick(Sender: TObject);
{  if the text box is clicked, allow the sound file to be changed.    }
begin
  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.wav; *.mp3';
    InitialDir := getCurrentDir + '\sounds';
    Title := 'Choose a sound file.';
    if Execute then
    begin
      EdtEventSound.Text := ExtractFileName(FileName);
      stsBrInfo.Panels.Items[4].Text := Filename + ' Chosen';
    end;    //  if Execute
    Free;
  end;
end;

procedure TfrmMain.btnEventTestSoundClick(Sender: TObject);
begin
  doPlaySound(EdtEventSound.Text, userOptions.volume);
end;
//
// ************************************************************* Reminder ******
//
procedure TfrmMain.btnReminderNewClick(Sender: TObject);
{  Loads form, so a new reminder can be input.    }
begin
  frmReminderInput.ShowModal;
  readReminderFile;              //  reread reminder file, to reflect changes - if any.
end;

procedure TfrmMain.readReminderFile;
{  Read the reminder file and passes each line to be parsed, added each
   parsed line to the list box for display.
   File is opened and closed within this procedure.
}
var
  readLine: string;
  rmndrData: UKlockUtils.reminderData;
begin
  ChckLstBxReminder.Clear;
  ChckLstBxReminder.Enabled := False;
  noReminder := 0;

  AssignFile(ReminderFile, ReminderData);
  try
    Reset(ReminderFile);

    repeat
      Readln(ReminderFile, readLine);     // Reads the whole line from the reminder file/
      if (readLine[1] <> '-') then
      begin  // ignore header lines.
        rmndrData := parseReminder(readLine);
        ChckLstBxReminder.Items.Add(rmndrData.message);

        if rmndrData.active then
          ChckLstBxReminder.Checked[noReminder] := True;

        noReminder += 1;

      end;
    until (EOF(ReminderFile)); // EOF(End Of File) The the program
  except
    ShowMessage('  ERROR: Cannot open Reminder File  :: ' + IntToStr(IOResult));
  end;

  if noReminder = 0 then
    ChckLstBxReminder.Items.Add('No Reminders on file')
  else
    ChckLstBxReminder.Enabled := True;

  CloseFile(ReminderFile);
end;
//
// *********************************************************** Menu procs ******
//
procedure TfrmMain.mnuItmOptionsClick(Sender: TObject);
{  if clicked, call the option screen, reapply options after.    }
var
  frmTop: integer;
  frmLeft: integer;
  res: integer;         //  return value from option screen.
begin
  if userOptions.screenSave then
  begin
    userOptions.formTop := frmMain.Top;
    userOptions.formLeft := frmMain.Left;
  end
  else
  begin
    frmTop := frmMain.Top;    //  return to same place, after option screen.
    frmLeft := frmMain.Left;
  end;

  res := frmOptions.ShowModal;

  if res = 1 then             //  1 = OK button press, 2 = cancel button pressed.
    SetDefaults;

  if not userOptions.screenSave then
  begin  //  not done in SetDefaults
    frmMain.Top := frmTop;
    frmMain.Left := frmLeft;
  end;
end;

procedure TfrmMain.mnuItmExitClick(Sender: TObject);
{  Close the program.
   Called by button panel exit, main menu exit and tray icon pop up menu exit.
}
begin
  Close;
end;

procedure TfrmMain.mnuItmAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.mnuItmHelpClick(Sender: TObject);
begin
  frmHelp.ShowModal;
end;

procedure TfrmMain.mnuItmLicenseClick(Sender: TObject);
begin
  frmLicense.ShowModal;
end;
//
// ********************************************************* Time Menu *********
//
procedure TfrmMain.mnuItmAnalogueKlockClick(Sender: TObject);
begin
  frmAnalogueKlock.Show;
end;
procedure TfrmMain.mnuItmLEDKlockClick(Sender: TObject);
begin
  frmLEDKlock.Show;
end;
procedure TfrmMain.mnuItmBinaryKlockClick(Sender: TObject);
begin
  frmBinaryKlock.Show;
end;
procedure TfrmMain.mnuItmSmallTextKlockClick(Sender: TObject);
begin
  frmSmallTextKlock.Show;
end;
//
// ********************************************************* Info Menu *********
//
procedure TfrmMain.mnuItmDaylightSavingClick(Sender: TObject);
begin
  frmInfo.Info := 'Daylight Saving';
  frmInfo.ShowModal;
end;

procedure TfrmMain.mnuItmEasterDatesClick(Sender: TObject);
begin
  frmInfo.Info := 'Easter Dates';
  frmInfo.ShowModal;
end;

procedure TfrmMain.mnuItmLentDatesClick(Sender: TObject);
begin
  frmInfo.Info := 'Lent Dates';
  frmInfo.ShowModal;
end;

procedure TfrmMain.mnuItmPowerSourceClick(Sender: TObject);
begin
  frmInfo.Info := 'Power Source';
  frmInfo.ShowModal;
end;
//
// ********************************************************* ButtonPannel ******
//
procedure TfrmMain.HelpButtonClick(Sender: TObject);
var
  helpText: string;
begin

  case PageControl1.TabIndex of
    0: helpText := 'Fuzzy Time' + LineEnding + 'Displays current time in a number of different formats';
    1: helpText := 'Countdown' + LineEnding + 'Select a time to countdown and an event to be triggered';
    2: helpText := 'Timer' + LineEnding + 'Simple timer with pause and split time function';
    3: helpText := 'Event' + LineEnding + 'Set a time and/or date to be reminded of';
    4: helpText := 'Reminder' + LineEnding + 'Set a time and/or date to be reminded of';
  end;

  ShowMessage(helpText);
end;

procedure TfrmMain.OKButtonClick(Sender: TObject);
{  if clicked will hide the main form and display the tray icon.    }
begin
  TrayIcon.Visible := True;
  TrayIcon.Show;

  frmMain.Visible := False;
end;

procedure TfrmMain.CloseButtonClick(Sender: TObject);
{  if clicked will hide the main form and display the tray icon.    }
begin
  close;
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

  KillOtherKlocks;    //  if made visable from the tray, kill any other klocks that are visable.

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
    popupTitle[1] := '';
    popupMessages[0] := '';
    DisplayMessage;
  end
  else
    ppMnItmTime.Checked := True;
end;

procedure TfrmMain.TrayIconDblClick(Sender: TObject);
{  double clicking the tray icon, will clear all messages and kill the pop-up notifier.
}
var
  f: integer;
begin
  for f := 0 to 3 do
  begin
    popupTitle[f] := '';
    popupMessages[f] := '';
  end;

  ppMnItmTime.Checked := False;
  DisplayMessage;
end;

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
    popupTitle[f] := '';
    popupMessages[f] := '';
  end;

  CloseAction := caFree;
end;
//
// *****************************************************************************
//
end.
