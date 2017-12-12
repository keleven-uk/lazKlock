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
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, Buttons, StdCtrls, Spin, PopupNotifier, EditBtn, ButtonPanel,
  formAbout, formHelp, formOptions, formLicense, UFuzzyTime, dateutils, LCLIntf, LCLType,
  CheckLst, UKlockUtils, formReminderInput, AvgLvlTree, uOptions, Windows, formAnalogueKlock;

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
    mnuTime: TMenuItem;
    MenuItem2: TMenuItem;
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
    mnuhelp: TMenuItem;
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
    procedure MenuItem2Click(Sender: TObject);
    procedure mnuItmAboutClick(Sender: TObject);
    procedure mnuItmExitClick(Sender: TObject);
    procedure mnuItmHelpClick(Sender: TObject);
    procedure mnuItmLicenseClick(Sender: TObject);
    procedure mnuItmOptionsClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
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
  public

  end; 

var
  frmMain : TfrmMain;
  rmndrStore : TAvgLvlTree;      //  to store all the reminders.
  userOptions: Options;          //  holds all the user options.
  ft : FuzzyTime;                //  the object to give the different times.
  appStartTime: Int64;           //  used by formAbout to determine how long the app has been running.
  countdownTicks: integer;
  countdownSoundName: String;
  EventSoundName: String;
  timerStart: TDateTime;
  timerPaused: TdateTime;
  popupMessages: Array [0..3] of string;
  popupTitle: Array [0..3] of String;
  noReminder: Integer;
  tick: integer=0;
implementation

{$R *.lfm}

{ TfrmMain }

// *********************************************************** Global **********
procedure TfrmMain.FormCreate(Sender: TObject);
{  Called at start - sets up fuzzy time and default sound files.
}
begin
  mainTimer.Enabled  := False;          //  disable main timer until all options and fuzzt time are set up.

  countdownSoundName     := getCurrentDir + '\sounds\alarm-fatal.wav';  // default to sound file
  EdtCountdownSound.Text := ExtractFileName(countdownSoundName);        //  in current working directory.

  EventSoundName     := getCurrentDir + '\sounds\alarm-fatal.wav';      // default to sound file
  EdtEventSound.Text := ExtractFileName(countdownSoundName);            //  in current working directory.

  DtEdtEvent.Date     := now;
  SpnEdtMins.Value    := MinuteOf(time);
  SpnEdtHour.Value    := HourOf(time);
  btnEventSet.Enabled := false;

  noReminder := 0;
  appStartTime := GetTickCount64;  //  tick count when application starts.

  rmndrStore := TAvgLvlTree.Create;
  userOptions := Options.Create;   //  create options file as c:\Users\<user>\AppData\Local\Stub\Options.xml
  ft := FuzzyTime.Create;

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
  SetDefaults;
  mainTimer.Enabled := True;        //  Now safe to enable main timer.
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{  called on form close, save screen position if needed.
}
begin
  if userOptions.screenSave then begin
    userOptions.formTop := frmMain.Top;
    userOptions.formLeft := frmMain.Left;
  end;

  userOptions.writeCurrentOptions ;
  CloseAction := caFree;
end;

procedure TfrmMain.SetDefaults;
{  called to set defaults on start-up.
   Set things that can be changed in the options screen, to the values in the options screen.
}
begin
  PageControl1.TabIndex := userOptions.defaultTab;
  CmbBxTime.Items := ft.fuzzyTypes;
  CmbBxTime.ItemIndex   := userOptions.defaultTime;
  mainIdleTimer.Enabled := userOptions.displayIdleTime;

  ft.displayFuzzy := userOptions.defaultTime;

  ft.fuzzyBase := 2;

  if userOptions.screenSave then begin
    frmMain.Top := userOptions.formTop;
    frmMain.Left := userOptions.formLeft;
  end;

  if userOptions.netTimeSeconds then
    mainTimer.Interval := 1
  else
    mainTimer.Interval := 1000;

end;

procedure TfrmMain.DisplayMessage;
{  display a message as a pop-up notifier.
   If this procedure is called with an empty message array,
   the pop-up notifier is cancelled.
}
VAR
  f       : integer;
  title   : string;       //  do we need to set title?
  message : string;
begin

  title   := '';
  message := '';

  for f := 0 to 3 do begin
    if (popupMessages[f] <> '') then begin
      message := message + popupMessages[f] + LineEnding;
    end;
    if (popupTitle[f] <> '') then begin
      Title := Title + popupTitle[f] + ' : ';
    end;
  end;

  PopupNotifier1.ShowAtPos(100,100) ;
  PopupNotifier1.Color := clyellow;
  PopupNotifier1.Title := title;
  PopupNotifier1.Text  := message;

  if (PopupNotifier1.Visible = false) then  // if not currently shown, show
    PopupNotifier1.Visible := true ;

  if (message = '') then begin    //  empty message array, close pop-up notifier
    PopupNotifier1.Visible := false ;
    PopupNotifier1.Destroy;
  end;

end;



procedure TfrmMain.PageControl1Change(Sender: TObject);
{   called when tabs is changed on the main tab control.
    Sets the appropiate information for each tab.
        0 = time
        1 = countdown
        2 = timer
        3 = event
        4 = reminder
}
begin

  case PageControl1.TabIndex of
    0 : begin                     //  fuzzy page
      stsBrInfo.Panels.Items[3].Text := '';
    end;
    1 : begin                     //  countdown page
      if CountdownTimer.Enabled = false then
        stsBrInfo.Panels.Items[3].Text := ''
      else
        stsBrInfo.Panels.Items[3].Text := format(' Counting down from %2.d minute[s]', [SpnEdtCountdown.Value]);
    end;
    2 : begin                     //  timer page
      stsBrInfo.Panels.Items[3].Text := '';

      if timerTimer.Enabled = false then begin

      if btnTimerStart.Caption = 'Resume' then
        stsBrInfo.Panels.Items[3].Text := 'Timer :: Paused';

        if btnTimerStart.Caption = 'Start' then begin
          if userOptions.timerMilliSeconds then begin
            lblTimer.Caption    := '00:00:00:00';
            lblSplitLap.Caption := '00:00:00:00';
          end
          else begin
            lblTimer.Caption    := '00:00:00';
            lblSplitLap.Caption := '00:00:00';
          end;  //  if OptionsRec.TimerMilliSeconds
        end;    //  btnTimerStart.Caption = 'Start'
      end;      //  if timerTimer.Enabled = false
    end;
    3 : begin                    //  event page
      stsBrInfo.Panels.Items[3].Text := '';
      if EventTimer.Enabled = false then begin   // only set display to current
        DtEdtEvent.Date     := now;
        SpnEdtMins.Value       := MinuteOf(time);
        SpnEdtHour.Value       := HourOf(time);
        btnEventSet.Enabled := false;
      end
      else begin
        stsBrInfo.Panels.Items[3].Text := format('Reminder set for %.2d:%.2d - %s',
             [SpnEdtHour.Value, SpnEdtMins.Value, DatetoStr(DtEdtEvent.Date)]);
      end;  //  if btnEventSet.Enabled
    end;
    4: begin                    //   reminder Page
      readReminderFile;
    end;
  end;

end;

procedure TfrmMain.mainTimerTimer(Sender: TObject);
{  on every tick on the clock, update the system.
      update real time to status panel.
      update desired time to either to main program, tray icon hint or pop-up notifier.
}
VAR
  strTime   : String;
  keyResult : String;
begin

  if TrayIcon.Visible then begin
    strTime := CmbBxTime.Items.Strings[CmbBxTime.ItemIndex] + ' time :: ' + ft.getTime;
    TrayIcon.Hint := strTime;

    if userOptions.fuzzyTimeBalloon then begin
      if (SecondOfTheDay(now) mod 300 = 0) then begin  //  only display on the five minutes.
         TrayIcon.BalloonHint:= strTime;
         trayIcon.ShowBalloonHint;
         ballonTimer.Enabled := true;                  //  balloon hint time-out bug - see below.
      end;
    end;  //  if OptionsRec.FuzzyTimeBalloon then

    if ppMnItmTime.Checked then begin
      popupTitle[0]    := 'Time';
      popupMessages[0] := strTime;
      DisplayMessage;
    end;      //  if ppMnItmTime.Checked then begin
  end         //  if TrayIcon.Visible then
  else begin  //  normal display i.e. not trayicon
    lblfuzzy.Caption    := ft.getTime;

    keyResult := ' cns ';
    if LCLIntf.GetKeyState(VK_CAPITAL) <> 0 then keyResult[2] := 'C';
    if LCLIntf.GetKeyState(VK_NUMLOCK) <> 0 then keyResult[3] := 'N';
    if LCLIntf.GetKeyState(VK_SCROLL)  <> 0 then keyResult[4] := 'S';

    stsBrInfo.Panels.Items[0].Text := TimeToStr(Time) ;
    stsBrInfo.Panels.Items[1].Text := FormatDateTime('DD MMM YYYY', Now);
    stsBrInfo.Panels.Items[2].Text := keyResult ;
    if userOptions.displayIdleTime then
      stsBrInfo.Panels.Items[3].Text := 'Idle Time :: ' + FormatDateTime('hh:nn:ss', tick / SecsPerDay)
    else
      stsBrInfo.Panels.Items[3].Text := '';
  end;  //  if TrayIcon.Visible then

end;

procedure TfrmMain.ballonTimerTimer(Sender: TObject);
{  There seems to be a bug in lcl, the balloon time-out does not work.
   So, a timer is fired when the balloon hint is displayed and then
   10 seconds later this kludge is performed.
}
begin
  ballonTimer.Enabled := false;
  TrayIcon.Visible    := false;
  TrayIcon.Visible    := True;
end;

procedure TfrmMain.mainIdleTimerTimer(Sender: TObject);

begin
  tick += 1;
end;

procedure TfrmMain.MenuItem2Click(Sender: TObject);
begin
  frmAnalogueKlock.Show;
end;

procedure TfrmMain.mainIdleTimerStopTimer(Sender: TObject);
begin
  tick := 0;
end;


// *********************************************************** Fuzzy Time ******
procedure TfrmMain.CmbBxTimeChange(Sender: TObject);
{  called to set the different format of time.
   If index = 9 then radix time is chosen, so display choice of bases.
}
begin

  ft.displayFuzzy     := CmbBxTime.ItemIndex;
  lblfuzzy.Caption    := ft.getTime;

  if CmbBxTime.ItemIndex = 9 then begin
    SpnEdtTimeBase.Visible := true;
    lblRadix.Visible       := true;
    ft.FuzzyBase := SpnEdtTimeBase.Value;
  end
  else begin
    SpnEdtTimeBase.Visible := false;
    lblRadix.Visible       := false;
  end;

end;

procedure TfrmMain.SpnEdtTimeBaseChange(Sender: TObject);
begin
  ft.FuzzyBase := SpnEdtTimeBase.Value;
end;


// *********************************************************** Countdown *******
procedure TfrmMain.CmbBxCountdownActionChange(Sender: TObject);
{  Set the desired action, for when the countdown is completed.
}
begin
  if CmbBxCountdownAction.ItemIndex = 0 then begin  //  Sound chosen
    chckBxCountdownSound.Visible  := true;
    EdtCountdownSound.Visible     := true;
    btnCountdownLoadSound.Visible := true;
    btnSoundTest.Visible          := true;
  end
  else begin
    chckBxCountdownSound.Visible  := false;
    EdtCountdownSound.Visible     := false;
    btnCountdownLoadSound.Visible := false;
    btnSoundTest.Visible          := false;
  end;

  if CmbBxCountdownAction.ItemIndex = 1 then begin  //  Reminder chosen
    chckBxCountdownReminder.Visible := true;
    EdtCountdownReminder.Visible    := true;
  end
  else begin
    chckBxCountdownReminder.Visible := false;
    EdtCountdownReminder.Visible    := false;
  end;

  if CmbBxCountdownAction.ItemIndex = 2 then begin  //  System event chosen
    chckBxCountdownEvent.Visible := true;
    CmbBxCountdownEvent.Visible  := true;
  end
  else begin
    chckBxCountdownEvent.Visible := false;
    CmbBxCountdownEvent.Visible  := false;
  end;

  if CmbBxCountdownAction.ItemIndex = 3 then begin  //  command chosen
    chckBxCountdownCommand.Visible  := true;
    btnCountdownLoadCommand.Visible := true;
    EdtCountdownCommand.Visible     := true;
  end
  else begin
    chckBxCountdownCommand.Visible  := false;
    btnCountdownLoadCommand.Visible := false;
    EdtCountdownCommand.Visible     := false;
  end;
end;

procedure TfrmMain.btnCountdownStartClick(Sender: TObject);
{ called when start button is clicked, can have three modes
      Start  :: Start countdown
      Pause  :: Pause countdown
      Resume :: Resume a paused countdown.
}
VAR
  val : integer;

begin
  if btnCountdownStart.Caption = 'Start' then begin
    btnCountdownStop.Enabled  := true;
    CountdownTimer.Enabled    := True;
    SpnEdtCountdown.Enabled   := false;
    VAL := CountdownTicks div 60;           //  in case the status message has changed
    stsBrInfo.Panels.Items[3].Text := format(' Counting down from %d minute[s]', [val]);

    btnCountdownStart.Caption := 'Pause';

    if (chckBxCountdownCommand.Checked) and (EdtCountdownCommand.Text = '') then begin
      popupTitle[1]    := 'Countdown';
      popupMessages[1] := 'er, need to give Klock a command.';
      DisplayMessage;
      btnCountdownStopClick(Sender);  //  pretend the stop button has been pressed.
    end;
  end
  else if btnCountdownStart.Caption = 'Pause' then begin
    CountdownTimer.Enabled    := False;
    btnCountdownStart.Caption := 'Resume';
    frmMain.Caption   := 'Countdown :: PAUSED';
    application.Title := 'Paused';
  end
  else if btnCountdownStart.Caption = 'Resume' then begin
      CountdownTimer.Enabled    := True;
      btnCountdownStart.Caption := 'Pause'
  end

end;

procedure TfrmMain.btnCountdownLoadSoundClick(Sender: TObject);
{  if the text box is clicked, allow the sound file to be changed.
      MUST BE A .wav FILE.
}
begin

  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.wav';
    InitialDir:= getCurrentDir + '\sounds';
    Title := 'Choose a sound file [.wav]' ;
    if Execute then begin
      if ExtractFileExt(FileName) = '.wav' then begin  //  only allow .wav
        countdownSoundName     := FileName;
        EdtCountdownSound.Text := ExtractFileName(FileName);
        stsBrInfo.Panels.Items[3].Text := Filename + ' Chosen'
      end;  //  if ExtractFileExt
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
    InitialDir:= getCurrentDir;
    Title := 'Choose a executable' ;
    if Execute then begin
      EdtCountdownCommand.Text := (FileName);
      stsBrInfo.Panels.Items[3].Text := Filename + ' Chosen'
    end;    //  if Execute
    Free;
  end;
end;

procedure TfrmMain.btnCountdownStopClick(Sender: TObject);
{  Called when the countdown stop button is clicked.
}
begin
  btnCountdownStart.Enabled := true;
  btnCountdownStart.Caption := 'Start' ;
  btnCountdownStop.Enabled  := false;
  CountdownTimer.Enabled    := false;
  SpnEdtCountdown.Enabled   := true;

  frmMain.Caption   := 'Countdown';
  application.Title := 'Countdown';
  LblCountdownTime.Caption := '00:00';
end;

procedure TfrmMain.SpnEdtCountdownChange(Sender: TObject);
{    called when the time is entered - only allow 1 - 90 minutes.
}
var
  val : integer;                 //  used to hold value from spin edit
                                 //  can't pass this to the function directly
begin
  val := SpnEdtCountdown.Value;

  if (val > 0) and (val <= 90) then begin
    LblCountdownTime.Caption  := format('%2.2d:00', [val]);
    btnCountdownStart.Enabled := true;
    countdownTicks            := val * 60;

    stsBrInfo.Panels.Items[3].Text := format(' Counting down from %d minute[s]', [val]);
  end
  else begin
    LblCountdownTime.Caption  := '00:00';
    btnCountdownStart.Enabled := false;
    countdownTicks            := 0;

    stsBrInfo.Panels.Items[3].Text := ' Only allow 1 - 90 minutes';
  end;
end;

procedure TfrmMain.StopCountDown(Sender: TObject);
{    Called when the timer has finished.
}
begin
  LblCountdownTime.Caption:= '00:00';

  btnCountdownStart.Enabled := true;       //  reset buttons
  btnCountdownStart.Caption := 'Start' ;
  btnCountdownStop.Enabled  := false;
  SpnEdtCountdown.Enabled   := true;
  CountdownTimer.Enabled    := false;

  stsBrInfo.Panels.Items[3].Text := ' Finished counting, now!';
  frmMain.Caption      := 'Countdown';
  application.Title    := 'Countdown';

  if chckBxCountdownSound.Checked then begin      //  only play sound if checked
    doPlaySound(countdownSoundName);
    chckBxCountdownSound.Checked := false;
    ChckBxCountdownSoundChange(Sender);           //  now box is unchecked, call change procedure
  end;

  if chckBxCountdownReminder.Checked then begin   //  only display reminder if checked
    popupTitle[1]    := 'Countdown';
    popupMessages[1] := EdtCountdownReminder.Text;
    DisplayMessage;
    chckBxCountdownReminder.Checked := false;
    chckBxCountdownReminderChange(Sender);        //  now box is unchecked, call change procedure
  end;

  if chckBxCountdownEvent.Checked then begin      //  only do event if checked
    btnCountdownShutdownAbort.Visible := true;
    doSystemEvent(CmbBxCountdownEvent.ItemIndex);
    chckBxCountdownEvent.Checked := false;
    chckBxCountdownEventChange(Sender);          //  now box is unchecked, call change procedure

    if TrayIcon.Visible then begin               //  if running in the system tray,
      ppMnItmShowClick(Sender);                  //  select the reminder tab and
      PageControl1.TabIndex := 1;                //  display main application so
    end;                                         //  the abort button can be used.
  end;

  if chckBxCountdownCommand.Checked then begin   //  only do command if checked
    doCommandEvent(EdtCountdownCommand.Text);
    chckBxCountdownCommand.Checked := false;
    chckBxCountdownCommandChange(Sender);        //  now box is unchecked, call change procedure
  end;

  //  reset the noOfTicks, so we start the timer again without changing the time.
  //  should be okay, already validated [if time is changed will be revalidated]
  countdownTicks := SpnEdtCountdown.Value * 60;

end;


procedure TfrmMain.btnCountdownShutdownAbortClick(Sender: TObject);
{  button only visible during delay prior to a system shutdown/reboot.
   Allows user to abort action.
   Also tidies up application = a bit messy i'm afraid.
}
begin

  abortSystemEvent;

  PopupNotifier1.Visible := false ;

  btnCountdownShutdownAbort.Visible := false;
  CmbBxCountdownEvent.Visible       := false;
  chckBxCountdownEvent.Checked      := false;
end;

procedure TfrmMain.CountdownTimerTimer(Sender: TObject);
{ tick of countdown timer - called every 1 second.
}
var
  minutes : integer;
  seconds : integer;
  message : string ;

begin
  countdownTicks := countdownTicks - 1;

  if countdownTicks = 0 then StopCountDown(Sender);

  if countdownTicks < 60 then
    message:= format('00:%2.2d', [countdownTicks])
  else begin
    minutes := countdownTicks div 60;
    seconds := countdownTicks mod 60;
    message := format('%2.2d:%2.2d', [minutes, seconds]);
  end;

  LblCountdownTime.Caption := message;
  application.Title  := message;
  frmMain.Caption    := 'Countdown :: ' + message;
end;


procedure TfrmMain.btnSoundTestClick(Sender: TObject);
{  Called to test the sound file.
}
begin
  doPlaySound(countdownSoundName);
end;

Procedure TfrmMain.ChckBxCountdownSoundChange(Sender: TObject);
{  Called to enable/disable the sound - from a check box.
}
begin
  if chckBxCountdownSound.Checked then begin
    stsBrInfo.Panels.Items[3].Text := 'Sound Enabled';
    EdtCountdownSound.Enabled      := true;
    btnCountdownLoadSound.Enabled  := true;
    btnSoundTest.Enabled           := true;
  end
  else begin
    stsBrInfo.Panels.Items[3].Text := 'Sound Disabled';
    EdtCountdownSound.Enabled      := false;
    btnCountdownLoadSound.Enabled  := false;
    btnSoundTest.Enabled           := false;
  end;
end;

procedure TfrmMain.chckBxCountdownReminderChange(Sender: TObject);
{  enable or disable reminders.
}
begin
  if chckBxCountdownReminder.Checked then begin
    stsBrInfo.Panels.Items[3].Text := 'Reminder Enabled';
    EdtCountdownReminder.Enabled   := true;
  end
  else begin
    stsBrInfo.Panels.Items[3].Text := 'Reminder Disabled';
    EdtCountdownReminder.Enabled   := false;
  end;
end;

procedure TfrmMain.chckBxCountdownEventChange(Sender: TObject);
{  enable or disable system events.
}
begin
  if chckBxCountdownEvent.Checked then begin
    stsBrInfo.Panels.Items[3].Text := 'System Event Enabled';
    CmbBxCountdownEvent.Enabled    := true;
    CmbBxCountdownEvent.ItemIndex  := 0;
  end
  else begin
    stsBrInfo.Panels.Items[3].Text := 'System Event Disabled';
    CmbBxCountdownEvent.Enabled    := false;
  end;
end;

procedure TfrmMain.chckBxCountdownCommandChange(Sender: TObject);
{  enable or disable commands.
}
begin
  if chckBxCountdownCommand.Checked then begin
    stsBrInfo.Panels.Items[3].Text  := 'Command Enabled';
    btnCountdownLoadCommand.Enabled := true;
    EdtCountdownCommand.Enabled     := true;
  end
  else begin
    stsBrInfo.Panels.Items[3].Text  := 'Command Disabled';
    btnCountdownLoadCommand.Enabled := false;
    EdtCountdownCommand.Enabled     := false;
  end;
end;

// *********************************************************** Timer ***********
procedure TfrmMain.timerTimerTimer(Sender: TObject);
{  is time is enables, this will be the timer tick.
}
VAR
  hh, mm, ss, ms : word;
  timerInterval  : TDateTime;
begin
  timerInterval := timerPaused + (time - timerStart);
  DecodeTime(timerInterval, hh, mm, ss, ms);
  if userOptions.timerMilliSeconds then
    lblTimer.Caption := format('%.2d:%.2d:%.2d:%.2d',[hh, mm, ss, ms])
  else
    lblTimer.Caption := format('%.2d:%.2d:%.2d',[hh, mm, ss])
end;

procedure TfrmMain.btnTimerStartClick(Sender: TObject);
{ called when start button is clicked, can have three modes
      Start  :: Start timer
      Pause  :: Pause timer
      Resume :: Resume a paused timer.
}
begin
  if btnTimerStart.Caption = 'Start' then begin

    if userOptions.timerMilliSeconds then
      timerTimer.Interval := 100;
    timerStart  := time;
    timerPaused := 0;
    btnTimerStop.Enabled  := true;
    timerTimer.Enabled    := true;
    btnTimerClear.Enabled := false;
    btnTimerStart.Caption := 'Pause';
    btnTimerSplit.Enabled := true;
    lblSplitLap.Enabled   := true;
    frmMain.Caption       := 'Timer :: Started';
    stsBrInfo.Panels.Items[3].Text := 'Timer Running';
  end
  else if btnTimerStart.Caption = 'Pause' then begin
    timerPaused := timerPaused + (time - timerStart);
    btnTimerStart.Caption := 'Resume';
    timerTimer.Enabled    := false;
    btnTimerSplit.Enabled := false;
    lblSplitLap.Enabled   := false;
    frmMain.Caption       := 'Timer :: Paused';
    stsBrInfo.Panels.Items[3].Text := 'Timer :: Paused';
  end
  else if btnTimerStart.Caption = 'Resume' then begin
    timerStart  := time;
    btnTimerStart.Caption := 'Pause';
    timerTimer.Enabled    := true;
    btnTimerSplit.Enabled := true;
    lblSplitLap.Enabled   := true;
    frmMain.Caption       := 'Timer :: Started';
    stsBrInfo.Panels.Items[3].Text := 'Timer Running';
  end
end;

procedure TfrmMain.btnTimerStopClick(Sender: TObject);
{  Stop the timer.
}
begin
  btnTimerStop.Enabled  := false;
  timerTimer.Enabled    := false;
  btnTimerSplit.Enabled := false;
  lblSplitLap.Enabled   := false;
  btnTimerClear.Enabled := true;
  btnTimerStart.Caption := 'Start';
  frmMain.Caption       := 'Timer :: Stoped';
  stsBrInfo.Panels.Items[3].Text := 'Timer :: Stoped';
end;

procedure TfrmMain.btnTimerSplitClick(Sender: TObject);
begin
  lblSplitLap.Caption := lblTimer.Caption;
end;

procedure TfrmMain.btnTimerClearClick(Sender: TObject);
{  Reset [clear] the timer.
}
begin
  if userOptions.timerMilliSeconds then  begin
    lblTimer.Caption    := '00:00:00:00';
    lblSplitLap.Caption := '00:00:00:00';
  end
  else begin
    lblTimer.Caption    := '00:00:00';
    lblSplitLap.Caption := '00:00:00';
  end;  //  if userOptions.timerMilliSeconds = 'True' then

    stsBrInfo.Panels.Items[3].Text := '';

    btnTimerSplit.Enabled := false;
    lblSplitLap.Enabled   := false;
end;

// ************************************************************* Event ********
procedure TfrmMain.SpnEdtHourChange(Sender: TObject);
{  will one day be used to validate the hours set.
}
begin
  EventValid
end;

procedure TfrmMain.SpnEdtMinsChange(Sender: TObject);
{  will one day be used to validate the minute set.
}
begin
  EventValid
end;

procedure TfrmMain.DtEdtEventChange(Sender: TObject);
{  will one day be used to validate the date set.
}
begin
  EventValid
end;

procedure TfrmMain.EventValid;
{  only allow the reminder set button to be enabled, if the reminder
   date is in the future.
}
VAR
  EvntDt : TDateTime;
begin
  EvntDt := EncodeDateTime(YearOf(DtEdtEvent.Date),
                           MonthOf(DtEdtEvent.Date),
                           DayOf(DtEdtEvent.Date),
                           SpnEdtHour.Value,
                           SpnEdtMins.Value,
                           0,
                           0);

  if (EvntDt > Now) then
    btnEventSet.Enabled := true
  else
    btnEventSet.Enabled := false;

end;
procedure TfrmMain.btnEventSetClick(Sender: TObject);
{  Set the reminder.
}
begin
  lblEvent.Caption := format('Event set for %.2d:%.2d - %s',
         [SpnEdtHour.Value, SpnEdtMins.Value, DatetoStr(DtEdtEvent.Date)]);
  stsBrInfo.Panels.Items[3].Text := format('Event set for %.2d:%.2d - %s',
         [SpnEdtHour.Value, SpnEdtMins.Value, DatetoStr(DtEdtEvent.Date)]);

  SpnEdtMins.Visible := false;
  SpnEdtHour.Visible := false;
  DtEdtEvent.Visible := false;

  btnEventClear.Enabled := true;
  btnEventSet.Enabled   := false;

  EventTimer.Enabled := true;
end;

procedure TfrmMain.btnEventClearClick(Sender: TObject);
begin
  resetEvent;
end;

procedure TfrmMain.EventTimerTimer(Sender: TObject);
{  if reminders are set, this will be ticking and tested to see if the reminder is due.
}
VAR
  EvntDt : TDateTime;
begin
  EvntDt := EncodeDateTime(YearOf(DtEdtEvent.Date),
                           MonthOf(DtEdtEvent.Date),
                           DayOf(DtEdtEvent.Date),
                           SpnEdtHour.Value,
                           SpnEdtMins.Value,
                           0,
                           0);

  if Now > EvntDt then
    EventTimerStop(Sender);

end;

procedure Tfrmmain.EventTimerStop(Sender: TObject);
{  Called when the rimder date/time is passed - calls any actions required.
}
begin
  EventTimer.Enabled  := false;
  btnEventSet.Enabled := false;

  if ChckBxEventSound.Checked then begin       //  only play sound if checked
    ChckBxEventSound.Checked := false;
    doPlaySound(EventSoundName);
  end;

  if ChckBxEventReminder.Checked then begin    //  only display reminder if checked
    ChckBxEventReminder.Checked := false;
    popupTitle[3]    := 'Reminder';
    popupMessages[3] := EdtEventText.Text;
    DisplayMessage;
  end;

  if ChckBxEventCommand.Checked then begin     //  only execute command if checked
    ChckBxEventCommand.Checked  := false;
    doCommandEvent(EdtEventCommand.Text);
  end;

  if ChckBxEventSystem.Checked then begin      //  only do system event if checked
    ChckBxEventSystem.Checked := false;
    btnEventAbort.Visible     := true;
    doSystemEvent(CmbBxEventSystem.ItemIndex);

    if TrayIcon.Visible then begin               //  if running in the system tray,
      ppMnItmShowClick(Sender);                  //  select the reminder tab and
      PageControl1.TabIndex := 3;                //  display main application so
    end;                                         //  the abort button can be used.
  end;  //  if ChckBxEventSystem.Checked then begin

  resetEvent;
end;

procedure TfrmMain.btnEventAbortClick(Sender: TObject);
{  button only visible during delay prior to a system shutdown/reboot.
   Allows user to abort action.
   Also tidies up application - a bit messy i'm afraid.
}
VAR
  f : Integer;
begin

  abortSystemEvent;

  for f := 1 to 3 do begin      //  clear the message array, but leave time.
    popupTitle[f]    := '';
    popupMessages[f] := '';
  end;

  DisplayMessage;               //  will clear popup if currently displayed.

  btnCountdownShutdownAbort.Visible := false;
  CmbBxEventSystem.Visible       := false;
  ChckBxEventSystem.Checked      := false;
  btnEventAbort.Visible          := false;

  resetEvent;
end;

procedure TfrmMain.resetEvent;
{  performs reminder reset.
}
begin
  lblEvent.Caption := 'Event not set';
  stsBrInfo.Panels.Items[3].Text := '';

  EventTimer.Enabled    := false;
  btnEventClear.Enabled := false;

  DtEdtEvent.Date  := now;
  SpnEdtMins.Value := MinuteOf(time);
  SpnEdtHour.Value := HourOf(time);

  DtEdtEvent.Enabled := true;
  spnEdtHour.Enabled := true;
  spnEdtMins.Enabled := true;

  SpnEdtMins.Visible := true;
  SpnEdtHour.Visible := true;
  DtEdtEvent.Visible := true;

end;

procedure TfrmMain.CmbBxEventActionChange(Sender: TObject);
begin
  if CmbBxEventAction.ItemIndex = 0 then begin  //  Sound chosen
    ChckBxEventSound.Visible  := true;
    EdtEventSound.Visible     := true;
    btnEventrLoadSound.Visible := true;
    btnEventTestSound.Visible := true;
  end
  else begin
    ChckBxEventSound.Visible  := false;
    EdtEventSound.Visible     := false;
    btnEventrLoadSound.Visible := false;
    btnEventTestSound.Visible := false;
  end;

  if CmbBxEventAction.ItemIndex = 1 then begin  //  Reminder chosen
    ChckBxEventReminder.Visible := true;
    EdtEventText.Visible        := true;
  end
  else begin
    ChckBxEventReminder.Visible := false;
    EdtEventText.Visible        := false;
  end;

  if CmbBxEventAction.ItemIndex = 2 then begin  //  System chosen
    ChckBxEventSystem.Visible         := true;
    CmbBxEventSystem.Visible          := true;
    btnCountdownShutdownAbort.Visible := true;
  end
  else begin
    ChckBxEventSystem.Visible         := false;
    CmbBxEventSystem.Visible          := false;
    btnCountdownShutdownAbort.Visible := false;
  end;

  if CmbBxEventAction.ItemIndex = 3 then begin  //  Command chosen
    ChckBxEventCommand.Visible  := true;
    btnEventLoadCommand.Visible := true;
    EdtEventCommand.Visible     := true;
  end
  else begin
    ChckBxEventCommand.Visible  := false;
    btnEventLoadCommand.Visible := false;
    EdtEventCommand.Visible     := false;
  end;
end;

procedure TfrmMain.ChckBxEventSoundChange(Sender: TObject);
begin
  if ChckBxEventSound.Checked then begin
    stsBrInfo.Panels.Items[3].Text := 'Sound Enabled';
    EdtEventSound.Enabled      := true;
    btnEventrLoadSound.Enabled := true;
    btnEventTestSound.Enabled  := true;
  end
  else begin
    stsBrInfo.Panels.Items[3].Text := 'Sound Disabled';
    EdtEventSound.Enabled      := false;
    btnEventrLoadSound.Enabled := false;
    btnEventTestSound.Enabled  := false;
  end;
end;

procedure TfrmMain.ChckBxEventReminderChange(Sender: TObject);
begin
  if ChckBxEventReminder.Checked then begin
    stsBrInfo.Panels.Items[3].Text := 'Reminder Enabled';
    EdtEventText.Enabled := true;
  end
  else begin
    stsBrInfo.Panels.Items[3].Text := 'Reminder Disabled';
    EdtEventText.Enabled := false;
  end;
end;

procedure TfrmMain.ChckBxEventSystemChange(Sender: TObject);
begin
  if ChckBxEventSystem.Checked then begin
    stsBrInfo.Panels.Items[3].Text := 'System Events Enabled';
    CmbBxEventSystem.Enabled := true;
    CmbBxEventSystem.ItemIndex := 0;
  end
  else begin
    stsBrInfo.Panels.Items[3].Text := 'System Events Disabled';
    CmbBxEventSystem.Enabled := false;
  end;
end;

procedure TfrmMain.ChckBxEventCommandChange(Sender: TObject);
begin
  if ChckBxEventCommand.Checked then begin
    stsBrInfo.Panels.Items[3].Text := 'Command Enabled';
    EdtEventCommand.Enabled     := true;
    btnEventLoadCommand.Enabled := true;
  end
  else begin
    stsBrInfo.Panels.Items[3].Text := 'Command Disabled';
    EdtEventCommand.Enabled     := false;
    btnEventLoadCommand.Enabled := false;
  end;
end;

procedure TfrmMain.btnEventLoadCommandClick(Sender: TObject);
{  if the command box is clicked, allow the command file to be loaded.
 }
begin

  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.*';
    InitialDir:= getCurrentDir;
    Title := 'Choose a executable' ;
    if Execute then begin
      EdtEventCommand.Text := (FileName);
      stsBrInfo.Panels.Items[3].Text := Filename + ' Chosen';
    end;    //  if Exectute
    Free;
  end;
end;

procedure TfrmMain.btnEventrLoadSoundClick(Sender: TObject);
{  if the text box is clicked, allow the sound file to be changed.
      MUST BE A .wav FILE.
}
begin

  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.wav';
    InitialDir:= getCurrentDir + '\sounds';
    Title := 'Choose a sound file [.wav]' ;
    if Execute then begin
      if ExtractFileExt(FileName) = '.wav' then begin  //  only allow .wav
        EventSoundName         := FileName;
        EdtCountdownSound.Text := ExtractFileName(FileName);
        stsBrInfo.Panels.Items[3].Text := Filename + ' Chosen'
      end;  //  if ExtractFileExt
    end;    //  if Execute
    Free;
  end;
end;

procedure TfrmMain.btnEventTestSoundClick(Sender: TObject);
begin
  doPlaySound(EventSoundName);
end;

// ************************************************************* Reminder ******

procedure TfrmMain.btnReminderNewClick(Sender: TObject);
{  Loads form, so a new reminder can be input.
}
begin
  frmReminderInput.ShowModal;
  readReminderFile;              //  reread reminder file, to reflect changes - if any.
end;

procedure TfrmMain.readReminderFile;
{  Read the reminder file and passes each line to be parsed, added each
   parsed line to the list box for display.
   File is opened and closed within this procedure.
}
VAR
  readLine : String;
  rmndrData : UKlockUtils.reminderData;
begin
  ChckLstBxReminder.Clear;
  ChckLstBxReminder.Enabled := false;
  noReminder := 0;

  AssignFile(ReminderFile, ReminderData);
  try
    Reset(ReminderFile);

    repeat
      Readln(ReminderFile, readLine);     // Reads the whole line from the reminder file/
      if (readLine[1] <> '-') then begin  // ignore header lines.
        rmndrData := parseReminder(readLine);
        ChckLstBxReminder.Items.Add(rmndrData.message);

        if rmndrData.active then
          ChckLstBxReminder.Checked[noReminder] := True;

        noReminder += 1;

      end;
    until(EOF(ReminderFile)); // EOF(End Of File) The the program
  except
    ShowMessage('  ERROR: Cannot open Reminder File  :: '  + IntToStr(IOResult));
  end;

  if noReminder = 0 then
    ChckLstBxReminder.Items.Add('No Reminders on file')
  else
    ChckLstBxReminder.Enabled := True;

  CloseFile(ReminderFile);
end;

// *********************************************************** Menu procs ******

procedure TfrmMain.mnuItmOptionsClick(Sender: TObject);
{  if clicked, call the option screen, reapply options after.
}
VAR
  frmTop: integer;
  frmLeft:integer;
  res: integer;         //  return value from option screen.
begin
  if userOptions.screenSave then begin
    userOptions.formTop := frmMain.Top;
    userOptions.formLeft := frmMain.Left;
  end
  else begin
    frmTop := frmMain.Top;    //  return to same place, after option screen.
    frmLeft := frmMain.Left;
  end;

  res := frmOptions.ShowModal;

  if res = 1 then             //  1 = Ok button press, 2 = cancel button pressed.
    SetDefaults;

  if NOT userOptions.screenSave then begin  //  not done in SetDefaults
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

// ********************************************************* ButtonPannel ******



procedure TfrmMain.HelpButtonClick(Sender: TObject);
VAR
  helpText : String ;
begin

  case PageControl1.TabIndex of
    0 : helpText := 'Fuzzy Time' + LineEnding + 'Displays current time in a number of different formats';
    1 : helpText := 'Countdown'  + LineEnding + 'Select a time to countdown and an event to be triggered';
    2 : helpText := 'Timer'      + LineEnding + 'Simple timer with pause and split time function';
    3 : helpText := 'Event'      + LineEnding + 'Set a time and/or date to be reminded of';
    4 : helpText := 'Reminder'   + LineEnding + 'Set a time and/or date to be reminded of';
  end ;

  ShowMessage(helpText);
end;

procedure TfrmMain.OKButtonClick(Sender: TObject);
{  if clicked will hide the main form and display the tray icon.
}
begin
  TrayIcon.Visible := true;
  TrayIcon.Show;

  frmMain.Visible := false;
end;

// ******************************************************* pop menu ************

procedure TfrmMain.ppMnItmShowClick(Sender: TObject);
{  on menu show, hides the tray icon and redisplays the main form.
   unchecks time menu item.
}
begin
  TrayIcon.Visible := false;
  TrayIcon.Hide;

  frmMain.Visible := true;

  if ppMnItmTime.Checked then
    ppMnItmTime.Checked:= false;
end;


procedure TfrmMain.ppMnItmTimeClick(Sender: TObject);
{  on menu click, toggle the checked status.
   if checked becomes false, clear time message and tries to kill pop-up notifier.
}
begin
  if ppMnItmTime.Checked then begin
    ppMnItmTime.Checked:= false;
    popupTitle[1]    := '';
    popupMessages[0] := '';
    DisplayMessage;
  end
  else
    ppMnItmTime.Checked:= true;
end;

procedure TfrmMain.TrayIconDblClick(Sender: TObject);
{  double clicking the tray icon, will clear all messages and kill the pop-up notifier.
}
VAR
  f : Integer;
begin
    for f := 0 to 3 do begin
      popupTitle[f]    := '';
      popupMessages[f] := '';
    end;

    ppMnItmTime.Checked:= false;
    DisplayMessage;
end;

procedure TfrmMain.PopupNotifier1Close(Sender: TObject;var CloseAction: TCloseAction);
{  if the pop-up is closed manually, assume closed by user after countdown
   of time - so clear these messages.
   NB  but leave time, just in case.
}
VAR
  f : Integer;
begin
    for f := 1 to 3 do begin
      popupTitle[f]    := '';
      popupMessages[f] := '';
    end;

    CloseAction := caFree;
end;


// *****************************************************************************

End.

