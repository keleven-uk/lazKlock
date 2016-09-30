unit Uklock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, Buttons, StdCtrls, Spin, PopupNotifier, EditBtn, ButtonPanel,
  UAbout, Uhelp, UOptions, uLicense, UFuzzyTime, dateutils,
  UKlockUtils;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnCountdownStart: TButton;
    btnCountdownStop: TButton;
    btnCountdownLoadSound: TButton;
    btnReminderAbort: TButton;
    btnTimerStart: TButton;
    btnTimerStop: TButton;
    btnTimerClear: TButton;
    btnReminderSet: TButton;
    btnSoundTest: TButton;
    btnReminderClear: TButton;
    btnTimerSplit: TButton;
    btnCountdownShutdownAbort: TButton;
    btnCountdownLoadCommand: TButton;
    btnReminderLoadSound: TButton;
    btnReminderTestSound: TButton;
    btnReminderLoadCommand: TButton;
    ButtonPanel1: TButtonPanel;
    ChckBxCountdownSound: TCheckBox;
    chckBxCountdownEvent: TCheckBox;
    chckBxCountdownReminder: TCheckBox;
    chckBxCountdownCommand: TCheckBox;
    ChckBxReminderSound: TCheckBox;
    ChckBxReminderReminder: TCheckBox;
    ChckBxReminderSystem: TCheckBox;
    ChckBxReminderCommand: TCheckBox;
    CmbBxTime: TComboBox;
    CmbBxCountdownAction: TComboBox;
    CmbBxCountdownEvent: TComboBox;
    CmbBxReminderAction: TComboBox;
    CmbBxReminderSystem: TComboBox;
    DtEdtReminder: TDateEdit;
    EdtReminderCommand: TEdit;
    EdtReminderText: TEdit;
    EdtReminderSound: TEdit;
    EdtCountdownCommand: TEdit;
    EdtCountdownReminder: TEdit;
    EdtCountdownSound: TEdit;
    lblRadix: TLabel;
    lblSplitLap: TLabel;
    lblfuzzy: TLabel;
    lblReminder: TLabel;
    lblTimer: TLabel;
    LblCountdownTime: TLabel;
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
    PopupNotifier1: TPopupNotifier;
    SpnEdtTimeBase: TSpinEdit;
    SpnEdtHour: TSpinEdit;
    SpnEdtMins: TSpinEdit;
    SpnEdtCountdown: TSpinEdit;
    stsBrInfo: TStatusBar;
    TbShtFuzzy: TTabSheet;
    TbShtCountdown: TTabSheet;
    TbShtTimer: TTabSheet;
    TbShtRimder: TTabSheet;
    mainTimer: TTimer;
    CountdownTimer: TTimer;
    ReminderTimer: TTimer;
    timerTimer: TTimer;
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
    procedure CmbBxReminderActionChange(Sender: TObject);
    procedure CmbBxTimeChange(Sender: TObject);
    procedure CmbBxCountdownActionChange(Sender: TObject);
    procedure CountdownTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LblCountdownTimeClick(Sender: TObject);
    procedure mnuItmAboutClick(Sender: TObject);
    procedure mnuItmExitClick(Sender: TObject);
    procedure mnuItmHelpClick(Sender: TObject);
    procedure mnuItmLicenseClick(Sender: TObject);
    procedure mnuItmOptionsClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure mainTimerTimer(Sender: TObject);
    procedure ReminderTimerTimer(Sender: TObject);
    procedure SpnEdtCountdownChange(Sender: TObject);
    procedure SpnEdtHourChange(Sender: TObject);
    procedure SpnEdtMinsChange(Sender: TObject);
    procedure SpnEdtTimeBaseChange(Sender: TObject);
    procedure timerTimerTimer(Sender: TObject);
  private
    procedure DisplayMessage(title : string ; message : string);
    procedure StopCountDown(Sender: TObject);
    procedure SetDefaults;
    procedure resetReminder;
    procedure ReminderTimerStop(Sender: TObject);
  public
    countdownTicks     : integer;
    countdownSoundName : String;
    ReminderSoundName  : String;
    timerStart         : TDateTime;
    timerPaused        : TdateTime;

    ft : FuzzyTime;

  end; 

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

// *********************************************************** Global **********
procedure TfrmMain.FormCreate(Sender: TObject);
{  Called at start - sets up fuzzy time and default sound file     }
begin
  countdownSoundName     := getCurrentDir + '\sounds\alarm-fatal.wav';  // default to sound file
  EdtCountdownSound.Text := ExtractFileName(countdownSoundName);        //  in current working directory.

  ReminderSoundName     := getCurrentDir + '\sounds\alarm-fatal.wav';  // default to sound file
  EdtReminderSound.Text := ExtractFileName(countdownSoundName);        //  in current working directory.

  ft := FuzzyTime.Create;

  DtEdtReminder.Date := now;
  SpnEdtMins.Value   := MinuteOf(time);
  SpnEdtHour.Value   := HourOf(time);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  SetDefaults;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if OptionsRec.ScreenSave then begin
    OptionsRec.setScreenTop(frmMain.Top);
    OptionsRec.setScreenLeft(frmMain.Left);
    frmOptions.writeIniFile;
  end;
end;

procedure TfrmMain.SetDefaults;
{  called to set defaults on startup.
   Set things that can be changed in the options screen, to the values in the options screen.      }
begin
  lblFuzzy.Font              := getTextFont(OptionsRec.FuzzyTextFont, OptionsRec.GlobalTextFont);
  lblfuzzy.Font.Size         := 18;
  SpnEdtCountdown.Font.Size  := 8;
  lblCountDownTime.Font      := getTextFont(OptionsRec.CountDownTextFont, OptionsRec.GlobalTextFont);
  LblCountdownTime.Font.Size := 26;
  SpnEdtCountdown.Font.Size  := 12;
  lblTimer.Font              := getTextFont(OptionsRec.TimerTextFont, OptionsRec.GlobalTextFont);
  lblTimer.Font.Size         := 26;
  lblSplitLap.Font           := getTextFont(OptionsRec.TimerTextFont, OptionsRec.GlobalTextFont);
  lblSplitLap.Font.Size      := 26;
  lblReminder.Font           := getTextFont(OptionsRec.ReminderTextFont, OptionsRec.GlobalTextFont);
  lblReminder.Font.Size      := 18;

  PageControl1.TabIndex := OptionsRec.DefaultTab;
  CmbBxTime.ItemIndex   := OptionsRec.DefaultTime;

  ft.displayFuzzy := OptionsRec.DefaultTime;
  stsBrInfo.Panels.Items[2].Text := CmbBxTime.Items.Strings[CmbBxTime.ItemIndex] + ' time' ;

  ft.FuzzyBase := 2;

  if OptionsRec.ScreenSave then begin
    frmMain.Left := OptionsRec.ScreenLeft;
    frmMain.Top  := OptionsRec.ScreenTop;
  end;

  if OptionsRec.NetTimeSeconds then
    mainTimer.Interval := 1
  else
    mainTimer.Interval := 1000;

end;

procedure TfrmMain.DisplayMessage(title : string ; message : string);
{  display a message as a popup message.                                       }
begin
  if PopupNotifier1.Visible = false then begin
    PopupNotifier1.ShowAtPos(100,100) ;
    PopupNotifier1.Title   := title;
    PopupNotifier1.Text    := message;
    PopupNotifier1.Visible := true ;
  end
  else begin
    PopupNotifier1.Visible := false ;
    PopupNotifier1.Title   := PopupNotifier1.Title + ' :: ' + title;
    PopupNotifier1.Text    := PopupNotifier1.Text  + LineEnding + message;
    PopupNotifier1.Visible := true ;
  end;
end;



procedure TfrmMain.PageControl1Change(Sender: TObject);
begin

  case PageControl1.TabIndex of
    0 : begin                     //  fuzzy page
      stsBrInfo.Panels.Items[2].Text := '';
      stsBrInfo.Panels.Items[3].Text := '';
      stsBrInfo.Panels.Items[2].Text := CmbBxTime.Items.Strings[CmbBxTime.ItemIndex] + ' time' ;
    end;
    1 : begin                     //  countdown page
      if CountdownTimer.Enabled = false then
        stsBrInfo.Panels.Items[3].Text := ''
      else
        stsBrInfo.Panels.Items[3].Text := format(' Counting down from %2.d minute[s]', [SpnEdtCountdown.Value]);
    end;
    2 : begin                     //  timer page
      stsBrInfo.Panels.Items[2].Text := '';
      stsBrInfo.Panels.Items[3].Text := '';

      if timerTimer.Enabled = false then begin

      if btnTimerStart.Caption = 'Resume' then
        stsBrInfo.Panels.Items[3].Text := 'Timer :: Paused';

        if btnTimerStart.Caption = 'Start' then begin
          stsBrInfo.Panels.Items[2].Text := '';
          if OptionsRec.TimerMilliSeconds then begin
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
    3 : begin                    //  reminder page
      stsBrInfo.Panels.Items[2].Text := '';
      stsBrInfo.Panels.Items[3].Text := '';
      if ReminderTimer.Enabled = false then begin   // only set display to current
        DtEdtReminder.Date := now;
        SpnEdtMins.Value   := MinuteOf(time);
        SpnEdtHour.Value   := HourOf(time);
      end
      else begin
        stsBrInfo.Panels.Items[3].Text := format('Reminder set for %.2d:%.2d - %s',
             [SpnEdtHour.Value, SpnEdtMins.Value, DatetoStr(DtEdtReminder.Date)]);
      end;  //  if btnReminderSet.Enabled
    end;
  end;

end;

procedure TfrmMain.mainTimerTimer(Sender: TObject);
begin
  stsBrInfo.Panels.Items[0].Text:= TimeToStr(Time) ;
  stsBrInfo.Panels.Items[1].Text:= FormatDateTime('DD MMM YYYY', Now);

  lblfuzzy.Caption := ft.getTime;
end;


// *********************************************************** Fuzzy Time ******
procedure TfrmMain.CmbBxTimeChange(Sender: TObject);
begin

  stsBrInfo.Panels.Items[2].Text := CmbBxTime.Items.Strings[CmbBxTime.ItemIndex]
                                                                     + ' time' ;
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

  if CmbBxCountdownAction.ItemIndex = 1 then begin  //  System event chosen
    chckBxCountdownReminder.Visible := true;
    EdtCountdownReminder.Visible    := true;
  end
  else begin
    chckBxCountdownReminder.Visible := false;
    EdtCountdownReminder.Visible    := false;
  end;

  if CmbBxCountdownAction.ItemIndex = 2 then begin  //  reminder chosen
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
      Resume :: Resume a paused countdown.                            }
VAR
  val : integer;

begin
  if btnCountdownStart.Caption = 'Start' then begin
    btnCountdownStop.Enabled  := true;
    CountdownTimer.Enabled    := True;
    SpnEdtCountdown.Enabled   := false;
    VAL := CountdownTicks div 60;           //  in case the satus message has changed
    stsBrInfo.Panels.Items[3].Text := format(' Counting down from %d minute[s]', [val]);

    btnCountdownStart.Caption := 'Pause';

    if (chckBxCountdownCommand.Checked) and (EdtCountdownCommand.Text = '') then begin
      DisplayMessage('CountDown', 'er, need to give Klock a command.');
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
      MUST BE A .wav FILE.                                           }
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
    end;    //  if Exectute
    Free;
  end;
end;

procedure TfrmMain.btnCountdownLoadCommandClick(Sender: TObject);
{  if the comand box is clicked, allow the command file to be loaded.                            }
begin

  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.*';
    InitialDir:= getCurrentDir;
    Title := 'Choose a executable' ;
    if Execute then begin
      EdtCountdownCommand.Text := (FileName);
      stsBrInfo.Panels.Items[3].Text := Filename + ' Chosen'
    end;    //  if Exectute
    Free;
  end;
end;


procedure TfrmMain.btnCountdownStopClick(Sender: TObject);
{  Called when the countdown stop button is clicked                }
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
{    called when the time is entered - only allow 1 - 90 minutes  }
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
{    Called when the timer has finished.  }
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
    ChckBxCountdownSoundChange(Sender);           //  now box in un-checked, call change procedure
  end;

  if chckBxCountdownReminder.Checked then begin   //  only display reminder if checked
    DisplayMessage('CountDown', EdtCountdownReminder.Text);
    chckBxCountdownReminder.Checked := false;
    chckBxCountdownReminderChange(Sender);        //  now box in un-checked, call change procedure
  end;

  if chckBxCountdownEvent.Checked then begin      //  only do event if checked
    btnCountdownShutdownAbort.Visible := true;
    doSystemEvent(CmbBxCountdownEvent.ItemIndex);
    chckBxCountdownEvent.Checked := false;
    chckBxCountdownEventChange(Sender);          //  now box in un-checked, call change procedure
  end;

  if chckBxCountdownCommand.Checked then begin   //  only do command if checked
    doCommandEvent(EdtCountdownCommand.Text);
    chckBxCountdownCommand.Checked := false;
    chckBxCountdownCommandChange(Sender);        //  now box in un-checked, call change procedure
  end;

  //  reset the noOfTicks, so we start the timer again without changing the time.
  //  should be okay, already validated [if time is changed will be re-validated]
  countdownTicks := SpnEdtCountdown.Value * 60;

end;


procedure TfrmMain.btnCountdownShutdownAbortClick(Sender: TObject);
{  button only visbale during delay prior to a system shutdown/reboot.
   Allows user to abort action.
   Also tidies up application = a bit messy i'm afraid.                        }
begin

  abortSystemEvent;

  PopupNotifier1.Visible := false ;

  btnCountdownShutdownAbort.Visible := false;
  CmbBxCountdownEvent.Visible       := false;
  chckBxCountdownEvent.Checked      := false;
end;

procedure TfrmMain.CountdownTimerTimer(Sender: TObject);
{ tick of countdown timer - called every 1 second        }
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
{  Called to test the sound file                                }
begin
  doPlaySound(countdownSoundName);
end;

Procedure TfrmMain.ChckBxCountdownSoundChange(Sender: TObject);
{  Called to enable/disable the sound - from a check box           }
begin
  if chckBxCountdownSound.Checked then begin
    stsBrInfo.Panels.Items[2].Text := 'Sound Enabled';
    EdtCountdownSound.Visible      := true;
    EdtCountdownSound.Enabled      := true;
    btnCountdownLoadSound.Visible  := true;
    btnCountdownLoadSound.Enabled  := true;
    btnSoundTest.Visible           := true;
    btnSoundTest.Enabled           := true;
  end
  else begin
    stsBrInfo.Panels.Items[2].Text := 'Sound Disabled';
    EdtCountdownSound.Visible      := false;
    EdtCountdownSound.Enabled      := false;
    btnCountdownLoadSound.Visible  := false;
    btnCountdownLoadSound.Enabled  := false;
    btnSoundTest.Visible           := false;
    btnSoundTest.Enabled           := false;
  end;
end;

procedure TfrmMain.chckBxCountdownReminderChange(Sender: TObject);
begin
  if chckBxCountdownReminder.Checked then begin
    stsBrInfo.Panels.Items[2].Text := 'Reminder Enabled';
    EdtCountdownReminder.Visible := true;
    EdtCountdownReminder.Enabled := true;
  end
  else begin
    stsBrInfo.Panels.Items[2].Text := 'Reminder Disabled';
    EdtCountdownReminder.Visible := false;
    EdtCountdownReminder.Enabled := false;
  end;
end;

procedure TfrmMain.chckBxCountdownEventChange(Sender: TObject);
begin
  if chckBxCountdownEvent.Checked then begin
    stsBrInfo.Panels.Items[2].Text := 'System Event Enabled';
    CmbBxCountdownEvent.Visible := true;
    CmbBxCountdownEvent.Enabled := true;
  end
  else begin
    stsBrInfo.Panels.Items[2].Text := 'System Event Disabled';
    CmbBxCountdownEvent.Visible := false;
    CmbBxCountdownEvent.Enabled := true;
  end;
end;

procedure TfrmMain.chckBxCountdownCommandChange(Sender: TObject);
begin
  if chckBxCountdownCommand.Checked then begin
    stsBrInfo.Panels.Items[2].Text := 'Command Enabled';
    btnCountdownLoadCommand.Visible := true;
    btnCountdownLoadCommand.Enabled := true;
    EdtCountdownCommand.Visible     := true;
    EdtCountdownCommand.Enabled     := true;
  end
  else begin
    stsBrInfo.Panels.Items[2].Text := 'Command Disabled';
    btnCountdownLoadCommand.Visible := false;
    btnCountdownLoadCommand.Enabled := false;
    EdtCountdownCommand.Visible     := false;
    EdtCountdownCommand.Enabled     := false;
  end;
end;

// *********************************************************** Timer ***********
procedure TfrmMain.timerTimerTimer(Sender: TObject);
VAR
  hh, mm, ss, ms : word;
  timerInterval  : TDateTime;
begin
  timerInterval := timerPaused + (time - timerStart);
  DecodeTime(timerInterval, hh, mm, ss, ms);
  if OptionsRec.TimerMilliSeconds then
    lblTimer.Caption := format('%.2d:%.2d:%.2d:%.2d',[hh, mm, ss, ms])
  else
    lblTimer.Caption := format('%.2d:%.2d:%.2d',[hh, mm, ss])
end;



procedure TfrmMain.btnTimerStartClick(Sender: TObject);
{ called when start button is clicked, can have three modes
      Start  :: Start timer
      Pause  :: Pause timer
      Resume :: Resume a paused timer.                            }
begin
  if btnTimerStart.Caption = 'Start' then begin

    if OptionsRec.TimerMilliSeconds then
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
begin
  if OptionsRec.TimerMilliSeconds then begin
    lblTimer.Caption    := '00:00:00:00';
    lblSplitLap.Caption := '00:00:00:00';
  end
  else begin
    lblTimer.Caption    := '00:00:00';
    lblSplitLap.Caption := '00:00:00';
  end;  //  if OptionsRec.TimerMilliSeconds

    stsBrInfo.Panels.Items[3].Text := '';

    btnTimerSplit.Enabled := false;
    lblSplitLap.Enabled   := false;
end;

// *********************************************************** Reminder ********
procedure TfrmMain.SpnEdtHourChange(Sender: TObject);
begin
  btnReminderSet.Enabled := true;
end;

procedure TfrmMain.SpnEdtMinsChange(Sender: TObject);
begin
  btnReminderSet.Enabled := true;
end;

procedure TfrmMain.btnReminderSetClick(Sender: TObject);
VAR
  RmndDt : TDateTime;
begin
  SpnEdtMins.Visible    := false;
  SpnEdtHour.Visible    := false;
  DtEdtReminder.Visible := false;

  RmndDt := EncodeDateTime(YearOf(DtEdtReminder.Date),
                           MonthOf(DtEdtReminder.Date),
                           DayOf(DtEdtReminder.Date),
                           SpnEdtHour.Value,
                           SpnEdtMins.Value,
                           0,
                           0);

  if RmndDt > Now then begin  // so reminder's can not be set in the past.
    lblReminder.Caption := format('Reminder set for %.2d:%.2d - %s',
         [SpnEdtHour.Value, SpnEdtMins.Value, DatetoStr(DtEdtReminder.Date)]);
    stsBrInfo.Panels.Items[3].Text := format('Reminder set for %.2d:%.2d - %s',
         [SpnEdtHour.Value, SpnEdtMins.Value, DatetoStr(DtEdtReminder.Date)]);

    ReminderTimer.Enabled := true;
    DtEdtReminder.Enabled := false;
    spnEdtHour.Enabled    := false;
    spnEdtMins.Enabled    := false;

  end
  else begin
    lblReminder.Caption := 'Cannot set reminder in the past';
    DtEdtReminder.Date  := now;
    SpnEdtMins.Value    := MinuteOf(time);
    SpnEdtHour.Value    := HourOf(time);
  end;

  btnReminderClear.Enabled := true;
  btnReminderSet.Enabled   := false;
end;

procedure TfrmMain.btnReminderClearClick(Sender: TObject);
begin
  resetReminder;
end;

procedure TfrmMain.ReminderTimerTimer(Sender: TObject);
VAR
  RmndDt : TDateTime;
begin
  RmndDt := EncodeDateTime(YearOf(DtEdtReminder.Date),
                           MonthOf(DtEdtReminder.Date),
                           DayOf(DtEdtReminder.Date),
                           SpnEdtHour.Value,
                           SpnEdtMins.Value,
                           0,
                           0);

  if Now > RmndDt then
    ReminderTimerStop(Sender);

end;

procedure Tfrmmain.ReminderTimerStop(Sender: TObject);
{  Called when the rimder date/time is passed - calles any actions required.                        }
begin
  ReminderTimer.Enabled  := false;
  btnReminderSet.Enabled := false;

  if ChckBxReminderSound.Checked then begin    //  only play sound if checked
   doPlaySound(ReminderSoundName);
   ChckBxReminderSound.Checked := false;
   ChckBxReminderSoundChange(Sender);
  end;

  if ChckBxReminderReminder.Checked then begin
    DisplayMessage('Reminder', EdtReminderText.Text);
    ChckBxReminderReminderChange(Sender);
  end;

  if ChckBxReminderCommand.Checked then begin
    doCommandEvent(EdtReminderCommand.Text);
    ChckBxReminderCommand.Checked  := false;
    ChckBxReminderCommandChange(Sender);
  end;

  if ChckBxReminderSystem.Checked then begin
    btnReminderAbort.Visible := true;
    doSystemEvent(CmbBxReminderSystem.ItemIndex);
    ChckBxReminderSystem.Checked := false;
    ChckBxReminderSystemChange(Sender);
  end;

  resetReminder;
end;

procedure TfrmMain.btnReminderAbortClick(Sender: TObject);
{  button only visbale during delay prior to a system shutdown/reboot.
   Allows user to abort action.
   Also tidies up application = a bit messy i'm afraid.                        }
begin

  abortSystemEvent;

  PopupNotifier1.Visible := false ;

  btnCountdownShutdownAbort.Visible := false;
  CmbBxReminderSystem.Visible       := false;
  ChckBxReminderSystem.Checked      := false;

  resetReminder;
end;

procedure TfrmMain.resetReminder;
begin
  lblReminder.Caption := 'Reminder not set';
  stsBrInfo.Panels.Items[3].Text := '';

  ReminderTimer.Enabled    := false;
  btnReminderSet.Enabled   := true;
  DtEdtReminder.Enabled    := true;
  spnEdtHour.Enabled       := true;
  spnEdtMins.Enabled       := true;
  btnReminderClear.Enabled := false;

  SpnEdtMins.Visible    := true;
  SpnEdtHour.Visible    := true;
  DtEdtReminder.Visible := true;

  DtEdtReminder.Date := now;
  SpnEdtMins.Value   := MinuteOf(time);
  SpnEdtHour.Value   := HourOf(time);
end;

procedure TfrmMain.CmbBxReminderActionChange(Sender: TObject);
begin
  if CmbBxReminderAction.ItemIndex = 0 then begin  //  Sound chosen
    chckBxReminderSound.Visible  := true;
    EdtReminderSound.Visible     := true;
    btnReminderLoadSound.Visible := true;
    btnReminderTestSound.Visible := true;
  end
  else begin
    chckBxReminderSound.Visible  := false;
    EdtReminderSound.Visible     := false;
    btnReminderLoadSound.Visible := false;
    btnReminderTestSound.Visible := false;
  end;


  if CmbBxReminderAction.ItemIndex = 1 then begin  //  Reminder chosen
    ChckBxReminderReminder.Visible := true;
    EdtReminderText.Visible        := true;
  end
  else begin
    ChckBxReminderReminder.Visible := false;
    EdtReminderText.Visible        := false;
  end;

  if CmbBxReminderAction.ItemIndex = 2 then begin  //  System chosen
    ChckBxReminderSystem.Visible      := true;
    CmbBxReminderSystem.Visible       := true;
    btnCountdownShutdownAbort.Visible := true;
  end
  else begin
    ChckBxReminderSystem.Visible      := false;
    CmbBxReminderSystem.Visible       := false;
    btnCountdownShutdownAbort.Visible := false;
  end;

  if CmbBxReminderAction.ItemIndex = 3 then begin  //  Command chosen
    ChckBxReminderCommand.Visible  := true;
    btnReminderLoadCommand.Visible := true;
    EdtReminderCommand.Visible     := true;
  end
  else begin
    ChckBxReminderCommand.Visible  := false;
    btnReminderLoadCommand.Visible := false;
    EdtReminderCommand.Visible     := false;
  end;
end;

procedure TfrmMain.ChckBxReminderSoundChange(Sender: TObject);
begin
  if ChckBxReminderSound.Checked then begin
    stsBrInfo.Panels.Items[2].Text := 'Sound Enabled';
    EdtReminderSound.Visible     := true;
    EdtReminderSound.Enabled     := true;
    btnReminderLoadSound.Visible := true;
    btnReminderLoadSound.Enabled := true;
    btnReminderTestSound.Visible := true;
    btnReminderTestSound.Enabled := true;
  end
  else begin
    stsBrInfo.Panels.Items[2].Text := 'Sound Disabled';
    EdtReminderSound.Visible     := false;
    EdtReminderSound.Enabled     := false;
    btnReminderLoadSound.Visible := false;
    btnReminderLoadSound.Enabled := false;
    btnReminderTestSound.Visible := false;
    btnReminderTestSound.Enabled := false;
  end;
end;

procedure TfrmMain.ChckBxReminderReminderChange(Sender: TObject);
begin
  if ChckBxReminderReminder.Checked then begin
    stsBrInfo.Panels.Items[2].Text := 'Reminder Enabled';
    EdtReminderText.Visible := true;
    EdtReminderText.Enabled := true;
  end
  else begin
    stsBrInfo.Panels.Items[2].Text := 'Reminder Disabled';
    EdtReminderText.Visible := false;
    EdtReminderText.Enabled := false;
  end;
end;

procedure TfrmMain.ChckBxReminderSystemChange(Sender: TObject);
begin
  if ChckBxReminderSystem.Checked then begin
    stsBrInfo.Panels.Items[2].Text := 'System Events Enabled';
    CmbBxReminderSystem.Visible := true;
    CmbBxReminderSystem.Enabled := true;
  end
  else begin
    stsBrInfo.Panels.Items[2].Text := 'System Events Disabled';
    CmbBxReminderSystem.Visible := false;
    CmbBxReminderSystem.Enabled := true;
  end;
end;

procedure TfrmMain.ChckBxReminderCommandChange(Sender: TObject);
begin
  if ChckBxReminderCommand.Checked then begin
    stsBrInfo.Panels.Items[2].Text := 'Command Enabled';
    EdtReminderCommand.Visible     := true;
    EdtReminderCommand.Enabled     := true;
    btnReminderLoadCommand.Visible := true;
    btnReminderLoadCommand.Enabled := true;
  end
  else begin
    stsBrInfo.Panels.Items[2].Text := 'Command Disabled';
    EdtReminderCommand.Visible     := false;
    EdtReminderCommand.Enabled     := false;
    btnReminderLoadCommand.Visible := false;
    btnReminderLoadCommand.Enabled := false;
  end;
end;

procedure TfrmMain.btnReminderLoadCommandClick(Sender: TObject);
  {  if the comand box is clicked, allow the command file to be loaded.                            }
begin

  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.*';
    InitialDir:= getCurrentDir;
    Title := 'Choose a executable' ;
    if Execute then begin
      EdtReminderCommand.Text := (FileName);
      stsBrInfo.Panels.Items[3].Text := Filename + ' Chosen';
    end;    //  if Exectute
    Free;
  end;
end;

procedure TfrmMain.btnReminderLoadSoundClick(Sender: TObject);
{  if the text box is clicked, allow the sound file to be changed.
      MUST BE A .wav FILE.                                           }
begin

  with TOpenDialog.Create(Self) do
  begin
    Filter := '*.wav';
    InitialDir:= getCurrentDir + '\sounds';
    Title := 'Choose a sound file [.wav]' ;
    if Execute then begin
      if ExtractFileExt(FileName) = '.wav' then begin  //  only allow .wav
        ReminderSoundName      := FileName;
        EdtCountdownSound.Text := ExtractFileName(FileName);
        stsBrInfo.Panels.Items[3].Text := Filename + ' Chosen'
      end;  //  if ExtractFileExt
    end;    //  if Exectute
    Free;
  end;
end;

procedure TfrmMain.btnReminderTestSoundClick(Sender: TObject);
begin
  doPlaySound(ReminderSoundName);
end;

// *********************************************************** Menu procs ******

procedure TfrmMain.mnuItmAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.mnuItmExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuItmHelpClick(Sender: TObject);
begin
  frmHelp.ShowModal;
end;

procedure TfrmMain.mnuItmLicenseClick(Sender: TObject);
begin
  frmLicense.ShowModal;
end;

procedure TfrmMain.mnuItmOptionsClick(Sender: TObject);
begin
  OptionsRec.ScreenLeft := frmMain.Left;   //  return to same place, after option screen.
  OptionsRec.ScreenTop  := frmMain.Top;

  frmOptions.ShowModal;
  SetDefaults;
end;
procedure TfrmMain.HelpButtonClick(Sender: TObject);
VAR
  helpText : String ;
begin

  case PageControl1.TabIndex of
    0 : helpText := 'Fuzzy Time' + LineEnding + 'Displays current time in a number of defferent formats';
    1 : helpText := 'Countdown'  + LineEnding + 'Select a time to countdown and an event to be triggered';
    2 : helpText := 'Timer'      + LineEnding + 'Simple timer with pasue and split time function';
    3 : helpText := 'Reminder'   + LineEnding + 'Set a time and/or date to be reminded of';
  end ;

  ShowMessage(helpText);
end;

 procedure TfrmMain.LblCountdownTimeClick(Sender: TObject);
 begin

 end;


// *****************************************************************************

End.

