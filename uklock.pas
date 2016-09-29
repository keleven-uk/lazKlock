unit Uklock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, Buttons, StdCtrls, Spin, PopupNotifier, EditBtn, ButtonPanel,
  UAbout, Uhelp, UOptions, uLicense, MMSystem, UFuzzyTime, dateutils;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnCountdownStart: TButton;
    btnCountdownStop: TButton;
    btnCountdownLoadSound: TButton;
    btnTimerStart: TButton;
    btnTimerStop: TButton;
    btnTimerClear: TButton;
    btnReminderSet: TButton;
    btnSoundTest: TButton;
    btnReminderClear: TButton;
    btnTimerSplit: TButton;
    ButtonPanel1: TButtonPanel;
    ChckBxCountdownSound: TCheckBox;
    CmbBxTime: TComboBox;
    DtEdtReminder: TDateEdit;
    EdtCountdownSound: TEdit;
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
    procedure btnCountdownLoadSoundClick(Sender: TObject);
    procedure btnCountdownStartClick(Sender: TObject);
    procedure btnCountdownStopClick(Sender: TObject);
    procedure btnReminderClearClick(Sender: TObject);
    procedure btnReminderSetClick(Sender: TObject);
    procedure btnSoundTestClick(Sender: TObject);
    procedure btnTimerClearClick(Sender: TObject);
    procedure btnTimerStartClick(Sender: TObject);
    procedure btnTimerStopClick(Sender: TObject);
    procedure btnTimerSplitClick(Sender: TObject);
    procedure ChckBxCountdownSoundChange(Sender: TObject);
    procedure CmbBxTimeChange(Sender: TObject);
    procedure CountdownTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure LblCountdownTimeClick(Sender: TObject);
    procedure lblfuzzyClick(Sender: TObject);
    procedure mnuItmAboutClick(Sender: TObject);
    procedure mnuItmExitClick(Sender: TObject);
    procedure mnuItmHelpClick(Sender: TObject);
    procedure mnuItmLicenseClick(Sender: TObject);
    procedure mnuItmOptionsClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure mainTimerTimer(Sender: TObject);
    procedure Panel14Click(Sender: TObject);
    procedure ReminderTimerTimer(Sender: TObject);
    procedure SpnEdtCountdownChange(Sender: TObject);
    procedure SpnEdtHourChange(Sender: TObject);
    procedure SpnEdtMinsChange(Sender: TObject);
    procedure timerTimerTimer(Sender: TObject);
  private
    procedure DisplayMessage(title : string ; message : string);
    procedure StopCountDown;
    procedure SetDefaults;
    procedure resetReminder;
  public
    countdownTicks     : integer;
    countdownSoundName : String;
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

  PageControl1.TabIndex := 0;   //  start on fuzzy time

  ft := FuzzyTime.Create;
  ft.displayFuzzy := 0;         //  start on fuzzy time

  stsBrInfo.Panels.Items[2].Text := CmbBxTime.Items.Strings[CmbBxTime.ItemIndex]
                                                                     + ' time' ;
  DtEdtReminder.Date := now;
  SpnEdtMins.Value   := MinuteOf(time);
  SpnEdtHour.Value   := HourOf(time);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  SetDefaults;
end;

procedure TfrmMain.SetDefaults;
{  called to set defaults on startup.
   Set things that can be changed in the options screen,
   to the values in the options screen.                             }
begin
  lblfuzzy.Font.Color         := OptionsRec.textColour;
  lblfuzzy.Font.Size          := 18;
  SpnEdtCountdown.Font.Size   := 8;
  LblCountdownTime.Font.Color := OptionsRec.textColour;
  LblCountdownTime.Font.Size  := 26;
  SpnEdtCountdown.Font.Size   := 12;
  lblTimer.Font.Color         := OptionsRec.textColour;
  lblTimer.Font.Size          := 26;
  lblSplitLap.Font.Color      := OptionsRec.textColour;
  lblSplitLap.Font.Size       := 26;
  lblReminder.Font.Color      := OptionsRec.textColour;
  lblReminder.Font.Size       := 18;
end;

procedure TfrmMain.DisplayMessage(title : string ; message : string);
{  display a message as a popup message
   TODO : if popup already visable, add new message to popup   }
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
  if PageControl1.TabIndex = 0 then begin                     //  fuzzy page
    stsBrInfo.Panels.Items[2].Text := '';
    stsBrInfo.Panels.Items[3].Text := '';
    stsBrInfo.Panels.Items[2].Text := CmbBxTime.Items.Strings[CmbBxTime.ItemIndex]
                                                                       + ' time' ;
  end;    //  if PageControl1.TabIndex = 0

  if PageControl1.TabIndex = 1 then begin                     //  countdown page
    if chckBxCountdownSound.Checked then begin
      stsBrInfo.Panels.Items[2].Text := 'Sound Enabled';
    end
    else begin
      stsBrInfo.Panels.Items[2].Text := 'Sound Disabled';
    end;

    if CountdownTimer.Enabled = false then
      stsBrInfo.Panels.Items[3].Text := ''
    else
      stsBrInfo.Panels.Items[3].Text := format(' Counting down from %2.d minute[s]', [SpnEdtCountdown.Value]);

  end;    //  if PageControl1.TabIndex = 1

  if PageControl1.TabIndex = 2 then begin                     //  timer page
      stsBrInfo.Panels.Items[2].Text := '';
      stsBrInfo.Panels.Items[3].Text := '';

    if timerTimer.Enabled = false then
      if btnTimerStart.Caption = 'Resume' then
        stsBrInfo.Panels.Items[3].Text := 'Timer :: Paused'
      else
        stsBrInfo.Panels.Items[2].Text := ''
    else
      stsBrInfo.Panels.Items[3].Text := 'Timer Running';

  end;    //  if PageControl1.TabIndex = 2

  if PageControl1.TabIndex = 3 then begin                    //  reminder page
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

  end;    //  if PageControl1.TabIndex = 3
end;

procedure TfrmMain.mainTimerTimer(Sender: TObject);
begin
  stsBrInfo.Panels.Items[0].Text:= TimeToStr(Time) ;
  stsBrInfo.Panels.Items[1].Text:= FormatDateTime('DD MMM YYYY', Now);

  lblfuzzy.Caption := ft.getTime;
end;

procedure TfrmMain.Panel14Click(Sender: TObject);
begin

end;

// *********************************************************** Fuzzy Time ******
procedure TfrmMain.CmbBxTimeChange(Sender: TObject);
begin

  stsBrInfo.Panels.Items[2].Text := CmbBxTime.Items.Strings[CmbBxTime.ItemIndex]
                                                                     + ' time' ;
  ft.displayFuzzy     := CmbBxTime.ItemIndex;
  lblfuzzy.Caption    := ft.getTime;

end;

// *********************************************************** Countdown *******

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

    btnCountdownStart.Caption := 'Pause'
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

procedure TfrmMain.StopCountDown;
{    Called when the timer has finished.  }
VAR
  PCharSoundName : PChar;    // PlaySound needs to be passed PChar and not a string
begin

  LblCountdownTime.Caption:= '00:00';

  btnCountdownStart.Enabled := true;       //  reset buttons
  btnCountdownStart.Caption := 'Start' ;
  btnCountdownStop.Enabled  := false;
  SpnEdtCountdown.Enabled   := true;
  CountdownTimer.Enabled    := false;

  if chckBxCountdownSound.Checked then begin
    PCharSoundname := @countdownSoundName[1];  //  convert to PCHAR - a pointer to first character
                                               //  of the string - i think.

    try                                        //  in case sound file is not found.
      PlaySound(PCharSoundname, 0, SND_ASYNC);
    except
      on EInOutError do beep ;
    end;
  end;

  stsBrInfo.Panels.Items[3].Text := ' Finished counting, now!';
  frmMain.Caption      := 'Countdown';
  application.Title    := 'Countdown';

  DisplayMessage('CountDown', 'Finished counting, now!');

  //  reset the noOfTicks, so we start the timer again without changing the time.
  //  should be okay, already validated [if time is changes will be re-validated]
  countdownTicks := SpnEdtCountdown.Value * 60;

end;

procedure TfrmMain.CountdownTimerTimer(Sender: TObject);
{ tick of countdown timer - called every 1 second        }
var
  minutes : integer;
  seconds : integer;
  message : string ;

begin
  countdownTicks := countdownTicks - 1;

  if countdownTicks = 0 then StopCountDown;

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

Procedure TfrmMain.ChckBxCountdownSoundChange(Sender: TObject);
{  Called to enable/disable the sound - from a check box           }
begin
  if chckBxCountdownSound.Checked then begin
    stsBrInfo.Panels.Items[2].Text := 'Sound Enabled';
    EdtCountdownSound.Enabled      := true;
    btnCountdownLoadSound.Enabled  := true;
    btnSoundTest.Enabled           := true;
  end
  else begin
    stsBrInfo.Panels.Items[2].Text := 'Sound Disabled';
    EdtCountdownSound.Enabled      := false;
    btnCountdownLoadSound.Enabled  := false;
    btnSoundTest.Enabled           := false;
  end;
end;

procedure TfrmMain.btnSoundTestClick(Sender: TObject);
{  Called to test the sound file                                }
VAR
  PCharSoundName : PChar;    // PlaySound needs to be passed PChar and not a string
begin
  PCharSoundname := @countdownSoundName[1];  //  convert to PCHAR - a pointer to first character

  try                                        //  in case sound file is not found.
    PlaySound(PCharSoundname, 0, SND_ASYNC);
  except
    on EInOutError do beep ;
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
  lblTimer.Caption := format('%.2d:%.2d:%.2d',[hh, mm, ss])
end;



procedure TfrmMain.btnTimerStartClick(Sender: TObject);
{ called when start button is clicked, can have three modes
      Start  :: Start timer
      Pause  :: Pause timer
      Resume :: Resume a paused timer.                            }
begin
  if btnTimerStart.Caption = 'Start' then begin
    timerStart  := time;
    timerPaused := 0;
    btnTimerStop.Enabled  := true;
    timerTimer.Enabled    := true;
    btnTimerClear.Enabled := false;
    btnTimerStart.Caption := 'Pause';
    btnTimerSplit.Enabled := true;
    lblSplitLap.Enabled   := true;
    lblSplitLap.Caption   := '00:00:00';
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
    lblTimer.Caption := '00:00:00';
    stsBrInfo.Panels.Items[3].Text := '';

    btnTimerSplit.Enabled := false;
    lblSplitLap.Enabled   := false;
    lblSplitLap.Caption   := '00:00:00';
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
begin
  lblReminder.Caption := format('Reminder set for %.2d:%.2d - %s',
           [SpnEdtHour.Value, SpnEdtMins.Value, DatetoStr(DtEdtReminder.Date)]);
  stsBrInfo.Panels.Items[3].Text := format('Reminder set for %.2d:%.2d - %s',
           [SpnEdtHour.Value, SpnEdtMins.Value, DatetoStr(DtEdtReminder.Date)]);
  ReminderTimer.Enabled    := true;
  btnReminderSet.Enabled   := false;
  DtEdtReminder.Enabled    := false;
  spnEdtHour.Enabled       := false;
  spnEdtMins.Enabled       := false;
  btnReminderClear.Enabled := true;
end;

procedure TfrmMain.btnReminderClearClick(Sender: TObject);
begin
  resetReminder;
end;

procedure TfrmMain.ReminderTimerTimer(Sender: TObject);
VAR
  RmndDt : TDateTime;
  rmndrM : String;
begin
  RmndDt := EncodeDateTime(YearOf(DtEdtReminder.Date),
                           MonthOf(DtEdtReminder.Date),
                           DayOf(DtEdtReminder.Date),
                           SpnEdtHour.Value,
                           SpnEdtMins.Value,
                           0,
                           0);
  if Now > RmndDt then begin
    ReminderTimer.Enabled  := false;
    btnReminderSet.Enabled := false;

    DisplayMessage('Reminder', rmndrM);

    resetReminder;
  end;
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

  DtEdtReminder.Date := now;
  SpnEdtMins.Value   := MinuteOf(time);
  SpnEdtHour.Value   := HourOf(time);
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
  frmOptions.ShowModal;
  SetDefaults;
end;
 procedure TfrmMain.HelpButtonClick(Sender: TObject);
begin
  frmHelp.ShowModal;
end;

 procedure TfrmMain.LblCountdownTimeClick(Sender: TObject);
 begin

 end;

 procedure TfrmMain.lblfuzzyClick(Sender: TObject);
 begin

 end;

// *****************************************************************************

End.

