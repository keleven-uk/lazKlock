unit Uklock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, Buttons, ButtonPanel, StdCtrls, Spin, PopupNotifier, ExtDlgs,
  Calendar, EditBtn, UAbout, Uhelp, UOptions, MMSystem, UFuzzyTime;

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
    ChckBxCountdownSound: TCheckBox;
    lblReminderHour: TEdit;
    EdtReminderMinute: TEdit;
    EdtCountdownSound: TEdit;
    lblfuzzy: TLabel;
    lblReminder: TLabel;
    lblTimer: TLabel;
    LblCountdownTime: TLabel;
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
    SpnEdtCountdown: TSpinEdit;
    stsBrInfo: TStatusBar;
    TbShtFuzzy: TTabSheet;
    TbShtCountdown: TTabSheet;
    TbShtTimer: TTabSheet;
    TbShtRimder: TTabSheet;
    mainTimer: TTimer;
    CountdownTimer: TTimer;
    timerTimer: TTimer;
    TgleBxFuzzy: TToggleBox;
    UpDwnReminderHour: TUpDown;
    UpDownReminderMinute: TUpDown;
    procedure btnCountdownLoadSoundClick(Sender: TObject);
    procedure btnCountdownStartClick(Sender: TObject);
    procedure btnCountdownStopClick(Sender: TObject);
    procedure btnReminderSetClick(Sender: TObject);
    procedure btnTimerClearClick(Sender: TObject);
    procedure btnTimerStartClick(Sender: TObject);
    procedure btnTimerStopClick(Sender: TObject);
    procedure ChckBxCountdownSoundChange(Sender: TObject);
    procedure CountdownTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuItmAboutClick(Sender: TObject);
    procedure mnuItmExitClick(Sender: TObject);
    procedure mnuItmHelpClick(Sender: TObject);
    procedure mnuItmOptionsClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure mainTimerTimer(Sender: TObject);
    function ItoS(i : Integer) : String ;
    procedure DisplayMessage(title : string ; message : string);
    procedure Panel9Click(Sender: TObject);
    procedure SpnEdtCountdownChange(Sender: TObject);
    procedure StopCountDown;
    procedure TgleBxFuzzyChange(Sender: TObject);
    procedure timerTimerTimer(Sender: TObject);
    procedure UpDownReminderMinuteClick(Sender: TObject; Button: TUDBtnType);
    procedure UpDwnReminderHourClick(Sender: TObject; Button: TUDBtnType);
  private
    { private declarations }
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

function TfrmMain.ItoS(i : Integer) : String;
{  Converts an integer to a two character string          }
begin
  if i < 10 then
    ItoS := '0' + IntToStr(i)
  else
    iToS := IntToStr(i);
end;

 procedure TfrmMain.DisplayMessage(title : string ; message : string);
 {  display a message as a popup message
    TODO : if popup already visable, add new message to popup   }
 begin
   PopupNotifier1.ShowAtPos(100,100) ;
   PopupNotifier1.Title   := title;
   PopupNotifier1.Text    := message;
   PopupNotifier1.Visible := true ;
 end;

 procedure TfrmMain.Panel9Click(Sender: TObject);
 begin

 end;

procedure TfrmMain.SpnEdtCountdownChange(Sender: TObject);
{    called when the time is entered - only allow 1 - 90 minutes  }
var
  val : integer;                 //  used to hold value from spin edit
                                 //  can't pass this to the function directly
begin
  val := SpnEdtCountdown.Value;

  if (val > 0) and (val <= 90) then begin
    LblCountdownTime.Caption  := ItoS(val) + ':00';
    btnCountdownStart.Enabled := true;
    countdownTicks            := val * 60;

    stsBrInfo.Panels.Items[3].Text := ' Counting down from ' + ItoS(val) + ' minute[s]';
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

    try                               //  in case sound file is not found.
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

procedure TfrmMain.TgleBxFuzzyChange(Sender: TObject);
begin
  if TgleBxFuzzy.Checked then begin
    TgleBxFuzzy.Caption := 'normal';
    ft.displayFuzzy     := true;
    lblfuzzy.Caption    := ft.getTime;
  end
  else begin
    TgleBxFuzzy.Caption := 'fuzzy';
    ft.displayFuzzy     := false;
    lblfuzzy.Caption    := ft.getTime;
  end;
end;



procedure TfrmMain.FormCreate(Sender: TObject);
{  Called at start - sets up fuzzy time and default sound file     }
begin
  countdownSoundName     := getCurrentDir + '\alarm-fatal.wav';  // default to sound file
  EdtCountdownSound.Text := ExtractFileName(countdownSoundName); //  in current working directory.
  stsBrInfo.Panels.Items[2].Text := 'Sound Enabled';

  PageControl1.TabIndex := 0;   // start on fuzzy time

  ft := FuzzyTime.Create;
  ft.displayFuzzy := true;
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
    message:= '00:' + ItoS(countdownTicks)
  else begin
    minutes := countdownTicks div 60;
    seconds := countdownTicks mod 60;
    message := ItoS(minutes) + ':' + ItoS(seconds);
  end;

  LblCountdownTime.Caption := message;
  application.Title  := message;
  frmMain.Caption    := 'Countdown :: ' + message;
end;

procedure TfrmMain.timerTimerTimer(Sender: TObject);
VAR
  hh, mm, ss, ms : word;
  timerInterval  : TDateTime;
begin
  timerInterval := timerPaused + (time - timerStart);
  DecodeTime(timerInterval, hh, mm, ss, ms);
  lblTimer.Caption := format('%.2d:%.2d:%.2d',[hh, mm, ss])
end;

procedure TfrmMain.UpDownReminderMinuteClick(Sender: TObject; Button: TUDBtnType
  );
begin
  EdtReminderMinute.Text := intToStr(UpDownReminderMinute.Position);
end;

procedure TfrmMain.UpDwnReminderHourClick(Sender: TObject; Button: TUDBtnType);
begin
     lblReminderHour.Text := intToStr(UpDwnReminderHour.Position);
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
    stsBrInfo.Panels.Items[3].Text := ' Counting down from ' + ItoS(val) + ' minute[s]';

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

procedure TfrmMain.btnReminderSetClick(Sender: TObject);
begin

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
    frmMain.Caption       := 'Timer :: Started';
  end
  else if btnTimerStart.Caption = 'Pause' then begin
    timerPaused := timerPaused + (time - timerStart);
    btnTimerStart.Caption := 'Resume';
    timerTimer.Enabled    := false;
    frmMain.Caption       := 'Timer :: Paused';
  end
  else if btnTimerStart.Caption = 'Resume' then begin
    timerStart  := time;
    btnTimerStart.Caption := 'Pause';
    timerTimer.Enabled    := true;
    frmMain.Caption       := 'Timer :: Started';
  end
end;

procedure TfrmMain.btnTimerStopClick(Sender: TObject);
begin
  btnTimerStop.Enabled  := false;
  timerTimer.Enabled    := false;
  btnTimerClear.Enabled := true;
  btnTimerStart.Caption := 'Start';
  frmMain.Caption       := 'Timer :: Stoped';
end;

procedure TfrmMain.btnTimerClearClick(Sender: TObject);
begin
    lblTimer.Caption := '00:00:00';
end;

procedure TfrmMain.ChckBxCountdownSoundChange(Sender: TObject);
{  Called to enable/disable the sound - from a check box           }
begin
  if chckBxCountdownSound.Checked then begin
    stsBrInfo.Panels.Items[2].Text := 'Sound Enabled';
    EdtCountdownSound.Enabled      := true;
  end
  else begin
    stsBrInfo.Panels.Items[2].Text := 'Sound Disabled';
    EdtCountdownSound.Enabled      := false;
  end;
end;


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

procedure TfrmMain.mnuItmOptionsClick(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TfrmMain.PageControl1Change(Sender: TObject);
begin
  if PageControl1.TabIndex = 0 then
    stsBrInfo.Panels.Items[2].Text := '';

  if PageControl1.TabIndex = 1 then begin
    if chckBxCountdownSound.Checked then begin
      stsBrInfo.Panels.Items[2].Text := 'Sound Enabled';
    end
    else begin
      stsBrInfo.Panels.Items[2].Text := 'Sound Disabled';
    end;
  end;

  if PageControl1.TabIndex = 2 then
    stsBrInfo.Panels.Items[2].Text := '';

  if PageControl1.TabIndex = 3 then
    stsBrInfo.Panels.Items[2].Text := '';
end;

procedure TfrmMain.Panel1Click(Sender: TObject);
begin

end;


procedure TfrmMain.mainTimerTimer(Sender: TObject);
begin
  stsBrInfo.Panels.Items[0].Text := TimeToStr(Time);
  stsBrInfo.Panels.Items[1].Text := DateToStr(Date);

  lblfuzzy.Caption := ft.getTime;
end;


End.

