unit UOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, EditBtn, Buttons, INIFiles;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    btnGlobalFont: TButton;
    btnFuzzyFont: TButton;
    btnCountdownFont: TButton;
    btnTimerFont: TButton;
    btnReminderFont: TButton;
    ButtonPanel1: TButtonPanel;
    ChckBxTimerMilli: TCheckBox;
    ClrBtnGlobal: TColorButton;
    ClrBtnFuzzy: TColorButton;
    ClrBtnCountdown: TColorButton;
    ClrBtnTimer: TColorButton;
    ClrBtnReminder: TColorButton;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    lblGlobalText: TLabel;
    lblFuzzyTime: TLabel;
    lblCountDown: TLabel;
    lblTimer: TLabel;
    lblReminder: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    RdGrpDefault: TRadioGroup;
    procedure btnExitClick(Sender: TObject);
    procedure btnGlobalFontClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure RdGrpDefaultClick(Sender: TObject);
  private
    procedure checkIniFile;
    procedure writeIniFile;
    procedure writeIniValues;
  public
    { public declarations }
  end; 


  {  create a options objects, that when created, will be used by main
     form. This will enable to change certain options i.e colout and font.

     TODO :: save to file i.e. xml                                          }


{                                                      ** Options Class  **                        }
  OptionsRecord = class

  Private

  Public
    DefaultTab          : Integer;                //  which tab opens by default
    Version             : string;                 //  application version
    GlobalTextColour    : TColor;                 //  Global colour of all main labels
    GlobalTextFont      : TFont;                  //  Global font of all main labels
    FuzzytextColour     : TColor;                 //  colour of fuzzy time text.
    CountDownTextColour : TColor;                 //  colour of countdown text.
    TimertextColour     : TColor;                 //  colour of timer text.
    TimerMilliSeconds   : Boolean;                //  timer to show milli seconds
    ReminderTextColour  : TColor;                 //  colour of Reminder text.

    Constructor init ;
    procedure setGlobalTextColour(c : TColor);    //  used to set global textColour
    procedure setGlobalTextFont(f : TFont);       //  used to set global text font
    procedure setFuzzyTextColour(c : TColor);     //  used to set fuzzy textColour
    procedure setCountDownTextColour(c : TColor); //  used to set countdown textColour
    procedure setTimerTextColour(c : TColor);     //  used to set timer textColour
    procedure setTimerMilliSeconds(f : Boolean);  //  used to set timer to show milli seconds
    procedure setReminderTextColour(c : TColor);  //  used to set reminder textColour
    procedure setDefaultTab(i : Integer);
  end;

{                                               ** End of Options Class  **                        }


var
  frmOptions : TfrmOptions;
  OptionsRec : OptionsRecord;              //  Options record
  IniFile    : TIniFile ;
  iniName    : String;
implementation

{$R *.lfm}

{ TfrmOptions }


{                      ********************************** Options Class methods  **                }

Constructor OptionsRecord.init;
begin
  self.DefaultTab := 0;
  self.Version    := '19';

  self.GlobalTextColour    := clBlack;
  self.GlobalTextFont      := frmOptions.Font;
  self.FuzzyTextColour     := clBlack;
  self.CountDownTextColour := clBlack;
  self.TimertextColour     := clBlack;
  self.TimerMilliSeconds   := false;
  self.ReminderTextColour  := clBlack;

end;

procedure OptionsRecord.setGlobalTextColour(c : TColor);
{  used to set textColour [global colour of all main labels]   }
begin
  self.GlobalTextColour := c;
end;

procedure OptionsRecord.setGlobalTextFont(f : TFont);
{  used to set text font [global font of all main labels]   }
begin
  self.GlobalTextFont := f;
end;

procedure OptionsRecord.setFuzzyTextColour(c : TColor);
{  used to set fuzzy textColour    }
begin
  self.FuzzyTextColour := c;
end;

procedure OptionsRecord.setCountDownTextColour(c : TColor);
{  used to set countdown textColour    }
begin
  self.CountdownTextColour := c;
end;

procedure OptionsRecord.setTimerTextColour(c : TColor);
{  used to set timer textColour   }
begin
  self.TimerTextColour := c;
end;

procedure OptionsRecord.setTimerMilliSeconds(f : Boolean);
{  used to set timer textColour   }
begin
  self.TimerMilliSeconds := f;
end;

procedure OptionsRecord.setReminderTextColour(c : TColor);
{  used to set reminder textColour }
begin
  self.ReminderTextColour := c;
end;

procedure OptionsRecord.setDefaultTab(i : Integer);
{  used to set textColour [global colour of all main labels]   }
begin
  self.DefaultTab := i;
end;


{                      *************************** End of Options Class methods **                 }

{                                               ** form procedures  **                             }
procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  iniName := 'klock.ini';

  OptionsRec := OptionsRecord.Create;  //  create options record,
                                       //  can then be used in main form

  checkIniFile;                        //  check for ini file, if not there - create

  ClrBtnGlobal.ButtonColor := OptionsRec.GlobaltextColour ;  // in case different
  ChckBxTimerMilli.Checked := OptionsRec.TimerMilliSeconds;
end;


procedure TfrmOptions.RdGrpDefaultClick(Sender: TObject);
begin
  OptionsRec.setDefaultTab(RdGrpDefault.ItemIndex);
end;

procedure TfrmOptions.btnGlobalFontClick(Sender: TObject);
begin
 if FontDialog1.Execute then begin
   lblGlobalText.Font := FontDialog1.Font;
   OptionsRec.setGlobalTextFont(FontDialog1.Font);
 end;
end;


{                                                 button pannel                }
procedure TfrmOptions.OKButtonClick(Sender: TObject);
{ if ok clicked, change options record  }
begin
  OptionsRec.setGlobalTextColour(ClrBtnGlobal.ButtonColor);
  OptionsRec.setDefaultTab(RdGrpDefault.ItemIndex);
  OptionsRec.setTimerMilliSeconds(ChckBxTimerMilli.Checked);

  writeIniFile;
end;

procedure TfrmOptions.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOptions.CancelButtonClick(Sender: TObject);
{  if cancel clicked, revert to previous options record.  }
begin
  ClrBtnGlobal.ButtonColor   := OptionsRec.GlobaltextColour ;
end;



{  ********************************************************************************** ini file **  }

procedure TfrmOptions.checkIniFile;
VAR
  code : integer;
begin
  IniFile := TINIFile.Create(iniName);

  if (FileExists(iniName)) then begin  // read ini files and populate options record.
    OptionsRec.version := (iniFile.ReadString('klock', 'Version', '0'));
    val(iniFile.ReadString('klock', 'defaultTab', '0'), OptionsRec.DefaultTab, code);

    OptionsRec.GlobaltextColour    := StringToColor(iniFile.ReadString('Labels', 'Colour', 'clBlack'));

    OptionsRec.FuzzyTextColour     := StringToColor(iniFile.ReadString('Fuzzy', 'Colour', 'clBlack'));

    OptionsRec.CountDownTextColour := StringToColor(iniFile.ReadString('CountDown', 'Colour', 'clBlack'));

    OptionsRec.TimertextColour     := StringToColor(iniFile.ReadString('Timer', 'Colour', 'clBlack'));
    OptionsRec.TimerMilliSeconds   := StrToBool(iniFile.ReadString('Timer', 'Milli', 'False'));

    OptionsRec.ReminderTextColour  := StringToColor(iniFile.ReadString('Reminder', 'Colour', 'clBlack'));

    val(iniFile.ReadString('klock', 'defaultTab', '0'), OptionsRec.DefaultTab, code);
  end
  else begin  //  ini file does not exist, create it.
      writeIniValues
  end;

  iniFile.Free;
end;

procedure TfrmOptions.writeIniFile;
{  write optione record to ini file.                                                               }
begin
  IniFile := TINIFile.Create(iniName);

  writeIniValues;

  iniFile.Free;
end;

procedure TfrmOptions.writeIniValues;
begin
  IniFile.WriteString('klock', 'Version', OptionsRec.Version);
  IniFile.WriteString('klock', 'defaultTab', IntToStr(OptionsRec.DefaultTab));

  IniFile.Writestring('Labels', 'Colour', ColorToString(OptionsRec.GlobalTextColour));

  IniFile.Writestring('Fuzzy', 'Colour', ColorToString(OptionsRec.FuzzyTextColour));

  IniFile.Writestring('CountDown', 'Colour', ColorToString(OptionsRec.CountDownTextColour));

  IniFile.Writestring('Timer', 'Colour', ColorToString(OptionsRec.TimerTextColour));
  IniFile.Writestring('Timer', 'Milli',   BoolToStr(OptionsRec.TimerMilliSeconds));

  IniFile.Writestring('Reminder', 'Colour', ColorToString(OptionsRec.ReminderTextColour));
end;

end.

