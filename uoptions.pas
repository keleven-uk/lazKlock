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
    ChckBxTimerMili: TCheckBox;
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
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure RdGrpDefaultClick(Sender: TObject);
  private
    procedure checkIniFile;
    procedure writeIniFile;
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
    GlobaltextColour    : TColor;                 //  Global colour of all main labels
    FuzzytextColour     : TColor;                 //  colour of fuzzy time text.
    CountDownTextColour : TColor;                 //  colour of countdown text.
    TimertextColour     : TColor;                 //  colour of timer text.
    ReminderTextColour  : TColor;                 //  colour of Reminder text.
    DefaultTab          : Integer;                //  which tab opens by default
    Constructor init ;
    procedure setGlobalTextColour(c : TColor);    //  used to set global textColour
    procedure setFuzzyTextColour(c : TColor);     //  used to set fuzzy textColour
    procedure setCountDownTextColour(c : TColor); //  used to set countdown textColour
    procedure setTimerTextColour(c : TColor);     //  used to set timer textColour
    procedure setReminderTextColour(c : TColor);  //  used to set reminder textColour
    procedure setDefaultTab(i : Integer);
  end;

{                                               ** End of Options Class  **                        }


var
  frmOptions: TfrmOptions;
  OptionsRec : OptionsRecord;              //  Options record
  IniFile    : TIniFile ;
implementation

{$R *.lfm}

{ TfrmOptions }


{                      ********************************** Options Class methods  **                }

Constructor OptionsRecord.init;
begin
  self.GlobaltextColour    := clBlack;
  self.FuzzytextColour     := clBlack;
  self.CountDownTextColour := clBlack;
  self.TimertextColour     := clBlack;
  self.ReminderTextColour  := clBlack;

  self.DefaultTab := 0;
end;

procedure OptionsRecord.setGlobalTextColour(c : TColor);
{  used to set textColour [global colour of all main labels]   }
begin
  self.GlobaltextColour := c;
end;

procedure OptionsRecord.setFuzzyTextColour(c : TColor);
{  used to set textColour [global colour of all main labels]   }
begin
  self.GlobaltextColour := c;
end;

procedure OptionsRecord.setCountDownTextColour(c : TColor);
{  used to set textColour [global colour of all main labels]   }
begin
  self.GlobaltextColour := c;
end;

procedure OptionsRecord.setTimerTextColour(c : TColor);
{  used to set textColour [global colour of all main labels]   }
begin
  self.GlobaltextColour := c;
end;

procedure OptionsRecord.setReminderTextColour(c : TColor);
{  used to set textColour [global colour of all main labels]   }
begin
  self.GlobaltextColour := c;
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
  OptionsRec := OptionsRecord.Create;  //  create options record,
                                       //  can then be used in main form

  checkIniFile;                        //  check for ini file, if not there - create

  ClrBtnGlobal.ButtonColor   := OptionsRec.GlobaltextColour ;  // in case different
end;

{                                                 form procedures              }

procedure TfrmOptions.RdGrpDefaultClick(Sender: TObject);
begin
  OptionsRec.setDefaultTab(RdGrpDefault.ItemIndex);
end;

{                                                 button pannel                }
procedure TfrmOptions.OKButtonClick(Sender: TObject);
{ if ok clicked, change options record  }
begin
  OptionsRec.setGlobalTextColour(ClrBtnGlobal.ButtonColor);
  OptionsRec.setDefaultTab(RdGrpDefault.ItemIndex);
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
  iniName : String;
begin
  iniName := 'klock.ini';
  IniFile := TINIFile.Create(iniName);

  if (FileExists(iniName)) then begin
    OptionsRec.GlobaltextColour    := StringToColor(iniFile.ReadString('Labels', 'Colour', 'clBlack'));
    OptionsRec.FuzzyTextColour     := StringToColor(iniFile.ReadString('Fuzzy', 'Colour', 'clBlack'));

    OptionsRec.CountDownTextColour := StringToColor(iniFile.ReadString('CountDown', 'Colour', 'clBlack'));

    OptionsRec.TimertextColour     := StringToColor(iniFile.ReadString('Timer', 'Colour', 'clBlack'));

    OptionsRec.ReminderTextColour  := StringToColor(iniFile.ReadString('Reminder', 'Colour', 'clBlack'));

    OptionsRec.DefaultTab := Integer(iniFile.ReadString('klock', 'defaultTab', '0'));
  end
  else begin
    IniFile.WriteString('klock', 'version', '17');
    IniFile.WriteString('klock', 'defaultTab', '0');

    IniFile.Writestring('Labels', 'Colour', ColorToString(OptionsRec.GlobaltextColour));
    IniFile.Writestring('Fuzzy', 'Colour', ColorToString(OptionsRec.FuzzyTextColour));
    IniFile.Writestring('Fuzzy', 'Prime', 'True');
    IniFile.Writestring('CountDown', 'Colour', ColorToString(OptionsRec.CountDownTextColour));
    IniFile.Writestring('CountDown', 'Prime', 'False');
    IniFile.Writestring('Timer', 'Colour', ColorToString(OptionsRec.TimerTextColour));
    IniFile.Writestring('Timer', 'Prime', 'False');
    IniFile.Writestring('Reminder', 'Colour', ColorToString(OptionsRec.ReminderTextColour));
    IniFile.Writestring('Reminder', 'Prime', 'false');
  end;

  iniFile.Free;
end;

procedure TfrmOptions.writeIniFile;
VAR
  iniName : String;
begin
  iniName := 'klock.ini';
  IniFile := TINIFile.Create(iniName);

  IniFile.WriteString('klock', 'version', '17');
  IniFile.WriteString('klock', 'defaultTab', IntToStr(OptionsRec.DefaultTab));
  IniFile.Writestring('Labels', 'Colour', ColorToString(OptionsRec.GlobalTextColour));
  IniFile.Writestring('Fuzzy', 'Colour', ColorToString(OptionsRec.FuzzyTextColour));
  IniFile.Writestring('Fuzzy', 'Prime', 'True');
  IniFile.Writestring('CountDown', 'Colour', ColorToString(OptionsRec.CountDownTextColour));
  IniFile.Writestring('CountDown', 'Prime', 'False');
  IniFile.Writestring('Timer', 'Colour', ColorToString(OptionsRec.TimerTextColour));
  IniFile.Writestring('Timer', 'Prime', 'False');
  IniFile.Writestring('Reminder', 'Colour', ColorToString(OptionsRec.ReminderTextColour));
  IniFile.Writestring('Reminder', 'Prime', 'false');
  iniFile.Free;
end;


end.

