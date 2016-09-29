unit UOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, EditBtn, Buttons, Calendar, INIFiles;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ButtonPanel1: TButtonPanel;
    ClrBtnLabel: TColorButton;
    ClrBtnLabel1: TColorButton;
    ClrBtnLabel2: TColorButton;
    ClrBtnLabel3: TColorButton;
    ClrBtnLabel4: TColorButton;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    procedure btnExitClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ClrBtnLabelColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
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
    Constructor init ;
    procedure setGlobalTextColour(c : TColor);    //  used to set global textColour
    procedure setFuzzyTextColour(c : TColor);     //  used to set fuzzy textColour
    procedure setCountDownTextColour(c : TColor); //  used to set countdown textColour
    procedure setTimerTextColour(c : TColor);     //  used to set timer textColour
    procedure setReminderTextColour(c : TColor);  //  used to set reminder textColour
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


{                      *************************** End of Options Class methods **                 }

{                                               ** form procedures  **                             }
procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  OptionsRec := OptionsRecord.Create;  //  create options record,
                                       //  can then be used in main form

  checkIniFile;                        //  check for ini file, if not there - create

  ClrBtnLabel.ButtonColor   := OptionsRec.GlobaltextColour ;  // in case different
end;

procedure TfrmOptions.OKButtonClick(Sender: TObject);
{ if ok clicked, change options record  }
begin
  OptionsRec.setGlobalTextColour(ClrBtnLabel.ButtonColor);
  writeIniFile;
end;

procedure TfrmOptions.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmOptions.CancelButtonClick(Sender: TObject);
{  if cancel clicked, revert to previous options record.  }
begin
  ClrBtnLabel.ButtonColor   := OptionsRec.GlobaltextColour ;
end;


procedure TfrmOptions.ClrBtnLabelColorChanged(Sender: TObject);
begin

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

  end
  else begin
    IniFile.WriteString('klock', 'version', '17');

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

