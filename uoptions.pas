unit UOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, EditBtn, Buttons, INIFiles, UoptionUtils;

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
    Label1: TLabel;
    lblGlobalText: TLabel;
    lblFuzzyTime: TLabel;
    lblCountDown: TLabel;
    lblTimer: TLabel;
    lblReminder: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RdGrpDefault: TRadioGroup;
    SpdBtnDefault: TSpeedButton;
    procedure btnGlobalFontClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ClrBtnCountdownColorChanged(Sender: TObject);
    procedure ClrBtnFuzzyColorChanged(Sender: TObject);
    procedure ClrBtnGlobalColorChanged(Sender: TObject);
    procedure ClrBtnReminderColorChanged(Sender: TObject);
    procedure ClrBtnTimerColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure RdGrpDefaultClick(Sender: TObject);
    procedure SpdBtnDefaultClick(Sender: TObject);
  private
    procedure checkIniFile;
    procedure writeIniFile;
    procedure writeIniValues;
    procedure resetForm;
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
    FuzzyTextFont       : TFont;                  //  Global font of all main labels
    CountDownTextColour : TColor;                 //  colour of countdown text.
    CountDownTextFont   : TFont;                  //  Global font of all main labels
    TimertextColour     : TColor;                 //  colour of timer text.
    TimerTextFont       : TFont;                  //  Global font of all main labels
    TimerMilliSeconds   : Boolean;                //  timer to show milli seconds
    ReminderTextColour  : TColor;                 //  colour of Reminder text.
    ReminderTextFont    : TFont;                  //  Global font of all main labels

    Constructor init ;
    procedure setGlobalTextColour(c : TColor);    //  used to set global textColour
    procedure setGlobalTextFont(f : TFont);       //  used to set global text font
    procedure setFuzzyTextColour(c : TColor);     //  used to set fuzzy textColour
    procedure setFuzzyTextFont(f : TFont);       //  used to set global text font
    procedure setCountDownTextColour(c : TColor); //  used to set countdown textColour
    procedure setCountDownTextFont(f : TFont);       //  used to set global text font
    procedure setTimerTextColour(c : TColor);     //  used to set timer textColour
    procedure setTimerTextFont(f : TFont);       //  used to set global text font
    procedure setTimerMilliSeconds(f : Boolean);  //  used to set timer to show milli seconds
    procedure setReminderTextColour(c : TColor);  //  used to set reminder textColour
    procedure setReminderTextFont(f : TFont);       //  used to set global text font
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
  self.Version    := '20';

  self.GlobalTextColour    := clBlack;
  self.GlobalTextFont      := frmOptions.Font;
  self.FuzzyTextColour     := clNone;
  self.FuzzyTextFont       := frmOptions.Font;
  self.CountDownTextColour := clNone;
  self.CountDownTextFont   := frmOptions.Font;
  self.TimertextColour     := clNone;
  self.TimerTextFont       := frmOptions.Font;
  self.TimerMilliSeconds   := false;
  self.ReminderTextColour  := clNone;
  self.ReminderTextFont    := frmOptions.Font;

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

procedure OptionsRecord.setFuzzyTextFont(f : TFont);
{  used to set text font [global font of all main labels]   }
begin
  self.FuzzyTextFont := f;
end;

procedure OptionsRecord.setCountDownTextColour(c : TColor);
{  used to set countdown textColour    }
begin
  self.CountdownTextColour := c;
end;

procedure OptionsRecord.setCountDownTextFont(f : TFont);
{  used to set text font [global font of all main labels]   }
begin
  self.CountDownTextFont := f;
end;

procedure OptionsRecord.setTimerTextColour(c : TColor);
{  used to set timer textColour   }
begin
  self.TimerTextColour := c;
end;

procedure OptionsRecord.setTimerTextFont(f : TFont);
{  used to set text font [global font of all main labels]   }
begin
  self.TimerTextFont := f;
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

procedure OptionsRecord.setReminderTextFont(f : TFont);
{  used to set text font [global font of all main labels]   }
begin
  self.ReminderTextFont := f;
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

  OptionsRec := OptionsRecord.Create;    //  create options record,
                                         //  can then be used in main form
  OptionsRec.init;                       //  does not seem to be called automatically.

  checkIniFile;                         //  check for ini file, if not there - create

  resetForm;
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

procedure TfrmOptions.ClrBtnGlobalColorChanged(Sender: TObject);
{  set global label colour after selection, not saved until the okay button is clicked.            }
begin
  lblGlobalText.Font.Color := ClrBtnGlobal.ButtonColor;
end;

procedure TfrmOptions.ClrBtnFuzzyColorChanged(Sender: TObject);
{  set fuzzy time label colour after selection, not saved until the okay button is clicked.            }
begin
  lblFuzzyTime.Font.Color := ClrBtnFuzzy.ButtonColor;
end;

procedure TfrmOptions.ClrBtnCountdownColorChanged(Sender: TObject);
{  set countdown label colour after selection, not saved until the okay button is clicked.            }
begin
  lblCountDown.Font.Color := ClrBtnCountdown.ButtonColor;
end;

procedure TfrmOptions.ClrBtnTimerColorChanged(Sender: TObject);
{  set timer label colour after selection, not saved until the okay button is clicked.            }
begin
  lblTimer.Font.Color := ClrBtnTimer.ButtonColor;
end;

procedure TfrmOptions.ClrBtnReminderColorChanged(Sender: TObject);
{  set reminder label colour after selection, not saved until the okay button is clicked.            }
begin
  lblReminder.Font.Color := ClrBtnReminder.ButtonColor;
end;



{                                                 button pannel  getTextColour(t1 : TColor ; t2 : TColor): TColor;              }
procedure TfrmOptions.OKButtonClick(Sender: TObject);
{ if ok clicked, change options record  }
begin
  OptionsRec.setGlobalTextColour(ClrBtnGlobal.ButtonColor);
  OptionsRec.setGlobalTextFont(lblGlobalText.Font);
  OptionsRec.setFuzzyTextColour(ClrBtnFuzzy.ButtonColor);
  OptionsRec.setFuzzyTextFont(lblFuzzyTime.Font);
  OptionsRec.setCountDownTextColour(ClrBtnCountdown.ButtonColor);
  OptionsRec.setCountDownTextFont(lblCountDown.Font);
  OptionsRec.setTimerTextColour(ClrBtnTimer.ButtonColor);
  OptionsRec.setTimerTextFont(lblTimer.Font);
  OptionsRec.setReminderTextColour(ClrBtnReminder.ButtonColor);
  OptionsRec.setReminderTextFont(lblReminder.Font);
  OptionsRec.setDefaultTab(RdGrpDefault.ItemIndex);
  OptionsRec.setTimerMilliSeconds(ChckBxTimerMilli.Checked);

  writeIniFile;
end;

procedure TfrmOptions.CancelButtonClick(Sender: TObject);
{  if cancel clicked, revert to previous options record.  }
begin
  resetForm;
end;

procedure TfrmOptions.SpdBtnDefaultClick(Sender: TObject);
{  reset all text colour back to colour of glocal text.                                            }
begin
  OptionsRec.GlobalTextColour    := ClrBtnGlobal.ButtonColor;
  OptionsRec.GlobalTextFont      := frmOptions.Font;
  OptionsRec.FuzzyTextColour     := clNone;
  OptionsRec.FuzzyTextFont       := frmOptions.Font;
  OptionsRec.CountDownTextColour := clNone;
  OptionsRec.CountDownTextFont   := frmOptions.Font;
  OptionsRec.TimertextColour     := clNone;
  OptionsRec.TimerTextFont       := frmOptions.Font;
  OptionsRec.ReminderTextColour  := clNone;
  OptionsRec.ReminderTextFont    := frmOptions.Font;
  resetForm;
end;

procedure TfrmOptions.resetForm;
{  reset form to options record, used on form create, reset of default colour
   and if the cancel button is clicked.                                                            }
begin
  ClrBtnGlobal.ButtonColor    := OptionsRec.GlobalTextColour ;
  ClrBtnFuzzy.ButtonColor     := getTextColour(OptionsRec.FuzzyTextColour, OptionsRec.GlobalTextColour); ;
  ClrBtnCountdown.ButtonColor := getTextColour(OptionsRec.CountDownTextColour, OptionsRec.GlobalTextColour); ;
  ClrBtnTimer.ButtonColor     := getTextColour(OptionsRec.TimerTextColour, OptionsRec.GlobalTextColour); ;
  ClrBtnReminder.ButtonColor  := getTextColour(OptionsRec.ReminderTextColour, OptionsRec.GlobalTextColour); ;

  lblGlobalText.Font := OptionsRec.GlobalTextFont ;
  lblFuzzyTime.Font  := getTextFont(OptionsRec.FuzzyTextFont, OptionsRec.GlobalTextFont);
  lblCountDown.Font  := getTextFont(OptionsRec.CountDownTextFont, OptionsRec.GlobalTextFont);
  lblTimer.Font      := getTextFont(OptionsRec.TimerTextFont, OptionsRec.GlobalTextFont);
  lblReminder.Font   := getTextFont(OptionsRec.ReminderTextFont, OptionsRec.GlobalTextFont);

  lblGlobalText.Font.Color := OptionsRec.GlobalTextColour;
  lblFuzzyTime.Font.Color  := getTextColour(OptionsRec.FuzzyTextColour, OptionsRec.GlobalTextColour);
  lblCountDown.Font.Color  := getTextColour(OptionsRec.CountDownTextColour, OptionsRec.GlobalTextColour);
  lblTimer.Font.Color      := getTextColour(OptionsRec.TimerTextColour, OptionsRec.GlobalTextColour);
  lblReminder.Font.Color   := getTextColour(OptionsRec.ReminderTextColour, OptionsRec.GlobalTextColour);

  ChckBxTimerMilli.Checked := OptionsRec.TimerMilliSeconds;
end;



{  ********************************************************************************** ini file **  }

procedure TfrmOptions.checkIniFile;
{  if ini file exist - reads the options. if the file does not exost, create it.                   }
VAR
  code   : integer;
  defFnt : String;
begin
  defFnt  := FonttoString(frmOptions.Font);
  IniFile := TINIFile.Create(iniName);

  if (FileExists(iniName)) then begin  // read ini files and populate options record.
    val(iniFile.ReadString('klock', 'defaultTab', '0'), OptionsRec.DefaultTab, code);

    OptionsRec.GlobaltextColour    := StringToColor(iniFile.ReadString('Labels', 'Colour', 'clBlack'));
    OptionsRec.GlobalTextFont      := StringtoFont(iniFile.ReadString('labels', 'Font', defFnt));

    OptionsRec.FuzzyTextColour     := StringToColor(iniFile.ReadString('Fuzzy', 'Colour', 'clNone'));
    OptionsRec.FuzzyTextFont       := StringtoFont(iniFile.ReadString('Fuzzy', 'Font', defFnt));

    OptionsRec.CountDownTextColour := StringToColor(iniFile.ReadString('CountDown', 'Colour', 'clNone'));
    OptionsRec.CountDownTextFont   := StringtoFont(iniFile.ReadString('CountDown', 'Font', defFnt));

    OptionsRec.TimerTextColour     := StringToColor(iniFile.ReadString('Timer', 'Colour', 'clNone'));
    OptionsRec.TimerTextFont       := StringtoFont(iniFile.ReadString('Timer', 'Font', defFnt));
    OptionsRec.TimerMilliSeconds   := StrToBool(iniFile.ReadString('Timer', 'Milli', 'False'));

    OptionsRec.ReminderTextColour  := StringToColor(iniFile.ReadString('Reminder', 'Colour', 'clNone'));
    OptionsRec.ReminderTextFont    := StringtoFont(iniFile.ReadString('Reminder', 'Font', defFnt));
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
{  actually perform the writing of the ini values.                                                 }
begin
  IniFile.WriteString('klock', 'Version', OptionsRec.Version);
  IniFile.WriteString('klock', 'defaultTab', IntToStr(OptionsRec.DefaultTab));

  IniFile.Writestring('Labels', 'Colour', ColorToString(OptionsRec.GlobalTextColour));
  IniFile.Writestring('Labels', 'Font', FontToString(OptionsRec.GlobalTextFont));

  IniFile.Writestring('Fuzzy', 'Colour', ColorToString(OptionsRec.FuzzyTextColour));
  IniFile.Writestring('Fuzzy', 'Font', FontToString(OptionsRec.FuzzyTextFont));

  IniFile.Writestring('CountDown', 'Colour', ColorToString(OptionsRec.CountDownTextColour));
  IniFile.Writestring('CountDown', 'Font', FontToString(OptionsRec.CountDownTextFont));

  IniFile.Writestring('Timer', 'Colour', ColorToString(OptionsRec.TimerTextColour));
  IniFile.Writestring('Timer', 'Font', FontToString(OptionsRec.TimerTextFont));
  IniFile.Writestring('Timer', 'Milli',   BoolToStr(OptionsRec.TimerMilliSeconds));

  IniFile.Writestring('Reminder', 'Colour', ColorToString(OptionsRec.ReminderTextColour));
  IniFile.Writestring('Reminder', 'Font', FontToString(OptionsRec.ReminderTextFont));
end;

end.

