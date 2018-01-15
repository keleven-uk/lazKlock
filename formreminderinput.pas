unit formReminderInput;

{  Gathers user input for a reminder.    }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, dateutils;

type

  { TfrmReminderInput }

  TfrmReminderInput = class(TForm)
    btnReminderExit: TButton;
    btnReminderSave: TButton;
    ChckBxReminderActive: TCheckBox;
    CmbBxReminderPeriod: TComboBox;
    CmbBxReminderType: TComboBox;
    DtEdtReminderDate: TDateEdit;
    edtReminderName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnReminderExitClick(Sender: TObject);
    procedure btnReminderSaveClick(Sender: TObject);
    procedure CmbBxReminderTypeChange(Sender: TObject);
    procedure DtEdtReminderDateChange(Sender: TObject);
    procedure edtReminderNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure checkReminderFile;
  public
    { public declarations }
  end;

var
  frmReminderInput: TfrmReminderInput;
  ReminderData: string;
  ReminderFile: TextFile;
  blType: boolean;
  blDate: boolean;
  blName: boolean;

implementation

uses
  formklock;

{$R *.lfm}

{ TfrmReminderInput }

procedure TfrmReminderInput.FormCreate(Sender: TObject);
var
  appData: string;
begin
  kLog.writeLog('FormReminderInput Create');

  appData := GetAppConfigDir(False);               //  retrieve the correct place to store .ini file
  //  calling with False - for current user only
  //  calling with True  - for all users
  //  CreateDir(appData);                            //  create said place :: Should Be Done.
  ReminderData := appData + 'kReminder.txt';       //  create .ini file path

  checkReminderFile;                             //  check for anniversary file, if not there - create
end;

procedure TfrmReminderInput.FormShow(Sender: TObject);
begin
  kLog.writeLog('FormReminderInput Show');

  edtReminderName.Text := '';          //  Set form defaults.
  DtEdtReminderDate.Date := Now;
  CmbBxReminderPeriod.ItemIndex := 0;
  CmbBxReminderType.ItemIndex := -1;
  ChckBxReminderActive.Checked := True;

  btnReminderSave.Enabled := False;             //  always start with save button not enabled.
  edtReminderName.SetFocus;

  blType := False;
  blDate := False;
  blName := False;
end;

procedure TfrmReminderInput.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfrmReminderInput.btnReminderExitClick(Sender: TObject);
begin
  Close;  //  close input form.
end;

procedure TfrmReminderInput.btnReminderSaveClick(Sender: TObject);
{  when save button is clicked, make up the comma separated string from
   the individual data components.  This string is then appended to
   the Reminder file.

   Validation should already of been done.
}
var
  str: string;
begin
  AssignFile(ReminderFile, ReminderData);
  Append(ReminderFile);

  str := format('%s, %s, %s, %s, %s',
    [edtReminderName.Text,
    DatetoStr(DtEdtReminderDate.Date),
    CmbBxReminderPeriod.Items.Strings[CmbBxReminderPeriod.ItemIndex],
    CmbBxReminderType.Items.Strings[CmbBxReminderType.ItemIndex],
    BoolToStr(ChckBxReminderActive.Checked)]);

  try
    WriteLn(ReminderFile, str);
    CloseFile(ReminderFile);
  except
    ShowMessage('  ERROR: Cannot write to Reminder File  :: ' + IntToStr(IOResult));
  end;

  Close;  //  close input form.
end;

procedure TfrmReminderInput.CmbBxReminderTypeChange(Sender: TObject);
{  only enable save button, is it looks like data has changed.
   A valid selection from the drop down box.
}
begin
  blType := True;
  if blType and blDate and blName then
    btnReminderSave.Enabled := True;

end;

procedure TfrmReminderInput.DtEdtReminderDateChange(Sender: TObject);
{  only enable save button, is it looks like data has changed.
}
begin
  blDate := True;
  if blType and blDate and blName then
    btnReminderSave.Enabled := True;
end;

procedure TfrmReminderInput.edtReminderNameChange(Sender: TObject);
{  only enable save button, is it looks like data has changed.
}
begin
  blName := True;
  if blType and blDate and blName then
    btnReminderSave.Enabled := True;
end;

procedure TfrmReminderInput.checkReminderFile;
{  if Reminder file exist - reads the options. if the file does not exist, create it.                   }

begin

  if (FileExists(ReminderData)) then
  begin  // read anniversary files.
    AssignFile(ReminderFile, ReminderData);
    Append(ReminderFile);
  end
  else
  begin  //  anniversary file does not exist, create it.
    AssignFile(ReminderFile, ReminderData);
    try
      Rewrite(ReminderFile);  // creating the file
      WriteLn(ReminderFile, '-------------------------------------------------------------------------------------------------');
      WriteLn(ReminderFile, '- A Reminder File for Klock                                                                     -');
      WriteLn(ReminderFile, '- Reminder data held in a comma separated text file in the following format                     -');
      WriteLn(ReminderFile, '- name, date, period [Yearly/Monthly], type[Wedding/Birthday/Motor/One Off/Other, active[-1/0]] -');
      WriteLn(ReminderFile, '-------------------------------------------------------------------------------------------------');
    except
      ShowMessage('  ERROR: Cannot create Reminder File  :: ' + IntToStr(IOResult));
    end;
  end;
  CloseFile(ReminderFile);
end;

end.
