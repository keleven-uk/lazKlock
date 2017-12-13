unit formOptions;

{  This form allows the amendment of all user options.
   The user options are held in the class Options which is held in uOptions.pas.
   The main user options is created withing formKlock.pas when the application is first started.
   This unit creates a secondary options objects to hold changes made wile the form is active.
   If ok is pressed the secondary options object with the changes are copied to the main
   options and the form is closed.
   If cancel is pressed, the form is closed and the changes are lost.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ECAccordion, Forms, Controls, Dialogs, UKlockUtils,
  Graphics, StdCtrls, ButtonPanel, Buttons, ComCtrls, ExtCtrls, uOptions;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    accItemGlobal: TAccordionItem;
    accItemTime: TAccordionItem;
    accOtherStuff: TAccordionItem;
    btrOptionsReset: TButton;
    ButtonPanel1: TButtonPanel;
    ChckGrpTimeOptions: TCheckGroup;
    ChckGrpTimerSettings: TCheckGroup;
    ChckGrpGlobalOptions: TCheckGroup;
    CmbBxDefaulTtab: TComboBox;
    CmbBxDefaultTime: TComboBox;
    AcrdnOptions: TECAccordion;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    lblSettingsFileName: TLabel;
    Label3: TLabel;
    Settings: TGroupBox;
    procedure btrOptionsResetClick(Sender: TObject);
    procedure ChckGrpGlobalOptionsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimeOptionsItemClick(Sender: TObject; Index: integer);
    procedure ChckGrpTimerSettingsItemClick(Sender: TObject; Index: integer);
    procedure CmbBxDefaulTtabChange(Sender: TObject);
    procedure CmbBxDefaultTimeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private

  public

  end;


var
  frmOptions: TfrmOptions;
  userBacOptions: Options;          //  holds all the user options.

implementation

{$R *.lfm}

uses
  formklock;

{ TfrmOptions }

{....................................... Form Routines ......................................................}

procedure TfrmOptions.FormCreate(Sender: TObject);
{  Run things when the form is first created.  }
begin
  userBacOptions := Options.Create('Options_temp.xml');  //  create options file as c:\Users\<user>\AppData\Local\Stub\Options_temp.xml

  lblSettingsFileName.Caption := userOptions.optionsName;
end;

procedure TfrmOptions.FormActivate(Sender: TObject);
{  Run things when ever the form is shown.

   See ChckGrpTimeOptionsItemClick for what the check Timer group index means.
   See ChckGrpTimerSettingsItemClick for what the check Timer group index means.
   See ChckGrpGlobalOptionsItemClick for what the check Global group index means.
}
begin
  userBacOptions.Assign(userOptions);                       //  make a copy of the current user options
  // userBacOptions.writeCurrentOptions;                    //  write the copy back to disk.

  AcrdnOptions.ItemIndex := 0;                              //  Always start on Global Options.
  CmbBxDefaulTtab.ItemIndex := userOptions.defaultTab;

  CmbBxDefaultTime.Items.AddStrings(ft.fuzzyTypes);
  CmbBxDefaultTime.ItemIndex := userOptions.defaultTime;

  ChckGrpGlobalOptions.Checked[0] := userOptions.screenSave;
  ChckGrpGlobalOptions.Checked[1] := userOptions.runAtStartUp;

  ChckGrpTimeOptions.Checked[0] := userOptions.netTimeSeconds;
  ChckGrpTimeOptions.Checked[1] := userOptions.swatchCentibeats;
  ChckGrpTimeOptions.Checked[2] := userOptions.fuzzyTimeBalloon;

  ChckGrpTimerSettings.Checked[0] := userOptions.timerMilliSeconds;
end;

procedure TfrmOptions.btrOptionsResetClick(Sender: TObject);
{  reset user settings to system default

   NOTE :: maybe should have a confirm dialog.
}
begin
  userOptions.writeDefaultOptions;
end;

{....................................... Options Routines ......................................................}

procedure TfrmOptions.CmbBxDefaulTtabChange(Sender: TObject);
{  The default tab has changed, relect is user options.  }
begin
  userBacOptions.defaultTab := CmbBxDefaulTtab.ItemIndex;
end;

procedure TfrmOptions.CmbBxDefaultTimeChange(Sender: TObject);
begin
  userBacOptions.defaultTime := CmbBxDefaultTime.ItemIndex;
end;

procedure TfrmOptions.ChckGrpGlobalOptionsItemClick(Sender: TObject; Index: integer);
{  Sets the use Global options according to the state of the radio group.

   index 0 - Save Screen Position
   index 1 - Run Kock on start up - HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\run
}
begin
  userBacOptions.screenSave := ChckGrpGlobalOptions.Checked[0];
  userBacOptions.runAtStartUp := ChckGrpGlobalOptions.Checked[1];
end;

procedure TfrmOptions.ChckGrpTimeOptionsItemClick(Sender: TObject; Index: integer);
{  Sets the use Time options according to the state of the radio group.

   index 0 - New Earth Time to display seconds
         1 - SwatchTime to display Centibeats
         2 - Display Time in balloon
}
begin
  userBacOptions.netTimeSeconds := ChckGrpTimeOptions.Checked[0];
  userBacOptions.swatchCentibeats := ChckGrpTimeOptions.Checked[1];
  userBacOptions.fuzzyTimeBalloon := ChckGrpTimeOptions.Checked[2];
end;

procedure TfrmOptions.ChckGrpTimerSettingsItemClick(Sender: TObject; Index: integer);
{  Sets the use Timer options according to the state of the radio group.

   index 0 - Timer To Show MilliSeconds
}
begin
  userBacOptions.timerMilliSeconds := ChckGrpTimerSettings.Checked[0];
end;

{....................................... Pannel Buttons ......................................................}

procedure TfrmOptions.CancelButtonClick(Sender: TObject);
{  The cancel button has been pressed, so we forget any changes.
    Since the changes are made to userBacOptions - we don't need to do nowt.
}
begin
end;

procedure TfrmOptions.OKButtonClick(Sender: TObject);
{  The OK button has been pressed, Copy the amended changes to main user options.
    Write back the main user options.
}
begin
  userOptions.Assign(userBacOptions);                     //  Copy amended options to main user options.
  UserOptions.writeCurrentOptions;                        //  write back amended options back to disk.

  applyRunAtStartUp(userOptions.runAtStartUp);
end;

end.



