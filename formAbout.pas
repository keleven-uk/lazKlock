unit formAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLVersion, ExtCtrls, ShellApi, strutils;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnAboutExit: TButton;
    btnAboutMSinfo: TButton;
    Image1: TImage;
    lblAppUpTime: TLabel;
    lblSysUpTime: TLabel;
    lblApplicationUpTime: TLabel;
    lblSystemUpTime: TLabel;
    lblDescription: TLabel;
    lblKlockversion: TLabel;
    lblFileVersion: TLabel;
    lblCompanyName: TLabel;
    lblContact: TLabel;
    lblProgrammer: TLabel;
    lblProgramDescription: TLabel;
    lblProgramName: TLabel;
    lblLazarusVersion: TLabel;
    LstBxDiscSpace: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TmrUpTime: TTimer;
    procedure btnAboutExitClick(Sender: TObject);
    procedure btnAboutMSinfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TmrUpTimeTimer(Sender: TObject);
  private
    function getUpTime(system: String): String;
  public
    { public declarations }
  end; 

var
  frmAbout: TfrmAbout;

implementation

uses
  formklock;

{$R *.lfm}

{ TfrmAbout }


procedure TfrmAbout.btnAboutExitClick(Sender: TObject);
{  Close About form.  }
begin
  Close;
end;

procedure TfrmAbout.btnAboutMSinfoClick(Sender: TObject);
{  Run the extermal application MSinfo.
   If ShellExecute returns 32 or less this indicates an error, just inform the user.
}
begin
   if ShellExecute(0,nil, PChar('"msinfo32.exe"'), nil, nil, 1) < 33 then
     ShowMessage('ERROR : running MSinfo');
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
VAR
  dskSize : String;
  dskFree : String;
  message: string;
  i     : Integer;
begin
  tmrUpTime.Enabled := True;
  lblAppUpTime.Caption := getUpTime('Application');
  lblSysUpTime.Caption := getUpTime('System');

  dskFree := FloatToStrF(DiskFree(0) / 1073741824, ffFixed, 3, 2);
  dskSize := FloatToStrF(DiskSize(0) / 1073741824, ffFixed, 3, 2);

  {$ifdef WIN32}
    lblLazarusVersion.Caption := format('Built with 32 bit Lazarus Version :: %s', [lcl_version]);
  {$else}
    lblLazarusVersion.Caption := format('Built with 64 bit Lazarus Version :: %s', [lcl_version]);
  {$endif}

  lblDescription.Caption:= userOptions.fileDescription ;
  lblProgrammer.Caption := userOptions.legalCopyright;
  lblKlockversion.Caption := format('lazKlock Build   :: %s', [userOptions.productVersion]);
  lblFileVersion.Caption := format('lazKlock Version :: %s', [userOptions.fileVersion]);
  lblCompanyName.Caption := userOptions.CompanyName;
  lblContact.Caption := userOptions.Comments;

    // Display the free space on drives B, C, D, E, F, where present
  for i := 2 to 10 do
  begin
    dskFree := FloatToStrF(DiskFree(i) / 1073741824, ffFixed, 3, 2);
    dskSize := FloatToStrF(DiskSize(i) / 1073741824, ffFixed, 3, 2);

    if DiskSize(i) >= 0 then
    begin
      message := format(' Disk %s : Free / Size :: %s / %s Gbytes', [Chr(i+64), dskFree, dskSize]);
      LstBxDiscSpace.Items.Add(message);
    end;
  end;

end;

procedure TfrmAbout.tmrUpTimeTimer(Sender: TObject);
begin
  lblAppUpTime.Caption := getUpTime('Application');
  lblSysUpTime.Caption := getUpTime('System');
end;

function TfrmAbout.getUpTime(system: String): String;
{  Determines the up time of either System or Application - depending on argument S or A.

   appStartTime := GetTickCount64; needs to be run when the app starts.

   NOTE :: Windows Only - use LclIntf.GetTickCount for cross platform.

   TODO : Need to check for roll over and acount for it.
}
VAR
    noTicks: int64;
    noSeconds: Integer;
    noOfDays: Integer;
    noOfHours: Integer;
    noOfMinutes: Integer;
    noOfSeconds: Integer;
begin
  system := AnsiLowerCase(system);

  if AnsiStartsStr('s', system) then
    noTicks := GetTickCount64                         //  How long has the system been running.
  else
    noTicks := GetTickCount64 - appStartTime;         //  How long has the application been running.

  noSeconds := noTicks div 1000;                      //  1000 ticks per second.

  noOfDays := noSeconds DIV 86400;
  noSeconds := noSeconds - (noOfDays * 86400);
  noOfHours := noSeconds DIV 3600;
  noSeconds := noSeconds - (noOfHours * 3600);
  noOfMinutes := noSeconds DIV 60;
  noSeconds := noSeconds - (noOfMinutes * 60);
  NoOfSeconds := noSeconds;

  getUpTime := format('%d days : %d hours : %d mins : %d secs', [noOfDays, noOfHours, noOfMinutes, noOfSeconds]);
end;

end.

