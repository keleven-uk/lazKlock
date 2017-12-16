unit formAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLVersion, ExtCtrls, ShellApi, UKlockUtils;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnAboutExit: TButton;
    btnAboutMSinfo: TButton;
    Image1: TImage;
    lblWindowsVersion: TLabel;
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
  if ShellExecute(0, nil, PChar('"msinfo32.exe"'), nil, nil, 1) < 33 then
    ShowMessage('ERROR : running MSinfo');
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  dskSize: string;
  dskFree: string;
  message: string;
  i: integer;
begin
  kLog.writeLog('FormAbout Create');

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

  lblDescription.Caption := userOptions.fileDescription;
  lblProgrammer.Caption := userOptions.legalCopyright;
  lblKlockversion.Caption := format('lazKlock Build   :: %s', [userOptions.productVersion]);
  lblFileVersion.Caption := format('lazKlock Version :: %s', [userOptions.fileVersion]);
  lblWindowsVersion.Caption := getWindowsVersion;
  lblCompanyName.Caption := userOptions.CompanyName;
  lblContact.Caption := userOptions.Comments;

  // Display the free space on drives B, C, D, E, F, where present
  for i := 2 to 10 do
  begin
    dskFree := FloatToStrF(DiskFree(i) / 1073741824, ffFixed, 3, 2);
    dskSize := FloatToStrF(DiskSize(i) / 1073741824, ffFixed, 3, 2);

    if DiskSize(i) >= 0 then
    begin
      message := format(' Disk %s : Free / Size :: %s / %s Gbytes', [Chr(i + 64), dskFree, dskSize]);
      LstBxDiscSpace.Items.Add(message);
    end;
  end;

end;

procedure TfrmAbout.tmrUpTimeTimer(Sender: TObject);
begin
  lblAppUpTime.Caption := getUpTime('Application');
  lblSysUpTime.Caption := getUpTime('System');
end;


end.
