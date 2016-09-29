unit UAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLVersion, ExtCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnAboutExit: TButton;
    lblDiskSize: TLabel;
    lblVersion: TLabel;
    lblProgrammer: TLabel;
    lblProgramDescription: TLabel;
    lblProgramName: TLabel;
    lblLazarusVersion: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnAboutExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }


procedure TfrmAbout.btnAboutExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
VAR
  dskSize : String;
  dskFree : String;
begin
  dskFree := FloatToStrF(DiskFree(0) / 1073741824, ffFixed, 3, 2);
  dskSize := FloatToStrF(DiskSize(0) / 1073741824, ffFixed, 3, 2);

  lblLazarusVersion.Caption := format('Built with Lazarus Version :: %s', [lcl_version]);
  lblVersion.Caption        := 'Klock Version :: 17';
  lblDiskSize.Caption       := format(' Disk Free / Size :: %s / %s Gbytes', [dskFree, dskSize]);
end;

end.

