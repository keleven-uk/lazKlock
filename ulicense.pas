unit uLicense;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmLicense }

  TfrmLicense = class(TForm)
    btnLicense: TButton;
    mmoLicence: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnLicenseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmLicense: TfrmLicense;

implementation

{$R *.lfm}

{ TfrmLicense }

procedure TfrmLicense.btnLicenseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmLicense.FormCreate(Sender: TObject);
begin
  mmoLicence.Append('Klock.');
  mmoLicence.Append('');
  mmoLicence.Append('');

  try
    mmoLicence.Lines.LoadFromFile('GNU GENERAL PUBLIC LICENSE.txt');
  except
    on Exception do begin
      mmoLicence.Append(' help License not found.');
      mmoLicence.Append('');
      mmoLicence.Append(' The application is issued under the GNU GENERAL PUBLIC LICENSE.');
    end;
  end;

  mmoLicence.Append('');
  mmoLicence.Append('Kevin Scott (c) - 2012.');
  mmoLicence.Append('Klock Version :: 16');
end;

end.

