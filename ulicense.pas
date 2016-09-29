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
  mmoLicence.Lines.LoadFromFile('GNU GENERAL PUBLIC LICENSE.txt');
  mmoLicence.Append('');
  mmoLicence.Append('Kevin Scott (c) - 2012.');
  mmoLicence.Append('Klock Version :: 13');
end;

end.

