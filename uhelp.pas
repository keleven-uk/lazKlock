unit Uhelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmHelp }

  TfrmHelp = class(TForm)
    btnhelpExit: TButton;
    mmoHelp: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnhelpExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mmoHelpChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmHelp: TfrmHelp;

implementation

{$R *.lfm}

{ TfrmHelp }

procedure TfrmHelp.FormCreate(Sender: TObject);
begin
  mmoHelp.Append('Klock.');
  mmoHelp.Append('');
  mmoHelp.Append('');

  try
    mmoHelp.Lines.LoadFromFile('help.txt');
  except
    on Exception do begin
      mmoHelp.Append(' help file not found.');
      mmoHelp.Append('');
      mmoHelp.Append(' This file should include full and detailed help intructions.');
    end;
  end;

  mmoHelp.Append('');
  mmoHelp.Append('');
  mmoHelp.Append('');
  mmoHelp.Append('Kevin Scott (c) - 2012.');
  mmoHelp.Append('Klock Version :: 16');

end;

procedure TfrmHelp.mmoHelpChange(Sender: TObject);
begin

end;

procedure TfrmHelp.btnhelpExitClick(Sender: TObject);
begin
  Close;
end;

end.

