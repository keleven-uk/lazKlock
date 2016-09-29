unit Uhelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, SynMemo;

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

  mmoHelp.Append('This program is issued under the GNU General Public License.');
  mmoHelp.Append('');
  mmoHelp.Append('This program is free software: you can redistribute it and/or modify');
  mmoHelp.Append('it under the terms of the GNU General Public License as published by');
  mmoHelp.Append('the Free Software Foundation, either version 3 of the License, or');
  mmoHelp.Append('(at your option) any later version.');
  mmoHelp.Append('');
  mmoHelp.Append('This program is distributed in the hope that it will be useful,');
  mmoHelp.Append('but WITHOUT ANY WARRANTY; without even the implied warranty of');
  mmoHelp.Append('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the');
  mmoHelp.Append('GNU General Public License for more details.');
  mmoHelp.Append('');
  mmoHelp.Append('You should have received a copy of the GNU General Public License');
  mmoHelp.Append('along with this program.  If not, see <http://www.gnu.org/licenses/>.');
  mmoHelp.Append('');
  mmoHelp.Append('Kevin Scott (c) - 2012.');
  mmoHelp.Append('Klock Version :: 12');

end;

procedure TfrmHelp.mmoHelpChange(Sender: TObject);
begin

end;

procedure TfrmHelp.btnhelpExitClick(Sender: TObject);
begin
  Close;
end;

end.

