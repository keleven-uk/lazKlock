unit formStickyNote;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls;

type

   { TfrmStickyNote }

   TfrmStickyNote = class(TForm)
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    Memo: TMemo;
    MnItmClose: TMenuItem;
    MnItmFont: TMenuItem;
    MntmColour: TMenuItem;
    PopupMenu1: TPopupMenu;
    tmrSticky: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemoMouseEnter(Sender: TObject);
    procedure MemoMouseLeave(Sender: TObject);
    procedure MnItmCloseClick(Sender: TObject);
    procedure MnItmFontClick(Sender: TObject);
    procedure MntmColourClick(Sender: TObject);
    procedure tmrStickyTimer(Sender: TObject);
  private

  public

  end;

var
  frmStickyNote: TfrmStickyNote;
  bodyText: TStringList;

implementation

uses
  formklock;

{$R *.lfm}

{ TfrmStickyNote }

procedure TfrmStickyNote.FormCreate(Sender: TObject);
begin
  kLog.writeLog('formStickyNote Create : ' + name);
  tmrSticky.Enabled := true;
  AlphaBlend := true;
end;

procedure TfrmStickyNote.FormShow(Sender: TObject);
begin
  kLog.writeLog('formStickyNote show : ' + name);
end;

procedure TfrmStickyNote.MemoMouseEnter(Sender: TObject);
begin
  tmrSticky.Enabled := false;
  AlphaBlend := false;
  AlphaBlendValue := 255;
end;

procedure TfrmStickyNote.MemoMouseLeave(Sender: TObject);
begin
  AlphaBlend := true;
  tmrSticky.Enabled := true;
end;

procedure TfrmStickyNote.tmrStickyTimer(Sender: TObject);
begin
  if AlphaBlendValue > 100 then
    AlphaBlendValue := AlphaBlendValue - 1
  else
    tmrSticky.Enabled := false;
end;
//
//.............................................. Pop Up Menu ...................
//
procedure TfrmStickyNote.MnItmCloseClick(Sender: TObject);
begin
  close;
end;

procedure TfrmStickyNote.MnItmFontClick(Sender: TObject);
begin
  if fontDialog1.Execute then
    Memo.Font := fontDialog1.Font;
end;

procedure TfrmStickyNote.MntmColourClick(Sender: TObject);
begin
  if colorDialog1.Execute then
    Memo.color := colorDialog1.Color;
end;


end.

