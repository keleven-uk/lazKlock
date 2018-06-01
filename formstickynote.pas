unit formStickyNote;

{  Implements a simple sticky note, which when created is displayed on the desktop.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls;

type

   { TfrmStickyNote }

   TfrmStickyNote = class(TForm)
    ColorDialog1: TColorDialog;
    FontDialog1 : TFontDialog;
    Memo        : TMemo;
    MntmSave    : TMenuItem;
    MnItmClose  : TMenuItem;
    MnItmFont   : TMenuItem;
    MntmColour  : TMenuItem;
    PopupMenu1  : TPopupMenu;
    tmrSticky   : TTimer;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemoMouseEnter(Sender: TObject);
    procedure MemoMouseLeave(Sender: TObject);
    procedure MnItmCloseClick(Sender: TObject);
    procedure MnItmFontClick(Sender: TObject);
    procedure MntmColourClick(Sender: TObject);
    procedure MntmSaveClick(Sender: TObject);
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

procedure TfrmStickyNote.FormDestroy(Sender: TObject);
{  After the Sticky Note has been destroyed [almost] update the Stick Note File.    }
begin
  stickies.updateStickyNotes;
end;

procedure TfrmStickyNote.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  kLog.writeLog('formStickyNote Close : ' + name);
end;

procedure TfrmStickyNote.FormShow(Sender: TObject);
begin
  kLog.writeLog('formStickyNote Show : ' + name);
end;

procedure TfrmStickyNote.MemoMouseEnter(Sender: TObject);
begin
  tmrSticky.Enabled := false;
  AlphaBlend        := false;
  AlphaBlendValue   := 255;
end;

procedure TfrmStickyNote.MemoMouseLeave(Sender: TObject);
begin
  AlphaBlend        := true;
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
  if fontDialog1.Execute then Memo.Font := fontDialog1.Font;
end;

procedure TfrmStickyNote.MntmColourClick(Sender: TObject);
begin
  if colorDialog1.Execute then Memo.color := colorDialog1.Color;
end;

procedure TfrmStickyNote.MntmSaveClick(Sender: TObject);
begin
  stickies.updateStickyNotes;
end;


end.

