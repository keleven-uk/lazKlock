unit ustickyNotes;

{  A class to manipulate sticky notes.
   The sticky note is specifies in another class and only used here.

   There is only three public procedures - new, updateStickyNotes and restoreStickyNotes.

   new - creates a new Sticky Note and places it on the screen.
         It can then be edited and/or closed when not needed.

   updateStickyNote - should be called periodically by the host program, also when closing.
                      It save all visible Sticky Notes to a binary file in the users data area.

   restoreStickyNotes - Should be called by the host program when first run.
                        It restore all visible Sticky Notes to the screen.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ustickyNote, fgl, Forms, Dialogs, Graphics, StdCtrls,
  ExtCtrls, LCLType, LCLIntf, Controls, Menus, formStickyNote, FileUtil;

type
  stickyNotes = class

  private
    _stickyNotesFile: string;
    _stickyNotesCount: integer;

    property stickyNotesFile: string read _stickyNotesFile write _stickyNotesFile;

    procedure createStickyNote;
    procedure createStickyForm;
    procedure writeToFile;
    procedure Remove(pos: integer);
  public
    property stickyNotesCount: integer read _stickyNotesCount write _stickyNotesCount;

    constructor Create; overload;
    procedure new;
    procedure restoreStickyNotes;
    procedure updateStickyNotes;
  end;

  const
    LEFT = 10;
    TOP = 10;
    WIDTH = 200;
    HEIGHT = 160;

type
  //  effectively a dictionary.
  sticky = specialize TFPGMap<integer, stickyNote>;

VAR
  stickyNotesStore: sticky;

implementation

constructor stickyNotes.Create; overload;
{  set up some variables on create.    }
begin
  stickyNotesFile := GetAppConfigDir(False) + 'StickyNotes.bin';
  stickyNotesStore := sticky.Create;
  stickyNotesStore.Sorted := true;
  stickyNotesCount := 0;
end;

procedure stickyNotes.new;
{  Creates a new Sticky Note.  This is called from the host program.
   A new sticky note is created and added to the store, then a sticky
   Note form is created and displayed on the screen.
}
begin
  createStickyNote;
  createStickyForm;
  stickyNotesCount := stickyNotesCount + 1;
end;

procedure stickyNotes.createStickyNote;
{  Create a sticky Note.    }
VAR
  sn: stickyNote;                   //  Sticky Note
begin
  sn := stickyNote.Create(stickyNotesCount);

  sn.left := LEFT;
  sn.top := TOP;
  sn.width := WIDTH;
  sn.height := HEIGHT;

  stickyNotesStore.Add(stickyNotesCount, sn);
end;

procedure stickyNotes.createStickyForm;
{  Create a sticky Note form.    }
var
  sf: TfrmStickyNote;                        //  sf = Sticky Note Form
  m: TMemo;                                  //  local TMemo.
begin
  sf := TfrmStickyNote.Create(nil);
  sf.Name := 'STICKY' + intToStr(stickyNotesCount);

  sf.SetBounds(stickyNotesStore[stickyNotesCount].left,
               stickyNotesStore[stickyNotesCount].top,
               stickyNotesStore[stickyNotesCount].width,
               stickyNotesStore[stickyNotesCount].height);
  sf.Caption := FormatDateTime('dddd MM YYYY : hh nn', now);
  sf.AlphaBlend := true;

  m := sf.FindChildControl('Memo') as TMemo;
  m.Lines.Text := stickyNotesStore[stickyNotesCount].body;
  m.Color := stickyNotesStore[stickyNotesCount].colour;
  m.Font := stickyNotesStore[stickyNotesCount].font;

  sf.Show;

  end;

procedure stickyNotes.Remove(pos: integer);
{  Remove a Sticky Note from the store at a given position.

   Seems to be a problem with either .remove or .delete.
   Both give me a list out of bounds error, even though the
   argument passed in is valid i.e. 0 < pos < stickyNotesStore.Count

   Added a visible parameter to Sticky note has a work around.
}
var
  r: integer;
begin
  for r := 0 to stickyNotesStore.Count -1 do
    showMessage(intToStr(stickyNotesStore.Keys[r]));

  r := stickyNotesStore.Remove(pos);
end;

procedure stickyNotes.writeToFile;
{  Write out the contents of the Sticky Note Store to a binary file.    }
var
  fileOut: TFileStream;
  f: integer;
begin
  fileOut := TFileStream.Create(stickyNotesFile, fmCreate or fmShareDenyWrite);
  try
    for f := 0 to stickyNotesCount -1 do
    begin
      try
      stickyNotesStore.Data[f].saveToFile(fileOut);
      except
        on E: EInOutError do
        ShowMessage('ERROR : Writing Sticky Note file');
      end;
    end;
  finally
    fileOut.Free;
  end;
end;

procedure stickyNotes.restoreStickyNotes;
{  Read in a binary file and convert contents to sticky notes and
   populate the store.
}
var
  fileIn: TFileStream;
  visableCount: integer = 0;
begin
  stickyNotesCount := 0;

  if not fileExists(stickyNotesFile) then exit;

  fileIn := TFileStream.Create(stickyNotesFile, fmOpenRead or fmShareDenyWrite);
  try
    repeat
      try
        stickyNotesStore.Add(stickyNotesCount, stickyNote.Create(fileIn));
        if stickyNotesStore[stickyNotesCount].visable then
        begin
          createStickyForm;
          visableCount += 1;
        end;
        stickyNotesCount := stickyNotesCount + 1;
        except
          on E: EInOutError do
          ShowMessage('ERROR : Writing Sticky Note file');
      end;  //  try
      until FileIn.Position >= fileIn.Size;
   finally
    fileIn.Free;
  end;        //  try

  //  if Sticky Note Count nis greater then 0 and the visibale count equals 0,
  //  then we have a file full of prevously closed Sticky Notes.
  //  So, we clear everythng and start again.
  if (stickyNotesCount > 0)  and (visableCount = 0) then
  begin
    DeleteFile(stickyNotesFile);
    stickyNotesCount := 0;
    stickyNotesStore.Clear;
  end;

end;

procedure stickyNotes.updateStickyNotes;
{  Called by the host program to save all visible Sticky Notes.
   All forms are scanned for forms that name start with STICKY.
   The if they are visible, their data is updated in the store,
   if they are not visible [they have been closed] their visible
   is set to false.
}
var
  sf: TCustomForm;
  f: integer;
  s: string;
  m: TMemo;
begin
  for f := 0 to stickyNotesCount -1 do
  begin
    if stickyNotesStore[f].visable then
    begin
      s := 'STICKY' + intToStr(f);
      sf := screen.FindForm(s);

      if sf.Visible then
      begin
        m := sf.FindChildControl('Memo') as TMemo;

        stickyNotesStore[f].body := m.Lines.text;
        stickyNotesStore[f].colour := m.Color;
        stickyNotesStore[f].font := m.Font;

        stickyNotesStore[f].left := sf.Left;
        stickyNotesStore[f].top := sf.Top;
        stickyNotesStore[f].width := sf.Width;
        stickyNotesStore[f].height := sf.Height;
      end
      else
        stickyNotesStore[f].visable := false;

    end;    //  if stickyNotesStore[f].visable then
  end;      //  for f := 0 to stickyNotesCount -1 do

  if stickyNotesCount > 0 then
    writeToFile;
end;

end.


