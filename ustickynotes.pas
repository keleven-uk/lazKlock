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
    procedure createStickyForm(colour: TColor; font: TFont);
    procedure writeToFile;
    procedure Remove(pos: integer);
  public
    property stickyNotesCount: integer read _stickyNotesCount write _stickyNotesCount;

    constructor Create; overload;
    destructor Destroy; override;
    procedure new(colour: TColor; font: TFont);
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

destructor stickyNotes.Destroy;
{  run on destroy.    }
begin
  stickyNotesStore.Free;

  inherited;
end;

procedure stickyNotes.new(colour: TColor; font: TFont);
{  Creates a new Sticky Note.  This is called from the host program.
   A new sticky note is created and added to the store, then a sticky
   Note form is created and displayed on the screen.
}
begin
  createStickyNote;
  createStickyForm(colour, font);
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

procedure stickyNotes.createStickyForm(colour: TColor; font: TFont);
{  Create a sticky Note form.
   has to use passed in variable and not access stickyNotesStore,
   because this is also used to create a new Sticky Form and colour and font
   could be set from user options defaults.
}
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

  m := sf.FindChildControl('Memo') as TMemo;      //  has to succesede.
  m.Lines.Text := stickyNotesStore[stickyNotesCount].body;
  m.Color := colour;
  m.Font := font;

  sf.Show;

  end;

procedure stickyNotes.Remove(pos: integer);
{  Remove a Sticky Note from the store at a given position.
}
begin
  stickyNotesStore.Remove(pos);
  stickyNotesCount := stickyNotesCount - 1;
  updateStickyNotes;
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
          createStickyForm(stickyNotesStore[stickyNotesCount].colour,
                           stickyNotesStore[stickyNotesCount].font);
          visableCount += 1;
        end;
        stickyNotesCount := stickyNotesCount + 1;
        except
          on E: EInOutError do
          ShowMessage('ERROR : Reading Sticky Note file');
      end;  //  try
      until FileIn.Position >= fileIn.Size;
   finally
    fileIn.Free;
  end;        //  try

  //  if Sticky Note Count is greater then 0 and the visible count equals 0,
  //  then we have a file full of previously closed Sticky Notes.
  //  So, we clear everything and start again.
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
   If they are visible, their data is updated in the store,
   if they are not visible [they have been closed] their visible
   is set to false.
}
var
  sf: TCustomForm;
  f: integer;
  s: string;
  m: TMemo;
begin
//  var
//  i: integer;
//  upFormClass: string;
//begin
//  result := nil;
//  upformClass := UpperCase(formClass);
//  for i := 0 to Screen.FormCount-1 do
//  begin
//    if UpperCase(Screen.Forms[i].ClassName) = upFormClass then
//    begin
//      result := Screen.Forms[i];
//    end;
// end;


  for f := 0 to stickyNotesCount -1 do
  begin
    if stickyNotesStore[f].visable then
    begin
      s := 'STICKY' + intToStr(f);
      sf := screen.FindForm(s);

      if not assigned(sf) then
        ShowMessage('ERROR : Sticky note form not found');

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

end.


