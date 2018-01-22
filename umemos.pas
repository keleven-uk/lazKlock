unit uMemos;

{  A class to manipulate memos.
   The Memo is specifies in another class and only used here.
   This is adapted from the StickyNotes class and used in a similar way.

   There is only three public procedures - new, updateMemos and restoreMemos.

   new - creates a new Memo and places it in the store.
         It can then be edited and/or deleted when not needed.

   updateMemo - should be called after a new memo is created.
                It save all memos to a binary file in the users data area.

   restoreMemo - Should be run on starrt up.
                 It restore all memos to the memo store.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMemo, fgl, Forms, Dialogs, Graphics, StdCtrls,
  ExtCtrls, LCLType, LCLIntf, Controls, Menus, FileUtil;

type
  Memos = class

  private
    _MemosFile: string;
    _MemosCount: integer;

    property MemosFile: string read _MemosFile write _MemosFile;

    procedure writeToFile;
    procedure Remove(pos: integer);
  public
    property MemosCount: integer read _MemosCount write _MemosCount;

    constructor Create; overload;
    procedure new(key: string; data: string; hide: boolean);
    procedure restoreMemos;
    procedure updateMemos;
  end;


type
  //  effectively a dictionary.
  keyStore = specialize TFPGMap<integer, Memo>;

VAR
  MemosStore: keyStore;

implementation

constructor Memos.Create; overload;
{  set up some variables on create.    }
begin
  MemosFile := GetAppConfigDir(False) + 'StickyNotes.bin';
  MemosStore := keyStore.Create;
  MemosStore.Sorted := true;
  MemosCount := 0;
end;

procedure Memos.new(key: string; data: string; hide: boolean);
{  Creates a new memo.  This is called from the host program.
   A new memo is created and added to the store.
}
VAR
  m: Memo;                   //  Memo.
begin
  m := Memo.Create(MemosCount);

  m.name := key;
  m.id := MemosCount;
  m.body := data;
  m.encrypt := hide;

  MemosStore.Add(MemosCount, m);

  MemosCount := MemosCount + 1;
end;

procedure Memos.Remove(pos: integer);
{  Remove a Sticky Note from the store at a given position.

   Seems to be a problem with either .remove or .delete.
   Both give me a list out of bounds error, even though the
   argument passed in is valid i.e. 0 < pos < stickyNotesStore.Count

   Added a visible parameter to Sticky note has a work around.
}
var
  r: integer;
begin
  for r := 0 to MemosStore.Count -1 do
    showMessage(intToStr(MemosStore.Keys[r]));

  r := MemosStore.Remove(pos);
end;

procedure Memos.writeToFile;
{  Write out the contents of the Sticky Note Store to a binary file.    }
var
  fileOut: TFileStream;
  f: integer;
begin
  fileOut := TFileStream.Create(memosFile, fmCreate or fmShareDenyWrite);
  try
    for f := 0 to MemosCount -1 do
    begin
      try
      MemosStore.Data[f].saveToFile(fileOut);
      except
        on E: EInOutError do
        ShowMessage('ERROR : Writing Sticky Note file');
      end;
    end;
  finally
    fileOut.Free;
  end;
end;

procedure Memos.restoreMemos;
{  Read in a binary file and convert contents to sticky notes and
   populate the store.
}

begin


end;

procedure Memos.updateMemos;
{  Called by the host program to save all visible Sticky Notes.
   All forms are scanned for forms that name start with STICKY.
   The if they are visible, their data is updated in the store,
   if they are not visible [they have been closed] their visible
   is set to false.
}

begin

end;


end.

