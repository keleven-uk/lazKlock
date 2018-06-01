unit uMemos;

{  A class to manipulate memos.
   The Memo is specifies in another class and only used here.
   This is adapted from the StickyNotes class and used in a similar way.

   There is only three public procedures - new, updateMemos and restoreMemos.

   new - creates a new Memo and places it in the store.
         It can then be edited and/or deleted when not needed.

   amend - Amends a memo - but only the body text and the encrypt flag.

   restoreMemo - Should be run on start up.
                 It restore all memos to the memo store.

   savememos - Saves the memos to a binary file.  Called by new and amend.
               [this could be private]

   NB : No updatememo is needed, the memos are saved to file after every add.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMemo, fgl, Forms, Dialogs, Graphics, StdCtrls,
  ExtCtrls, LCLType, LCLIntf, Controls, Menus, FileUtil;

type
  Memos = class

  private
    _MemosFile : string;
    _MemosCount: integer;

    property MemosFile: string read _MemosFile write _MemosFile;

  public
    property MemosCount: integer read _MemosCount write _MemosCount;

    constructor Create; overload;
    destructor Destroy; override;
    procedure new(key: string; data: string; hide: boolean);
    procedure amend(id:integer; data: string; hide: boolean);
    function retrieve(id: integer): Memo;
    procedure restoreMemos;
    procedure saveMemos;
    procedure Remove(pos: integer);
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
  MemosFile         := GetAppConfigDir(False) + 'Memo.bin';
  MemosStore        := keyStore.Create;
  MemosStore.Sorted := true;
  MemosCount        := 0;
end;

destructor Memos.Destroy;
{  run on destroy.    }
begin
  MemosStore.free;

  inherited;
end;

procedure Memos.new(key: string; data: string; hide: boolean);
{  Creates a new memo.  This is called from the host program.
   A new memo is created and added to the store.
   The memo store is then saved to file.
}
VAR
  m: Memo;                   //  Memo.
begin
  MemosCount := MemosCount + 1;
  m := Memo.Create(MemosCount);

  m.name    := key;
  m.id      := MemosCount;
  m.body    := data;
  m.encrypt := hide;

  MemosStore.Add(MemosCount, m);

  saveMemos;
end;

procedure Memos.amend(id:integer; data: string; hide: boolean);
{  Amends a memo - but only the body text and the encrypt flag.
   The memo store is then saved to file.
}
begin
  MemosStore.Data[id].body    := data;
  MemosStore.Data[id].encrypt := hide;

  saveMemos;
end;

function Memos.retrieve(id: integer): Memo;
{returns a memo out of the store at position id.    }
VAR
  m: Memo;                   //  Memo.
begin
  m         := Memo.Create(0);
  m.name    := MemosStore.Data[id].name;
  m.body    := MemosStore.Data[id].body;
  m.encrypt := MemosStore.Data[id].encrypt;

  result := m
end;

procedure Memos.Remove(pos: integer);
{  Remove a memo from the store at a given position.    }
var
  r: integer;
begin
  r          := MemosStore.Remove(pos);
  MemosCount := MemosCount - 1;
  saveMemos;
end;

procedure Memos.saveMemos;
{  Write out the contents of the memo Store to a binary file.    }
var
  fileOut: TFileStream;
  f: integer;
begin
  fileOut := TFileStream.Create(memosFile, fmCreate or fmShareDenyWrite);

  try
    for f := 0 to MemosCount - 1 do
    begin
      try
      MemosStore.Data[f].saveToFile(fileOut);
      except
        on E: EInOutError do
        ShowMessage('ERROR : Writing Memo file');
      end;
    end;
  finally
    fileOut.Free;
  end;
end;

procedure Memos.restoreMemos;
{  Read in a binary file and convert contents to memos and populate the store.
}
var
  fileIn: TFileStream;
begin
  MemosCount := 0;

  if not fileExists(memosFile) then exit;       //  file not there then exit
  if fileSize(memosFile) = 0   then exit;       //  empty file then exit.

  fileIn := TFileStream.Create(memosFile, fmOpenRead or fmShareDenyWrite);

  try
    repeat
      try
        MemosStore.Add(MemosCount, memo.Create(fileIn));
        MemosCount := MemosCount + 1;
        except
          on E: EInOutError do
          ShowMessage('ERROR : Reading memo file');
      end;  //  try
      until FileIn.Position >= fileIn.Size;
   finally
    fileIn.Free;
  end;        //  try

end;


end.

