unit uEvents;
{  A class to manipulate Events.
   The Event is specifies in another class and only used here.
   This is adapted from the StickyNotes class and used in a similar way.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uEvent, fgl, Forms, Dialogs, Graphics, StdCtrls,
  ExtCtrls, LCLType, LCLIntf, Controls, Menus, FileUtil;

type
  Events = class

  private
    _EventsFile : string;
    _EventsCount: integer;

    property EventsFile: string read _EventsFile write _EventsFile;

  public
    property EventsCount: integer read _EventsCount write _EventsCount;

    constructor Create; overload;
    destructor Destroy; override;
    procedure new(key: string; date: string; etype: integer; data: string);
    procedure amend(id:integer; itmDate: string; itmdata: string);
    function retrieve(id: integer): Event;
    procedure restoreEvents;
    procedure saveEvents;
    procedure Remove(pos: integer);
  end;

  //  effectively a dictionary.
  keyStore = specialize TFPGMap<integer, Event>;

VAR
  EventsStore: keyStore;

implementation

constructor Events.Create; overload;
{  set up some variables on create.    }
begin
  EventsFile         := GetAppConfigDir(False) + 'Event.bin';
  EventsStore        := keyStore.Create;
  EventsStore.Sorted := true;
  EventsCount        := 0;
end;

destructor Events.Destroy;
{  run on destroy.    }
begin
  EventsStore.free;

  inherited;
end;

procedure Events.new(key: string; date: string; etype: integer; data: string);
{  Creates a new Events.  This is called from the host program.
   A new Events is created and added to the store.
   The Events store is then saved to file.
}
VAR
  e: Event;                   //  Memo.
begin
  EventsCount := EventsCount + 1;
  e := Event.Create(EventsCount);

  e.name  := key;
  e.id    := EventsCount;
  e.date  := date;
  e.etype := etype;
  e.notes := data;

  EventsStore.Add(EventsCount, e);

  saveEvents;
end;

procedure Events.amend(id:integer; itmDate: string; itmdata: string);
{  Amends a event - the date abd bodt etxt can be amended.
   The event store is then saved to file.
}
begin
  EventsStore.Data[id].date  := itmdate;
  EventsStore.Data[id].notes := itmdata;

  saveEvents;
end;

function Events.retrieve(id: integer): Event;
{returns a Event out of the store at position id.    }
VAR
  e: Event;                   //  Events.
begin
  e       := Event.Create(0);
  e.name  := EventsStore.Data[id].name;
  e.id    := EventsStore.Data[id].id;
  e.date  := EventsStore.Data[id].date;
  e.etype := EventsStore.Data[id].etype;
  e.notes := EventsStore.Data[id].notes;

  result := e
end;

procedure Events.Remove(pos: integer);
{  Remove a Event from the store at a given position.    }
var
  r: integer;
begin
  r           := EventsStore.Remove(pos);
  EventsCount := EventsCount - 1;
  saveEvents;
end;

procedure Events.saveEvents;
{  Write out the contents of the Event Store to a binary file.    }
var
  fileOut: TFileStream;
  f      : integer;
begin
  fileOut := TFileStream.Create(EventsFile, fmCreate or fmShareDenyWrite);

  try
    for f := 0 to EventsCount - 1 do
    begin
      try
      EventsStore.Data[f].saveToFile(fileOut);
      except
        on E: EInOutError do
        ShowMessage('ERROR : Writing Event file');
      end;
    end;
  finally
    fileOut.Free;
  end;
end;

procedure Events.restoreEvents;
{  Read in a binary file and convert contents to memos and populate the store.
}
var
  fileIn: TFileStream;
begin
  EventsCount := 0;

  if not fileExists(EventsFile) then exit;       //  file not there then exit
  if fileSize(EventsFile) = 0   then exit;       //  empty file then exit.

  fileIn := TFileStream.Create(EventsFile, fmOpenRead or fmShareDenyWrite);

  try
    repeat
      try
        EventsStore.Add(EventsCount, Event.Create(fileIn));
        EventsCount := EventsCount + 1;
        except
          on E: EInOutError do
          ShowMessage('ERROR : Reading Event file');
      end;  //  try
      until FileIn.Position >= fileIn.Size;
   finally
    fileIn.Free;
  end;        //  try

end;

end.

