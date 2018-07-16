unit uEvents;
{  A class to manipulate Events.
   The Event is specifies in another class and only used here.
   This is adapted from the StickyNotes class and used in a similar way.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uEvent, fgl, Forms, Dialogs, Graphics, StdCtrls,
  ExtCtrls, LCLType, LCLIntf, Controls, Menus, FileUtil, dateUtils;

type
  Events = class

  private
    _EventsFile : string;
    _EventsCount: integer;
    _stage1     : integer;
    _stage2     : integer;
    _stage3     : integer;
    _stage1Mess : string;
    _stage2Mess : string;
    _stage3Mess : string;

    property EventsFile: string read _EventsFile write _EventsFile;
    function checkStages(age: integer): integer;
    procedure actionEvent(pos: integer; stage: integer);
  public
    property EventsCount: integer read _EventsCount write _EventsCount;
    property stage1     : integer read _stage1      write _stage1;
    property stage2     : integer read _stage2      write _stage2;
    property stage3     : integer read _stage3      write _stage3;
    property stage1Mess : string  read _stage1Mess  write _stage1Mess;
    property stage2Mess : string  read _stage2Mess  write _stage2Mess;
    property stage3Mess : string  read _stage3Mess  write _stage3Mess;

    constructor Create; overload;
    destructor Destroy; override;
    procedure new(key: string; date: string; etype: integer; data: string; floating: Boolean);
    procedure amend(id:integer; itmDate: string; itmType: integer; itmData: string; itmFloat: Boolean);
    function retrieve(id: integer): Event;
    procedure restoreEvents;
    procedure updateEvents;
    procedure saveEvents;
    procedure Remove(pos: integer);
  end;

  //  effectively a dictionary.
  keyStore = specialize TFPGMap<integer, Event>;

VAR
  EventsStore: keyStore;

implementation

uses
  formklock;

constructor Events.Create; overload;
{  set up some variables on create.    }
begin
  EventsFile         := GetAppConfigDir(False) + 'Event.bin';
  EventsStore        := keyStore.Create;
  EventsStore.Sorted := true;
  EventsCount        := 0;

  stage1 := 5;
  stage2 := 10;
  stage3 := 20;

  stage1Mess := 'is realy soon';
  stage2Mess := 'Will very soon be here';
  stage3Mess := 'will soon be here';
end;

destructor Events.Destroy;
{  run on destroy.    }
begin
  EventsStore.free;

  inherited;
end;

procedure Events.new(key: string; date: string; etype: integer; data: string; floating: Boolean);
{  Creates a new Events.  This is called from the host program.
   A new Events is created and added to the store.
   The Events store is then saved to file.
}
VAR
  e: Event;                   //  Event.
begin
  EventsCount := EventsCount + 1;

  e := Event.Create(EventsCount);

  e.name      := key;
  e.id        := EventsCount;
  e.date      := date;
  e.etype     := etype;
  e.notes     := data;
  e.float     := floating;
  e.dueShort := '';
  e.dueLong  := '';

  EventsStore.Add(EventsCount, e);

  saveEvents;
end;

procedure Events.amend(id:integer; itmDate: string; itmType: integer; itmData: string; itmFloat: Boolean);
{  Amends an event - the date and body text can be amended.
   The event store is then saved to file.
}
begin
  EventsStore.Data[id].date  := itmDate;
  EventsStore.Data[id].etype := itmType;
  EventsStore.Data[id].notes := itmData;
  EventsStore.Data[id].float := itmFloat;

  saveEvents;
end;

function Events.retrieve(id: integer): Event;
{returns a Event out of the store at position id.    }
VAR
  e: Event;                   //  Events.
begin
  e          := Event.Create(0);
  e.name     := EventsStore.Data[id].name;
  e.id       := EventsStore.Data[id].id;
  e.date     := EventsStore.Data[id].date;
  e.etype    := EventsStore.Data[id].etype;
  e.notes    := EventsStore.Data[id].notes;
  e.float    := EventsStore.Data[id].float;
  e.dueShort := EventsStore.Data[id].dueShort;
  e.dueLong  := EventsStore.Data[id].dueLong;

  result := e
end;

procedure Events.Remove(pos: integer);
{  Remove a Event from the store at a given position.    }
begin
  EventsStore.Remove(pos);
  EventsCount := EventsCount - 1;
  saveEvents;
end;

procedure Events.saveEvents;
{  Write out the contents of the Event Store to a binary file.    }
var
  fileOut: TFileStream;
  f      : integer;
begin
  if EventsCount = 0 then exit;

  fileOut := TFileStream.Create(EventsFile, fmCreate or fmShareDenyWrite);

  try
    for f := 0 to EventsCount - 1 do
    begin
      try
      EventsStore.Data[f].saveToFile(fileOut);
      except
        on E: EInOutError do
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

procedure Events.updateEvents;
{  For each event, calculate the due interval.
   dueInterval = due interval in days.

   If the the event is a birthday or wedding anniversary, then the original year shoud
   of been used - so we substitute the current year to calculate the days due.
   If the event has already happended, then increment the year.

   todo :: needs to check for event type i.e. if type of motor is passed then the days due should be negative.
}
var
  f : integer;
  eventDate: TDateTime;
  subsDate : TDateTime;
  dueTime  : TDateTime;
  dueTimef : string;
  dueDays  : integer = 0;
  stage    : integer;

begin
  if EventsCount = 0 then exit;

  for f := 0 to EventsCount - 1 do
  begin
    eventDate := StrToDate(EventsStore.Data[f].date);
    dueTime   := now - Tomorrow;                                       //  Start of tomorrow i.e. midnight tonight.
    dueTimef  := FormatDateTime('hh:nn:ss', DueTime);                  //  time due as formated string.

    if eventDate > Today then                                          //  an upcoming event
      dueDays := DaysBetween(Today, eventDate);

    if eventDate < Today then                                          //  an event with an original year.
      begin
        subsDate := RecodeYear(eventDate, YearOf(Today));              //  substitutute current year.
        if subsDate < Today then subsDate := Incyear(subsDate);        //  is event before today? then inc year
        dueDays := DaysBetween(Today, subsDate);                       //  days due.
      end;

    stage := checkStages(dueDays);
    klog.writeLog(format('event %d, duedays %d, stage %d', [f, dueDays, stage]));
    if stage <> 0 then actionEvent(f, stage);

    if dueDays = 0 then                                                //  event must be today.
      EventsStore.Data[f].dueShort := format('%s is today', [EventsStore.Data[f].name])
    else
      EventsStore.Data[f].dueShort := format('%s in %d days', [EventsStore.Data[f].name, dueDays]);

  end;  //  for f := 0 to EventsCount - 1 do

  saveEvents;
end;

function Events.checkStages(age: integer): integer;
{  Checks if the days due falls between the age bands, if it does the return
   the number of the stage.  If the event is not due, then return 0.
}
begin
  klog.writeLog('age = ' + intToStr(age));

  if (age < stage1) then exit(1);
  if (age < stage2) then exit(2);
  if (age < stage3) then exit(3);
  if (age > stage3) then exit(0);     //  event is not due.
end;

procedure Events.actionEvent(pos: integer; stage: integer);

begin
   showMessage(EventsStore.Data[pos].name + stage1Mess);
end;

end.

