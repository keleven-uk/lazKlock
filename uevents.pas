unit uEvents;
{  A class to manipulate Events.
   The Event is specified in another class and only used here.
   This is adapted from the StickyNotes class and used in a similar way.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uEvent, fgl, Forms, Dialogs, Graphics, StdCtrls,
  ExtCtrls, LCLType, LCLIntf, Controls, Menus, FileUtil, dateUtils,
  formEvent;

type
  Events = class

  private
    _EventsFile      : string;
    _EventsCount     : integer;
    _stage1Days      : integer;
    _stage2Days      : integer;
    _stage3Days      : integer;
    _stage1Mess      : string;
    _stage2Mess      : string;
    _stage3Mess      : string;
    _stage1BackColour: TColor;
    _stage2BackColour: TColor;
    _stage3BackColour: TColor;
    _stage1ForeColour: TColor;
    _stage2ForeColour: TColor;
    _stage3ForeColour: TColor;

    property EventsFile: string read _EventsFile write _EventsFile;
    function checkStages(age: integer): integer;
    procedure actionEvent(pos: integer; stage: integer);
  public
    property EventsCount: integer read _EventsCount write _EventsCount;
    property stage1Days : integer read _stage1Days  write _stage1Days;
    property stage2Days : integer read _stage2Days  write _stage2Days;
    property stage3Days : integer read _stage3Days  write _stage3Days;
    property stage1Mess : string  read _stage1Mess  write _stage1Mess;
    property stage2Mess : string  read _stage2Mess  write _stage2Mess;
    property stage3Mess : string  read _stage3Mess  write _stage3Mess;
    property stage1BackColour: TColor read _stage1BackColour write _stage1BackColour;
    property stage2BackColour: TColor read _stage2BackColour write _stage2BackColour;
    property stage3BackColour: TColor read _stage3BackColour write _stage3BackColour;
    property stage1ForeColour: TColor read _stage1ForeColour write _stage1ForeColour;
    property stage2ForeColour: TColor read _stage2ForeColour write _stage2ForeColour;
    property stage3ForeColour: TColor read _stage3ForeColour write _stage3ForeColour;

    constructor Create; overload;
    destructor Destroy; override;
    procedure new(key: string; date: string; etype: integer; data: string; floating: Boolean);
    procedure amend(id:integer; itmDate: string; itmType: integer; itmData: string; itmFloat: Boolean);
    function retrieve(id: integer): Event;
    procedure restoreEvents;
    procedure updateEvents;
    procedure saveEvents;
    procedure killEvents;
    procedure Remove(pos: integer);
  end;

  //  effectively a dictionary.
  keyStore = specialize TFPGMap<integer, Event>;

const
  LEFT   = 10;
  TOP    = 10;
  WIDTH  = 320;
  HEIGHT = 30;

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


  //  set up defualts, are overridded by user options.
  stage1Days := 5;                             //  Days until stage 1 is triggered.
  stage2Days := 10;                            //  Days until stage 2 is triggered.
  stage3Days := 30;                            //  Days until stage 3 is triggered.

  stage1Mess := ' is realy soon';              //  Display message for stage 1.
  stage2Mess := ' Will very soon be here';     //  Display message for stage 2.
  stage3Mess := ' will soon be here';          //  Display message for stage 3.

  stage1BackColour := clred;                   //  Paper colour of stage 1.
  stage2BackColour := clYellow;                //  Paper colour of stage 2.
  stage3BackColour := clSkyBlue;               //  Paper colour of stage 3.

  stage1ForeColour := clBlack;                 //  Text colour of stage 1.
  stage2ForeColour := clBlack;                 //  Text colour of stage 2.
  stage3ForeColour := clBlack;                 //  Text colour of stage 3.
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
  e.stage1Ack := false;    //  if an event becomes due, it will generate a prompt for the user.
  e.stage2Ack := false;    //  There are three prompts that will be generated as the event gets nearer.
  e.stage3Ack := false;    //  If an prompt is acknowledged, these are set to true, so it wont be duplicated.
  e.dueShort  := '';
  e.dueLong   := '';

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
  e           := Event.Create(0);
  e.name      := EventsStore.Data[id].name;
  e.id        := EventsStore.Data[id].id;
  e.date      := EventsStore.Data[id].date;
  e.etype     := EventsStore.Data[id].etype;
  e.notes     := EventsStore.Data[id].notes;
  e.float     := EventsStore.Data[id].float;
  e.stage1Ack := EventsStore.Data[id].stage1Ack;
  e.stage2Ack := EventsStore.Data[id].stage2Ack;
  e.stage3Ack := EventsStore.Data[id].stage3Ack;
  e.dueShort  := EventsStore.Data[id].dueShort;
  e.dueLong   := EventsStore.Data[id].dueLong;

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

   If the event is a birthday or wedding anniversary, then the original year should
   of been used - so we substitute the current year to calculate the days due.
   If the event has already happened, then increment the year.

   todo :: needs to check for event type i.e. if type of motor is passed then the days due should be negative.
}
var
  f        : integer;
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
    dueTimef  := FormatDateTime('hh:nn:ss', DueTime);                  //  time due as formatted string.

    if eventDate > Today then                                          //  an upcoming event
      dueDays := DaysBetween(Today, eventDate);

    if eventDate < Today then                                          //  an event with an original year.
      begin
        subsDate := RecodeYear(eventDate, YearOf(Today));              //  substitutute current year.
        if subsDate < Today then subsDate := Incyear(subsDate);        //  is event before today? then inc year
        dueDays := DaysBetween(Today, subsDate);                       //  days due.
      end;

    stage := checkStages(dueDays);
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
  if (age < stage1Days) then exit(1);
  if (age < stage2Days) then exit(2);
  if (age < stage3Days) then exit(3);
  if (age > stage3Days) then exit(0);     //  event is not due.
end;

procedure Events.actionEvent(pos: integer; stage: integer);
{  Inform user of impending event.
}
VAR
  mess: string;
  fcol: TColor;                           //  Fore colour - coulour of font.
  bcol: TColor;                           //  back colour - colour of the paper [form].
  ev  : TfrmEvent;                        //  ev = event Form
  lb  : TLabel;
begin
  //  if already acknowledged then exit.
  case stage of
    3:
    begin
      if EventsStore.Data[pos].stage3Ack then exit;
      mess := EventsStore.Data[pos].name + ' ' + stage3Mess;
      bcol := stage1BackColour;
      fcol := stage1ForeColour;
      EventsStore.Data[pos].stage3Ack := true;
    end;
    2:
    begin
      if EventsStore.Data[pos].stage2Ack then exit;
      mess := EventsStore.Data[pos].name + ' ' + stage2Mess;
      bcol := stage2BackColour;
      fcol := stage3ForeColour;
      EventsStore.Data[pos].stage2Ack := true;
    end;
    1:
    begin
      if EventsStore.Data[pos].stage1Ack then exit;
      mess := EventsStore.Data[pos].name + ' ' + stage1Mess;
      bcol := stage3BackColour;
      fcol := stage3ForeColour;
      EventsStore.Data[pos].stage1Ack := true;
    end;
  end;  //  case stage of

  ev := TfrmEvent.Create(nil);

  ev.SetBounds(LEFT, TOP + (pos * HEIGHT), WIDTH, HEIGHT);
  ev.Name       := format('frmEvent_%d', [EventsStore.Data[pos].id]);
  ev.Color      := bcol;
  //ev.Font.Color := fcol;
  ev.AlphaBlend := true;

  lb            := ev.FindChildControl('lblEvent') as TLabel;
  lb.Font.Color := fcol;
  lb.Caption    := mess;

  ev.show
end;

procedure Events.killEvents;
{  When Klock cloese, kill all displayed events if any.
   Tries to find a form for all events - most will fail.  Clumbsy I know.
}
VAR
  f : integer;
  s : string;
  ev: TCustomForm;
begin
  if EventsCount = 0 then exit;

  for f := 0 to EventsCount - 1 do
  begin
    try
      s := format('frmEvent_%d', [EventsStore.Data[f].id]);
    except
      //  event form not found.
      exit;
    end;

    ev := screen.FindForm(s);

    if assigned(ev) then ev.Close;
  end;
end;

end.

